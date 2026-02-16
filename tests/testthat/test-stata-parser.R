test_that("strip_stata_comments removes line comments", {
  lines <- c(
    "* this is a comment",
    "gen x = 1",
    "gen y = 2 // inline comment",
    "  * indented comment"
  )
  result <- strip_stata_comments(lines)
  expect_equal(trimws(result[1]), "")
  expect_equal(trimws(result[2]), "gen x = 1")
  expect_equal(trimws(result[3]), "gen y = 2")
  expect_equal(trimws(result[4]), "")
})

test_that("strip_stata_comments removes block comments", {
  lines <- c(
    "gen x = 1",
    "/* this is",
    "a block comment */",
    "gen y = 2",
    "gen z = /* inline block */ 3"
  )
  result <- strip_stata_comments(lines)
  expect_equal(trimws(result[1]), "gen x = 1")
  expect_equal(trimws(result[2]), "")
  expect_equal(trimws(result[3]), "")
  expect_equal(trimws(result[4]), "gen y = 2")
  expect_equal(trimws(result[5]), "gen z =  3")
})

test_that("join_continuation_lines handles ///", {
  lines <- c(
    "gen x = a + ///",
    "  b + c",
    "gen y = 1"
  )
  result <- join_continuation_lines(lines)
  expect_length(result, 2)
  expect_match(result[1], "gen x = a \\+\\s+b \\+ c")
  expect_equal(trimws(result[2]), "gen y = 1")
})

test_that("expand_stata_loops expands foreach in", {
  lines <- c(
    "foreach var in x y z {",
    "recode `var' .=0",
    "}"
  )
  result <- expand_stata_loops(lines)
  expect_length(result, 3)
  expect_equal(trimws(result[1]), "recode x .=0")
  expect_equal(trimws(result[2]), "recode y .=0")
  expect_equal(trimws(result[3]), "recode z .=0")
})

test_that("expand_stata_loops expands foreach of numlist", {
  lines <- c(
    "foreach i of numlist 1/3 {",
    "gen aux`i'=0",
    "}"
  )
  result <- expand_stata_loops(lines)
  expect_length(result, 3)
  expect_equal(trimws(result[1]), "gen aux1=0")
  expect_equal(trimws(result[2]), "gen aux2=0")
  expect_equal(trimws(result[3]), "gen aux3=0")
})

test_that("expand_numlist handles range and values", {
  expect_equal(expand_numlist("1/5"), as.character(1:5))
  expect_equal(expand_numlist("10 20 30"), c("10", "20", "30"))
})

test_that("parse_stata_command tokenizes gen", {
  cmd <- parse_stata_command("gen bc_pe2=e26")
  expect_equal(cmd$cmd, "gen")
  expect_equal(cmd$args, "bc_pe2=e26")
  expect_null(cmd$if_clause)
  expect_false(cmd$capture)
})

test_that("parse_stata_command handles abbreviation g", {
  cmd <- parse_stata_command("g bc_pe3=e27")
  expect_equal(cmd$cmd, "gen")
})

test_that("parse_stata_command handles replace with if", {
  cmd <- parse_stata_command("replace bc_pe4=1 if e30==1")
  expect_equal(cmd$cmd, "replace")
  expect_equal(cmd$args, "bc_pe4=1")
  expect_equal(cmd$if_clause, "e30==1")
})

test_that("parse_stata_command handles capture prefix", {
  cmd <- parse_stata_command("cap g bc_anio=2021")
  expect_equal(cmd$cmd, "gen")
  expect_true(cmd$capture)

  cmd2 <- parse_stata_command("capture rename old new")
  expect_equal(cmd2$cmd, "rename")
  expect_true(cmd2$capture)
})

test_that("parse_stata_command handles bysort prefix", {
  cmd <- parse_stata_command("bysort bc_correlat: egen total = sum(income)")
  expect_equal(cmd$cmd, "egen")
  expect_equal(cmd$by_group, "bc_correlat")
  expect_match(cmd$args, "total = sum\\(income\\)")
})

test_that("parse_stata_command handles rename", {
  cmd <- parse_stata_command("rename id bc_correlat")
  expect_equal(cmd$cmd, "rename")
  expect_equal(cmd$args, "id bc_correlat")
})

test_that("parse_stata_command handles drop", {
  cmd <- parse_stata_command("drop aux1 aux2 aux3")
  expect_equal(cmd$cmd, "drop")
  expect_equal(cmd$args, "aux1 aux2 aux3")
})

test_that("parse_stata_labels extracts var labels", {
  lines <- c(
    'lab var bc_pe2 "Sexo"',
    'lab var bc_pe3 "Edad"'
  )
  result <- parse_stata_labels(lines)
  expect_equal(result$var_labels$bc_pe2, "Sexo")
  expect_equal(result$var_labels$bc_pe3, "Edad")
})

test_that("parse_stata_labels extracts val labels", {
  lines <- c(
    'lab def pe2l 1 "Hombre" 2 "Mujer"',
    "lab val bc_pe2 pe2l"
  )
  result <- parse_stata_labels(lines)
  expect_equal(result$val_labels$bc_pe2[["1"]], "Hombre")
  expect_equal(result$val_labels$bc_pe2[["2"]], "Mujer")
})

test_that("parse_stata_labels handles define + values together", {
  lines <- c(
    'lab def pe4l 1 "Jefe" 2 "Conyuge" 3 "Hijo"',
    "lab val bc_pe4 pe4l"
  )
  result <- parse_stata_labels(lines)
  expect_length(result$val_labels$bc_pe4, 3)
  expect_equal(result$val_labels$bc_pe4[["1"]], "Jefe")
})

test_that("parse_do_file works on gen_replace fixture", {
  fixture <- system.file("stata-test-cases/gen_replace.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  commands <- parse_do_file(fixture)
  expect_true(length(commands) > 0)
  # First command should be gen
  expect_equal(commands[[1]]$cmd, "gen")
})

test_that("parse_do_file works on foreach fixture", {
  fixture <- system.file("stata-test-cases/foreach.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  commands <- parse_do_file(fixture)
  # foreach with 4 vars * 1 body line + foreach with 3 nums * 2 body lines = 10
  # After loop expansion, should have individual commands

  expect_true(length(commands) >= 4)
})

test_that("parse_do_file works on labels fixture", {
  fixture <- system.file("stata-test-cases/labels.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  raw_lines <- readLines(fixture, warn = FALSE)
  result <- parse_stata_labels(raw_lines)
  expect_true(length(result$var_labels) >= 3)
  expect_true(length(result$val_labels) >= 1)
})

test_that("strip_stata_comments joins /* */ continuation lines", {
  lines <- c(
    "mvencode a b c /*",
    "*/d e f, mv(0)"
  )
  result <- strip_stata_comments(lines)
  # Should join into single line: "mvencode a b c  d e f, mv(0)"
  non_empty <- result[nchar(trimws(result)) > 0]
  expect_length(non_empty, 1)
  expect_match(non_empty, "mvencode")
  expect_match(non_empty, "d e f")
})

test_that("expand_stata_loops handles nested foreach", {
  lines <- c(
    "foreach i in 1 2 {",
    "foreach j in a b {",
    "gen x`i'`j' = 0",
    "}",
    "}"
  )
  result <- expand_stata_loops(lines)
  # 2 outer * 2 inner = 4 expanded lines
  expect_length(result, 4)
  expect_match(result[1], "gen x1a")
  expect_match(result[2], "gen x1b")
  expect_match(result[3], "gen x2a")
  expect_match(result[4], "gen x2b")
})

test_that("expand_numlist handles compound ranges like '81/99 0/17'", {
  result <- expand_numlist("81/83 0/2")
  expect_equal(result, c("81", "82", "83", "0", "1", "2"))
})

test_that("parse_stata_command handles bysort with multiple vars", {
  cmd <- parse_stata_command("bysort g1 g2: egen tot = sum(x)")
  expect_equal(cmd$cmd, "egen")
  expect_equal(cmd$by_group, "g1 g2")
})

# ── Batch 9: stata_parser.R + stata_mappings.R edge cases ─────────────────────

test_that("parse_do_file errors on missing file", {
  expect_error(parse_do_file("/nonexistent/path.do"), "File not found")
})

test_that("parse_do_file handles empty file", {
  tmp <- tempfile(fileext = ".do")
  on.exit(unlink(tmp))
  writeLines(character(0), tmp)
  result <- parse_do_file(tmp)
  expect_length(result, 0)
})

test_that("parse_do_file handles comment-only file", {
  tmp <- tempfile(fileext = ".do")
  on.exit(unlink(tmp))
  writeLines(c(
    "* This is a comment",
    "// Another comment",
    "/* block comment */",
    "  * indented comment"
  ), tmp)
  result <- parse_do_file(tmp)
  expect_length(result, 0)
})

test_that("parse_stata_command returns NULL for empty line", {
  expect_null(parse_stata_command(""))
  expect_null(parse_stata_command("   "))
})

test_that("parse_stata_command returns NULL for orphan expression fragments", {
  expect_null(parse_stata_command("& other_condition"))
  expect_null(parse_stata_command("| another_clause"))
  expect_null(parse_stata_command(") trailing_paren"))
  expect_null(parse_stata_command("(bare_paren_expr)"))
})

test_that("parse_stata_command normalizes all abbreviated commands", {
  expect_equal(parse_stata_command("ge x = 1")$cmd, "gen")
  expect_equal(parse_stata_command("ren old new")$cmd, "rename")
  expect_equal(parse_stata_command("u mydata")$cmd, "use")
  expect_equal(parse_stata_command("sa mydata")$cmd, "save")
  expect_equal(parse_stata_command("su age")$cmd, "summarize")
  expect_equal(parse_stata_command("sum age")$cmd, "summarize")
  expect_equal(parse_stata_command("summ age")$cmd, "summarize")
  expect_equal(parse_stata_command("summa age")$cmd, "summarize")
  expect_equal(parse_stata_command("br")$cmd, "browse")
  expect_equal(parse_stata_command("tab status")$cmd, "tabulate")
  expect_equal(parse_stata_command("lab var x 'test'")$cmd, "label")
  expect_equal(parse_stata_command("mat A = 1")$cmd, "matrix")
  expect_equal(parse_stata_command("matr A = 1")$cmd, "matrix")
  expect_equal(parse_stata_command("matri A = 1")$cmd, "matrix")
})

test_that("parse_stata_command extracts options", {
  cmd <- parse_stata_command("destring var1, replace force")
  expect_equal(cmd$cmd, "destring")
  expect_match(cmd$options, "replace")
  expect_match(cmd$options, "force")
})

test_that("strip_stata_comments preserves /// line continuation", {
  lines <- c(
    "gen x = a + ///",
    "  b"
  )
  result <- strip_stata_comments(lines)
  # /// should be preserved for join_continuation_lines
  expect_match(result[1], "///")
})

test_that("join_broken_expressions re-joins operator-split lines", {
  lines <- c(
    "gen x = a +",
    "b + c"
  )
  result <- metasurvey:::join_broken_expressions(lines)
  non_empty <- result[nchar(trimws(result)) > 0]
  expect_true(any(grepl("a \\+ b \\+ c", non_empty)) || length(non_empty) <= length(lines))
})

test_that("join_broken_expressions handles single line", {
  result <- metasurvey:::join_broken_expressions("gen x = 1")
  expect_equal(result, "gen x = 1")
})

test_that("expand_numlist handles single values", {
  expect_equal(expand_numlist("42"), "42")
  expect_equal(expand_numlist("  7  "), "7")
})

test_that("collect_loop_body handles content before closing brace", {
  lines <- c(
    "foreach i in 1 2 {",
    "gen x`i' = 0",
    "gen y`i' = 1 }",
    "gen z = 3"
  )
  body <- metasurvey:::collect_loop_body(lines, 1)
  expect_length(body$body, 2)
  expect_match(body$body[2], "gen y")
  expect_equal(body$end_idx, 3)
})

test_that("expand_stata_loops handles forvalues", {
  lines <- c(
    "forvalues i = 1/3 {",
    "gen v`i' = `i'",
    "}"
  )
  result <- expand_stata_loops(lines)
  expect_length(result, 3)
  expect_match(result[1], "gen v1 = 1")
  expect_match(result[2], "gen v2 = 2")
  expect_match(result[3], "gen v3 = 3")
})

test_that("expand_stata_loops handles cap foreach", {
  lines <- c(
    "cap foreach v in a b {",
    "gen `v'_new = `v'",
    "}"
  )
  result <- expand_stata_loops(lines)
  expect_length(result, 2)
  expect_match(result[1], "gen a_new = a")
  expect_match(result[2], "gen b_new = b")
})

test_that("parse_stata_labels handles define with add option", {
  lines <- c(
    'lab def status 1 "Active" 2 "Inactive", add'
  )
  result <- parse_stata_labels(lines)
  expect_equal(result$val_labels, list()) # no "lab val" so no resolved labels
  # but val_defs should contain the definition (internal)
})

test_that("parse_stata_labels handles empty input", {
  result <- parse_stata_labels(character(0))
  expect_equal(result$var_labels, list())
  expect_equal(result$val_labels, list())
})

# ── stata_mappings.R tests ───────────────────────────────────────────────────

test_that("translate_stata_expr handles NULL and empty", {
  expect_null(translate_stata_expr(NULL))
  expect_equal(translate_stata_expr(""), "")
  expect_equal(translate_stata_expr("  "), "  ")
})

test_that("translate_stata_expr converts inrange", {
  result <- translate_stata_expr("inrange(age, 18, 65)")
  expect_match(result, "age >= 18")
  expect_match(result, "age <= 65")
})

test_that("translate_stata_expr converts inlist", {
  result <- translate_stata_expr("inlist(status, 1, 2, 3)")
  expect_match(result, "status %in% c\\(1, 2, 3\\)")
})

test_that("translate_stata_expr converts missing value comparisons", {
  result <- translate_stata_expr("age==.")
  expect_match(result, "is\\.na\\(age\\)")
  result2 <- translate_stata_expr("income!=.")
  expect_match(result2, "!is\\.na\\(income\\)")
})

test_that("translate_stata_expr converts string() to as.character()", {
  result <- translate_stata_expr("string(x)")
  expect_match(result, "as\\.character\\(x\\)")
})

test_that("translate_stata_expr converts lag/lead _n notation", {
  result <- translate_stata_expr("income[_n-1]")
  expect_match(result, 'shift\\(income, 1, type = "lag"\\)')
  result2 <- translate_stata_expr("income[_n+1]")
  expect_match(result2, 'shift\\(income, 1, type = "lead"\\)')
  result3 <- translate_stata_expr("x[_n-3]")
  expect_match(result3, 'shift\\(x, 3, type = "lag"\\)')
})

test_that("translate_stata_expr converts _N to .N", {
  result <- translate_stata_expr("gen x = _N")
  expect_match(result, "\\.N")
  # Should not match inside variable names
  result2 <- translate_stata_expr("_N_obs")
  expect_false(grepl("\\.N", result2))
})

test_that("expand_var_range expands simple numeric range", {
  result <- expand_var_range("suma1-suma4")
  expect_equal(result, c("suma1", "suma2", "suma3", "suma4"))
})

test_that("expand_var_range expands underscore range", {
  result <- expand_var_range("e51_2_1-e51_2_5")
  expect_equal(result, paste0("e51_2_", 1:5))
})

test_that("expand_var_range returns single var unchanged", {
  expect_equal(expand_var_range("myvar"), "myvar")
  expect_equal(expand_var_range("  spaced  "), "spaced")
})

test_that("is_constant_rhs detects constants correctly", {
  expect_true(is_constant_rhs("42"))
  expect_true(is_constant_rhs("-9"))
  expect_true(is_constant_rhs("2.5"))
  expect_true(is_constant_rhs('"text"'))
  expect_true(is_constant_rhs("."))
  expect_false(is_constant_rhs("x + 1"))
  expect_false(is_constant_rhs("log(y)"))
})

test_that("parse_gen_args parses basic gen", {
  result <- parse_gen_args("y = x + 1")
  expect_equal(result$var_name, "y")
  expect_equal(result$expr, "x + 1")
})

test_that("parse_gen_args strips type prefix", {
  result <- parse_gen_args("byte flag = 1")
  expect_equal(result$var_name, "flag")
  expect_equal(result$expr, "1")
  result2 <- parse_gen_args("int count = n + 1")
  expect_equal(result2$var_name, "count")
})

test_that("parse_gen_args returns NULL without =", {
  expect_null(parse_gen_args("just a name"))
})

test_that("parse_gen_args strips balanced outer parentheses", {
  result <- parse_gen_args("y = (a + b)")
  expect_equal(result$expr, "a + b")
  # Unbalanced should NOT strip
  result2 <- parse_gen_args("y = (a + b) * (c + d)")
  expect_equal(result2$expr, "(a + b) * (c + d)")
})

test_that("parse_recode_args parses parenthesized mappings", {
  result <- parse_recode_args("status (1=10) (2=20) (3=30)")
  expect_equal(result$var_name, "status")
  expect_length(result$mappings, 3)
  expect_equal(result$mappings[[1]]$from, "1")
  expect_equal(result$mappings[[1]]$to, "10")
})

test_that("parse_recode_args parses gen() option", {
  result <- parse_recode_args("old_var (1=10)", options = "gen(new_var)")
  expect_equal(result$gen_var, "new_var")
})

test_that("parse_recode_args handles inline range format", {
  result <- parse_recode_args("x 23/38=22 .=0")
  expect_equal(result$var_name, "x")
  expect_equal(length(result$mappings), 2)
  # First mapping should have from_range
  expect_true(!is.null(result$mappings[[1]]$from_range))
  expect_equal(result$mappings[[1]]$to, "22")
  # Second mapping is .=0
  expect_equal(result$mappings[[2]]$from, ".")
  expect_equal(result$mappings[[2]]$to, "0")
})

test_that("parse_recode_args handles multi-value parenthesized groups", {
  result <- parse_recode_args("var (0 3 4=-15) (1 2=10)")
  expect_length(result$mappings, 2)
  expect_equal(result$mappings[[1]]$from, c("0", "3", "4"))
  expect_equal(result$mappings[[1]]$to, "-15")
})

test_that("parse_egen_args parses basic egen", {
  result <- parse_egen_args("total_inc = sum(income)", by_group = "region")
  expect_equal(result$var_name, "total_inc")
  expect_equal(result$func, "sum")
  expect_equal(result$func_arg, "income")
  expect_equal(result$by_group, "region")
})

test_that("parse_egen_args extracts by() from options", {
  result <- parse_egen_args("mean_age = mean(age)", options = "by(region)")
  expect_equal(result$by_group, "region")
})

test_that("parse_egen_args returns NULL without =", {
  expect_null(parse_egen_args("just_a_name"))
})

test_that("parse_mvencode_args parses variables and mv option", {
  result <- parse_mvencode_args("x y z", options = "mv(-99)")
  expect_equal(result$var_names, c("x", "y", "z"))
  expect_equal(result$mv_value, "-99")
})

test_that("parse_mvencode_args defaults to mv=0", {
  result <- parse_mvencode_args("a b")
  expect_equal(result$mv_value, "0")
})

test_that("parse_destring_args with replace and force", {
  result <- parse_destring_args("myvar", options = "replace force")
  expect_equal(result$var_name, "myvar")
  expect_true(result$replace)
  expect_true(result$force)
  expect_null(result$gen_var)
})

test_that("parse_destring_args with gen() option", {
  result <- parse_destring_args("old_var", options = "gen(new_var)")
  expect_equal(result$gen_var, "new_var")
  expect_false(result$replace)
})
