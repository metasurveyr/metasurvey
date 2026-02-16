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
    'lab val bc_pe2 pe2l'
  )
  result <- parse_stata_labels(lines)
  expect_equal(result$val_labels$bc_pe2[["1"]], "Hombre")
  expect_equal(result$val_labels$bc_pe2[["2"]], "Mujer")
})

test_that("parse_stata_labels handles define + values together", {
  lines <- c(
    'lab def pe4l 1 "Jefe" 2 "Conyuge" 3 "Hijo"',
    'lab val bc_pe4 pe4l'
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
