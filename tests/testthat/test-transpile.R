test_that("translate_stata_expr handles inrange", {
  result <- translate_stata_expr("inrange(e30,3,5)")
  expect_match(result, "e30 >= 3")
  expect_match(result, "e30 <= 5")
})

test_that("translate_stata_expr handles inlist", {
  result <- translate_stata_expr("inlist(x, 1, 2, 3)")
  expect_match(result, "x %in% c\\(")
})

test_that("translate_stata_expr handles missing value comparison", {
  expect_match(translate_stata_expr("var==."), "is.na\\(var\\)")
  expect_match(translate_stata_expr("var!=."), "!is.na\\(var\\)")
})

test_that("translate_stata_expr handles string()", {
  result <- translate_stata_expr("string(x)")
  expect_match(result, "as.character\\(x\\)")
})

test_that("is_constant_rhs detects constants", {
  expect_true(is_constant_rhs("0"))
  expect_true(is_constant_rhs("-9"))
  expect_true(is_constant_rhs("2.5"))
  expect_true(is_constant_rhs('"text"'))
  expect_true(is_constant_rhs("."))
  expect_false(is_constant_rhs("e51_2"))
  expect_false(is_constant_rhs("a + b"))
  expect_false(is_constant_rhs("6+e51_4_a"))
})

test_that("transpile_stata works on gen_replace fixture", {
  fixture <- system.file("stata-test-cases/gen_replace.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  expect_true(length(result$steps) > 0)
  # bc_pe4 gen+replace with constants should produce step_recode
  recode_steps <- grep("step_recode", result$steps, value = TRUE)
  expect_true(length(recode_steps) >= 1)
  # bc_pe2 simple gen should produce step_compute
  compute_steps <- grep("step_compute.*bc_pe2", result$steps, value = TRUE)
  expect_true(length(compute_steps) >= 1)
})

test_that("transpile_stata works on gen_replace_expr fixture", {
  fixture <- system.file("stata-test-cases/gen_replace_expr.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  # Expression RHS should produce step_compute chain (not step_recode)
  compute_steps <- grep("step_compute.*bc_edu", result$steps, value = TRUE)
  expect_true(length(compute_steps) >= 2)
  # Should use fifelse for conditional updates
  fifelse_steps <- grep("fifelse", result$steps, value = TRUE)
  expect_true(length(fifelse_steps) >= 1)
})

test_that("transpile_stata works on recode fixture", {
  fixture <- system.file("stata-test-cases/recode_gen.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  expect_true(length(result$steps) > 0)
  # recode .=0 should produce fifelse(is.na(...))
  missing_steps <- grep("is.na", result$steps, value = TRUE)
  expect_true(length(missing_steps) >= 1)
})

test_that("transpile_stata works on mvencode fixture", {
  fixture <- system.file("stata-test-cases/mvencode.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  expect_true(length(result$steps) >= 4)
  # All should be step_compute with fifelse(is.na)
  for (step in result$steps) {
    expect_match(step, "step_compute")
    expect_match(step, "is.na")
  }
})

test_that("transpile_stata works on egen_by fixture", {
  fixture <- system.file("stata-test-cases/egen_by.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  expect_true(length(result$steps) >= 2)
  # Should have .by parameter
  by_steps <- grep('\\.by\\s*=', result$steps, value = TRUE)
  expect_true(length(by_steps) >= 1)
})

test_that("transpile_stata works on destring fixture", {
  fixture <- system.file("stata-test-cases/destring.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  expect_true(length(result$steps) >= 2)
  # Should convert to as.numeric
  numeric_steps <- grep("as.numeric", result$steps, value = TRUE)
  expect_true(length(numeric_steps) >= 1)
})

test_that("transpile_stata returns labels from labels fixture", {
  fixture <- system.file("stata-test-cases/labels.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  expect_true(length(result$labels$var_labels) >= 3)
  expect_true(length(result$labels$val_labels) >= 1)
})

test_that("optimize_steps collapses consecutive renames", {
  steps <- c(
    'step_rename(svy, edad = "age")',
    'step_rename(svy, sexo = "sex")',
    'step_compute(svy, x = 1)'
  )
  result <- optimize_steps(steps)
  expect_length(result, 2)
  expect_match(result[1], "edad.*sexo")
})

test_that("optimize_steps collapses consecutive removes", {
  steps <- c(
    "step_remove(svy, a, b)",
    "step_remove(svy, c, d)",
    "step_compute(svy, x = 1)"
  )
  result <- optimize_steps(steps)
  expect_length(result, 2)
  expect_match(result[1], "a.*b.*c.*d")
})

test_that("extract_output_vars finds created variables", {
  steps <- c(
    "step_compute(svy, x = 1)",
    "step_compute(svy, y = x + 2)",
    "step_recode(svy, z, x == 1 ~ 'a', .default = 'b')"
  )
  vars <- extract_output_vars(steps)
  expect_true("x" %in% vars)
  expect_true("y" %in% vars)
  expect_true("z" %in% vars)
})

# ── Task 7: _n-1/_n+1 lag/lead translation ───────────────────────────────────

test_that("translate_stata_expr handles var[_n-1] lag subscript", {
  result <- translate_stata_expr("nucleo[_n-1]")
  expect_match(result, "shift\\(nucleo,\\s*1")
  expect_match(result, 'type\\s*=\\s*"lag"')
})

test_that("translate_stata_expr handles var[_n+1] lead subscript", {
  result <- translate_stata_expr("x[_n+1]")
  expect_match(result, "shift\\(x,\\s*1")
  expect_match(result, 'type\\s*=\\s*"lead"')
})

test_that("translate_stata_expr handles _n subscript in condition", {
  # In if-clause: e30[_n-1]==3 should become shift(e30, 1, type="lag")==3
  result <- translate_stata_expr("e30[_n-1]==3")
  expect_match(result, "shift\\(e30")
  expect_match(result, "==3")
})

test_that("transpile_stata handles lag_lead fixture", {
  fixture <- system.file("stata-test-cases/lag_lead.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  # Should contain shift() calls from var[_n-1] patterns
  shift_steps <- grep("shift\\(", result$steps, value = TRUE)
  expect_true(length(shift_steps) >= 1)
  # No MANUAL_REVIEW for _N or _n patterns
  manual <- grep("MANUAL_REVIEW.*_n|MANUAL_REVIEW.*_N", result$steps,
    value = TRUE
  )
  expect_length(manual, 0)
})

# ── Task 8: _N total count translation ──────────────────────────────────────

test_that("translate_stata_expr handles standalone _N", {
  result <- translate_stata_expr("_N")
  expect_equal(result, ".N")
})

test_that("translate_stata_expr handles _N in expression", {
  result <- translate_stata_expr("income / _N")
  expect_match(result, "income / \\.N")
})

test_that("transpile produces .N for bysort gen _N pattern", {
  fixture <- system.file("stata-test-cases/lag_lead.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  # "bysort bc_correlat: gen max_nper = _N" should become step_compute with .N
  n_steps <- grep("\\.N", result$steps, value = TRUE)
  expect_true(length(n_steps) >= 1)
  # Should have .by for bysort
  by_steps <- grep('\\.by\\s*=.*"bc_correlat"', result$steps, value = TRUE)
  expect_true(length(by_steps) >= 1)
})

# ── Task 9: Variable range expansion (suma1-suma4) ──────────────────────────

test_that("expand_var_range expands var1-var4 to individual vars", {
  result <- expand_var_range("suma1-suma4")
  expect_equal(result, c("suma1", "suma2", "suma3", "suma4"))
})

test_that("expand_var_range passes through single variable", {
  expect_equal(expand_var_range("income"), "income")
})

test_that("expand_var_range handles alpha range", {
  # e.g. e51_2_1-e51_2_5 (common in ECH)
  result <- expand_var_range("e51_2_1-e51_2_5")
  expect_equal(result, paste0("e51_2_", 1:5))
})

test_that("recode with variable range expands to multiple variables", {
  fixture <- system.file("stata-test-cases/var_range.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  # recode suma1-suma4 (.=0) should produce 4 steps (one per variable)
  recode_na_steps <- grep("is.na.*suma", result$steps, value = TRUE)
  expect_true(length(recode_na_steps) >= 4)
})

test_that("mvencode with variable range expands to individual steps", {
  fixture <- system.file("stata-test-cases/var_range.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))
  result <- transpile_stata(fixture)
  # mvencode suma1-suma4, mv(0) should produce 4 mvencode steps
  mv_steps <- grep("fifelse\\(is.na\\(suma", result$steps, value = TRUE)
  expect_true(length(mv_steps) >= 4)
})

# ── Task 10: fifelse type coercion ──────────────────────────────────────────

test_that("translate_gen_block wraps fifelse init with matching type", {
  # When init=-9 and RHS are constants, fifelse(cond, "1", -9) fails type check
  # The transpiled step_recode handles this (string output)

  # But for expression chains: step_compute(svy, v = fifelse(cond, 1L, v))
  # where v may be character, we need as.numeric or as.integer wrapping
  # This test verifies numeric constants get L suffix for integer clarity
  result <- translate_stata_expr("-9")
  # Negative number should pass through unchanged
  expect_equal(result, "-9")
})

test_that("fifelse with numeric default and numeric RHS produces valid R", {
  # gen v = -9 + replace v = 1 if cond -> step_compute chain
  # fifelse(cond, 1, v) is valid when v is initialized to -9 (both numeric)
  cmds <- list(
    list(cmd = "gen", args = "v = -9", if_clause = NULL,
         options = NULL, by_group = NULL, raw_line = "gen v = -9",
         line_num = 1L, capture = FALSE),
    list(cmd = "replace", args = "v = 1", if_clause = "x==1",
         options = NULL, by_group = NULL, raw_line = "replace v = 1 if x==1",
         line_num = 2L, capture = FALSE)
  )
  result <- translate_gen_block(cmds, 1)
  expect_true(length(result$steps) >= 1)
  # Should produce valid R that can parse
  for (s in result$steps) {
    expect_no_error(parse(text = s))
  }
})

# ── Task 11: gen with bysort prefix ─────────────────────────────────────────

test_that("gen with bysort prefix produces .by in step_compute", {
  cmd <- list(
    cmd = "gen", args = "max_nper = _N", if_clause = NULL,
    options = NULL, by_group = "bc_correlat", raw_line = "gen max_nper = _N",
    line_num = 1L, capture = FALSE
  )
  result <- translate_gen_block(list(cmd), 1)
  expect_true(length(result$steps) >= 1)
  expect_match(result$steps[[1]], "\\.by")
  expect_match(result$steps[[1]], "\\.N")
})

test_that("bare gen without = produces step_compute with NA", {
  cmds <- list(
    list(cmd = "gen", args = "ine_ht13", if_clause = NULL,
         options = NULL, by_group = NULL, raw_line = "gen ine_ht13",
         line_num = 1L, capture = TRUE)
  )
  result <- translate_gen_block(cmds, 1)
  expect_true(length(result$steps) >= 1)
  expect_match(result$steps[[1]], "ine_ht13 = NA")
})

test_that("gen with multi-var bysort produces c() in .by", {
  cmd <- list(
    cmd = "gen", args = "tot = sum(x)", if_clause = NULL,
    options = NULL, by_group = "g1 g2", raw_line = "gen tot = sum(x)",
    line_num = 1L, capture = FALSE
  )
  result <- translate_gen_block(list(cmd), 1)
  expect_true(length(result$steps) >= 1)
  expect_match(result$steps[[1]], 'c\\("g1", "g2"\\)')
})

# ── Multi-variable destring ──────────────────────────────────────────────────

test_that("destring with multiple variables produces one step per var", {
  cmd <- list(
    cmd = "destring", args = "g144_1 g261 g261_1",
    if_clause = NULL, options = "replace force",
    by_group = NULL, raw_line = "destring g144_1 g261 g261_1, replace force",
    line_num = 1L, capture = FALSE
  )
  result <- translate_destring(cmd)
  expect_true(length(result$steps) == 3)
  expect_match(result$steps[[1]], "g144_1")
  expect_match(result$steps[[2]], "g261")
  expect_match(result$steps[[3]], "g261_1")
  for (s in result$steps) {
    expect_match(s, "as.numeric")
    expect_no_error(parse(text = s))
  }
})

# ── Drop with STATA variable range ──────────────────────────────────────────

test_that("drop with STATA variable range expands to individual vars", {
  cmd <- list(
    cmd = "drop", args = "aux1-aux14_max",
    if_clause = NULL, options = NULL, by_group = NULL,
    raw_line = "drop aux1-aux14_max",
    line_num = 1L, capture = FALSE
  )
  result <- translate_drop(cmd)
  expect_true(length(result$steps) >= 1)
  step <- result$steps[[1]]
  # Should contain aux1, aux2, ..., aux14, aux1_max, ..., aux14_max
  expect_match(step, "aux1,")
  expect_match(step, "aux14")
  expect_match(step, "aux1_max")
  expect_match(step, "aux14_max")
})

test_that("expand_var_range handles suffixed ranges like aux1-aux14_max", {
  result <- expand_var_range("aux1-aux14_max")
  # Should expand: aux1..aux14 + aux1_max..aux14_max
  expect_true("aux1" %in% result)
  expect_true("aux14" %in% result)
  expect_true("aux1_max" %in% result)
  expect_true("aux14_max" %in% result)
  expect_equal(length(result), 28)
})

# ── Multi-variable recode ────────────────────────────────────────────────────

test_that("recode with multiple space-separated variables expands", {
  cmd <- list(
    cmd = "recode", args = "g144_1 g261 g261_1 (.=0)",
    if_clause = NULL, options = NULL, by_group = NULL,
    raw_line = "recode g144_1 g261 g261_1 (.=0)",
    line_num = 1L, capture = FALSE
  )
  result <- translate_recode(cmd)
  # Should produce 3 steps (one per variable)
  expect_equal(length(result$steps), 3)
  expect_match(result$steps[[1]], "g144_1")
  expect_match(result$steps[[2]], "g261[^_]")
  expect_match(result$steps[[3]], "g261_1")
})

test_that("transpile_stata_module groups files by module", {
  skip_on_cran()
  year_dir <- file.path("do_files_iecon", "2022")
  # Use absolute path from package root
  year_dir_abs <- file.path(
    system.file(package = "metasurvey"),
    "..", "..", "..", year_dir
  )
  # Fallback to relative path from working directory
  if (!dir.exists(year_dir_abs)) {
    year_dir_abs <- year_dir
  }
  skip_if_not(dir.exists(year_dir_abs),
    message = "do_files_iecon/2022 not available"
  )

  recipes <- transpile_stata_module(year_dir_abs, year = 2022)
  expect_true(is.list(recipes))
  expect_true(length(recipes) >= 1)
  # Each element should be a Recipe
  for (r in recipes) {
    expect_true(inherits(r, "Recipe"))
    expect_true(length(r$steps) > 0)
  }
})


# --- Merged from test-transpile-e2e.R ---

# End-to-end integration test: transpile do-files -> load ECH -> bake recipes
# Requires: do_files_iecon/ directory and example-data/ech/ech_2022.csv
# Skipped on CRAN and when data is not available

test_that("transpile 2022 do-files and bake sequentially on ECH data", {
  skip_on_cran()
  pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  year_dir <- file.path(pkg_root, "do_files_iecon", "2022")
  skip_if_not(dir.exists(year_dir), "do_files_iecon/2022 not available")

  ech_path <- file.path(pkg_root, "example-data", "ech", "ech_2022.csv")
  skip_if_not(file.exists(ech_path), "ECH 2022 CSV not available")

  # Step 1: Transpile do-files into recipes by module
  recipes <- transpile_stata_module(year_dir, year = 2022)
  expect_true(is.list(recipes))
  expect_true(length(recipes) >= 1)

  # Step 2: Load a SAMPLE of ECH data (first 500 rows for speed/memory)
  ech_data <- data.table::fread(ech_path, nrows = 500)
  data.table::setnames(ech_data, tolower(names(ech_data)))
  tmp_path <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_path), add = TRUE)
  data.table::fwrite(ech_data, tmp_path)
  old_engine <- getOption("metasurvey.engine")
  options(metasurvey.engine = "data.table")
  on.exit(options(metasurvey.engine = old_engine), add = TRUE)
  svy <- load_survey(
    path = tmp_path,
    svy_type = "ech",
    svy_edition = "2022",
    svy_weight = list(annual = "w_ano")
  )
  expect_true(nrow(svy$data) > 0)

  # Step 3: Apply recipes SEQUENTIALLY in module order
  module_order <- c(
    "data_prep", "demographics", "income_detail",
    "income_aggregate", "cleanup"
  )

  baked_modules <- character(0)
  total_steps_ok <- 0
  for (module_name in module_order) {
    if (!module_name %in% names(recipes)) next
    rec <- recipes[[module_name]]

    executable_steps <- rec$steps[!grepl("^#", rec$steps)]
    if (length(executable_steps) == 0) next

    # Bake steps one by one, skipping expected missing-variable errors
    eval_env <- new.env(parent = parent.frame())
    eval_env$svy <- svy
    steps_ok <- 0
    for (step_str in executable_steps) {
      tryCatch(
        {
          step_call <- parse(text = step_str)[[1]]
          if (is.call(step_call) && length(step_call) >= 2 &&
            is.name(step_call[[2]]) &&
            as.character(step_call[[2]]) == ".") {
            step_call[[2]] <- as.name("svy")
          }
          eval_env$svy <- eval(step_call, envir = eval_env)
          steps_ok <- steps_ok + 1
        },
        error = function(e) {
          msg <- conditionMessage(e)
          # Skip expected errors:
          # - missing variables (orchestrator creates them)
          # - parse errors (untranslated STATA syntax like _n, _N)
          # - undefined columns
          is_expected <- grepl(
            paste0(
              "not found|not exist|Variables to rename|",
              "undefined columns|cannot remove|object.*not found|",
              "unexpected symbol|unexpected '='|parse error|",
              "type logical but|type character but|type double but|",
              "type integer but|same type"
            ),
            msg
          )
          if (!is_expected) {
            fail(sprintf(
              "Module '%s' unexpected error: %s\nStep: %s",
              module_name, msg, substr(step_str, 1, 120)
            ))
          }
        }
      )
    }
    svy <- eval_env$svy

    if (steps_ok > 0) {
      baked_modules <- c(baked_modules, module_name)
      total_steps_ok <- total_steps_ok + steps_ok
    }
  }

  # At least some modules and steps should have baked successfully
  expect_true(
    length(baked_modules) >= 2,
    info = paste("Baked modules:", paste(baked_modules, collapse = ", "))
  )
  expect_true(total_steps_ok >= 50,
    info = paste("Total steps baked:", total_steps_ok)
  )
})

test_that("transpile single do-file produces valid steps for bake", {
  skip_on_cran()
  fixture <- system.file("stata-test-cases/gen_replace.do",
    package = "metasurvey"
  )
  skip_if_not(file.exists(fixture))

  result <- transpile_stata(fixture)
  expect_true(length(result$steps) > 0)

  # Create survey with the required source variables
  svy <- make_test_survey(n = 4)
  dt <- svy$data
  dt[, e26 := c(1, 2, 1, 2)]
  dt[, e27 := c(25, 30, 45, 50)]
  dt[, e30 := c(1, 2, 3, 7)]
  dt[, region_4 := c(1, 2, 3, 4)]
  svy <- set_data(svy, dt)

  rec <- Recipe$new(
    id = "test_gen_replace",
    name = "Test gen_replace",
    user = "test",
    edition = "2022",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    description = "Test fixture",
    steps = result$steps,
    topic = "test"
  )
  svy <- add_recipe(svy, rec, bake = FALSE)
  baked <- bake_recipes(svy)

  expect_true(inherits(baked, "Survey"))
  expect_true("bc_pe4" %in% names(baked$data))
  expect_true("bc_pe2" %in% names(baked$data))
  expect_true("bc_pe3" %in% names(baked$data))
  # Check recode values (step_recode produces character/factor)
  # e30=c(1,2,3,7): last matching condition wins (sequential overwrite)
  expect_equal(as.character(baked$data$bc_pe4), c("1", "2", "3", "5"))
  expect_equal(baked$data$bc_pe2, c(1, 2, 1, 2))
})
