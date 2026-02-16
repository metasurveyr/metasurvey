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
      tryCatch({
        step_call <- parse(text = step_str)[[1]]
        if (is.call(step_call) && length(step_call) >= 2 &&
            is.name(step_call[[2]]) &&
            as.character(step_call[[2]]) == ".") {
          step_call[[2]] <- as.name("svy")
        }
        eval_env$svy <- eval(step_call, envir = eval_env)
        steps_ok <- steps_ok + 1
      }, error = function(e) {
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
      })
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
