# Stress tests for the step pipeline
# Validates memory and correctness under heavy workloads
# These tests are slower (~5-10s) but ensure the fixes hold at scale
# Skipped on CRAN to respect time limits

test_that("stress: 100 step_compute on 10k rows stays bounded", {
  skip_on_cran()
  n <- 10000
  set.seed(123)
  dt <- data.table::data.table(
    id = seq_len(n),
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    w = rep(1, n)
  )
  svy <- Survey$new(
    data = dt, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  base_data_bytes <- as.numeric(object.size(dt))

  # Chain 100 step_compute calls
  for (i in seq_len(100)) {
    vname <- paste0("v", i)
    args <- list(svy = svy)
    args[[vname]] <- substitute(x + val * y, list(val = i))
    svy <- do.call(step_compute, args)
  }

  expect_length(get_steps(svy), 100)

  # Memory: survey with 100 pending lazy steps should be
  # roughly 1 data copy + step metadata, NOT 100 data copies
  svy_bytes <- as.numeric(object.size(svy))

  # 100 new columns at 10k doubles = ~8MB, plus original ~0.4MB
  # With svy_before chain it would be ~100 * 0.4MB = 40MB+
  # Without the chain, total should be under 20x base
  expect_lt(svy_bytes, base_data_bytes * 20,
    label = sprintf(
      "Survey %.1fMB should be < %.1fMB (20x base %.1fMB)",
      svy_bytes / 1e6, base_data_bytes * 20 / 1e6, base_data_bytes / 1e6
    )
  )

  # No svy_before retained
  for (step in get_steps(svy)) {
    expect_null(step$svy_before)
  }
})

test_that("stress: bake 100 steps on 10k rows produces correct results", {
  skip_on_cran()
  n <- 10000
  set.seed(456)
  dt <- data.table::data.table(
    id = seq_len(n),
    x = rnorm(n),
    y = rnorm(n),
    w = rep(1, n)
  )
  svy <- Survey$new(
    data = dt, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  # Chain 100 steps
  for (i in seq_len(100)) {
    vname <- paste0("v", i)
    args <- list(svy = svy)
    args[[vname]] <- substitute(x + val * y, list(val = i))
    svy <- do.call(step_compute, args)
  }

  # Bake
  baked <- bake_steps(svy)
  baked_dt <- get_data(baked)

  # Verify first, middle, last computed columns
  expect_equal(baked_dt$v1, dt$x + 1 * dt$y)
  expect_equal(baked_dt$v50, dt$x + 50 * dt$y)
  expect_equal(baked_dt$v100, dt$x + 100 * dt$y)

  # All 100 vars exist
  for (i in seq_len(100)) {
    expect_true(paste0("v", i) %in% names(baked_dt))
  }

  # All steps baked
  for (step in get_steps(baked)) {
    expect_true(step$bake)
  }

  # Design valid

  expect_true(baked$design_initialized)
})

test_that("stress: mixed 50 steps (compute + recode + rename + remove) then bake", {
  skip_on_cran()
  n <- 5000
  set.seed(789)
  dt <- data.table::data.table(
    id = seq_len(n),
    x = rnorm(n),
    y = rnorm(n),
    age = sample(18:80, n, replace = TRUE),
    income = round(runif(n, 500, 10000), 2),
    region = sample(1:5, n, replace = TRUE),
    temp1 = 1, temp2 = 2, temp3 = 3,
    w = rep(1, n)
  )
  svy <- Survey$new(
    data = dt, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  # 30 compute steps
  for (i in 1:30) {
    vname <- paste0("c", i)
    args <- list(svy = svy)
    args[[vname]] <- substitute(x + val, list(val = i))
    svy <- do.call(step_compute, args)
  }

  # 10 recode steps
  for (i in 1:10) {
    vname <- paste0("r", i)
    threshold <- 20 + i * 5
    recode_args <- list(
      svy = svy,
      new_var = as.name(vname),
      substitute(age < thr ~ "below", list(thr = threshold)),
      .default = "above"
    )
    svy <- do.call(step_recode, recode_args)
  }

  # 3 remove steps
  svy <- step_remove(svy, temp1)
  svy <- step_remove(svy, temp2)
  svy <- step_remove(svy, temp3)

  # 2 rename steps
  svy <- step_rename(svy, identifier = id)
  svy <- step_rename(svy, years = age)

  # Total: 30 + 10 + 3 + 2 = 45 steps
  expect_length(get_steps(svy), 45)

  # Bake all
  baked <- bake_steps(svy)
  baked_dt <- get_data(baked)

  # Computes correct
  expect_equal(baked_dt$c1, dt$x + 1)
  expect_equal(baked_dt$c30, dt$x + 30)

  # Recodes exist
  for (i in 1:10) {
    expect_true(paste0("r", i) %in% names(baked_dt))
  }

  # Removes applied
  expect_false("temp1" %in% names(baked_dt))
  expect_false("temp2" %in% names(baked_dt))
  expect_false("temp3" %in% names(baked_dt))

  # Renames applied
  expect_true("identifier" %in% names(baked_dt))
  expect_true("years" %in% names(baked_dt))
  expect_false("id" %in% names(baked_dt))
  expect_false("age" %in% names(baked_dt))
})

test_that("stress: bake_steps memory peak is bounded (not N * data_size)", {
  skip_on_cran()
  n <- 50000
  set.seed(101)
  dt <- data.table::data.table(
    x = rnorm(n),
    y = rnorm(n),
    w = rep(1, n)
  )
  svy <- Survey$new(
    data = dt, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  data_bytes <- as.numeric(object.size(dt))

  # 50 steps
  for (i in seq_len(50)) {
    vname <- paste0("v", i)
    args <- list(svy = svy)
    args[[vname]] <- substitute(x + val, list(val = i))
    svy <- do.call(step_compute, args)
  }

  # Force GC before measuring
  gc(verbose = FALSE)
  mem_before <- gc(verbose = FALSE)[2, 2] * 1024 * 1024  # bytes

  baked <- bake_steps(svy)

  gc(verbose = FALSE)
  mem_after <- gc(verbose = FALSE)[2, 2] * 1024 * 1024

  # Memory increase during bake should be bounded
  # Without fix: each bake_step creates a copy → ~50 * data_bytes
  # With fix: only 1 shallow_clone + in-place mutations → ~1-2 * data_bytes
  mem_increase <- mem_after - mem_before

  # Allow generous headroom but not 50x (which would indicate N copies)
  # Note: GC timing makes this very approximate — R may not reclaim
  # memory immediately, so we use a wide margin
  expect_lt(mem_increase, data_bytes * 25,
    label = sprintf(
      "Bake memory increase %.1fMB should be < %.1fMB",
      mem_increase / 1e6, data_bytes * 25 / 1e6
    )
  )

  # Results still correct
  baked_dt <- get_data(baked)
  expect_equal(baked_dt$v1, dt$x + 1)
  expect_equal(baked_dt$v50, dt$x + 50)
})

test_that("stress: RotativePanelSurvey with 4 follow-ups and 20 steps", {
  skip_on_cran()
  n <- 2000
  set.seed(202)

  make_svy <- function(month) {
    dt <- data.table::data.table(
      id = seq_len(n),
      x = rnorm(n),
      y = rnorm(n),
      age = sample(18:65, n, replace = TRUE),
      mes = month,
      anio = 2023,
      numero = seq_len(n),
      w = rep(1, n)
    )
    svy <- Survey$new(
      data = dt, edition = "2023", type = "ech",
      psu = NULL, engine = "data.table",
      weight = add_weight(annual = "w")
    )
    svy$periodicity <- "monthly"
    svy
  }

  panel <- RotativePanelSurvey$new(
    implantation = make_svy(1),
    follow_up = list(make_svy(2), make_svy(3), make_svy(4), make_svy(5)),
    type = "ech",
    default_engine = "data.table",
    steps = list(), recipes = list(),
    workflows = list(), design = NULL
  )

  # Apply 20 steps across all levels
  for (i in 1:15) {
    vname <- paste0("v", i)
    args <- list(svy = panel, .level = "auto")
    args[[vname]] <- substitute(x + val * y, list(val = i))
    panel <- do.call(step_compute, args)
  }

  panel <- step_recode(panel, age_cat,
    age < 30 ~ "young",
    age < 50 ~ "adult",
    .default = "senior",
    .level = "auto"
  )

  # Bake
  baked <- bake_steps(panel)

  # Verify implantation
  impl_dt <- get_data(baked$implantation)
  expect_true("v1" %in% names(impl_dt))
  expect_true("v15" %in% names(impl_dt))
  expect_true("age_cat" %in% names(impl_dt))

  # Verify all 4 follow-ups
  expect_length(baked$follow_up, 4)
  for (j in seq_along(baked$follow_up)) {
    fu_dt <- get_data(baked$follow_up[[j]])
    expect_true("v1" %in% names(fu_dt),
      info = paste("follow_up", j)
    )
    expect_true("v15" %in% names(fu_dt),
      info = paste("follow_up", j)
    )
    expect_true("age_cat" %in% names(fu_dt),
      info = paste("follow_up", j)
    )
    # Spot-check correctness
    raw_dt <- data.table::data.table(
      x = fu_dt$x, y = fu_dt$y
    )
    expect_equal(fu_dt$v1, raw_dt$x + 1 * raw_dt$y,
      info = paste("follow_up", j, "v1 values")
    )
  }
})

test_that("stress: sequential bake-add-bake cycle works correctly", {
  skip_on_cran()
  svy <- make_test_survey(500)

  # First batch of steps
  svy <- step_compute(svy, a = x + 1)
  svy <- step_compute(svy, b = y + 2)
  svy <- bake_steps(svy)

  dt1 <- get_data(svy)
  expect_true("a" %in% names(dt1))
  expect_true("b" %in% names(dt1))

  # Second batch
  svy <- step_compute(svy, c = a + b)
  svy <- step_compute(svy, d = c * 2)
  svy <- bake_steps(svy)

  dt2 <- get_data(svy)
  expect_true("c" %in% names(dt2))
  expect_true("d" %in% names(dt2))
  expect_equal(dt2$c, dt2$a + dt2$b)
  expect_equal(dt2$d, dt2$c * 2)

  # Third batch with recode depending on previous computed vars
  svy <- step_recode(svy, d_cat,
    d < 20 ~ "low",
    .default = "high"
  )
  svy <- bake_steps(svy)

  dt3 <- get_data(svy)
  expect_true("d_cat" %in% names(dt3))
})
