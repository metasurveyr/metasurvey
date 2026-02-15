# Tests for tidyverse-style functional API wrappers

# --- Constructor wrappers ---

test_that("recipe_user creates individual user", {
  u <- recipe_user("Juan", type = "individual", email = "j@test.com")
  expect_s3_class(u, "RecipeUser")
  expect_equal(u$name, "Juan")
  expect_equal(u$user_type, "individual")
  expect_equal(u$email, "j@test.com")
})

test_that("recipe_user creates institution", {
  inst <- recipe_user("IECON", type = "institution", url = "https://iecon.edu.uy", verified = TRUE)
  expect_equal(inst$user_type, "institution")
  expect_true(inst$verified)
})

test_that("recipe_user creates institutional_member with institution object", {
  inst <- recipe_user("IECON", type = "institution")
  m <- recipe_user("Maria", type = "institutional_member", institution = inst)
  expect_equal(m$institution$name, "IECON")
})

test_that("recipe_user creates institutional_member with institution name shortcut", {
  m <- recipe_user("Maria", type = "institutional_member", institution = "IECON")
  expect_equal(m$institution$name, "IECON")
  expect_equal(m$institution$user_type, "institution")
})

test_that("recipe_user type defaults to individual", {
  u <- recipe_user("Juan")
  expect_equal(u$user_type, "individual")
})

test_that("recipe_category creates category", {
  cat <- recipe_category("labor_market", "Labor market indicators")
  expect_s3_class(cat, "RecipeCategory")
  expect_equal(cat$name, "labor_market")
})

test_that("recipe_category creates subcategory with parent string", {
  cat <- recipe_category("employment", "Employment stats", parent = "labor_market")
  expect_equal(cat$parent$name, "labor_market")
})

test_that("recipe_category creates subcategory with parent object", {
  labor <- recipe_category("labor_market", "Labor")
  emp <- recipe_category("employment", "Employment", parent = labor)
  expect_equal(emp$parent$name, "labor_market")
})

test_that("recipe_certification creates community by default", {
  cert <- recipe_certification()
  expect_s3_class(cert, "RecipeCertification")
  expect_equal(cert$level, "community")
})

test_that("recipe_certification creates with level and user", {
  inst <- recipe_user("IECON", type = "institution")
  cert <- recipe_certification("official", certified_by = inst)
  expect_equal(cert$level, "official")
})

# --- Pipe-friendly recipe modifiers ---

test_that("add_category adds by name string", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |> add_category("labor_market")
  expect_equal(length(r2$categories), 1)
  expect_equal(r2$categories[[1]]$name, "labor_market")
})

test_that("add_category adds by name with description", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |> add_category("income", "Income distribution")
  expect_equal(r2$categories[[1]]$description, "Income distribution")
})

test_that("add_category adds by RecipeCategory object", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  cat <- recipe_category("health", "Health indicators")
  r2 <- r |> add_category(cat)
  expect_equal(r2$categories[[1]]$name, "health")
})

test_that("add_category is pipeable (chaining)", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |>
    add_category("labor_market") |>
    add_category("income") |>
    add_category("demographics")
  expect_equal(length(r2$categories), 3)
})

test_that("remove_category removes by name", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |>
    add_category("labor_market") |>
    add_category("income") |>
    remove_category("labor_market")
  expect_equal(length(r2$categories), 1)
  expect_equal(r2$categories[[1]]$name, "income")
})

test_that("certify_recipe certifies with user and level", {
  inst <- recipe_user("IECON", type = "institution")
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |> certify_recipe(inst, "official")
  expect_equal(r2$certification$level, "official")
  expect_equal(r2$certification$certified_by$name, "IECON")
})

test_that("certify_recipe is pipeable", {
  inst <- recipe_user("IECON", type = "institution")
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |>
    add_category("labor_market") |>
    certify_recipe(inst, "official")
  expect_equal(r2$certification$level, "official")
  expect_equal(length(r2$categories), 1)
})

test_that("set_user_info sets user on recipe", {
  u <- recipe_user("Juan", type = "individual")
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |> set_user_info(u)
  expect_equal(r2$user_info$name, "Juan")
})

test_that("set_version sets version on recipe", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  r2 <- r |> set_version("2.0.0")
  expect_equal(r2$version, "2.0.0")
})

# --- Full pipeline style ---

test_that("full pipeline: recipe |> add_category |> set_user_info |> certify_recipe", {
  inst <- recipe_user("IECON", type = "institution")
  member <- recipe_user("Maria", type = "institutional_member", institution = inst)

  r <- Recipe$new(
    name = "Labor ECH", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "Maria", description = "Labor indicators",
    steps = list(), id = "pipe_1", doi = NULL, topic = "labor"
  )

  r2 <- r |>
    add_category("labor_market", "Labor market") |>
    add_category("income") |>
    set_user_info(member) |>
    certify_recipe(inst, "official") |>
    set_version("1.1.0")

  expect_equal(length(r2$categories), 2)
  expect_equal(r2$user_info$name, "Maria")
  expect_equal(r2$certification$level, "official")
  expect_equal(r2$version, "1.1.0")
})

# --- Backend/registry functional wrappers ---

test_that("search_recipes searches in local backend", {
  tmp <- tempfile(fileext = ".json")
  on.exit({ unlink(tmp); options(metasurvey.backend = NULL) })
  set_backend("local", path = tmp)

  r <- Recipe$new(
    name = "Labor Analysis", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = "sr_1", doi = NULL, topic = NULL
  )
  get_backend()$publish(r)

  results <- search_recipes("Labor")
  expect_equal(length(results), 1)
  expect_equal(results[[1]]$name, "Labor Analysis")
})

test_that("rank_recipes ranks by downloads", {
  tmp <- tempfile(fileext = ".json")
  on.exit({ unlink(tmp); options(metasurvey.backend = NULL) })
  set_backend("local", path = tmp)
  b <- get_backend()

  b$publish(Recipe$new(
    name = "Low", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = "rk_1", doi = NULL, topic = NULL, downloads = 10L
  ))
  b$publish(Recipe$new(
    name = "High", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = "rk_2", doi = NULL, topic = NULL, downloads = 100L
  ))

  ranked <- rank_recipes(n = 2)
  expect_equal(ranked[[1]]$name, "High")
})

test_that("filter_recipes filters by criteria", {
  tmp <- tempfile(fileext = ".json")
  on.exit({ unlink(tmp); options(metasurvey.backend = NULL) })
  set_backend("local", path = tmp)
  b <- get_backend()

  b$publish(Recipe$new(
    name = "ECH Recipe", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = "fr_1", doi = NULL, topic = NULL
  ))
  b$publish(Recipe$new(
    name = "EAII Recipe", edition = "2023", survey_type = "eaii",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = "fr_2", doi = NULL, topic = NULL
  ))

  results <- filter_recipes(svy_type = "ech")
  expect_equal(length(results), 1)
  expect_equal(results[[1]]$name, "ECH Recipe")
})

test_that("list_recipes lists all", {
  tmp <- tempfile(fileext = ".json")
  on.exit({ unlink(tmp); options(metasurvey.backend = NULL) })
  set_backend("local", path = tmp)
  b <- get_backend()
  b$publish(Recipe$new(
    name = "A", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "t", description = "d", steps = list(),
    id = "lr_1", doi = NULL, topic = NULL
  ))
  expect_equal(length(list_recipes()), 1)
})

test_that("default_categories returns list with known names", {
  cats <- default_categories()
  names_list <- vapply(cats, function(c) c$name, character(1))
  expect_true("labor_market" %in% names_list)
  expect_true("income" %in% names_list)
})
