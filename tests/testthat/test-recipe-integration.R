# Integration tests for the full recipe ecosystem

test_that("Full workflow: user -> recipe -> categorize -> register -> search -> retrieve", {
  # Create user
  user <- RecipeUser$new(name = "Juan Perez", user_type = "individual", email = "juan@test.com")

  # Create recipe with user_info
  labor <- RecipeCategory$new("labor_market", "Labor market indicators")
  r <- Recipe$new(
    name = "Employment Rate ECH 2023",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "Juan Perez", description = "Computes employment rate from ECH",
    steps = list(), id = "recipe_001", doi = NULL,
    topic = "employment", user_info = user
  )

  # Categorize
  r$add_category(labor)
  expect_equal(length(r$categories), 1)

  # Register in local registry
  reg <- RecipeRegistry$new()
  reg$register(r)

  # Search
  found <- reg$search("Employment")
  expect_equal(length(found), 1)
  expect_equal(found[[1]]$name, "Employment Rate ECH 2023")

  # Retrieve by id
  retrieved <- reg$get("recipe_001")
  expect_equal(retrieved$user_info$name, "Juan Perez")
  expect_equal(retrieved$categories[[1]]$name, "labor_market")
})

test_that("Institution certification flow", {
  # Create institution
  inst <- RecipeUser$new(name = "Instituto de Economia", user_type = "institution", verified = TRUE)

  # Create recipe as community
  r <- Recipe$new(
    name = "Official Labor Stats",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "IECON", description = "Official labor statistics",
    steps = list(), id = "recipe_official", doi = NULL,
    topic = "labor"
  )
  expect_equal(r$certification$level, "community")

  # Certify as official
  r$certify(inst, "official")
  expect_equal(r$certification$level, "official")
  expect_equal(r$certification$certified_by$name, "Instituto de Economia")

  # Verify certification level
  expect_true(r$certification$is_at_least("community"))
  expect_true(r$certification$is_at_least("reviewed"))
  expect_true(r$certification$is_at_least("official"))
})

test_that("Ranking: 5 recipes with different downloads", {
  reg <- RecipeRegistry$new()
  for (i in 1:5) {
    r <- Recipe$new(
      name = paste0("Recipe_", i),
      edition = "2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list(),
      user = "tester", description = "test",
      steps = list(), id = paste0("rank_", i), doi = NULL,
      topic = NULL, downloads = as.integer(i * 10)
    )
    reg$register(r)
  }

  ranked <- reg$rank_by_downloads(3)
  expect_equal(length(ranked), 3)
  expect_equal(ranked[[1]]$downloads, 50L)
  expect_equal(ranked[[2]]$downloads, 40L)
  expect_equal(ranked[[3]]$downloads, 30L)
})

test_that("Mixed ranking: certification levels x downloads", {
  reg <- RecipeRegistry$new()
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  member <- RecipeUser$new(name = "Maria", user_type = "institutional_member", institution = inst)

  # Community with high downloads
  r1 <- Recipe$new(
    name = "Popular_Community", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "anon", description = "d", steps = list(),
    id = "mc_1", doi = NULL, topic = NULL,
    downloads = 500L
  )

  # Reviewed with medium downloads
  r2 <- Recipe$new(
    name = "Reviewed_Medium", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "member", description = "d", steps = list(),
    id = "mc_2", doi = NULL, topic = NULL,
    downloads = 50L,
    certification = RecipeCertification$new("reviewed", certified_by = member)
  )

  # Official with low downloads
  r3 <- Recipe$new(
    name = "Official_Low", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "inst", description = "d", steps = list(),
    id = "mc_3", doi = NULL, topic = NULL,
    downloads = 5L,
    certification = RecipeCertification$new("official", certified_by = inst)
  )

  reg$register(r1)
  reg$register(r2)
  reg$register(r3)

  ranked <- reg$rank_by_certification()
  expect_equal(ranked[[1]]$name, "Official_Low")
  expect_equal(ranked[[2]]$name, "Reviewed_Medium")
  expect_equal(ranked[[3]]$name, "Popular_Community")
})

test_that("User filtering: institution + members", {
  reg <- RecipeRegistry$new()
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  member <- RecipeUser$new(name = "Maria", user_type = "institutional_member", institution = inst)
  individual <- RecipeUser$new(name = "Juan", user_type = "individual")

  r1 <- Recipe$new(
    name = "By_Institution", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "IECON", description = "d", steps = list(),
    id = "uf_1", doi = NULL, topic = NULL,
    user_info = inst
  )
  r2 <- Recipe$new(
    name = "By_Member", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "Maria", description = "d", steps = list(),
    id = "uf_2", doi = NULL, topic = NULL,
    user_info = member
  )
  r3 <- Recipe$new(
    name = "By_Individual", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "Juan", description = "d", steps = list(),
    id = "uf_3", doi = NULL, topic = NULL,
    user_info = individual
  )
  reg$register(r1)
  reg$register(r2)
  reg$register(r3)

  iecon_recipes <- reg$list_by_institution("IECON")
  expect_equal(length(iecon_recipes), 2)
  names_found <- vapply(iecon_recipes, function(r) r$name, character(1))
  expect_true("By_Institution" %in% names_found)
  expect_true("By_Member" %in% names_found)
})

test_that("Full save/load cycle preserves everything", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  reg <- RecipeRegistry$new()
  inst <- RecipeUser$new(name = "IECON", user_type = "institution", verified = TRUE)
  member <- RecipeUser$new(name = "Maria", user_type = "institutional_member", institution = inst)
  labor <- RecipeCategory$new("labor_market", "Labor")
  income <- RecipeCategory$new("income", "Income")

  r <- Recipe$new(
    name = "Full Cycle", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "Maria", description = "Full test",
    steps = list(), id = "fc_1", doi = "10.1234/test", topic = "labor",
    categories = list(labor, income),
    downloads = 99L,
    certification = RecipeCertification$new("reviewed", certified_by = member),
    user_info = member,
    version = "2.1.0"
  )
  reg$register(r)
  reg$save(tmp)

  # Load in fresh registry
  reg2 <- RecipeRegistry$new()
  reg2$load(tmp)

  loaded <- reg2$list_all()
  expect_equal(length(loaded), 1)
  loaded_r <- loaded[[1]]

  expect_equal(loaded_r$name, "Full Cycle")
  expect_equal(loaded_r$version, "2.1.0")
  expect_equal(loaded_r$downloads, 99L)
  expect_equal(loaded_r$doi, "10.1234/test")
  expect_equal(length(loaded_r$categories), 2)
  cat_names <- vapply(loaded_r$categories, function(c) c$name, character(1))
  expect_true("labor_market" %in% cat_names)
  expect_true("income" %in% cat_names)
  expect_equal(loaded_r$certification$level, "reviewed")
  expect_equal(loaded_r$user_info$name, "Maria")
  expect_equal(loaded_r$user_info$institution$name, "IECON")
})

test_that("Backward compatibility: old recipes load and register", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  # Simulate old-format recipe JSON in a registry
  old_data <- list(
    list(
      name = "Legacy Recipe",
      user = "old_user",
      svy_type = "ech",
      edition = "2020",
      description = "Old recipe without ecosystem fields",
      steps = list(),
      id = "legacy_1"
    )
  )
  jsonlite::write_json(old_data, tmp, auto_unbox = TRUE)

  reg <- RecipeRegistry$new()
  reg$load(tmp)
  loaded <- reg$list_all()

  expect_equal(length(loaded), 1)
  r <- loaded[[1]]
  expect_equal(r$name, "Legacy Recipe")
  expect_equal(r$downloads, 0L)
  expect_equal(r$version, "1.0.0")
  expect_equal(r$certification$level, "community")
  expect_equal(length(r$categories), 0)
  expect_null(r$user_info)
})

test_that("Backend with local: full workflow end-to-end", {
  tmp <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp)
    options(metasurvey.backend = NULL)
  })

  set_backend("local", path = tmp)
  backend <- get_backend()

  # Publish
  r1 <- Recipe$new(
    name = "Backend Test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Backend workflow",
    steps = list(), id = "bt_1", doi = NULL, topic = NULL,
    downloads = 0L
  )
  backend$publish(r1)

  # Search
  found <- backend$search("Backend")
  expect_equal(length(found), 1)

  # Increment downloads
  backend$increment_downloads("bt_1")
  updated <- backend$get("bt_1")
  expect_equal(updated$downloads, 1L)

  # Rank
  ranked <- backend$rank()
  expect_equal(length(ranked), 1)
})
