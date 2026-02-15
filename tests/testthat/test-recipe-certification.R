test_that("RecipeCertification creates community level with no certifier", {
  cert <- RecipeCertification$new(level = "community")
  expect_s3_class(cert, "RecipeCertification")
  expect_equal(cert$level, "community")
  expect_null(cert$certified_by)
  expect_s3_class(cert$certified_at, "POSIXct")
})

test_that("RecipeCertification creates reviewed level with member certifier", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  member <- RecipeUser$new(name = "Maria", user_type = "institutional_member", institution = inst)
  cert <- RecipeCertification$new(level = "reviewed", certified_by = member)
  expect_equal(cert$level, "reviewed")
  expect_equal(cert$certified_by$name, "Maria")
})

test_that("RecipeCertification creates official level with institution certifier", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution", verified = TRUE)
  cert <- RecipeCertification$new(level = "official", certified_by = inst)
  expect_equal(cert$level, "official")
  expect_equal(cert$certified_by$name, "IECON")
})

test_that("official certification requires institution user", {
  individual <- RecipeUser$new(name = "Juan", user_type = "individual")
  expect_error(
    RecipeCertification$new(level = "official", certified_by = individual),
    "institution"
  )
})

test_that("official certification rejects institutional_member", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  member <- RecipeUser$new(name = "Maria", user_type = "institutional_member", institution = inst)
  expect_error(
    RecipeCertification$new(level = "official", certified_by = member),
    "institution"
  )
})

test_that("reviewed certification rejects individual user", {
  individual <- RecipeUser$new(name = "Juan", user_type = "individual")
  expect_error(
    RecipeCertification$new(level = "reviewed", certified_by = individual),
    "institutional_member"
  )
})

test_that("reviewed certification accepts institution user", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  cert <- RecipeCertification$new(level = "reviewed", certified_by = inst)
  expect_equal(cert$level, "reviewed")
})

test_that("invalid level throws error", {
  expect_error(RecipeCertification$new(level = "gold"))
  expect_error(RecipeCertification$new(level = ""))
  expect_error(RecipeCertification$new(level = NULL))
})

test_that("certified_at is auto-set to current time", {
  before <- Sys.time()
  cert <- RecipeCertification$new(level = "community")
  after <- Sys.time()
  expect_true(cert$certified_at >= before)
  expect_true(cert$certified_at <= after)
})

test_that("notes field stores extra info", {
  cert <- RecipeCertification$new(level = "community", notes = "Initial publication")
  expect_equal(cert$notes, "Initial publication")
})

test_that("is_at_least compares levels correctly", {
  community <- RecipeCertification$new(level = "community")
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  member <- RecipeUser$new(name = "Maria", user_type = "institutional_member", institution = inst)
  reviewed <- RecipeCertification$new(level = "reviewed", certified_by = member)
  official <- RecipeCertification$new(level = "official", certified_by = inst)

  # community
  expect_true(community$is_at_least("community"))
  expect_false(community$is_at_least("reviewed"))
  expect_false(community$is_at_least("official"))

  # reviewed
  expect_true(reviewed$is_at_least("community"))
  expect_true(reviewed$is_at_least("reviewed"))
  expect_false(reviewed$is_at_least("official"))

  # official
  expect_true(official$is_at_least("community"))
  expect_true(official$is_at_least("reviewed"))
  expect_true(official$is_at_least("official"))
})

test_that("to_list serialization works", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  cert <- RecipeCertification$new(level = "official", certified_by = inst, notes = "Approved")
  lst <- cert$to_list()

  expect_type(lst, "list")
  expect_equal(lst$level, "official")
  expect_equal(lst$certified_by$name, "IECON")
  expect_equal(lst$notes, "Approved")
  expect_true(!is.null(lst$certified_at))
})

test_that("from_list deserialization works", {
  lst <- list(
    level = "reviewed",
    certified_by = list(
      name = "Maria",
      user_type = "institutional_member",
      institution = list(name = "IECON", user_type = "institution", verified = TRUE)
    ),
    certified_at = as.character(Sys.time()),
    notes = "Peer reviewed"
  )
  cert <- RecipeCertification$from_list(lst)
  expect_s3_class(cert, "RecipeCertification")
  expect_equal(cert$level, "reviewed")
  expect_equal(cert$certified_by$name, "Maria")
  expect_equal(cert$notes, "Peer reviewed")
})

test_that("to_list/from_list round-trip", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution", verified = TRUE)
  cert <- RecipeCertification$new(level = "official", certified_by = inst, notes = "v1 approved")

  restored <- RecipeCertification$from_list(cert$to_list())
  expect_equal(restored$level, "official")
  expect_equal(restored$certified_by$name, "IECON")
  expect_equal(restored$notes, "v1 approved")
})

test_that("from_list with NULL returns NULL", {
  expect_null(RecipeCertification$from_list(NULL))
})

test_that("community from_list with no certified_by", {
  lst <- list(level = "community", certified_at = as.character(Sys.time()))
  cert <- RecipeCertification$from_list(lst)
  expect_equal(cert$level, "community")
  expect_null(cert$certified_by)
})

test_that("print method works for community", {
  cert <- RecipeCertification$new(level = "community")
  expect_output(print(cert), "community")
})

test_that("print method works for official", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  cert <- RecipeCertification$new(level = "official", certified_by = inst)
  expect_output(print(cert), "official")
})

test_that("numeric_level returns correct ordering", {
  community <- RecipeCertification$new(level = "community")
  inst <- RecipeUser$new(name = "I", user_type = "institution")
  member <- RecipeUser$new(name = "M", user_type = "institutional_member", institution = inst)
  reviewed <- RecipeCertification$new(level = "reviewed", certified_by = member)
  official <- RecipeCertification$new(level = "official", certified_by = inst)

  expect_equal(community$numeric_level(), 1L)
  expect_equal(reviewed$numeric_level(), 2L)
  expect_equal(official$numeric_level(), 3L)
})
