# Ver RFC-MODULARIZACION.md · metapaquete metasurvey

test_that("core symbols are re-exported", {
  for (s in c("Survey", "Recipe", "step_compute", "workflow", "bake_steps")) {
    expect_true(s %in% getNamespaceExports("metasurvey"), info = s)
  }
})

test_that("sub-package symbols are re-exported", {
  expect_true("transpile_stata" %in% getNamespaceExports("metasurvey"))
  expect_true("anda_variables" %in% getNamespaceExports("metasurvey"))
  expect_true("api_login" %in% getNamespaceExports("metasurvey"))
})

test_that("orchestration helpers are exported", {
  expect_true(is.function(reproduce_workflow))
  expect_true(is.function(resolve_weight_spec))
})

test_that("reproduce_workflow validates its input", {
  expect_error(reproduce_workflow(list()), "RecipeWorkflow")
})

test_that("resolve_weight_spec returns NULL for NULL input", {
  expect_null(resolve_weight_spec(NULL))
})

test_that("a re-exported symbol points to its source", {
  # R6 generators are environments; compare the generator's classname and a
  # plain function by body rather than environment identity.
  expect_equal(Survey$classname, metasurvey.core::Survey$classname)
  expect_identical(
    body(transpile_stata),
    body(metasurvey.fromstata::transpile_stata)
  )
})
