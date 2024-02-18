test_that(
  "Engine por defecto",
  {
    testthat::expect_message(
      set_engine(),
      "Engine: data.table"
    )
  }
)


test_that(
  "Engine no soportado",
  {
    testthat::expect_error(
      set_engine("python")
    )
  }
)


test_that(
  "Custom engine",
  {
    testthat::expect_message(
      set_engine("tidyverse"),
      "Engine: tidyverse"
    )
  }
)
