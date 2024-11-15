test_that(
  "Cargar una encuesta de Panel (ECH 2023)",
  {
    path_dir <- here::here("example-data", "ech", "ech_2023")
    ech_2023 = load_panel_survey(
      path_implantation = file.path(path_dir, "ECH_implantacion_2023.csv"),
      path_follow_up = file.path(path_dir, "seguimiento"),
      svy_type = "ECH",
      svy_weight_implantation = add_weight(
        annual = "W_ANO"
      ),
      svy_weight_follow_up = add_weight(
        monthly = "W"
      )
    )
    testthat::expect_true(
      inherits(ech_2023, "RotativePanelSurvey")
    )
    testthat::expect_equal(
      ech_2023$get_type(),
      "ECH"
    )
    testthat::expect_equal(
      ech_2023$get_implantation()$periodicity,
      "Annual"
    )
    testthat::expect_equal(
      unique(sapply(ech_2023$get_follow_up(), function(f) f$periodicity)),
      "Monthly"
    )
    testthat::expect_equal(
      ech_2023$get_default_engine(),
      "data.table"
    )

    testthat::expect_equal(
      dim(ech_2023$get_implantation()$data),
      c(55378, 517)
    )

    testthat::expect_equal(
      ech_2023$get_follow_up()[[1]]$design$monthly$variables,
      {
        design = survey::svydesign(
          ids = ~1,
          weights = ~W,
          data = ech_2023$get_follow_up()[[1]]$data
        )
        design$variables
      }
    )

    testthat::expect_s3_class(
      ech_2023$get_follow_up()[[1]]$design$monthly,
      "survey.design"
    )


  }
)