test_that(
    "Probar extraer time pattern anual",
    {
        testthat::expect_equal(
            metasurvey:::extract_time_pattern("ECH_2023"),
            list(
                type = "ECH",
                year = 2023,
                periodicity = "Annual"
            )
        )
    }
)


test_that(
    "Probar extraer time pattern mensual",
    {
        testthat::expect_equal(
            metasurvey:::extract_time_pattern("ECH_2023_01"),
            list(
                type = "ECH",
                year = 2023,
                month = 1,
                periodicity = "Monthly"
            )
        )
    }
)

test_that(
    "Probar extraer time pesos replicados en MM-AAAA",
    {
        testthat::expect_equal(
            metasurvey:::extract_time_pattern("pesos_replicados_01-2023"),
            list(
                type = "pesos",
                year = 2023,
                month = 1,
                periodicity = "Monthly"
            )
        )
    }
)

test_that(
    "Probar extraer time pattern trianual",
    {
        testthat::expect_equal(
            metasurvey:::extract_time_pattern("ECH_2023_2025"),
            list(
                type = "ECH",
                year_start = 2023,
                year_end = 2025,
                periodicity = "Trianual"
            )
        )
    }
)

test_that(
    "Probar extraer time pattern multianual",
    {
        testthat::expect_equal(
            metasurvey:::extract_time_pattern("ECH_2023_2026"),
            list(
                type = "ECH",
                year_start = 2023,
                year_end = 2026,
                periodicity = "Multianual"
            )
        )
    }
)

test_that(
    "Probar extraer time pattern mensual  MMYYYY",
    {
        testthat::expect_equal(
            metasurvey:::extract_time_pattern("ECH_012023"),
            list(
                type = "ECH",
                year = 2023,
                month = 1,
                periodicity = "Monthly"
            )
        )
    }
)

test_that(
    "Probar extraer time pattern mensual  MMYY",
    {
        testthat::expect_equal(
            metasurvey:::extract_time_pattern("ECH_0123"),
            list(
                type = "ECH",
                year = 2023,
                month = 1,
                periodicity = "Monthly"
            )
        )
    }
)

