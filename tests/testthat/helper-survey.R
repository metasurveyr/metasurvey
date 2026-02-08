# Helper functions for tests
# Creates a minimal Survey object for testing purposes

make_test_survey <- function(n = 10) {
  set.seed(42)
  df <- data.table::data.table(
    id = seq_len(n),
    age = sample(18:65, n, replace = TRUE),
    income = round(runif(n, 1000, 5000), 2),
    region = sample(1:4, n, replace = TRUE),
    status = sample(1:3, n, replace = TRUE),
    w = round(runif(n, 0.5, 2), 4)
  )
  Survey$new(
    data = df,
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
}

make_test_data <- function(n = 10) {
  set.seed(42)
  data.table::data.table(
    id = seq_len(n),
    age = sample(18:65, n, replace = TRUE),
    income = round(runif(n, 1000, 5000), 2),
    region = sample(1:4, n, replace = TRUE),
    status = sample(1:3, n, replace = TRUE),
    w = round(runif(n, 0.5, 2), 4)
  )
}
