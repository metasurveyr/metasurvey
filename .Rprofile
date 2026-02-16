# source("renv/activate.R")

if (!nzchar(Sys.getenv("CI"))) {

library(utils)

clean_directory_ech <- function(cache = TRUE) {
  # Remove example-data directory if it exists
  if (cache) {
    message("Cache is enabled, skipping directory cleanup")
  } else {
    if (dir.exists("example-data")) {
      unlink("example-data", recursive = TRUE)
      sys("rm -rf example-data")
      message("Removed existing example-data directory")
    } else {
      message("No existing example-data directory to remove")
    }
  }
}

download_example_ech <- function() {
  zip_url <- "https://informe-tfg.s3.us-east-2.amazonaws.com/example-data.zip"
  dest_zip <- "example-data.zip"
  temp_dir <- tempfile("example-data")

  # Download the zip file
  download.file(zip_url, destfile = dest_zip, mode = "wb")
  message("Downloaded example-data.zip")

  # Unzip to temporary directory
  dir.create(temp_dir)
  unzip(dest_zip, exdir = temp_dir)
  message("Unzipped example-data.zip")

  target_dir <- file.path("example-data")
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  file.rename(
    list.files(file.path(temp_dir, "example-data"), full.names = TRUE),
    file.path(target_dir, basename(list.files(file.path(temp_dir, "example-data"))))
  )
  message("Moved unzipped data to chapters/example-data")

  # Cleanup
  unlink(dest_zip)
  unlink(temp_dir, recursive = TRUE)
  message("Cleanup done")
}


if (dir.exists("example-data")) {
  message("example-data directory already exists")
  cache <- TRUE
} else {
  message("example-data directory does not exist")
  cache <- FALSE
}

if (cache) {
  message("Cache is enabled, skipping directory cleanup")
} else {
  clean_directory_ech(cache = FALSE)
  download_example_ech()
}

} # end CI guard
