test_that("included vignettes have no relative links to excluded vignettes", {
  skip_on_cran()

  pkg_root <- system.file(package = "metasurvey")

  # When running under devtools::test(), system.file points to the source tree
  # Fall back to walking up from working directory

  if (pkg_root == "" || !file.exists(file.path(pkg_root, ".Rbuildignore"))) {
    pkg_root <- getwd()
    while (pkg_root != dirname(pkg_root)) {
      if (file.exists(file.path(pkg_root, "DESCRIPTION"))) break
      pkg_root <- dirname(pkg_root)
    }
  }

  rbuildignore_path <- file.path(pkg_root, ".Rbuildignore")
  vignettes_dir <- file.path(pkg_root, "vignettes")
  skip_if_not(file.exists(rbuildignore_path))
  skip_if_not(dir.exists(vignettes_dir))

  rbuildignore <- readLines(rbuildignore_path)

  # Extract excluded vignette basenames from .Rbuildignore
  # Patterns look like: ^vignettes/api-database\.Rmd$
  excluded_patterns <- grep("^\\^vignettes/.*\\.Rmd\\$$", rbuildignore,
                            value = TRUE)
  excluded_names <- gsub("^\\^vignettes/", "", excluded_patterns)
  excluded_names <- gsub("\\\\\\.Rmd\\$$", "", excluded_names)

  # All vignette Rmd files
  all_rmds <- list.files(vignettes_dir, pattern = "\\.Rmd$")
  included_rmds <- setdiff(all_rmds, paste0(excluded_names, ".Rmd"))

  broken <- character(0)

  for (rmd in included_rmds) {
    content <- readLines(file.path(vignettes_dir, rmd))
    # Find relative .html links in markdown: ](name.html)
    matches <- unlist(regmatches(
      content,
      gregexpr("\\]\\([a-zA-Z][-a-zA-Z0-9]*\\.html\\)", content)
    ))
    if (length(matches) == 0) next

    targets <- gsub("^\\]\\(|\\.html\\)$", "", matches)
    bad <- intersect(targets, excluded_names)
    if (length(bad) > 0) {
      broken <- c(broken, sprintf(
        "  %s -> %s",
        rmd, paste(bad, collapse = ", ")
      ))
    }
  }

  expect(
    length(broken) == 0,
    sprintf(
      paste(
        "Included vignettes link to excluded vignettes (causes CRAN rejection).",
        "Use absolute pkgdown URLs instead:\n%s"
      ),
      paste(broken, collapse = "\n")
    )
  )
})
