if (!require("pkgdown", quietly = TRUE)) {
  install.packages("pkgdown", repos = "https://cloud.r-project.org")
}
# Build without running examples
pkgdown::build_site(install = FALSE, examples = FALSE, new_process = FALSE)
