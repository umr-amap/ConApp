if (!require("pkgdown", quietly = TRUE)) {
  install.packages("pkgdown", repos = "https://cloud.r-project.org")
}
# Build in a fresh process (default behavior)
pkgdown::build_site(install = FALSE, examples = FALSE)
