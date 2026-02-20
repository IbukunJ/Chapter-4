# Install required packages for Chapter 4 analysis
# Run from the repository root.

pkgs <- read.csv("environment/required_packages.csv", stringsAsFactors = FALSE)$package

# Install missing packages
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing)
}

message("Done. Consider running renv::snapshot() to create a lockfile.")
