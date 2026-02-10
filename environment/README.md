# Reproducible R environment

This project is designed to be reproducible via **renv**.

## Recommended setup (renv)

1. Open an R session at the repository root.
2. Install `renv` if needed:

```r
install.packages("renv")
```

3. Initialise an isolated project library:

```r
renv::init(bare = TRUE)
```

4. Install required packages (see `required_packages.csv`). One simple approach is:

```r
pkgs <- read.csv("environment/required_packages.csv")$package
install.packages(pkgs)
```

5. Snapshot the environment so others can reproduce it:

```r
renv::snapshot()
```

## Notes

- If you use any packages from sources other than CRAN (e.g., GitHub), install them first and then run `renv::snapshot()` so the lockfile records the source.
- For thesis archiving, keep `renv.lock` committed alongside the scripts and data.
