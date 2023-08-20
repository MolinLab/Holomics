## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


## rhub::check_for_cran() results
### Errors
* checking package dependencies ... ERROR
Packages required but not available: 'mixOmics', 'BiocParallel'

Both are Bioconductor packages and can therefore not be found in CRAN,
this is why the check fails.

### Warnings
* Strong dependencies not in mainstream repositories:
  mixOmics
  
Again this is a Bioconductor package and the dependency is on purpose.

## Additional notes
The example in run_app.R needs to have \dontrun, although its the first submission to CRAN, because
its showing how to start the shiny application and does not end, so the automated tests would never finish.
