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

* Possibly misspelled words in DESCRIPTION:
    Holomics (17:24)
    Omics (3:55)
    omics (12:11, 12:41, 13:12, 13:55, 15:29)
    workflow (17:33)
These were ignored, as the words are spelled correctly

