# rtemis.core CRAN comments

## URL check

`urlchecker::url_check()`

All URLs are correct

## Local check

macOS 27.0

`R CMD check --as-cran`

0 errors, 0 warnings, 0 notes

## Cross-platform checks

`rhub::rhub_check(platforms = c('linux', 'macos-arm64', 'windows'))`

0 errors, 0 warnings, 0 notes

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
