smwrQW
========

R functions to support water-quality data analysis for statistical methods in water resources.

## Package Status
[![status](https://img.shields.io/badge/USGS-Orphan-red.svg)](https://owi.usgs.gov/R/packages.html#orphan)

This package is currently in an 'orphaned' state, and
looking for a new maintainer. For more information, see:
[https://owi.usgs.gov/R/packages.html#orphan](https://owi.usgs.gov/R/packages.html#orphan)

If you are interested in becoming the official maintainer of `rloadest`, please email gs-w_r_admin@usgs.gov.

In the meantime, we rely on community involvement to report and fix bugs.

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://github.com/USGS-R/smwrQW/issues](https://github.com/USGS-R/rloadest/issues)

Follow `@USGS-R` on Twitter for updates on USGS R packages:
[https://twitter.com/USGS_R](https://twitter.com/USGS_R)

### Current build tests:

|Linux|Test Coverage|
|----------|------------|
| [![travis](https://travis-ci.org/USGS-R/smwrQW.svg?branch=master)](https://travis-ci.org/USGS-R/smwrQW)|[![Coverage Status](https://coveralls.io/repos/github/USGS-R/smwrQW/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/smwrQW?branch=master)|

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/rloadest/blob/master/CONDUCT.md) for more information.

## Package Installation
To install the `smwrQW` package:

USGS R Installation Instructions: [https://owi.usgs.gov/R/training-curriculum/installr/](https://owi.usgs.gov/R/training-curriculum/installr/)

1. Install R (version 3.0 or greater) from: [https://cran.r-project.org/](https://cran.r-project.org/)

2. Install RStudio from: [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

3. Add the USGS R repository to your "Rprofile" to get automatic updates. Run the following code:
  
  ```r
  rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
  write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

  cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')
  ```

4. Restart R!

5. In the RStudio "Console" window (usually left or lower-left), run the following command:

  ```r
  install.packages("rloadest")
  ```
  

6. Update often. Next time you update your packages, `rloadest` will automatically update:

   ![update](images/update.png)
   

