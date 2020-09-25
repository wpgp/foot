# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

usethis::use_package("data.table")
usethis::use_package("sf")
usethis::use_package("stars")
usethis::use_package("lwgeom")
usethis::use_package("doParallel")
usethis::use_package("parallel")
usethis::use_package("foreach")
usethis::use_package("filelock")
# usethis::use_package("purrr")
usethis::use_package("units")

# library("roxygen2")
# package documentation
devtools::document()

# # vignettes
# devtools::build_vignettes()

# install package
devtools::build()
# install.packages(getwd(), repo=NULL, type='source')
devtools::install()
devtools::reload(devtools::inst('foot'))
.rs.restartR()


