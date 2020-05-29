# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

usethis::use_package("data.table")
usethis::use_package("sf")
usethis::use_package("stars")
usethis::use_package("lwgeom")
usethis::use_package("purrr")

# package documentation
devtools::document()

# # vignettes
# devtools::build_vignettes()

# install package
# install.packages(getwd(), repo=NULL, type='source')
devtools::install()
devtools::reload(devtools::inst('foot'))
.rs.restartR()


