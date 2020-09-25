# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library("data.table")
library("sf")
library("stars")
library("lwgeom")
library("doParallel")
library("parallel")
library("foreach")
library("filelock")
library("purrr")
library("units")

library("roxygen2")
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


