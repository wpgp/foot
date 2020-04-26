# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

usethis::use_package("data.table")

# package documentation
devtools::document()

# # vignettes
# devtools::build_vignettes('pkg')

# install package
# install.packages(getwd(), repo=NULL, type='source')
devtools::install()
devtools::reload(devtools::inst('foot'))
.rs.restartR()

