#' Data frame with lists of footprint metrics, names, and groups to look-up
#' 
#' 

fs_footprint_metrics <- data.frame(
  'name' = c("fs_settled", "fs_area_mean", "fs_area_total", "fs_area_cv"),
  'short_name' = c("settled", "area_mean", "area_total","area_cv"),
  'group' = c("binary", "area", "area", "area"),
  'units' = c(NULL, "ha", "ha", NULL),
  stringsAsFactors=FALSE
)

# save(fs_footprint_metrics, file="../R/sysdata.rda")

usethis::use_data(fs_footprint_metrics, internal=FALSE, overwrite=TRUE)

