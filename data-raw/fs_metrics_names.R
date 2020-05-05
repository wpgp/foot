#' Data frame with lists of footprint metrics, names, and groups to look-up
#' 
#' 

fs_footprint_metrics <- data.frame(
  'name' = c("fs_settled", "fs_count", 
             "fs_area_mean", "fs_area_sd", "fs_area_total", "fs_area_cv",
             "fs_perim_mean", "fs_perim_sd", "fs_perim_total", "fs_perim_cv",
             "fs_NNdist_mean", "fs_NNdist_sd", "fs_NNindex",
             "fs_angle_entropy", "fs_compact_mean"),
  'short_name' = c("settled", "count", 
                   "area_mean", "area_sd", "area_total", "area_cv",
                   "perim_mean", "perim_sd", "perim_total", "perim_cv",
                   "NNdist_mean", "NNdist_sd", "NNindex",
                   "angle_entropy", "compact_mean"),
  'group' = c("binary", "count", 
              "area", "area", "area", "area",
              "perim", "perim", "perim", "perim",
              "NNdist", "NNdist", "NNdist",
              "angle", "shape"),
  'default_units' = c("", "", 
                      "ha", "ha", "ha", "",
                      "m", "m", "m", "",
                      "m", "m", "m",
                      "", ""),
  stringsAsFactors=FALSE
)

# save(fs_footprint_metrics, file="../R/sysdata.rda")

usethis::use_data(fs_footprint_metrics, internal=FALSE, overwrite=TRUE)

