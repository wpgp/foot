#
# Example script with basic geometry processing for building footprints
# There is potential to combine these processing steps in to a longer
#  function and more efficiently parallelise the loops.
#
# Chris Jochem
#


library(sf)
library(units)
library(lwgeom)
library(wpgpDownloadR)
library(raster)
library(data.table)

# set paths to data
dir <- "//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_GRID3"
inputData <- "DataIn/raw/DigitizeAfrica_building_footprints"
outputData <- "Working/SSD/sett"
iso <- "SSD"

# find all shapefiles in directory
folderPath <- paste(dir, inputData, iso, "South_Sudan", sep="/")
# recursive search for all matching shapefiles
allFiles <- list.files(path=folderPath, 
                        pattern="\\.shp$",
                        recursive=TRUE,
                        full.names=TRUE)
  allFiles

# read into a list of files ... a little slow
# or use a parallelised loop for all reading/processing
buildings <- lapply(allFiles, function(x){ st_read(x) })  

# calculate basic geometry-based measures
buildings <- lapply(buildings, function(b){
  b$year <- as.numeric(substr(as.character(b$ImgDate), 1, 4))  # source image date
  b$area <- st_area(b)  # units set by projection (usually meters)
  b$area_ha <- set_units(b$area, ha)  # convert to hectares
  
  b$perim <- lwgeom::st_perimeter(b)  # units by projection
  
  return(b)
})

# get master grid from WorldPop Global Project
# Rasters will be created to align to the origin, extent, and resolution
mgrid <- raster(wpgpGetCountryDataset(ISO3="SSD", covariate="level0_100m_2000_2020"))

# centroid-based processing
building_pts <- lapply(buildings, function(b){
  # create centroids
  b <- st_centroid(b)
  # transform points to match grid projection (usually WGS84)
  b <- st_transform(b, crs=st_crs(mgrid)$epsg)
  # get a cell ID to the mastergrid
  b$cID <- cellFromXY(mgrid, st_coordinates(b))
  
  return(b)
})

rm(buildings); gc()  # not using polygons

# combine into a single file
# this could be useful to searcg local areas or combine regions
# or consider doing calculations on each file independently
# use data.table trick
# source: https://github.com/r-spatial/sf/issues/798#issuecomment-405171212
building_pts <- st_as_sf(data.table::rbindlist(building_pts))
  dim(building_pts)  # n= 3706822
  head(building_pts)

building_pts <- data.table(st_drop_geometry(building_pts))
# remove records that don't cover mastergrid
setkey(building_pts, cID)
building_pts <- building_pts[!is.na(cID)]

# summary by mastergrid cell locations
building_summary <- building_pts[, .(built=1, 
                                     # year=mean(year), 
                                     count=.N,
                                     total_area=sum(area_ha),
                                     total_perim=sum(perim),
                                     p_a_ratio=mean(perim / area),  # using m^2
                                     area_mean=mean(area_ha),
                                     area_sd=sd(area_ha),
                                     perim_mean=mean(perim),
                                     perim_sd=sd(perim)
                                    ), 
                                 by=cID]
# year by mode
modeYr <- setkeyv(building_pts[, .N, by=.(cID, year)], c("cID","N"))[, .(year=year[.N]), by=cID]
# calculate group-level variations
building_summary$area_cv <- building_summary$area_sd / building_summary$area_mean
building_summary$perim_cv <- building_summary$perim_sd / building_summary$perim_mean
  building_summary
  
