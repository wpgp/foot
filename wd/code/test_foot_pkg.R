#
# TEST script for Foot package
# ** Run .pkg_building.R first

library(foot)
library(sf)
library(wpgpDownloadR)
library(raster)

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

# TESTING - read just part of the first file
fileName <- sub(pattern="(.*)\\..*$", replacement="\\1", basename(allFiles[1]))  
chunk <- 1  # starting chunk
chunkSize <- 100L  # number of rows per chunk, must be integer 'L'

  
buildings <- st_read(allFiles[[1]], 
                     query=paste0("SELECT * FROM ", fileName, 
                                  " LIMIT ", chunkSize , " OFFSET ", as.integer((chunk-1)*chunkSize)))

dim(buildings)
class(buildings)
head(buildings)

# get cell ID
mgrid <- raster(wpgpGetCountryDataset(ISO3="SSD", covariate="level0_100m_2000_2020"))

centroids <- st_centroid(buildings)
centroids <- st_transform(centroids, crs=st_crs(mgrid)$epsg)
cID <- cellFromXY(mgrid, st_coordinates(centroids)[,1:2])

calculate_footstats(buildings, index=cID, gridded=F)

fs_area_mean_calc(buildings, cID, "ha")
