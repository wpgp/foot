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

# local files for testing
#st_write(buildings, "C:/Users/Admin/Documents/GitHub/foot/wd/in/ssd_sample_buildings.shp")
#writeRaster(mgrid, "C:/Users/Admin/Documents/GitHub/foot/wd/in/ssd_mgrid.tif")

#######
library(foot)
# load local files for testing
buildings <- st_read("C:/Users/Admin/Documents/GitHub/foot/wd/in/ssd_sample_buildings.shp")
mgrid <- raster("C:/Users/Admin/Documents/GitHub/foot/wd/in/ssd_mgrid.tif")


# NNI
nni <- calculate_footstats(buildings, metrics="fs_NNindex", gridded=F)

centroids <- st_centroid(buildings)
centroids <- st_transform(centroids, crs=st_crs(mgrid)$epsg)
cID <- cellFromXY(mgrid, st_coordinates(centroids))


res <- calculate_footstats(buildings, "all", index=cID, gridded=F)
print(res)

res <- calculate_footstats(buildings, "fs_settled", index=cID, gridded=F)
res

res <- calculate_footstats(buildings, "fs_area_mean", index=cID, gridded=F)
res

res <- calculate_footstats(buildings, "fs_area_total", index=cID, gridded=F
res

res <- calculate_footstats(buildings, cID, metrics=c("fs_area_mean","fs_area_sd", "area_cv"))
res

res <- calculate_footstats(buildings, cID, metrics=c("fs_perim_total","fs_area_sd"))

print(calculate_footstats(buildings,index=cID, "fs_area_cv" ))
calculate_footstats(buildings,index=cID, "fs_area_sd" )

fs_settled(buildings, cID)
fs_area_mean(buildings, cID)
fs_area_total(buildings, cID)

fs_area_mean(buildings, unit="m^2")  # no index, so group into 1
foot:::fs_area_mean_calc(buildings, cID, "ha")
fs_area_total(buildings)

# Adjacency
cells <- sample(1:ncell(mgrid), 100, replace=F)

adjM <- adjacentCells(mgrid, cells, directions=8) # matrix
head(adjM)

adjD <- adjacentCells(mgrid, cells, directions=8, dataTable=T) # DT
print(adjD)

adjR <- raster::adjacent(mgrid, cells, directions=8) # raster
adjR

cells <- sample(1:ncell(mgrid), 100000, replace=F)
adj1 <- adjacentCells(mgrid, cells, directions=8, dataTable=T)

adj2 <- adjacent(mgrid, cells, directions=8)

w <- make_circular_filter(25)
w


cells <- sample(1:ncell(mgrid), 1000000, replace=F)
w <- make_circular_filter(9)
w
adj1 <- adjacentCells(mgrid, cells, directions=w, dataTable=T)
adj2 <- raster::adjacent(mgrid, cells, directions=w)
