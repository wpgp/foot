
library(foot)
library(stars)
library(sf)
library(wpgpDownloadR)

bldgPath <- "/mnt/worldpopFS/Projects/WP517763_GRID3/Working/SSD/sett/made_2/ssd_buildings_wgs84_merge.gpkg"
shpPath <- "/mnt/worldpopFS/Projects/WP517763_GRID3/DataIn/raw/DigitizeAfrica_building_footprints/SSD/updates/111819/UTM35N/UTM35N/South_Sudan_UTM35N.shp"
# bldgPath <- "/home/jochem/Documents/projects/zmb/dataIn"

# load the mastergrid
mgPath <- wpgpGetCountryDataset(ISO3="SSD", 
                                covariate="level0_100m_2000_2020")
mgrid <- read_stars(mgPath, proxy=TRUE)

#output template - need one per metric (store filepaths)
outTemplate <- st_as_stars(matrix(NA, nrow=nrow(mgrid), ncol=ncol(mgrid)), 
                           dimensions=st_dimensions(mgrid))

write_stars(outTemplate, paste0(tempdir(), "/SSD_temp.tif")) # default is float32

# create tiles
tiles <- gridTiles(mgrid, 
                   px= c(500, 500), #c(200, 200), # approximate size row, col pixels
                   overlap=0
)

# create buffered tiles (assuming max of 250m buffer at each location)
# tilesBuff <- gridTiles(mgrid,
#                        px=c(200,200),
#                        overlap=3  # overlap in pixels, at least 250m
# )

head(tiles)


i <- 1500 # example
job <- tiles[i,]
# get the tile extent
rio <- list(nXOff = job[[1]], 
            nYOff = job[[2]], 
            nXSize = job[[5]], 
            nYSize = job[[6]], 
            bands = c(1))

# create a tile from the mastergrid
# without a buffer will define the output locations
# mgTile <- read_stars(mgPath,
#                      RasterIO=rio,
#                      proxy=TRUE
# )

mgTile <- st_as_stars(mgrid[,job$xl:job$xu, job$yl:job$yu])

  st_dimensions(mgTile)
  dim(mgTile)

# blank tile for the results
naTile <- st_as_stars(matrix(NA, nrow=nrow(mgTile), ncol=ncol(mgTile)), 
                      dimensions=st_dimensions(mgTile))

# set-up a spatial filter for file reading
# based on the bounding box of the tile
bbox <- st_as_sfc(st_bbox(mgTile))
# bbox <- st_transform(bbox, crs=32735)
wkt <- st_as_text(bbox)

# read in the footprints
buildings <- st_read(bldgPath,
                     wkt_filter=wkt
)
  buildings

mgPoly <- st_as_sf(st_as_stars(mgTile))
mgPoly$id <- 1:nrow(mgPoly)
  mgPoly
# at this point, can create a buffer for focal statistics
  
b <- zonalIndex(buildings, mgPoly)
  b
  
foot::fs_footprint_metrics

fsTile <- calculate_footstats(b, 
                              "zoneID", 
                              metrics=c("fs_area_mean",
                                        "fs_count",
                                        "fs_area_cv",
                                        "fs_angle_entropy",
                                        "fs_shape_mean",
                                        "fs_compact_mean"), 
                              verbose=T,
                              gridded=F)
  fsTile
  
mgPoly <- merge(mgPoly, fsTile, by.x="id", by.y="index")
units(mgPoly$fs_area_ha_mean) <- NULL
resArea <- st_rasterize(mgPoly["fs_area_ha_mean"], template=naTile)
  resArea
  plot(resArea)  
  
# update tile offset
d <- st_dimensions(resArea)
d[["x"]]$from <- job$xl
d[["x"]]$to <- job$xu
d[["y"]]$from <- job$yl
d[["y"]]$to <- job$yu

resArea <- structure(resArea, dimensions=d)
  resArea
  
write_stars(resArea, "Path/filename.tif", update=TRUE)




