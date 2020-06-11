## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy.opts=list(width.cutoff=80), 
  tidy=TRUE
)

## ----setup--------------------------------------------------------------------
library(foot)

## -----------------------------------------------------------------------------
data("kampala")

buildings <- kampala$buildings
adminzones <- kampala$adminZones
clusters <- kampala$clusters

## ----echo=FALSE, fig.cap="Sample buildings and zones in Kampala", fig.height=6, fig.width=6, message=FALSE, warning=FALSE----
plot(sf::st_geometry(buildings), 
     col=sf::sf.colors(12, categorical=TRUE),
     border=NA, axes=TRUE)

plot(sf::st_geometry(adminzones), border="red", add=T)

## -----------------------------------------------------------------------------
# mean of all footprint areas
built_area <- fs_area_mean(buildings)

built_area

# standard deviation of perimeters
perim_sd <- fs_perim_sd(buildings)

perim_sd

# median of areas
area_med <- fs_area_median(buildings, unit="m^2")
area_med

## ---- tidy=F------------------------------------------------------------------
# random sample of buildings
result <- buildings %>%
          dplyr::filter(FID_1 %in% sample(FID_1, 1000)) %>%
          fs_area_mean()

# show the resulting table
result

## -----------------------------------------------------------------------------
# nearest distance for the first 10 buildings to any other building
# measured between polygon edges
fs_nndist(buildings[1:10,], buildings, maxSearch=200, unit="m")

# omitting 'Y' measures distance among the footprints supplied
fs_nndist(buildings[1:10,], maxSearch=NULL)  # unrestricted distance

## -----------------------------------------------------------------------------
# To obtain the rotated rectangle as a spatial object
mbr <- fs_mbr(buildings[5,], returnShape=T)

plot(sf::st_geometry(buildings[5,]))
plot(mbr, border="red", add=T)

# Or obtain just the angle
fs_mbr(buildings[5,], returnShape=F)


## -----------------------------------------------------------------------------
# create a vector of ten random zones
id <- sample(1:10, nrow(buildings), replace=T)
buildings$id <- id   # add it to the buildings object
  table(buildings$id)  # splitting observations into 10 groups

# pass the index by name
fs_area_total(buildings, index="id")  # note default units

# pass the index by column number
fs_count(buildings, index=3)

# pass a separate vector object
print(fs_angle_entropy(buildings, index=id, normalize=T))

## ----message=FALSE, warning=FALSE---------------------------------------------
# Return a table of index values based on administrative zone polygons
zID <- zonalIndex(buildings, adminzones, returnObject=F)
  zID # the xID column are row numbers
  
# Alternatively, join the zones to create and return a new spatial object
# A custom zone name can be added but must be supplied to the summary functions
zObj <- zonalIndex(buildings, clusters, zoneField="Id", returnObject=T)
  zObj

## ----fig.height=6, fig.width=6------------------------------------------------
# use the new object
zarea <- fs_area_mean(zObj, index="Id")  
clusters <- merge(clusters, zarea, by.x="Id", by.y="index")
  plot(clusters["fs_area_ha_mean"])

## ----warning=FALSE------------------------------------------------------------
# Note the selected structures extend beyond the cluster boundary
plot(sf::st_geometry(clusters)[[6]])
plot(sf::st_geometry(buildings), add=T)
plot(sf::st_geometry(zObj[zObj$Id==6,]), col="red", add=T)

## ----message=FALSE, warning=FALSE---------------------------------------------
zClip <- zonalIndex(buildings, clusters, zoneField="Id", clip=T)

plot(sf::st_geometry(clusters)[[6]])
plot(sf::st_geometry(buildings), add=T)
plot(sf::st_geometry(zClip[zClip$Id==6,]), col="red", add=T)

## ----message=FALSE, warning=FALSE---------------------------------------------
# create a temporary shape by shifting one cluster
newClusters <- st_sfc(sf::st_geometry(clusters)[[1]], 
                      sf::st_cast(sf::st_geometry(clusters)[[1]] + c(.001,.0001), "POLYGON"),
                      crs=sf::st_crs(clusters)
                     )

newClusters <- st_sf(geometry=newClusters, crs=sf::st_crs(clusters))

newObj <- zonalIndex(buildings, newClusters, clip=T)

# areas of overlap are in a purple hue
plot(sf::st_geometry(newClusters))
plot(sf::st_geometry(newObj[newObj$zoneID==1,]), col="red", add=T)
plot(sf::st_geometry(newObj[newObj$zoneID==2,]), col=sf::sf.colors(n=1, alpha = 0.5), add=T)
plot(sf::st_geometry(buildings), add=T)

## ---- tidy=F------------------------------------------------------------------
# Creates zonal index and calculates multiple metrics
results <- calculate_footstats(buildings, 
                               adminzones, 
                               metrics=c("fs_area_mean","fs_area_cv"), 
                               gridded=F)
  print(results)  

## ---- fig.width=6, fig.height=6, tidy=F---------------------------------------
mgrid <- kampala$mastergrid # ~100m raster layer

# get the cell numbers from the grid
cellIDs <- raster::cellFromXY(mgrid, 
                              sf::st_coordinates(sf::st_centroid(buildings)))
# use these numbers as the zone
gridRes <- calculate_footstats(buildings, 
                               index=cellIDs, 
                               metrics=c("fs_perim_total",
                                         "fs_area_total",
                                         "fs_count",
                                         "fs_settled"),
                               gridded=F)
# buildings lacking a grid cell are NA
gridRes <- na.omit(gridRes)
  gridRes
  
# create a blank raster and fill with summary measures
outSettled <- mgrid
outSettled[] <- NA
# `$index` are the cell numbers from the raster 
outSettled[gridRes$index] <- gridRes$fs_area_ha_total

outCount <- mgrid
outCount[] <- NA
outCount[gridRes$index] <- gridRes$fs_count

raster::plot(outCount)
plot(sf::st_geometry(buildings), add=T)

## ----message=FALSE, warning=FALSE, tidy=F-------------------------------------
# convert the grid to a `stars` object and then `sf` to get polygons
sgrid <- stars::st_as_stars(mgrid)
sgrid <- sf::st_as_sf(sgrid)

# add a cell number ID to the grid polygons
sgrid$cellIDs <- raster::cellFromXY(mgrid, sf::st_coordinates(sf::st_centroid(sgrid)))

# create a zonal index on the building objects
pxZone <- zonalIndex(buildings, 
                     sgrid, 
                     "cellIDs", 
                     returnObject=T, 
                     clip=T)

pxResult <- calculate_footstats(pxZone, 
                                index="cellIDs",
                                metrics=c("fs_area_total", "fs_settled"))
  print(pxResult)

## ---- fig.cap="Centroid-based (left) and a pixel-based (right) map of total building area", fig.show='hold', fig.width=6----
# create output
pxSettled <- mgrid
pxSettled[] <- NA
pxSettled[pxResult$index] <- pxResult$fs_area_ha_total

# compare centroid and pixel settled maps
par(mfrow=c(1,2))
raster::plot(outSettled, zlim=c(0,1), xlim=c(32.623, 32.626), ylim=c(0.338, 0.342))
plot(sf::st_geometry(buildings), add=T)
raster::plot(pxSettled, zlim=c(0,1), xlim=c(32.623, 32.626), ylim=c(0.338, 0.342))
plot(sf::st_geometry(buildings), add=T)

## ---- fig.width=6, fig.height=6, tidy=F---------------------------------------
outputPath <- tempdir()

# re-using the sf pixels as "zones"
calculate_footstats(buildings, 
                    index=sgrid, 
                    metrics=c("fs_area_mean", "fs_settled"),
                    gridded=TRUE, 
                    template=mgrid,  # using a rasterlayer
                    outputPath=outputPath # or a temp directory is created
                   )

# get the output
raster::plot(raster::raster(paste0(outputPath, "/fs_area_ha_mean.tif")))
plot(st_geometry(buildings), add=T)

## -----------------------------------------------------------------------------
sessionInfo()

