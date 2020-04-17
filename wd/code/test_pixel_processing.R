# testing pixel-level covariates processing

library(stars)
library(sf)
library(foot)

r <- read_stars("C:/Users/Admin/Documents/GitHub/foot/wd/in/ssd_mgrid.tif", proxy=T)
  r

# create an even grid of 1k x 1k pixels covering mastergrid bounding box
xfrom <- st_dimensions(r)$x$from
xto <- st_dimensions(r)$x$to
yfrom <- st_dimensions(r)$y$from
yto <- st_dimensions(r)$y$to

xl <- seq(xfrom, xto, 1000)
xu <- xl - 1L + 1000
yl <- seq(yfrom, yto, 1000)
yu <- yl - 1L + 1000

l <- expand.grid(xl=xl, yl=yl)
u <- expand.grid(xu=xu, yu=yu)
tiles <- cbind(l, u)

tiles[tiles$xl < xfrom, "xl"] <- xfrom
tiles[tiles$yl < yfrom, "yl"] <- yfrom
tiles[tiles$xu > xto, "xu"] <- xto
tiles[tiles$yu > yto, "yu"] <- yto

tiles$cropXsize <- tiles$xu - tiles$xl + 1L
tiles$cropYsize <- tiles$yu - tiles$yl + 1L

dim(tiles)

# get an example tile from the middle
rr <- read_stars(path,
                 RasterIO=list(nXOff = tiles[60,1], 
                               nYOff = tiles[60,2], 
                               nXSize = tiles[60,5], 
                               nYSize = tiles[60,6], 
                               bands = c(1)),
                 proxy=T)
# set-up for spatial filter
wkt <- st_as_text(st_as_sfc(st_bbox(rr)))

# read and spatial filter the building footprints - could this be faster?
build <- st_read("Z:/Projects/WP517763_GRID3/Working/SSD/sett/made/South_Sudan_building_merge_wgs84.shp", 
                 wkt_filter=wkt)

# for simplicity, read in and convert to a polygon grid type (drops NA pixels)
rf <- st_as_stars(rr)
rf <- st_as_sf(rf)
# create "zone ID" from the cell IDs
rf$cid <- raster::cellFromXY(raster::raster(path), st_coordinates(rr, center=T))

# create an index between the buildings and grid cells
zi <- zonalIndex(build, rf, zoneField="cid", returnObject=T, clip=F)

# calculate 
px_area <- calculate_footstats(zi, index=zi$cid, metrics="fs_area_total")
  dim(px_area)
  px_area
  