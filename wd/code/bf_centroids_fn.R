
# This R script contains a function that creates the following rasters for a specified country using building footprints (Digitize Africa data © 2020 Maxar Technologies, Ecopia.AI):
# 1. binary settled-unsettled
# 2. building count per pixel
# 3. building density per pixel
# 4. coefficient of variations in building area per pixel
# 5. mean building area per pixel
# 6. pixel area
# 7. standard deviation in building area per pixel
# 8. total building area per pixel

# all rasters are based on building footprint centroids (i.e. a building 'occupies' the pixel in which its centroid falls)
# running time was ~40mins for Zambia (~8.5 million building centroids) and ~ 1 hour for Ghana (~10.2 million building centroids)

# Current script structure is 1. the function, 2. an example run, 3. some code to source mastergrids from the wp ftp site




library(sf)
library(rgdal)
library(rgeos)
library(data.table)


### bf_rasters function 

bf_rasters <- function(ISO_code, country_name, bf_char, master_grid_raster,
                      min_thresh = 0, max_thresh = 10000000, output_filepath, fgdb_filepath){
  
# # need to add initial checks # #

# define input file geodatabase
fgdb <- fgdb_filepath
  
# list all feature classes in the file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)

# create centroids for each of the feature classes
centroids_wgs <- list()
for(i in 1:length(fc_list)){
  bf <- read_sf(dsn=fgdb,layer=paste(fc_list[i]))
  if(min_thresh > 0){bf <- bf[bf$Shape_Area>=min_thresh,]}
  if(max_thresh < 1000000){bf <- bf[bf$Shape_Area<=max_thresh,]}
  centroids_utm <- st_centroid(bf)
  rm(bf)
  centroids_utm <- centroids_utm[,bf_char]
  centroids_wgs[[i]] <- st_transform(centroids_utm, crs ="+proj=longlat +datum=WGS84") # reproject
  rm(centroids_utm)
}

# merge centroid layers
centroids_wgs_master <- do.call(rbind, centroids_wgs)
rm(centroids_wgs)
# get cellID for each building
points_IDs <- cellFromXY(master_grid_raster, as(centroids_wgs_master, "Spatial"))
# create data.table including area/length, each row is a building
dt <- data.table(cellID = points_IDs,centroids_wgs_master[,bf_char])
dt <- dt[,1:2]
names(dt)[2] <- 'barea'
# exclude NA cellIDs
dt <- na.omit(dt)

# sum buildings for each cellID
bc <- dt[,.N,by=cellID]

# sum building areas for each cellID
tba <- dt[,.(totarea = sum(barea)),by=cellID]

# mean building areas for each cellID
mba <- dt[,.(meanarea = mean(barea)),by=cellID]

# sd building areas for each cellID
sdba <- dt[,.(sdarea = sd(barea)),by=cellID]
sdba$barea[is.na(sdba$sdarea)] <- 0

# cv building areas for each cellID
cvba <- dt[,.(cvarea = sd(barea)/mean(barea)),by=cellID]
cvba$barea[is.na(cvba$cvarea)] <- 0

# master grid
mgrid <- master_grid_raster
mgrid[] <- NA

# create binary raster
binr <- mgrid
binr[bc$cellID] <- 1
writeRaster(binr,filename = paste(output_filepath,'/',ISO_code,'_binary_settled.tif',sep=''))
rm(binr)

# create rasters
tbar <- mgrid
tbar[tba$cellID] <- tba$totarea
writeRaster(tbar,filename = paste(output_filepath,'/',ISO_code,'_total_building_area.tif',sep=''))
rm(tbar)

mbar <- mgrid
mbar[mba$cellID] <- mba$meanarea
writeRaster(mbar,filename = paste(output_filepath,'/',ISO_code,'_mean_building_area.tif',sep=''))
rm(mbar)

sdbar <- mgrid
sdbar[mba$cellID] <- sdba$sdarea
writeRaster(sdbar,filename = paste(output_filepath,'/',ISO_code,'_sd_building_area.tif',sep=''))
rm(sdbar)

cvbar <- mgrid
cvbar[mba$cellID] <- cvba$cvarea
writeRaster(cvbar,filename = paste(output_filepath,'/',ISO_code,'_cv_building_area.tif',sep=''))
rm(cvbar)

bcr <- mgrid
bcr[bc$cellID] <- bc$N

# create px area raster
pxarea <- area(mgrid)

# create density raster: bc/px area
bdensr <- bcr/pxarea

writeRaster(bcr,filename = paste(output_filepath,'/',ISO_code,'_building_count.tif',sep=''))
writeRaster(bdensr,filename = paste(output_filepath,'/',ISO_code,'_building_density.tif',sep=''))
writeRaster(pxarea,filename = paste(output_filepath,'/',ISO_code,'_pixel_area.tif',sep=''))


# # or return rasterstack # #

}

#


### run bf_rasters function

bf_rasters(ISO_code = 'GHA', country_name ='GHANA', bf_char = 'Shape_Area',
           master_grid_raster = raster("Z:/Projects/WP517763_GRID3/Working/GHA/Building_footprint_layers/gha_level0_100m_2000_2020.tif"),
           out_filepath = 'Z:/Projects/WP517763_GRID3/Working/GHA/Building_footprint_layers',
           fgdb_filepath = 'Z:/Projects/WP517763_GRID3/DataIn/raw/DigitizeAfrica_building_footprints/GHA/AFRICA_GHANA_building/AFRICA_GHANA_building.gdb')


### might be helpful to source the mastergrid from ftp site

library(curl)
temp_dir <- 'Z:/Personal/cad1c14'
isocode <- 'GHA'

urlx <- paste("ftp://ftp.worldpop.org/GIS/Mastergrid/Global_2000_2020/",isocode,"/L0/",tolower(isocode),"_level0_100m_2000_2020.tif",sep='')
utils::download.file(url = urlx,
                     destfile = paste(temp_dir,"/temp.tif",sep=''),
                     mode="wb",
                     quiet=FALSE,
                     method="libcurl")

mg <- raster(paste(temp_dir,"/temp.tif",sep=''))

# then do 'master_grid_raster = mg' in function specs

