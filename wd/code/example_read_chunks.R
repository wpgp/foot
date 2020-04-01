#
# Example script working with South Sudan building footprint files
#
# ...I needed a solution for working with limited RAM on my laptop...
# 
# Reading and writing "chunks" of shapefile rows
# Chunking based on SQL query limits of row counts
# Output to Geopackage which is faster than Shapefile
# 
# Chris Jochem
#


library(sf)
# library(data.table)

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

# Test on first .shp -- or loop over the list of allFiles
fileName <- sub(pattern="(.*)\\..*$", replacement="\\1", basename(allFiles[1]))  
chunk <- 1  # starting chunk
chunkSize <- 1e+05L  # number of rows per chunk, must be integer 'L'
hasNext <- TRUE  # check if file finished, because final length is unknown (or use an iterator?)

# Loop - Read - Process - Write chunks of files
while(hasNext){
  print(chunk)
  shpChunk <- st_read(allFiles[1], 
                      query=paste0("SELECT * FROM ", fileName, 
                                   " LIMIT ", chunkSize , " OFFSET ", as.integer((chunk-1)*chunkSize)))
  # write to Geopackage
  st_write(shpChunk, 
           dsn=paste(dir, outputData, paste0(fileName, ".gpkg"), sep="/"), 
           layer="buildings", 
           driver="GPKG", 
           append=TRUE) # requires sf > ver. 0.8
  # check if at EOF
  if(nrow(shpChunk) < chunkSize){
    hasNext <- FALSE # break the loop
  } else{
    chunk <- chunk + 1 # increment chunk counter
  }
}


