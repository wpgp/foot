#'
#'
#' @import mmap


make_templateGrid <- function(filename=NULL, datatype="FLT8S",  
                              template=NULL, interleave="BSQ", overwrite=FALSE){
  
  if(is.null(filename)){
    filename <- tempfile()
    file.rename(filename, paste0(filename, '.gri'))
    
  } else if(!is.character(filename)){
    stop("Filename should be a character string.")
    
  }else {
    filename <- sub("\\.[[:alnum:]]+$", "", filename)
    filename <- paste0(filename, ".gri") 
    
    if(file.exists(filename)){
      if(overwrite==FALSE){
        stop("File already exists. Set overwrite to TRUE or set new filename.")
      } else{
        file.remove(filename)
      }
    } 
  }
  
  if(class(template) == "RasterLayer"){
    nrows <- raster::nrow(template)
    ncols <- raster::ncol(template)
    nlayers <- raster::nlayers(template)
    
    xmin <- raster::xmin(template)
    xmax <- raster::xmax(template)
    ymin <- raster::ymin(template)
    ymax <- raster::ymax(template)
    
    proj <- sf::st_crs(template)$proj4string
    
  } else if(any(class(template) == "stars")){
    nrows <- stars::st_dimensions(template)$y$to
    ncols <- stars::st_dimensions(template)$x$to
    nlayers <- stars::st_dimensions(template)$band$to
    if(is.null(nlayers)) nlayers <- 1
    
    xmin <- sf::st_bbox(template)$xmin
    xmax <- sf::st_bbox(template)$xmax
    ymin <- sf::st_bbox(template)$ymin
    ymax <- sf::st_bbox(template)$ymax
    
    proj <- sf::st_crs(template)$proj4string
    
  } else if(class(template) == "numeric"){
    nrows <- template[1]
    ncols <- template[2]
    nlayers <- if(length(template)==3) template[3] else 1
    
  } else{
    stop("Unsupported template format.")
  }
  
  ncells <- nrows * ncols * nlayers
  bytes <- as.numeric(substr(datatype, 4, 4))
  
  outf <- file(filename, "wb")
  seek(outf, (ncells-1) * bytes)
  writeBin(raw(bytes), outf)
  close(outf)
  
  make_templateHeader(filename, nrows, ncols, xmin, ymin, xmax, ymax, proj,
                      datatype, nlayers, interleave, nodata=-9999)
  
  return(filename)
}


# Based on: https://stackoverflow.com/questions/32910919/converting-band-interleaved-by-pixel-binary-files-into-raster-grids
make_templateHeader <- function(filename, nrows, ncols, xmin, ymin, xmax, ymax, proj,
                                datatype, nlayers=1, interleave="BSQ", nodata){
  
  filename <- sub("\\.[[:alnum:]]+$", "", filename)
  filename <- paste0(filename, ".grd")
  
  hdrText <- c("[georeference]",
               paste0("nrows=", nrows),
               paste0("ncols=", ncols),
               paste0("xmin=", xmin),
               paste0("ymin=", ymin),
               paste0("xmax=", xmax),
               paste0("ymax=", ymax),
               paste0("projection=", proj),
               "[data]",
               paste0("datatype=", datatype),
               paste0("byetorder=", .Platform$endian),
               paste0("nbands=", nlayers),
               paste0("bandorder=", interleave),
               paste0("nodatavalue=", nodata),
               "[description]",
               "layername='")
  
  con <- file(filename, "w")
  writeLines(hdrText, con)
  close(con)
  
  invisible(filename)
}


write_imageBinary <- function(data, cellNumbers=1:length(data), filename, mapMode){ # TO DO add mmap mode lookup 
  if(missing(data)){
    stop("Data not found.")
  }
  
  if(missing(filename)){
    stop("Missing output file.")
  }
  
  # writing step
  mapOut <- mmap::mmap(filename, mmap::real64())
  mapOut[cellNumbers] <- as.numeric(data)
  mmap::munmap(mapOut)
}

