#' Writing gridded data to binary files
#'
#' @description Write data to binary files and structure as native \code{raster} files.
#' @param filename character string path to desired output file.
#' @param dataType character string of \code{raster} data types. Default is \code{FLT8S}
#' @param template A filepath or a gridded data object to be used as a 
#' template to define the output file's resolution and positioning.
#' @param interleave character string for band interleaving. Currently only 'BSQ' supported.
#' @param overwrite logical. Should the output file be overwritten if it already exists?
#' Default is \code{False}. 
#' 
#' @details These functions provide a lightweight interface to binary writing demonstrated
#' in the \code{\link[spatial.tools]{create_blank_raster}} package. The advantage of using binary files is 
#' the opportunity to using memory mapping (\code{mmap}) to support parallel writing.
#'
#' @import mmap
#' 
#' @name make_templateGrid
#' @export
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
  
  make_templateHeader(filename, 
                      nrows, ncols, xmin, ymin, xmax, ymax, 
                      proj, datatype, nlayers, interleave, 
                      nodata=-9999)
  
  return(filename)
}

#' @title Construct headers for gridded data
#' @description Creates a .grd file with information on resolution and spatial extent for 
#' use with a binary file (.gri). 
#' @inheritParams make_templateGrid
#' @param nrows number of rows in the data
#' @param ncols number of columns in the data
#' @param xmin,ymin,xmax,ymax geographic extent coordinates of the output data
#' @param proj Proj4String of a coordinate reference system.
#' @param nlayers number of layers (multiband) in the gridded data. Default is 1.
#' @param nodata Value to represent no data in the output.
#' 
#' @details These gridded files are the native files used by the 
#' \code{raster} package. The .gri (data) and .grd (header) files must have matching filenames.
#' 
#' @name make_templateHeader
#' @export
# Based on: https://stackoverflow.com/questions/32910919/converting-band-interleaved-by-pixel-binary-files-into-raster-grids
make_templateHeader <- function(filename, 
                                nrows, ncols, xmin, ymin, xmax, ymax, 
                                proj, datatype, nlayers=1, interleave="BSQ", 
                                nodata){
  
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

#' @title Fast writing of binary files
#' @description \code{write_imageBinary} uses memory mapping functions to write 
#' gridded data to a binary output format. Binary outputs can be read natively 
#' but some software, or with the addition of header files, be converted to 
#' formats easily read by most software.
#' 
#' @param data vector of 
#' @param cellNumbers vector of cell indices to update with data values
#' @param filename character string of path to the output file
#' @param mapMode character string of the \code{raster} data type.
#' 
#' @details The function uses \code{\link[mmap]{mmap}} create and update a 
#' binary output file on the hard drive.
#' 
#' The mapMode converts the \code{raster} data type strings to a \code{CType} used 
#' by \code{mmap}. 
#' \itemize{
#' \item "LOG1S" \code{mmap::logi8()}
#' \item "INT1S" \code{mmap::int8()}
#' \item "INT1U" \code{mmap::uint8()}
#' \item "INT2S" \code{mmap::int16()}
#' \item "INT2U" \code{mmap::uint16()}
#' \item "INT4S" \code{mmap::int32()}
#' \item "INT4U" \code{NA}
#' \item "INT4S" \code{mmap::real32()}
#' \item "INT8S" \code{mmap::real64()}
#' } 
#' 
#' @name write_imageBinary
#' @export
write_imageBinary <- function(data, cellNumbers=1:length(data), filename, mapMode){
  if(missing(data)){
    stop("Data not found.")
  }
  
  if(missing(filename)){
    stop("Missing output file.")
  }
  
  rasterTypes=c("LOG1S","INT1S","INT1U","INT2S",
                "INT2U","INT4S","INT4U","FLT4S","FLT8S")
  mmType <- list(mmap::logi8(), mmap::int8(), mmap::uint8(), 
                 mmap::int16(), mmap::uint16(), mmap::int32(), 
                 NA, mmap::real32(), mmap::real64())
  
  matchType <- match(mapMode, rasterTypes)
  
  if(is.na(matchType)){
    stop("Mmap C Type not found.")
  } else{
    mapMode <- mmap::as.Ctype(mmType[[matchType]])
  }

  # writing step
  mapOut <- mmap::mmap(filename, mapMode)
  mapOut[cellNumbers] <- as.numeric(data)
  mmap::munmap(mapOut)
}
