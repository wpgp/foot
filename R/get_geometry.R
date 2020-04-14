
fs_area <- function(X, unit=NULL){
  area_calc <- sf::st_area(X)
  
  if(!is.null(unit)){
    area_calc <- units::set_units(area_calc, unit, mode="standard")
  }
  
  return(area_calc)
}

fs_perimeter <- function(X, unit=NULL){
  perim_calc <- lwgeom::st_perimeter(X)
  
  if(!is.null(unit)){
    perim_calc <- units::set_units(perim_calc, unit, mode="standard")
  }
  
  return(perim_calc)
}
