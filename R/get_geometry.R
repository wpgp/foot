
fs_area <- function(X, unit="ha"){
  area_calc <- sf::st_area(X)
  area_calc <- units::set_units(area_calc, unit, mode="standard")
  
  return(area_calc)
}

fs_perimeter <- function(X, unit="m"){
  perim_calc <- lwgeom::st_perimeter(X)
  perim_calc <- units::set_units(perim_calc, unit, mode="standard")
  
  return(perim_calc)
}
