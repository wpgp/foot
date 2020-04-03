
fs_area <- function(X, unit="ha"){
  area_calc <- sf::st_area(X)
  area_calc <- units::set_units(area_calc, unit, mode="standard")
  
  return(area_calc)
}
