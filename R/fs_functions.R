
cv <- function(x){
  return(sd(x) / mean(x))
}


majority <- function(x){
  return(names(which.max(table(x))))
}


binary <- function(x){
  return(ifelse(is.null(x), 0, 1))
}


count <- function(x){
  return(length(x))
}


# creating a function factory to use the pre-made function foot::fs_nnindex
gen_nnindex <- function(z, zoneField=NULL, unit="m"){
  force(z)
  force(zoneField)
  force(unit)
  
  function(x){
    if(length(x) == 1){
      return(0)
    } else{
      res <- fs_nnindex(sf::st_as_sf(x), z, zoneField, unit)
      return(res[["fs_nnindex"]])
    }
  }
}


entropy <- function(X){
  bins <- cut(X, seq(5, 355, 10), labels=F) + 1
  bins[is.na(bins)] <- 1
  calc_ent <- -1 * sum(prop.table(table(bins)) * log(prop.table(table(bins))))
  
  # normalizing step (see Boeing (2019))
  hmax <- 3.584
  hg <- 1.386
  
  calc_ent <- 1 - ((calc_ent-hg) / (hmax - hg))^2
  return(calc_ent)
}

