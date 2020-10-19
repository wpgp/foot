
footstats <- function(X, index, what='all', how=NULL,
                      controlZone, controlUnits, controlDist,
                      verbose=TRUE){
  # validate values
  if(is.null(what)) stop("Missing list of characteristics.")
  if(tolower(what) == 'all'){ getAllMetrics}
  what <- list(what)
  # set defaults
  controlUnits
  getProjection
  # create zonal index
  # precalculate 'what's
  uchars <- unique(unlist(what))
  
  # construct variable names?
  ncols <- length(what)
  
  # expand function list
  if(!any(sapply(how, is.list)) && ncols > 1) { 
    how <- replicate(ncols, how, simplify=F)
  }
  nfuns <- length(nfuns)
  stopifnot(ncols == nfuns)
  # remove "invalid" functions e.g nni only dist; entropy only angle
  
  
  DT <- data.table(mtcars)
  DT
  what <- list("mpg","hp")
  what <- list(list("mpg","hp"), list("gear","carb"))
  how <- list("mean","min")
  how <- "mean"
  how <- list(list("mean"), list("min","sum"))
  
  ff <- function(x, y){ sum(1000*x + y) }
  what <- list(list("mpg","hp"), list(list("am","gear")))
  how <- list(list("mean"), list("ff"))
  
  # DT[, lapply(.SD, function(u){ sapply(how, function(f){ do.call(f, list(u)) }) }), by=cyl, .SDcols=unlist(what)] # not correct
  
  # main processing loop

  (results <- lapply(seq_along(what), function(i){
    c <- what[[i]]
    print(c)
    # if(is.list(c)) c <- unlist(c)
    # if(all(c) == "" | is.null(c)) c <- colnames(X)
    
    funcs <- how[[i]]
    # if(length(fun)==1) fun <- list(fun)
    
    rt <- DT[, lapply(funcs, function(f){ do.call(f, unname(.SD)) }), by=cyl, .SDcols=unlist(c)]
    setnames(rt, c("cyl",paste(c, how, sep="_")))
  }))
  
  
  # version 31
  for(i in seq_along(what)){
    cols <- what[[i]]
    funcs <- how[[i]]
    
    for(c in cols){
      for(func in funcs){
        print(DT[, do.call(func, unname(.SD)), by=cyl, .SDcols=c])
      }
      # print(DT[, lapply(funcs, function(func){ do.call(func, unname(.SD)) }), by=cyl, .SDcols=c])
    }
  }
  
  lapply(seq_along(what), function(i){
    cols <- what[[i]]
    funcs <- how[[i]]
    DT[, lapply(.SD, function(c){ sapply(funcs, function(func){ do.call(func, unname(.SD)) })}), by=cyl, .SDcols=unlist(cols)]
  })
  
  
  # OK https://stackoverflow.com/questions/29620783/apply-multiple-functions-to-multiple-columns-in-data-table
  lapply_at <- function(var, funs, ...){
    results <- sapply(var, function(var){ lapply(funs, do.call, list(var, ...))})
    names(results) <- vapply(names(var), paste, funs, sep="_", FUN.VALUE=character(length(funs)), USE.NAMES=F)
    results
  }
  
  f3 <- function(x, prob=0.25){ quantile(x, prob)}
  DT[, lapply_at(.SD, c("mean","sum", "f3")), by=cyl, .SDcols=c("mpg","hp")]
  
  f4 <- function(hp, mpg) { sum(hp/mpg) }
  # f4 <- Curry(sum(x^carb))
  DT[, do.call("f4", .SD), by=cyl, .SDcols=c("mpg","hp")]
  
  
  
  
  
  tf <- function(x){ print(is.list(x))}
  DT[, do.call(tf, list(.SD)), by=cyl, .SDcols=c('mpg', 'hp')]
  
  
  
  
  # testing
  ngroups <- length(what)
  (results <- lapply(seq_along(ngroups), function(i){
    cols <- what[[i]]
    funcs <- how[[i]]
    
    DT[, lapply(.SD, function(c){ lapply(funcs, function(f){ do.call(f, list(c)) }) }), by=cyl, .SDcols=unlist(cols)]
  }))
  
  # check/make unique names
}


how <- list(mean, min, max)
how <- list(mean, list(min, max))

any(sapply( how, is.list))

funs <- list(how)
nargs <- length(funs)
if(nargs==1 && is.list(funs[[1L]])){
  nargs <- length(funs <- funs[[1L]])
  # funs <- replicate(length(what), funs[[1L]], simplify=F)
} 

nargs
if(nargs != length(what)) stop("arguments do not match")
funs

# footstats(X, index, what=c("area","shape","perimeter"), how=c(mean, min, sd) )
# footstats(X, index, what=c(list("area","shape"),"perimeter"), how=list(mean, list(min, sd) ))
# footstats(X, index, what=c("area","shape","perimeter"), how=list(mean, list(min, sd) ))

what = "area"; how=list("mean","max","total")
what = c("area","mean")
what = list("area","shape","perimeter"); how = "mean" # or just mean
what = list("area","shape","perimeter"); how = list("mean","min","sd","cv")
what = list(list("area","shape"), list("perimeter")); how = list(list("mean","min"), list("sd","cv"))

is.list(what)
lw <- list(what)
length(lw[[1]])==1 && is.list(lw[[1]])


# expand.args <- function(...){
#   dots <- list(...)
#   max_l <- max(lengths(dots))
#   lapply(dots, rep, length.out=max_l)
}

expand.args <- function(...){
  dots <- list(...)
  max_l <- max(lengths(what))
  lapply(dots, rep, length.out=max_l)
}


if(!any(sapply(how, is.list))) how <- replicate(length(what), how, simplify=F)
nargs <- length(how)
if(nargs != length(what)) stop("argument do not match")
how
