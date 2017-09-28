######################################################################
### simple arithmetics with ModelRun data slots ######################
######################################################################
#' Simple calculation of new ModelObjects
#' 
#' Basic calculation of new ModelObjects by operation '+', '-', '*' or '/'
#' 
#' @param x The first \code{\linkS4class{ModelObject}}
#' @param y The second \code{\linkS4class{ModelObject}}
#' @param x.col the column of the first ModelObect. If empty or NULL all columns are used.
#' @param y.col the column of the second ModelObect. If empty or NULL all columns are used.
#' @param op which arithmetic should be performed: addition ('+'), substraction ('-'), multiplication ('*') or division ('/').
#' @param quant new Quantity definition to use, if NULL it will be guessed.
#' @param verbose print some messages.
#' @return hopefully a new ModelObject.
#' @export
#' @import data.table
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
calcNewModelObj <- function(x, y, op, x.col=NULL, y.col=NULL, quant=NULL, verbose=TRUE) {
  ## check if valid arguments are given
  if (missing(x) || missing(y) || missing(op))
    stop("Missing values for 'x', 'y' and/or 'op'!")

  if (!is.ModelObject(x))
    stop(paste("'x' is not a ModelObject!", sep=""))
  if (!is.ModelObject(y))
    stop(paste("'y' is not a ModelObject!", sep=""))
  
  if (grepl("^a", op, ignore.case=TRUE) || op == "+") {
    op = "+"
  } else if (grepl("^s", op, ignore.case=TRUE) || op == "-") {
    op = "-"
  } else if (grepl("^m", op, ignore.case=TRUE) || op == "*") {
    op = "*"
  } else if (grepl("^d", op, ignore.case=TRUE) || op == "/") {
    op = "/"
  } else {
    stop(paste("Operator '", op, "' not implemented (yet)!", sep=""))
  }

  if (x@temporal.aggregate.method != y@temporal.aggregate.method ||
      x@spatial.aggregate.method != y@spatial.aggregate.method)
    stop("'x' and 'y' are averaged differently.")

  if (verbose)
    message("Passed initial checks.")

  ## get the data
  if (verbose)
    message("Getting the data.")

  x.quant     <- x@quant
  x.t.extent  <- x@temporal.extent
  x.sp.extent <- x@spatial.extent
  x.run       <- x@run
  x.dt        <- copy(x@data)
  y.quant     <- y@quant
  y.t.extent  <- y@temporal.extent
  y.sp.extent <- y@spatial.extent
  y.dt        <- copy(y@data)

  if (!is.equal(x.t.extent, y.t.extent)) 
    warning("Temporal extents differ.")
  if (!is.equal(x.sp.extent, y.sp.extent))
    warning("Spatial extents differ.")

  if (!is.equal(x.quant, y.quant) && (op == "+" || op == "-"))
    warning("Quantity definitions differ.")

  if (!is.Quantity(quant) ) {
    quant <- x.quant
    quant@id = paste0(x.quant@id, op, y.quant@id)
    quant@name = quant@id
    if (op == "*" || op == "/")
      quant@units = paste0("(", x.quant@units, ") ", op, " (", y.quant@units, ")")
  }

  if (verbose)
    message("Performing calculations.")

  ## perform the calculation
  if (is.null(x.col) && is.null(y.col)) {
    if (length(colnames(x.dt)) != length(colnames(y.dt)))
      stop(paste("Number of columns in run (", length(colnames(x.dt)), "/", length(colnames(y.dt)), ") differ.", sep=""))
    if (all(colnames(x.dt) %in% colnames(y.dt)) && all(colnames(y.dt) %in% colnames(x.dt)))
      stop(paste("run Objects have different column names:\n", colnames(x.dt), "\n", colnames(y.dt)))

    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    setnames(y.dt, val.names, paste("y.", val.names, sep=""))

    list.str <- paste(val.names, "=x.",val.names, op, "y.", val.names, sep="", collapse=", ")
    if (x@temporal.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }

    return(new("ModelObject",
               id = paste0(x@id, op, y@id),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               temporal.aggregate.method = x@temporal.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               run = as(x.run, "ModelRunInfo")))      

  } else if (is.null(x.col) && !is.null(y.col)) {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    
    list.str <- paste(val.names, "=x.", val.names, op, y.col, sep="", collapse=", ")
    if (x@temporal.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("ModelObject",
               id = paste0(x@id, op, y@id, "_", y.col, "_"),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               temporal.aggregate.method = x@temporal.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               run = as(x.run, "ModelRunInfo")))      
    
  } else if (!is.null(x.col) && is.null(y.col)) {
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(y.dt, val.names, paste0("y.", val.names))
    
    list.str <- paste(val.names, "=", x.col, op, "y.", val.names, sep="", collapse=", ")
    if (x@temporal.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste0("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("ModelObject",
               id = paste0(x@id, "_", x.col, "_", op, y@id),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               temporal.aggregate.method = x@temporal.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               run = as(x.run, "ModelRunInfo")))      
  } else {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(x.dt, val.names, paste0("x.", val.names))
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(y.dt, val.names, paste0("y.", val.names))

    if (x.col == y.col) {
      list.str <- paste0(x.col,"=x.",x.col, op, "y.", y.col)
    } else {
      list.str <- paste0("value=x.", x.col, op, "y.", y.col)
    }
    if (x@temporal.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("ModelObject",
               id = paste0(x.col, op, y.col),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               temporal.aggregate.method = x@temporal.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               run = as(x.run, "ModelRunInfo")))      
  }
  stop("MISSING: Not implemented yet.")
}
