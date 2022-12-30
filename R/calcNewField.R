######################################################################
### Simple arithmetics with Field data slots ######################
######################################################################
#' Simple calculation of new Fields
#' 
#' Basic calculation of new Fields by operation '+', '-', '*' or '/'
#' 
#' @param x The first \code{\linkS4class{Field}}
#' @param y The second \code{\linkS4class{Field}}
#' @param x.col the column of the first Field. If empty or NULL all columns are used.
#' @param y.col the column of the second Field. If empty or NULL all columns are used.
#' @param op which arithmetic should be performed: addition ('+'), substraction ('-'), multiplication ('*') or division ('/').
#' @param quant new Quantity definition to use, if NULL it will be guessed.
#' @param verbose print some messages.
#' @return A new Field.
#' @export
#' @import data.table
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' 
#' @examples 
#' \donttest{
#'  
#' # First define a Source and read a Field
#' test.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(name = "Run 1", dir = test.dir,  format = GUESS)
#' test.Field <- getField(source = test.Source, quant = "lai", year.aggregate.method = "mean")
#' test.Source.2 <- defineSource(name = "Run 2", dir = test.dir,  format = GUESS)
#' test.Field.2 <- getField(source = test.Source.2, quant = "lai", year.aggregate.method = "mean")
#' 
#' # division, result should be 1 or NA everywhere since Fields are the same 
#' # (NAs come wherever the Field is zero)
#' test.division <- calcNewField(x=test.Field, y = test.Field.2, op = "/")
#' print(plotSpatial(test.division))
#' 
#' # subtraction, result should be 0 everywhere since Fields are the same
#' test.subtraction <- calcNewField(x=test.Field, y = test.Field.2, op = "-", verbose = FALSE)
#' print(plotSpatial(test.subtraction))
#' 
#' # addition, select Total only and compare to original
#' test.addition <- calcNewField(x=test.Field, y = test.Field.2,
#'                               x.col = "Total", y.col = "Total", op = "+")
#' print(plotSpatial(list(test.addition, test.Field), layers = "Total"))
#' 
#' # multiplication, select a couple of Layers only and compare to original
#' test.multiplication <- calcNewField(x=test.Field, y = test.Field.2, x.col = c("C3G", "TeBS"), 
#'                                     y.col = c("C3G", "TeBS"), op = "*", verbose = FALSE)
#' print(plotSpatial(list(test.multiplication, test.Field), layers =  c("C3G", "TeBS")))
#' 
#' }
calcNewField <- function(x, y, op, x.col=NULL, y.col=NULL, quant=NULL, verbose=TRUE) {
  ## check if valid arguments are given
  if (missing(x) || missing(y) || missing(op))
    stop("Missing values for 'x', 'y' and/or 'op'!")

  if (!is.Field(x))
    stop(paste("'x' is not a Field!", sep=""))
  if (!is.Field(y))
    stop(paste("'y' is not a Field!", sep=""))
  
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

  if (x@year.aggregate.method != y@year.aggregate.method ||
      x@spatial.aggregate.method != y@spatial.aggregate.method)
    stop("'x' and 'y' are averaged differently.")

  if (verbose)
    message("Passed initial checks.")

  ## get the data
  if (verbose)
    message("Getting the data.")

  x.quant     <- x@quant
  x.first.year  <- x@first.year
  x.last.year  <- x@last.year
  x.sp.extent <- x@spatial.extent
  x.run       <- x@source
  x.dt        <- copy(x@data)
  y.quant     <- y@quant
  y.first.year  <- y@first.year
  y.last.year  <- y@last.year
  y.sp.extent <- y@spatial.extent
  y.dt        <- copy(y@data)

  if (!(x.first.year == y.first.year) | !(x.last.year == y.last.year)) 
    warning("Temporal extents (ie first and last yers of data) differ.")
  if (!identical(x.sp.extent, y.sp.extent))
    warning("Spatial extents differ.")

  if (!equivalentQuantities(x.quant, y.quant) && (op == "+" || op == "-"))
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
    if (!all(colnames(x.dt) %in% colnames(y.dt)) || !all(colnames(y.dt) %in% colnames(x.dt)))
      stop(paste("run Objects have different column names:\n", colnames(x.dt), "\n", colnames(y.dt)))

    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    setnames(y.dt, val.names, paste("y.", val.names, sep=""))

    list.str <- paste(val.names, "=x.",val.names, op, "y.", val.names, sep="", collapse=", ")
    if (x@year.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }

    return(new("Field",
               id = paste0(x@id, op, y@id),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               first.year = x.first.year,
               last.year = x.last.year,
               year.aggregate.method = x@year.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               source = x.run))      

  } else if (is.null(x.col) && !is.null(y.col)) {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    
    list.str <- paste(val.names, "=x.", val.names, op, y.col, sep="", collapse=", ")
    if (x@year.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("Field",
               id = paste0(x@id, op, y@id, "_", y.col, "_"),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               first.year = x.first.year,
               last.year = x.last.year,
               year.aggregate.method = x@year.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               source = x.run))      
    
  } else if (!is.null(x.col) && is.null(y.col)) {
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(y.dt, val.names, paste0("y.", val.names))
    
    list.str <- paste(val.names, "=", x.col, op, "y.", val.names, sep="", collapse=", ")
    if (x@year.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste0("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("Field",
               id = paste0(x@id, "_", x.col, "_", op, y@id),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               first.year = x.first.year,
               last.year = x.last.year,
               year.aggregate.method = x@year.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               source = x.run))      
  } else {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(x.dt, val.names, paste0("x.", val.names))
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x == key.names)})]
    setnames(y.dt, val.names, paste0("y.", val.names))
    
    if (identical(x.col, y.col)) {
      list.str <- paste0(x.col,"=x.",x.col, op, "y.", y.col)
    } else {
      list.str <- paste0("value=x.", x.col, op, "y.", y.col)
    }
    if (x@year.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@spatial.aggregate.method != "none") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("Field",
               id = paste0(x.col, op, y.col),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               first.year = x.first.year,
               last.year = x.last.year,
               year.aggregate.method = x@year.aggregate.method,
               spatial.aggregate.method = x@spatial.aggregate.method,
               source = x.run))      
  }
  stop("MISSING: Not implemented yet.")
}
