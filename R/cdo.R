#' Check for CDO
#' 
#' Checks if the CDO (climate data operators) are available on the system
#' 
#' @keywords internal
#' 
#' @return TRUE if cdo present, false if not
check.cdo <- function(){
  if (Sys.which("cdo")!="") {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
  
}


#' CDO wrapper
#' 
#' Right now only defined for remaplaf, but in principle it is easy to extend to other operators.  Operator chaining is not supported.
#' 
#' @param fun The CDO operator to be used (right now must be remaplaf)
#' @param ifile The input, either a RasterLayer/Stack/Brick or a character string pointing to netCDF file on disk
#' @param ofile Character sting for the output file (path on disk)
#' @param grid Character string specifying the grid for the output file. Can be a code (sag, "n31' or "r192x145", or a file on disk), see CDO documentation  
#' @param varname If operating on a raster (not a netCDF file) the name for the variable in the resulting netCDF file.
#' @param return.raster If TRUE, read the resulting netCDF file and return it is a a raster
#' @param verbose If TRUE print some progress and diagnostic output
#' @param remove.temps Logical, if TRUE remove the intermediate .nc files from disk
#' 
#' Basically just calls CDO on the commandline.  If the input arguments is an R RasterLayer/Stack/Brick, then is uses writeRaster() to write a netCDF file for processing (in the directory of the output file)
#' 
#' 
#' @return NULL, unless return.raster is TRUE (j which case it returns a raster of the result of the cdo processing)
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 
#'    
#'      

cdo <- function(fun, ifile, ofile, grid = NULL, varname = "layer", return.raster = FALSE, verbose = TRUE, remove.temps = TRUE) {
  
  ### CHECK INPUT FILE
  this.class <- class(ifile)[1]
  
  # if ifile character string, check file exists on disk
  
  if(this.class == "character"){
    if(!file.exists(ifile)) {
      warning(paste("Can't find file", ifile, "on disk.  Exiting cdo wrapper", sep = " "))
      return()
    }
  }
  
  #  if ifile is a Raster/Stack/Brick it must be written to disk for cdo to work with
  if(this.class == "RasterLayer" | this.class == "RasterBrick" | this.class == "RasterStack") {
    
    # make a temporary netcdf intermediatary file
    working.dir <- dirname(ofile)
    temp.file <- file.path(dirname(ofile), paste0(deparse(substitute(ifile)), ".nc"))
    writeRaster(ifile, temp.file, varname = varname, overwrite = TRUE)
    ifile <- temp.file
    
  }
  
  # if cdo available 
  if(check.cdo()){
    
    # remap functions
    if(fun == "remaplaf"){
      
      # if target grid not specified
      if(is.null(grid)) {
        stop("cdo wrapper: for remapping operator", fun, "you must provide a target grid in the \'grid\' argument!")
      }
      
      command.string <- paste("cdo", paste0(fun,",",grid), ifile, ofile, sep = " ")
      if(verbose) message(paste0("Performing: ", command.string))
      system(command.string, ignore.stdout = !verbose)
      
      
    }
    
  }
  
  # else fail
  else {
    stop("cdo not available on this system")
  }
  
  # remove the temporary file
  if(remove.temps) system(paste("rm", temp.file, sep = " "), ignore.stdout = !verbose)
  
  if(return.raster) {
    return(stack(ofile))
  }
  else{
    return(NULL)
  }
  
  
}

