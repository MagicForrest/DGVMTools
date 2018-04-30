#' @title dummy text
#' 
#' @description A global scope list of standrd quantites which should be defined for many models (is Format objects).
#' Once the package is loaded this is just a standard R list so can be modified and extended as you wish.  It can be conveniently accesed by the \code{lookupQuantity()} function.
#' 
#' @details These quantities have sensible colour schemes, long names and cut ranges for standard global runs,
#' but they will likely need to be modified and new ones aded for a specific analysis.
#' 
#' @format \code{dgvm.quantities} is list of \code{Quantity} objects that store meta-data for common output variables from supported models as well as some standard quantities
#' which should be derivable from all model output.
#' @rdname Quantity-class
#' @keywords datasets
#' @importFrom fields tim.colors  
#' @seealso \code{lookupQuantity}
Standard.quantities <- list(
  
  ######################################################################
  ########## STANDARD (CROSS-FORMAT) QUANTITIES  ########################
  #####################################################################
  
  new("Quantity",
      id = "fraction",
      type = "unknown",
      name = "Fraction",
      units = "",
      colours = grDevices::colorRampPalette(c("grey85", "black")), 
      model = c("Standard"),
      cf.name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      type = "",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      model = c("Standard"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      type = "PFT",
      units = "kg m-2",
      colours = reversed.viridis,
      model = c("Standard"),
      cf.name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      type = "PFT",
      units = "1",
      colours = viridis::viridis,
      model = c("Standard"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      type = "monthly",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      model = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      type = "PFT",
      units = "kgC m-2 year-1",
      colours = fields::tim.colors,
      model = c("Standard"),
      cf.name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      type = "PFT",
      units = "kgC/m^2/year",
      colours = fields::tim.colors,
      model = c("Standard")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      type = "-",
      units = "m",
      colours = reversed.magma,
      model = c("Standard"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      type = "annual",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      model = c("Standard"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      type = "annual",
      units = "fraction",
      colours = veg.palette,
      model = c("Standard"),
      cf.name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      type = "annual",
      units = "GtC/year",
      colours = veg.palette,
      model = c("Standard"))
  
)
