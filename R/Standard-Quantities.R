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
      format = c("Standard"),
      cf.name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      type = "",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      format = c("Standard"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      type = "PFT",
      units = "kg m-2",
      colours = reversed.viridis,
      format = c("Standard"),
      cf.name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      type = "PFT",
      units = "1",
      colours = viridis::viridis,
      format = c("Standard"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      type = "monthly",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      type = "PFT",
      units = "kgC m-2 year-1",
      colours = fields::tim.colors,
      format = c("Standard"),
      cf.name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      type = "PFT",
      units = "kgC/m^2/year",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      type = "-",
      units = "m",
      colours = reversed.magma,
      format = c("Standard"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      type = "annual",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("Standard"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      type = "annual",
      units = "fraction",
      colours = veg.palette,
      format = c("Standard"),
      cf.name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      type = "annual",
      units = "GtC/year",
      colours = veg.palette,
      format = c("Standard"))
  
)
