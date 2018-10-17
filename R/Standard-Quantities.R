#' @rdname Quantity-class
#' @docType data
#' @seealso \code{lookupQuantity}
#' @include colour-palettes.R
#' @export
Standard.quantities <- list(
  
  ######################################################################
  ########## STANDARD (CROSS-FORMAT) QUANTITIES  #######################
  ######################################################################
  
  new("Quantity",
      id = "fraction",
      name = "Fraction",
      units = "",
      colours = grDevices::colorRampPalette(c("grey85", "black")), 
      format = c("Standard"),
      cf.name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      format = c("Standard"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      units = "kg m-2",
      colours = reversed.viridis,
      format = c("Standard"),
      cf.name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      units = "1",
      colours = viridis::viridis,
      format = c("Standard"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      units = "kgC m-2 year-1",
      colours = fields::tim.colors,
      format = c("Standard"),
      cf.name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      units = "kgC/m^2/year",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      units = "m",
      colours = reversed.magma,
      format = c("Standard"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("Standard"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      units = "fraction",
      colours = veg.palette,
      format = c("Standard"),
      cf.name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      units = "GtC/year",
      colours = veg.palette,
      format = c("Standard")),
  
  new("Quantity",
      id = "mprec",
      name = "Monthly total precipitation",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "mtemp",
      name = "Monthly mean temperature",
      units = "deg C",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "minsol",
      name = "Monthly mean daily solar radiation",
      units = "W m-2",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "mwetdays",
      name = "Wet days per month",
      units = "days/month",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "mwetdays_3",
      name = "Wet days per month (3 mm threshold)",
      units = "days/month",
      colours = fields::tim.colors,
      format = c("Standard"))
  
)
