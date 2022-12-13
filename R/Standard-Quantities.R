#' @rdname Quantity-class
#' @docType data
#' @seealso \link{lookupQuantity}
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
      standard_name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      name = "Area Fraction",
      units = "%",
      colours = grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black")), 
      format = c("Standard"),
      standard_name = "area_fraction_percent"), 
  
  
  new("Quantity",
      id = "landcover_std",
      name = "Area Fraction",
      units = "%",
      colours = grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black")), 
      format = c("Standard"),
      standard_name = "area_fraction_percent"), 
  
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      units = "kg m-2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("Standard"),
      standard_name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      units = "1",
      colours = viridis::viridis,
      format = c("Standard"),
      standard_name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      units = "kgC/m^2",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      units = "kgC m-2 year-1",
      colours = viridis::inferno,
      format = c("Standard"),
      standard_name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      units = "kgC/m^2/year",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      units = "m",
      colours = function(n) rev(viridis::magma(n)),
      format = c("Standard"),
      standard_name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = viridis::turbo,
      format = c("Standard"),
      standard_name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      units = "fraction",
      colours = grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black")),
      format = c("Standard"),
      standard_name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      units = "GtC/year",
      colours = grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black")),
      format = c("Standard")),
  
  new("Quantity",
      id = "mprec_std",
      name = "Monthly total precipitation",
      units = "mm/month",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "mtemp_std",
      name = "Monthly mean temperature",
      units = "deg C",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "minsol_std",
      name = "Monthly mean daily solar radiation",
      units = "W m-2",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "mwetdays_std",
      name = "Wet days per month",
      units = "days/month",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "mwetdays_3_std",
      name = "Wet days per month (3 mm threshold)",
      units = "days/month",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "mfire_ignitions_std",
      name = "Total number of successful fire ignitions",
      units = "ign/month",
      colours = viridis::viridis,
      format = c("Standard")),
 
  new("Quantity",
      id = "mfire_size_std",
      name = "Average fire size",
      units = "km2",
      colours = viridis::viridis,
      format = c("Standard")),
  
  new("Quantity",
      id = "mfire_duration_std",
      name = "Average fire duration",
      units = "days",
      colours = viridis::viridis,
      format = c("Standard")),
  
  new("Quantity",
      id = "mfire_line_std",
      name = "Average daily fire line",
      units = "km",
      colours = viridis::viridis,
      format = c("Standard")),
  
  new("Quantity",
      id = "mfire_expansion_std",
      name = "Average daily fire expansion",
      units = "km2/day",
      colours = viridis::viridis,
      format = c("Standard")),
  
  new("Quantity",
      id = "mfire_speed_std",
      name = "Average fire speed",
      units = "km/day",
      colours = viridis::viridis,
      format = c("Standard"))
  
)
