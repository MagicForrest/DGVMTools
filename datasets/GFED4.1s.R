# use the RVCTools v2.9
source("~/Tools/RVCTools/v2.9/rvc-tools.R")

BA <- stack("HDF5:/data/forrest/Fire/GFED4.1s/Monthly/GFED4.1s_1997.hdf5://ancill/basis_regions")      

BA[BA == 0] <- NA

BA.HD <- aggregate(BA, fun = modal, na.rm  = TRUE)