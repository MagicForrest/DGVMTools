require(rgdal)


tree <- raster("HDF4_EOS:EOS_GRID:/home/forrest/Downloads/MOD44B.A2000065.h35v10.051.2014327051304.hdf:MOD44B_250m_GRID:Percent_Tree_Cover")
nontree <- raster("HDF4_EOS:EOS_GRID:/home/forrest/Downloads/MOD44B.A2000065.h35v10.051.2014327051304.hdf:MOD44B_250m_GRID:Percent_NonTree_Vegetation")
nonveg <- raster("HDF4_EOS:EOS_GRID:/home/forrest/Downloads/MOD44B.A2000065.h35v10.051.2014327051304.hdf:MOD44B_250m_GRID:Percent_NonVegetated")


all <- stack(tree, nontree, nonveg)

combined <- calc(all, sum)