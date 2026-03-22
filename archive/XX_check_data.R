ndviMet <- read.csv("G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/data/processed_files/landsat_ndvi_metVars_combined.csv")
cat("Landcover levels:\n")
print(table(ndviMet$landcover))
cat("\nGrassland obs:", sum(ndviMet$landcover == "grassland"), "\n")
