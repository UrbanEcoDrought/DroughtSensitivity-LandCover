# XX_compare_forest_classes.R
# Quick comparison of "forest" vs "forest-wet" land cover classes
# to inform whether to keep both or merge them in the pipeline.

# Load raw NDVI data (before any recoding)
ndvi.all <- read.csv("G:/Shared drives/Urban Ecological Drought/data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv", header=TRUE)
ndvi.all$date <- as.Date(ndvi.all$date)
ndvi.all$year <- as.integer(format(ndvi.all$date, "%Y"))
ndvi.all$yday <- as.integer(format(ndvi.all$date, "%j"))
ndvi.all$month <- as.integer(format(ndvi.all$date, "%m"))

cat("=== ALL LANDCOVER TYPES IN RAW DATA ===\n")
print(table(ndvi.all$type))

cat("\n=== FOREST vs FOREST-WET: SAMPLE SIZES ===\n")
forest.types <- ndvi.all[ndvi.all$type %in% c("forest", "forest-wet"),]
cat("Total obs:\n")
print(table(forest.types$type))
cat("\nObs per mission:\n")
print(table(forest.types$type, forest.types$mission))

cat("\n=== NDVI SUMMARY STATISTICS ===\n")
cat("\nForest:\n")
f <- ndvi.all[ndvi.all$type == "forest",]
print(summary(f$NDVI))
cat("SD:", sd(f$NDVI, na.rm=TRUE), "\n")

cat("\nForest-wet:\n")
fw <- ndvi.all[ndvi.all$type == "forest-wet",]
print(summary(fw$NDVI))
cat("SD:", sd(fw$NDVI, na.rm=TRUE), "\n")

cat("\n=== GROWING SEASON NDVI (April-Oct, DOY 91-304) ===\n")
f.gs <- f[f$yday >= 91 & f$yday <= 304,]
fw.gs <- fw[fw$yday >= 91 & fw$yday <= 304,]

cat("\nForest (growing season):\n")
print(summary(f.gs$NDVI))
cat("SD:", sd(f.gs$NDVI, na.rm=TRUE), "\n")
cat("N:", nrow(f.gs), "\n")

cat("\nForest-wet (growing season):\n")
print(summary(fw.gs$NDVI))
cat("SD:", sd(fw.gs$NDVI, na.rm=TRUE), "\n")
cat("N:", nrow(fw.gs), "\n")

cat("\n=== T-TEST: GROWING SEASON NDVI ===\n")
tt <- t.test(f.gs$NDVI, fw.gs$NDVI)
print(tt)
cat("Mean difference (forest - forest-wet):", tt$estimate[1] - tt$estimate[2], "\n")

cat("\n=== YEAR RANGE ===\n")
cat("Forest years:", range(f$year, na.rm=TRUE), "\n")
cat("Forest-wet years:", range(fw$year, na.rm=TRUE), "\n")

cat("\n=== MONTHLY MEAN NDVI COMPARISON (growing season) ===\n")
f.monthly <- aggregate(NDVI ~ month, data=f[f$month %in% 4:10,], FUN=mean, na.rm=TRUE)
fw.monthly <- aggregate(NDVI ~ month, data=fw[fw$month %in% 4:10,], FUN=mean, na.rm=TRUE)
comp <- merge(f.monthly, fw.monthly, by="month", suffixes=c(".forest", ".forestwet"))
comp$diff <- comp$NDVI.forest - comp$NDVI.forestwet
print(comp)

cat("\n=== ANNUAL MEAN GROWING SEASON NDVI ===\n")
f.annual <- aggregate(NDVI ~ year, data=f.gs, FUN=mean, na.rm=TRUE)
fw.annual <- aggregate(NDVI ~ year, data=fw.gs, FUN=mean, na.rm=TRUE)
annual.comp <- merge(f.annual, fw.annual, by="year", suffixes=c(".forest", ".forestwet"))
annual.comp$diff <- annual.comp$NDVI.forest - annual.comp$NDVI.forestwet
print(annual.comp)
cat("\nCorrelation of annual means:", cor(annual.comp$NDVI.forest, annual.comp$NDVI.forestwet), "\n")
