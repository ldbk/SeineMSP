# Pour full_join

toto4Part<- toto2part
toto4Part<- toto4Part %>% select(-Clust)
coordinates(toto4Part)<- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(toto4Part) <- TRUE
# coerce to raster
rasterPart4<- raster(toto4Part)
rasterPart4
plot(rasterPart4, col= terrain.colors(5), main="Detritus", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dispart4<- disaggregate(rasterPart4, fact=(res(rasterPart4)/res(rasterchlnew)))
mPart4<- mask(dispart4,res)
plot(mPart4)


# Conversion raster - tableau
fortify.Raster <- function(mPart4, maxPixel = 1000000) {
  
  if (ncell(mPart4) > maxPixel) {
    x <- sampleRegular(mPart4, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mPart4, seq_len(ncell(mPart4)))
  out <- mPart4 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabPartfin<- fortify(mPart4)

save(TabPartfin, file="data/satellite/Particles/TabPartfin.Rdata")
