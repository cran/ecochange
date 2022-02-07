## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  # install.packages('ecochange')
#  require('ecochange')
#  
#  dependencies <- c("raster","sf","gdalUtilities","readr","rgdal","parallel","curl","ggplot2","graphics","rgeos","rvest","landscapemetrics","tibble","utils","xml2","dplyr","R.utils","httr","getPass","methods","rlang","forcats","lattice","rasterDT","data.table","viridis","stats","rasterVis")
#  
#  # Test that all dependencies are available. Values in the next list must be all TRUE
#  sapply(dependencies, require, character.only = TRUE)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  # The getGADM() function provides the names of administrative units. By default, it shows level 2 administrative units in the country of Colombia but those settings can be changed through the parameters level and country
#  getGADM()
#  
#  # List the products that can be downloaded with ecochange
#  listGP()
#  
#  # Download a layer containing water occurrence for the municipality
#  # of Chimichagua
#  waterocc=getrsp("Chimichagua", lyrs=c("occurrence"))
#  
#  # the object is downloaded by default to a temporary folder.
#  # Users can define a different path using the argument path()
#  waterocc
#  
#  # Let's plot the object
#  plot(raster(waterocc[1]))

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  wo=rsp2ebv('Chimichagua', lyrs = c('occurrence'), mc.cores = 6)
#  plot(wo)
#  
#  # check that the projection is different to the file that was originally downloaded
#  crs(raster(waterocc[1]))
#  crs(wo)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  ebv <- rsp2ebv('Chimichagua', lyrs = c('treecover2000','lossyear'), mc.cores = 6)
#  plot(ebv)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  forExt <- echanges(ebv, eco = 'tree', eco_range=c(95,100), echanges = 'loss', change_vals = c(5,10,20), binary_output=TRUE, mc.cores = 6)
#  plotebv(forExt)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  defExt <- echanges(ebv, eco = 'tree', eco_range=c(95,100), echanges = 'loss', change_vals = c(5,10,19), binary_output=TRUE, mc.cores = 6,
#                     get_unaffected=FALSE)
#  plotebv(defExt)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  defExtTC <- echanges(ebv, eco = 'tree', eco_range=c(95,100), echanges = 'loss', change_vals = c(5,10,19), binary_output=FALSE, mc.cores = 6, get_unaffected=FALSE)
#  plotebv(defExtTC)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  library(landscapemetrics)
#  list_lsm()

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  forArea=gaugeIndicator(forExt, ncores=6)
#  defArea=gaugeIndicator(defExt, ncores=6)
#  forArea
#  defArea
#  plotind(forArea)
#  plotind(defArea)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  defAreaTC=gaugeIndicator(defExtTC, ncores=6)
#  defAreaTC
#  plotind(defAreaTC)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  chgstats=EBVstats(defExtTC)
#  chgstats
#  barplot(chgstats)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  # Let's first calculate forest extent by all tree cover classes for 2019
#  ech <- echanges(ebv, eco = 'tree', echanges = 'loss',
#                  change_vals =19, mc.cores = 2)
#  plot(ech)
#  
#  si <- sampleIndicator(ech, mc.cores = 6)
#  si
#  plotebv(si)

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  citation('ecochange')

