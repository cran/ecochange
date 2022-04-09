## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  # install.packages('ecochange')
#  require('ecochange')
#  
#  dependencies <- c("raster", "sf","rgdal","parallel","ggplot2","landscapemetrics","tibble","utils","httr","getPass","methods","rlang","lattice","rasterDT","stats","rasterVis", "viridis")
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
#  plot(raster(waterocc), axes = T,
#       main = 'Occurrence (Colombia)')

## ----scene, echo=FALSE, fig.cap="", out.width = '100%'------------------------
knitr::include_graphics("images/scene_01.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  wo=rsp2ebv('Chimichagua', lyrs = c('occurrence'), mc.cores = detectCores())
#  plot(wo, main = 'Occurrence (Municipality of Chimichagua)')

## ----ocu_chimi, echo=FALSE, fig.cap="", out.width = '100%'--------------------
knitr::include_graphics("images/scene_02.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  # check that the projection is different to the file that was originally downloaded
#  crs(raster(waterocc[1]))
#  crs(wo[[1]])

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  ebv <- rsp2ebv('Chimichagua', lyrs = c('treecover2000','lossyear'), mc.cores = detectCores())
#  plot(ebv, main = 'Forest cover and loss (Chimichagua)')

## ----treeloss_chimi, echo=FALSE, fig.cap="", out.width = '100%'---------------
knitr::include_graphics("images/scene_03.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  forExt <- echanges(ebv, eco = 'tree', eco_range=c(95,100), echanges = 'loss', change_vals = c(5,10,20), binary_output=TRUE, mc.cores = detectCores())
#  plot(forExt, main = 'Changes in forest cover')

## ----ecochanges_echimi, echo=FALSE, fig.cap="", out.width = '100%'------------
knitr::include_graphics("images/scene_04.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  defExt <- echanges(ebv, eco = 'tree', eco_range=c(95,100), echanges = 'loss', change_vals = c(5,10,20), binary_output=TRUE, mc.cores = detectCores(),
#                     get_unaffected=FALSE)
#  plot(defExt, main = 'Changes in forest loss')

## ----unafectedF_chimi, echo=FALSE, fig.cap="", out.width = '100%'-------------
knitr::include_graphics("images/scene_05.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  defExtTC <- echanges(ebv, eco = 'tree', eco_range=c(95,100), echanges = 'loss', change_vals = c(5,10,20), binary_output=FALSE, mc.cores = detectCores(), get_unaffected=FALSE)
#  plot(defExtTC, main = 'Changes in deforested pixels')

## ----tree_unafectedF_chimi, echo=FALSE, fig.cap="", out.width = '100%'--------
knitr::include_graphics("images/scene_06.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  library(landscapemetrics)
#  list_lsm()

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  forArea=gaugeIndicator(forExt, ncores=6)
#  defArea=gaugeIndicator(defExt, ncores=6)
#  forArea
#  defArea
#  plot(forArea, cex = 1.1, xlab = 'Year', ylab = 'Area (ha)', title = "Ecosystem extents", subtitle = 'Forest cover', fill = 'Pixel \nvalue')
#  plot(defArea, cex = 1.1, xlab = 'Year', ylab = '', title = '', subtitle = 'Forest loss', fill = '')

## ----area_unafectedF_chimi, echo=FALSE, fig.cap="", out.width = '100%'--------
knitr::include_graphics("images/scene_065.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  defAreaTC=gaugeIndicator(defExtTC, ncores=6)
#  defAreaTC
#  plot(defAreaTC, y = viridis(6), cex = 1.1, xlab = 'Year',
#       ylab = 'Area (ha)', title = 'Forest loss',
#       subtitle = 'Tree-canopy cover values', fill = '(%)')

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  plot(defAreaTC, type = 'b', cex = 1.1, xlab = 'Year',
#  ylab = '',title = '', subtitle = 'Tree-canopy cover distributions', fill = 'Year')

## ----box_area_afectedDefF_chimi, echo=FALSE, fig.cap="", out.width = '100%'----
knitr::include_graphics("images/scene_075.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  chgstats=EBVstats(defExtTC)
#  chgstats

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  # Let's first calculate forest extent by all tree cover classes for 2019
#  ech <- echanges(ebv, eco = 'tree', echanges = 'loss',
#                  change_vals = c(0,20), mc.cores = 2)
#  plot(ech, main = 'Forest cover')
#  
#  si <- sampleIndicator(ech, mc.cores = detectCores())
#  si
#  plot(si, main = 'Conditional entropy')

## ----sampleInd_chimi, echo=FALSE, fig.cap="", out.width = '100%'--------------
knitr::include_graphics("images/scene_11.png")

## ----include = TRUE, message=F, warning=F, eval=FALSE-------------------------
#  citation('ecochange')

