## ----load_libraries_hidden,eval=TRUE,message=FALSE,results='hide',warning=FALSE----
# install.packages('ecochange')
require('ecochange')

dependencies <- c('ecochange','raster','rgdal','parallel', 'R.utils', 'rvest','xml2',
            'tidyverse','landscapemetrics','sf','dplyr','httr','getPass','gdalUtils',
            'gdalUtilities','rgeos','viridis', 'rasterVis','rlang', 'rasterDT')

# Values in the next list must be allx TRUE
sapply(dependencies, require, character.only = TRUE)


## -----------------------------------------------------------------------------
?rsp2ebv

ebv <- rsp2ebv('Chimichagua', lyrs = c('treecover2000','lossyear'), mc.cores = 2)
ebv

## ---- fig.height=3, fig.width=7-----------------------------------------------
plot(ebv)

## ---- fig.height=3, fig.width=7-----------------------------------------------
ech <- echanges(ebv, eco = 'tree', echanges = 'loss',
                change_vals = c(0,10,19), mc.cores = 2)
plotebv(ech)

## ---- fig.height=7, fig.width=7-----------------------------------------------
?gaugeIndicator

# computating ecosystem areas (default)
gi <- gaugeIndicator(ech, mc.cores = 2)
gi

plotind(gi)

## ---- fig.height=3, fig.width=7,message=FALSE,results='hide',warning=FALSE----
si_ent <- sampleIndicator(ech, mc.cores = 2)
si_ent
plotebv(si_ent)

## ---- fig.height=3, fig.width=7-----------------------------------------------
##Statistics for changes in entropy:
sts <- EBVstats(si_ent)
sts

## In-package bar plot method:
barplot(sts)

## ---- eval=FALSE--------------------------------------------------------------
#  citation('ecochange')

