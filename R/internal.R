## Internal utility functions used by ecochange

## Classes
## setClass('FCMask', contains = 'RasterBrick')
## setClass('FCPolygon', contains = 'RasterBrick')


## fa <- function(x){
##     ar <- area(x, na.rm = TRUE)
##     ar <- na.omit(values(ar))
##     ar. <- length(ar)*median(ar)
##     return(ar.)}

## fa <- function(x, fun = 'median') {
##     v <- area(x, na.rm = TRUE)
##     y <- cellStats(x, stat = sum)
##     if(fun == 'mean')
##         z <- cellStats(v, stat = fun)
##     if(fun == 'median')
##         z <- quantile(v, probs = 0.5,
##                       names = FALSE)
##     ar <- y * z  
##     return(ar)}


## getTileEdges <- function(pol, add.zero2NS = c(gsw = FALSE, gfc = TRUE)){
##     edgs <- function(pol, add.zero2NS){
##         fn. <- function(ext){
##             e2v <- as.vector(ext)
##             xtr <- e2v[c(1,4)]
##             iswe <- ifelse(xtr[1L] < 0, 'W', 'E')
##             issn <- ifelse(xtr[2L] < 0, 'S', 'N')
##             abxtr <- abs(xtr)
##             is.lon100 <- xtr[2L] < 100
##             is.lat10 <- xtr[1L] < 10
##             txtt <- paste0(abxtr, c(iswe, issn))
##             if(add.zero2NS){
##                 txtt <- rev(txtt)
##                 ## if(is.lat10)
##                 ##     txtt  <- paste(c('0',''), txtt, sep = '')
##                 if(is.lon100)
##                     txtt  <- paste(c('','0'), txtt, sep = '')}
##             ## if(add.zero2NS){
##             ##     txtt  <- rev(paste(c('0',''), txtt, sep = ''))
##             ## if(add.zero2NS&is.lat10)
##             ##     txtt  <- paste(c('',''), txtt, sep = '')}
##             return(txtt)}
##         edg <- Map(function(x)
##             fn.(x), selTile(pol))
##         return(edg)}
##     edgs <- Map(function(x)
##         edgs(pol, x),
##         x = add.zero2NS)
##     edgs <- lapply(edgs, function(x)
##         lapply(x, function(y)
##             paste(y[1],y[2], sep ='_')))
## edgs <- lapply(edgs, unlist)
## return(edgs)
## }

## ziptoEnv <- function(zfe, int.patt){
## zps <- Map(function(x)
##     unzip(x, list = TRUE,exdir = tempdir()), zfe)
## znms <- lapply(zps, function(x)x[1L,1L])
## find <- lapply(znms, function(x)x[grepl(int.patt, x)])
## zps <- Map(function(x,y)
##     unzip(x, files = y,exdir = tempdir()), x = zfe, y = find)
## zpu <- unlist(zps, use.names = FALSE)
## tifimag <- Map(function(x)
##     raster(x), x = zpu)
## return(tifimag)
## }

#<==================================================================

bindLayers <- function(ars., time, pol){
    fn. <- function(ars., time, pol){
        nmss <- names(ars.)
        yr.. <- nmss[grepl(time, nmss)]
        lyrsel <- ars.[yr..]
        if(!missing(pol))
            lyrsel <- lapply(lyrsel, function(x)
                cropRaster(x, pol))
        names(lyrsel)[1:2]  <- c('x','y')
        do.call('merge', lyrsel)}
    rst <- Map(function(x)
        fn.(ars., x, pol), x = time)
    return(rst)}

cropRaster <- function(rst, br){
    crp <- crop(rst, br)
    msk <- rasterize(br, crp, mask = TRUE)
    return(msk)}

crwrs <- function(rst, yr., mn.){
    yr.. <- names(rst)[grepl(yr., names(rst))]
    lyrsel <- rst[yr..]
    lyrsel <- lapply(lyrsel, function(x)
        crop(x, round(extent(mn.))))
    names(lyrsel)[1:2]  <- c('x','y')
    mrg <- do.call('merge', lyrsel)
    mrg <- mask(mrg, mn.)
    return(mrg)}

decomp <- function(zfe, ext = '.tar',td = tempdir(),
int.patt = '[[:digit:]|N|S|E|W].tif'){
    zfe. <- zfe[grepl(ext, zfe)]
zfe.. <- zfe[!grepl(ext, zfe)]
lsfn <- list(list = TRUE,exdir = td)
fn <- 'unzip'
    if(ext%in%'.tar')
        fn <- 'untar'
    zps <- Map(function(x)
        tryCatch(do.call(fn, c(x,lsfn)),error = function(e)NULL), zfe.)
    znms <- lapply(zps, function(x)x[1L])
    if(ext%in%'.zip')
        znms <- lapply(zps, function(x)x[,1L])
    find <- lapply(znms, function(x)
        x[grepl(int.patt, x)])
    find <- unlist(find, use.names = FALSE)
    lsfn <- function(x,y)
        list(x, files = y, exdir = td)
fnd. <- find[!find%in%dir(td)]
    if(length(fnd.) !=0)
    uz <- suppressWarnings(Map(function(x,y)
        tryCatch(do.call(fn, lsfn(x,y)),error = function(e)NULL),
        x = zfe., y = fnd.))
        ## x = zfe., y = find))
    ## find <- unlist(find, use.names = FALSE)
    find.. <- file.path(td, find)
    toext <- c(zfe..[!zfe..%in%find..], find..)
    toext <- toext[!grepl('.tar|.zip|.rds|/raster|\\raster|/ebv', toext)]
    toext <- toext[order(toext)]
    attributes(toext) <- c(attributes(toext), list(infile = zfe.))
    return(toext)
}

decomp0 <- function(zfe, ext = '.tar',td = tempdir(),
                   int.patt = '[[:digit:]|N|S|E|W].tif'){
    zfe <- basename(zfe)
    zfe <- file.path(td, zfe)
    zfe. <- zfe[grepl(ext, zfe)]
    zfe.. <- zfe[!grepl(ext, zfe)]
    zfe.. <- zfe..[grepl('.tif', zfe..)]
    lsfn <- function(x)
        list(x,list = TRUE,exdir = td)
    fn <- 'unzip'
    if(ext%in%'.tar'){
        fn <- 'untar'}
    blanck <- 'no_data_at_all'
    zps <- Map(function(x)
        tryCatch(do.call(fn, lsfn(x)),error = function(e)
            data.frame(blanck, stringsAsFactors = FALSE)), zfe.)
    znms <- lapply(zps, function(x)x[1L])
    if(ext%in%'.zip')
        znms <- lapply(zps, function(x)x[,1L])
    int.patt <- paste0(int.patt,'|',blanck)
    find <- lapply(znms, function(x)
        x[grepl(int.patt, x)])
    find <- unlist(find, use.names = FALSE)
    if(length(find)!=0){
    lsfn1 <- function(x,y)
        list(x, files = y, exdir = td)
        uz <- suppressWarnings(Map(function(x,y)
            tryCatch(do.call(fn, lsfn1(x,y)),error = function(e)''),
            x = zfe., y = find))}
    find.. <- file.path(td, find)
    toext <- unique(c(zfe.., find..))
    find.. <- find..[!grepl(blanck, find..)]
    toext <- toext[!grepl(blanck,toext)]
    attributes(toext) <- c(attributes(toext), list(inzip = find..))
    return(toext)
}

decompMap <- function(zfe, td = tempdir(),
int.patt = '[[:digit:]|N|S|E|W].tif'){
    ls2r <- unique(unlist(Map(function(x)
        decomp(zfe, x, td, int.patt),
        c('.zip','.tar'))))
##     tifimag <- Map(function(x)
##         raster(x), x = ls2r)
## return(tifimag)}
return(ls2r)}


decompMap0 <- function(zfe, td = tempdir(),
int.patt = '[[:digit:]|N|S|E|W].tif'){
    ## ls2r <- unique(unlist(Map(function(x)
    ls2r <- Map(function(x)
        decomp0(zfe, x, td, int.patt),
        c('.zip','.tar'))
    ls3r <- unique(unlist(ls2r))
    lsinz <- unique(unlist(lapply(ls2r, function(x)
        attr(x, 'inzip'))))
    attributes(ls3r) <- c(attributes(ls3r), list(inzip = lsinz))
    return(ls3r)}


edge2url <- function(str, dtnm, url.){
flnm <- paste(dtnm, '_', str[1L], '_', str[2L], "_v1_1.tif", sep = '')
urll <- paste(url., dtnm, flnm, sep ='/')
return(urll)
}

f16 <- function(x, a, filename='', ...) {
    out <- raster(x)
    big <- ! canProcessInMemory(out, 3)
    filename <- trim(filename)
    if (big & filename == '') {
        filename <- rasterTmpFile()
    }
    if (filename != '') {
        out <- writeStart(out, filename, ...)
        todisk <- TRUE
    } else {
        vv <- matrix(ncol=nrow(out), nrow=ncol(out))
        todisk <- FALSE
    }
    bs <- blockSize(x)
    pb <- pbCreate(bs$n, ...)
    if (todisk) {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            v[!v%in%a] <- NA
            out <- writeValues(out, v, bs$row[i])
            pbStep(pb, i)
        }
        out <- writeStop(out)
    } else {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            v[!v%in%a] <- NA
            cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
            vv[,cols] <- matrix(v, nrow=out@ncols)
            pbStep(pb, i)
        }
        out <- setValues(out, as.vector(vv))
    }
    pbClose(pb)
    return(out)
}

f2 <- function(x, a) {
    v <- getValues(x)
    v[v == 0] <- NA
    v[!v%in%a] <- NA
    v[v>1] <- 1
    x <- setValues(x, v)
    return(x)
}

f22 <- function(x, a){
    fn. <- function(x, a) {
        v <- getValues(x)
        v[!v%in%a] <- NA
        x <- setValues(x, v)
        return(x)}
    Map(function(y)
        fn.(y, a), x)}

f23 <- function(x, a){
        v <- getValues(x)
        v[!v%in%a] <- NA
        x <- setValues(x, v)
        return(x)}

f8 <- function(x, a, filename='', ...) {
    out <- raster(x)
    big <- ! canProcessInMemory(out, 3)
    filename <- trim(filename)
    if (big & filename == '') {
        filename <- rasterTmpFile()
    }
    if (filename != '') {
        out <- writeStart(out, filename, ...)
        todisk <- TRUE
    } else {
        vv <- matrix(ncol=nrow(out), nrow=ncol(out))
        todisk <- FALSE
    }
    bs <- blockSize(x)
    pb <- pbCreate(bs$n, ...)
    if (todisk) {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            ## v[v == 0] <- NA
            v[!v%in%a] <- NA
            ## v[v>1] <- 1
            v[v>0] <- 1
            out <- writeValues(out, v, bs$row[i])
            pbStep(pb, i)
        }
        out <- writeStop(out)
    } else {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            ## v[v == 0] <- NA
            v[!v%in%a] <- NA
            ## v[v>1] <- 1
            v[v>0] <- 1
            cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
            vv[,cols] <- matrix(v, nrow=out@ncols)
            pbStep(pb, i)
        }
        out <- setValues(out, as.vector(vv))
    }
    pbClose(pb)
    return(out)
}

fget <- function(x,y, overwrite = TRUE, path){
    GET(x,write_disk(y, overwrite = overwrite), timeout(1E4))
    print(file.path(path,basename(x)))}

fgetpss <- function(x,y,cr = getOption('pw'), overwrite = TRUE, path){
    GET(x,authenticate(cr[['nm']], cr[['pw']]),
        write_disk(y, overwrite = overwrite), timeout(1E4))
    print(file.path(path,basename(x)))}

flg <- function(nms = list('usgs.gov-username','usgs-password')){
pw <- lapply(nms, function(x) getPass(msg = x))
names(pw) <- c('nm','pw')
lse <- list2env(pw)}

fmmx <- function(rst, extr.val){
    mn <- lapply(as.list(rst), function(x)
        slot(x@'data', extr.val))
    mnf <- unlist(Filter(function(x)is.finite(x),mn))
    extr.val <- do.call(extr.val,list(mnf))
    ## extr.val <- get(extr.val)(mnf)
    return(extr.val)}

fperc <- function(x) {
    v <- getValues(x)
    v[v > 100] <- NA
    x <- setValues(x, v)
    return(x)
}

fsides <- function(x){
ex <- extent(x)
side <- (ex[2L] - ex[1L]) / 30
side <- round(side,0)
return(side)}

ftibb <- function(msk, tyr){
    tyr <- as.numeric(
        sub("\\D+","", names(msk)))
    if(all(is.na(tyr)))
        tyr <- names(msk)
    levs <- 'landscape' 
    names(levs) <- c('l')
    mtrs <- c('FC_area'); names(mtrs) <- 'tafc' 
    dim <- nlayers(msk)
    tbb <- tibble(layer = tyr,
                  level = rep(levs, dim),
                  class = rep(NA, dim),
                  id = rep(NA, dim),
                  metric = rep(mtrs, dim),
                  value = lsm_l_tafc(msk))
    return(tbb)
}

get_EOURL <- function(adm, lyrs, funs., path, verify.web = FALSE){
    if(missing(funs.))
    funs. <- c('gwsBase','gfcBase','gfccBase')
    if(verify.web){#<--- web verification
        dts <- verifyInWebs(getOption('webs'), lyrs)} 
    else{dts <- verifyInList(lyrs)}
    dt1 <- lapply(dts,function(x)as.list(x))
    names(funs.) <- names(getOption('webs')) 
    funs.. <- funs.[names(dt1)]
    allf <- Map(function(adm,w,z,path)
        do.call(w, list(adm, z, path)), MoreArgs = list(adm = adm, path = path),
        w = funs.., z = dt1)
    allf <- allf[order(names(allf))]
    return(allf)}

getRoi <- function(im, ...){
    plotRGB(im, ...)
    e <- drawExtent()
    plotRGB(im, ..., ext = e)
    cr <- crop(im, e)
    return(cr)}

gfcBase <- function(adm, lyrs,path = tempdir()){
    wb <- getOption('apis')['gfc']
    eds <- urlE(adm, TRUE) #<-
    unlist(lapply(lyrs, function(f)
  paste0(wb,'/','Hansen_GFC-2019-v1.7_',f,'_',eds,'.tif')))}

gfccBase <- function(adm, lyrs, path = tempdir()){
## gfccBase <- function(adm, lyrs){
    wb <- getOption('apis')['daac']
    pr <- getWRS(adm, path) #<-
    prd <- unique(as.character(pr$PR))
    pth <- paste0('p',substr(prd, 1,3),'r',substr(prd, 4,6))
    webs.. <- sapply(lyrs, function(x)
        sub('.01.01','',x))
    unlist(Map(function(x,y)
        paste0(wb,'GFCC30TC.003','/',x,'/','GFCC30TC','_',pth,
           '_','TC_',y,'.zip'), lyrs, webs..))}

gwsBase <- function(adm, lyrs, path = tempdir()){
    wb <- getOption('apis')['gsw']
eds <- urlE(adm, FALSE) #<-
    unlist(lapply(lyrs, function(f)
    paste0(wb,'/',f,'/',f,'_',eds,'_v1_1.tif')))}

isWin <- Sys.info()['sysname']%in%'Windows'

layers2url <- function(pol, lyrs){
dts <- verifyInWebs(getOption('webs'), lyrs)
dt1 <- dts
dt1 <- lapply(dt1,function(x)as.list(x))
eds <- urlEdges(pol)[names(dts)]
fbal <- function(i)
    lapply(dt1[[i]], function(x)
    paste0(x,"_",eds[[i]]))
concs <- lapply(1:length(eds), function(i)
       fbal(i))
names(concs) <- names(eds)
concs. <- concs
concs <- unlist(concs)
## cortsub <- paste(paste0("_",unlist(getTileEdges(pol,FALSE))),
cortsub <- paste(paste0("_",unlist(urlEdges(pol,FALSE))),
                 collapse = "|")
lons <- sub(cortsub,'', concs)
return(concs)
filenm <- c()
for(i in 1:length(concs)){
    if(grepl('gsw', names(concs[i]))){
        filenm[i] <- paste0(getOption('apis')['gsw'],
                            "/",lons[i], "/",concs[i], "_v1_1.tif")
    }
if(grepl('gfc', names(concs[i]))){
    filenm[i] <- paste0(getOption('apis')['gfc'],
                        "/",'Hansen_GFC-2019-v1.7', "_",concs[i], ".tif")
}}
return(filenm)}

loadFromZip <- function(pt., patt = '.tif',
                        mc.cores = detectCores()){
    ## fld <- dir(pt.)
    ## zif <- fld[grepl('.zip', fld)]
    ## pt. <- file.path(pt., zif)
    tmp. <- tempdir()
    temp. <- file.path(tmp., 'data')
    fprll <- getOption('fapp')
    if(!getOption('isWin'))
        marg[['mc.cores']] <- mc.cores
    marg. <- c(list(FUN = function(x)
        unzip(x,exdir = tmp., list = TRUE),
        x = pt.),marg)
    lstar. <- do.call(fprll, marg.)
    lstar. <- unlist(lstar.)
    lstar. <- lstar.[grepl('.tif', lstar.)]
    lstar.. <- file.path(tmp.,lstar.)
    if(!all(lstar.%in%dir(tmp.))){
        marg.$'FUN' <- function(x)
            unzip(x,exdir = tmp.)
        lstr.. <- do.call(fprll, marg.)
        lstar.. <- unlist(lstr..)
        lstar.. <- lstar..[grepl('.tif', lstar..)]
    }
    lstar.. <- lstar..[grepl(patt, lstar..)]
    tifimag <- Map(function(x)
        raster(x),
        x = lstar..)
    aa <- names(tifimag)
    ab <- basename(aa)
    ac <- sub('.tif','',ab)
    names(tifimag) <- ac
    return(tifimag)}

long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1}

lonlat2utm <- function(tmp){
    rst <- stack(tmp)
    l2u <- long2UTM(extent(rst)[1L])
    sr <- sub('utm.z',l2u, getOption('utm'))
    polpr <- projectExtent(raster::subset(rst, 1), crs = sr)
    rst <- projectRaster(rst, polpr)
    return(rst)}

lsm_l_tafc <- function(msk){
    msk <- stack(msk)
    cells <- cellStats(msk, sum)
    med <- Reduce('*',res(msk)[1:2])
    area <- cells * med
    area <- area/1E4
    return(area)}

marg <- list(SIMPLIFY = FALSE)

myClamp <- function(x, lw, up)
    raster::clamp(x, lw, up, useValues = FALSE)

ordLayers <- function(ars., time){
    fn. <- function(ars.,time){
        nmss <- names(ars.)
        yr.. <- nmss[grepl(time, nmss)]
        lyrsel <- ars.[[yr..]]}
    srt <- Map(function(x)
        fn.(raster::as.list(ars.), x), x = time)
    stack(srt)}

rasterizeMetric <- function(x,w,z, val = 'value'){
spl1 <- as.data.frame(x)
rstt <- rasterize(w, z, field = spl1[,val])
return(rstt)}

recMatrix <- function(x, n){
cuts <- cut(x, n, include.lowest = FALSE)
ct <- levels(cuts)
lvn <- gsub("\\[|\\]|\\(|\\)", "", ct)
dfc <- as.character(lvn)
dfc1 <- do.call('rbind', strsplit(dfc, split ="\\,"))
dfc1 <- data.frame(dfc1, X3 = 1:n, stringsAsFactors=FALSE)
frw <- c(-Inf,dfc1[1,1],NA)
lrw <- c(dfc1[nrow(dfc1),2],Inf,NA)
dfc1 <- rbind(frw,dfc1,lrw)
names(dfc1) <- c('from','to','class')
dfc1[,1:ncol(dfc1)] <- lapply(dfc1[,1:ncol(dfc1)], 'as.numeric')
dfc1 <- as.matrix(dfc1)
return(dfc1)}

recTable <- function(thr = 0, recl = NA, 
## recTable <- function(thr = 0, 
                     oneFirst = TRUE){ 
    m <- diag(c(-1,1) * Inf)
    then <- c(1, recl)
    ## then <- c(1, NA)
    if(thr != 0)
        m[m == 0] <- thr
    if(!oneFirst)
        then <- rev(then)
    mat <- cbind(m, then)
    return(mat)}

reprojectRaster <- function(cr, td, uxt, tr. = c(30,30), sr){
    flnm <- 'ebv'
    if(length(file.path(td, flnm)) != 0)
    unlink(file.path(td, flnm), recursive = TRUE)
nex <- as.vector(uxt)
te. <- nex[c(1,3,2,4)]
crf <- dir(file.path(td, flnm))
if(length(crf)==0)
 dir.create(file.path(td,flnm))
nwp <- file.path(td, flnm)
nnm <- paste0('PR_', basename(names(cr)))
wrir <- suppressWarnings(Map(function(x,y)
    writeRaster(x,
                filename = file.path(nwp, basename(y)),
                format ='GTiff',overwrite=TRUE), cr, names(cr)))
dfn <- file.path(nwp, nnm)
fls <- file.path(nwp,dir(nwp))
if(any(grepl('PR_', file.path(nwp, nnm))))
pr <- Map(function(x,y,z)
    gdalwarp(x, y, crs(z), sr, te = te., tr = tr.,output_Raster = TRUE,
             overwrite=TRUE, srcnodata = '0',dstnodata = '0'), fls, dfn,cr)
if(any(basename(names(cr))%in%dir(nwp)))
file.remove(file.path(nwp, basename(names(cr))))
return(pr)}

rnm.lyrs <- function(lyrs,
                     excl = "\\..*",
                     maint = '^[0-9]',
                     incl = 'TC_'){
    ly. <- gsub(excl,"",lyrs)
    num <- grepl(maint, ly.)
    tc <- NULL
    if(any(num))
        tc <- paste0(incl,ly.[num])
    lyr <- c(lyrs[!num],tc)
    return(lyr)}

rnm.lyrs0 <- function(lyrs){
    num <- grepl('[0-9]', lyrs)
infi <- c('TC_','.01.01')
    if(!any(grepl(paste(infi, collapse = '|'), lyrs))){
        return(lyrs)}
    num <- Filter(function(x)any(x),Map(function(x)
    grepl(x, lyrs),infi))
num.. <- infi[grepl(names(num),infi)]
num. <- infi[!grepl(names(num),infi)]
ly. <- gsub(num..,"",lyrs)
if(num.==infi[1L]){
    tc <- paste0(num.,ly.[num[[1L]]])}
else{tc <- paste0(ly.[num[[1L]]],num.)}
    lyr <- c(lyrs[!num[[1L]]],tc)
return(lyr)}

scaleYear <- function(yr.){
scaleYr <- function(year){
    if(any(year >= 2E3))
    year <- as.vector(scale(year,2E3,1))
    return(year)}
sc <- sapply(yr., function(x) scaleYr(x))
return(sc)}

selTile <- function(pol){
## Select 10x10 degree tile using polygon
    ep <- extent(pol)
ex <- c(-180, 180, -90, 90)
ext <- extent(ex)
pl <- as(ext, 'SpatialPolygons')
crs(pl) <- crs(pol)
pl <- st_as_sf(pl) ## new line just added
    grd <- sf::st_make_grid(pl, cellsize = 10, what = 'polygons',
                            square = TRUE)
bbox <- Map(function(x)
    c(st_bbox(grd[x])), 1:length(grd)) 
nmord <- names(bbox[[1L]])[c(c(1,3),c(2,4))]
xnp <- Map(function(x)
    x[nmord], bbox)
exts <- Map(function(x)
    extent(x), xnp)
ints <- Filter(function(x)
    !is.null(raster::intersect(ep, x)), exts)
return(ints)}

source2E <- function(zfe, int.patt){
    zfe. <- zfe[grepl('.zip', zfe)]
    zfe.. <- zfe[!grepl('.zip', zfe)]
    zps <- Map(function(x)
        unzip(x, list = TRUE,exdir = tempdir()), zfe.)
    znms <- lapply(zps, function(x)x[,1L])
    find <- lapply(znms, function(x)
        x[grepl(int.patt, x)])
    uz <- suppressWarnings(Map(function(x,y)
        unzip(x, files = y,
              overwrite = FALSE,
              exdir = tempdir()),
        x = zfe., y = find))
    find <- unlist(find, use.names = FALSE)
    find.. <- file.path(tempdir(), find)
    toext <- c(zfe..[!zfe..%in%find..], find..)
    tifimag <- Map(function(x)
        raster(x), x = toext)
    attributes(tifimag) <- c(attributes(tifimag),list(zpf = find..))
    return(tifimag)
}

source2Env <- function(zfe, int.patt){
    zfe <- unlist(zfe)
    if(all(grepl('.zip', zfe))){
        zps <- Map(function(x)
            unzip(x, list = TRUE,exdir = tempdir()), zfe)
        znms <- lapply(zps, function(x)x[1L,1L])
        find <- lapply(znms, function(x)x[grepl(int.patt, x)])
        zps <- Map(function(x,y)
            unzip(x, files = y,exdir = tempdir()), x = zfe, y = find)
        zpu <- unlist(zps, use.names = FALSE)
    } else {
        zpu <- zfe                                             
    }
    tifimag <- Map(function(x)
        raster(x), x = zpu)
    return(tifimag)
}

urlE <- function(pol, add.0 = TRUE){
ext <- selTile(pol)
fn. <- function(ext, add.0){
e2v <- as.vector(ext)
xtr <- e2v[c(1,4)]
iswe <- ifelse(xtr[1L] < 0, 'W', 'E')
issn <- ifelse(xtr[2L] < 0, 'S', 'N')
abxtr <- abs(xtr)
is.lon100 <- xtr[1L] < 100
is.lat10 <- xtr[2L] < 10
txtt <- paste(abxtr, c(iswe, issn), sep = '')
if(add.0){
    txtt <- rev(txtt)
    if(is.lat10)
        txtt  <- paste0(c('0',''), txtt)
    if(is.lon100)
        txtt  <- paste0(c('','0'), txtt)}
txtt <- paste(txtt, collapse = "_")
return(txtt)}
ond <- Map(function(x)
           fn.(x, add.0), selTile(pol))
return(ond)}

urlEdges <- function(pol, add.0 = list(gsw = FALSE, gfc = TRUE)){
fn.. <- function(pol, add.0 = TRUE){
ext <- selTile(pol)
fn. <- function(ext, add.0){
e2v <- as.vector(ext)
xtr <- e2v[c(1,4)]
iswe <- ifelse(xtr[1L] < 0, 'W', 'E')
issn <- ifelse(xtr[2L] < 0, 'S', 'N')
abxtr <- abs(xtr)
is.lon100 <- xtr[1L] < 100
is.lat10 <- xtr[2L] < 10
txtt <- paste(abxtr, c(iswe, issn), sep = '')
if(add.0){
    txtt <- rev(txtt)
    if(is.lat10)
        txtt  <- paste0(c('0',''), txtt)
    if(is.lon100)
        txtt  <- paste0(c('','0'), txtt)}
txtt <- paste(txtt, collapse = "_")
return(txtt)}
ond <- Map(function(x)
           fn.(x, add.0), selTile(pol))
return(ond)}
ur4lay <- Map(function(x)
    fn..(pol, x), x = add.0)
edgs <- lapply(ur4lay, unlist)
return(edgs)}

verifyInList <- function(lyrs){
lst <- listGP(api.code = TRUE)
lyrs. <- lyrs
if(any(grepl('.01.01', lyrs)))
    lyrs. <- rnm.lyrs0(lyrs)
lstt <- lst[lst$'layer'%in%lyrs.,]
spl <- split(as.data.frame(lstt)$'layer', lstt$'api.code')
spl <- lapply(spl,'rnm.lyrs0')
return(spl)}

verifyInWebs <- function(url.ls = getOption('webs'), layers){
        if(!curl::has_internet())
        return("no_internet")
        fn. <- function(urlw, layers){
            attr.nodes <- c('a', 'input')
            attr.names <- c('href')
            ## attr.names <- c('href','value')
            doc <- xml2::read_html(urlw)
            nod <- sapply(attr.nodes, function(x)
                rvest::html_nodes(doc,x))
            nod. <- Filter(function(x)
                length(x)!=0, nod)
            href <- Map(function(x,y)
                rvest::html_attr(x,y), nod., attr.names)
            href <- unlist(href)
            bn <- basename(href)
            layers. <- paste(layers, sep = '')
            layers. <- sapply(layers., function(x)
                any(grepl(x, bn)))
            layers. <- layers[layers.]
        return(layers.)}
        gwattr <- Map(function(x)
            fn.(x, layers), x = url.ls)
        if('gfc'%in%names(gwattr)){
            if('change'%in%gwattr[['gfc']])
                gwattr[['gfc']] <- gwattr[['gfc']][
                    !gwattr[['gfc']]%in%'change']} 
        gwattr <- Filter(function(x)
            length(x)!=0, gwattr)
    return(gwattr)}

.onAttach <- function(lib, pkg)
{
  ## unlockBinding("ecochange", asNamespace("ecochange")) 
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  
  if(interactive())
    { # > figlet ecochange
        packageStartupMessage(
          "ecochange
version: ", version)
}
else
    { packageStartupMessage(
          "Package 'ecochange' version ", version) } 

  packageStartupMessage("Type 'citation(\"ecochange\")' for citing this R package in publications.")
  invisible()
}

.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(gfc = "https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.6.html",
                  wrs = "https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/atoms/files/WRS2_descending_0.zip",
                  apis = c(gsw = "http://storage.googleapis.com/global-surface-water/downloads2",
                           gfc = "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2019-v1.7",
                           daac = "https://e4ftl01.cr.usgs.gov/MEASURES/"),
                  webs = c(gsw = "https://global-surface-water.appspot.com/download",
                           gfc = "https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html",
                           gfcc = "https://e4ftl01.cr.usgs.gov/MEASURES/GFCC30TC.003"),
                  inh = c('SpatialPolygonsDataFrame','Extent','character','NULL'),
                  utm = "+proj=utm +zone=utm.z +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                  utm1 = "+proj=utm +zone=utm.z +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  longlat = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
                  isWin = isWin,
                  fapp = 'mcmapply',
                  miss = ' Missing layer ',
                  trls = c('treecover2000','lossyear'),
                  plotCol = list(red = 31, green = 133, blue = 222,
                                 maxColorValue = 255)
                  )

toset <- !(names(op.FC) %in% names(op))
  if(any(toset)) options(op.FC[toset])
invisible()

}
