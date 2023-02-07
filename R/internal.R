## Internal utility functions used by ecochange

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

decompMap0 <- function(zfe, td = tempdir(),
int.patt = '[[:digit:]|N|S|E|W].tif'){
    ls2r <- Map(function(x)
        decomp0(zfe, x, td, int.patt),
        c('.zip','.tar'))
    ls3r <- unique(unlist(ls2r))
    lsinz <- unique(unlist(lapply(ls2r, function(x)
        attr(x, 'inzip'))))
    attributes(ls3r) <- c(attributes(ls3r), list(inzip = lsinz))
    return(ls3r)}

fget <- function(x,y, overwrite = TRUE, path){
    h <- curl::new_handle(CONNECTTIMEOUT = 1E4)
    hd <- structure(list(handle = h, url = x), class = "handle")
    GET(x, write_disk(y, overwrite = overwrite),handle = hd)
    fp <- file.path(path,basename(x))
    return(fp)}

fgetpss <- function(x,y,cr = getOption('pw'), overwrite = TRUE, path){
    h <- curl::new_handle(CONNECTTIMEOUT = 1E4)
    hd <- structure(list(handle = h, url = x), class = "handle")
    GET(x,authenticate(cr[['nm']], cr[['pw']]),
        write_disk(y, overwrite = overwrite), handle = hd)
    fp <- file.path(path,basename(x))
    return(fp)}

flg <- function(nms = list('usgs.gov-username','usgs-password')){
pw <- lapply(nms, function(x) getPass(msg = x))
names(pw) <- c('nm','pw')
lse <- list2env(pw)}

get_EOURL <- function(adm, lyrs, funs., path, verify.web = FALSE){
    if(missing(funs.))
    ## funs. <- c('gwsBase','gfcBase','gfccBase')
    funs. <- c('gwsB','gfcB','gfccB')
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

gfcB <- function(adm,
                 lyrs,
                 path = tempdir(),
                 vrs = 'v1.9',
                 year = '2021',
                 request = 'earthenginepartners-hansen'
                 ){
    api <- getOption('apis')['gapi']
    degree <- urlE(adm, TRUE) #<-
    subdir <- paste0('GFC-',year,'-',vrs)
    unlist(lapply(lyrs, function(f)
        paste0(api,
               '/',request,'/',subdir,'/Hansen_',subdir,'_',f,'_',degree,'.tif')))}

## gfcBase <- function(adm, lyrs,path = tempdir()){
##     wb <- getOption('apis')['gfc']
##     eds <- urlE(adm, TRUE) #<-
##     unlist(lapply(lyrs, function(f)
##   paste0(wb,'/','Hansen_GFC-2021-v1.9_',f,'_',eds,'.tif')))}

gfccB <- function(adm, lyrs, path = tempdir(),
                  vrs = 'GFCC30TC.003',
                  year = NULL,
                  request = 'MEASURES'){
    ## usapi <- "https://e4ftl01.cr.usgs.gov"
    usapi <- getOption('apis')['usapi']
    wb <- paste0(usapi,'/', request, '/')
    pr <- getWRS(adm, path) #<-
    prd <- unique(as.character(pr$PR))
    pth <- paste0('p',substr(prd, 1,3),'r',substr(prd, 4,6))
    if(any(grepl('TC_', lyrs))){
        lyrs <- rnm.lyrs0(lyrs)}
    webs.. <- sapply(lyrs, function(x)
        sub('.01.01','',x))
    unlist(Map(function(x,y)
        paste0(wb,vrs,'/',x,'/','GFCC30TC','_',pth,
               '_','TC_',y,'.zip'), lyrs, webs..))}

## gfccBase <- function(adm, lyrs, path = tempdir()){
##     wb <- getOption('apis')['daac']
##     pr <- getWRS(adm, path) #<-
##     prd <- unique(as.character(pr$PR))
##     pth <- paste0('p',substr(prd, 1,3),'r',substr(prd, 4,6))
##     if(any(grepl('TC_', lyrs))){
##         lyrs <- rnm.lyrs0(lyrs)}
##     webs.. <- sapply(lyrs, function(x)
##         sub('.01.01','',x))
##     unlist(Map(function(x,y)
##         paste0(wb,'GFCC30TC.003','/',x,'/','GFCC30TC','_',pth,
##            '_','TC_',y,'.zip'), lyrs, webs..))}

gwsB <- function(adm,
                 lyrs,
                 path = tempdir(),
                 vrs = 'v1_4',
                 year = '2021',
                 request = 'global-surface-water'){
    api <- getOption('apis')['gapi']
    eds <- urlE(adm, FALSE) #<-
    rc1 <- paste0('downloads',year)
    unlist(lapply(lyrs, function(f)
        paste0(api,
               '/',request,'/',rc1,'/',f,'/',f,'_',eds,vrs,'_',year,'.tif')))}

## gwsBase <- function(adm, lyrs, path = tempdir()){
##     wb <- getOption('apis')['gsw']
## eds <- urlE(adm, FALSE) #<-
##     unlist(lapply(lyrs, function(f)
##     paste0(wb,'/',f,'/',f,'_',eds,'_v1_1.tif')))}

isWin <- Sys.info()['sysname']%in%'Windows'
## hasOsgeo4w <- 'OSGeo4W64'%in%list.files('C:/')

long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1}

marg <- list(SIMPLIFY = FALSE)

msk_0_ <- function(treeTemp, lossTemp, perc = c(1,100),tim = c(1,19)){
treeTemp[!(treeTemp >= perc[1] & treeTemp <= perc[2])] <- NA
    lgb <- '(treeTemp >= perc[1] & treeTemp <= perc[2])*(lossTemp >= tim[1] & lossTemp <= tim[2])*lossTemp'
    out2 <- eval(parse(text = lgb))
    names(out2) <- names(treeTemp)
    return(out2)}

msk_2_ <- function(treeTemp,lossTemp, remnant = TRUE, keep = TRUE,perc = c(1,100), tim = c(0,19), noData = 0){
    lga <- '(treeTemp >= perc[1] & treeTemp <= perc[2]) * (lossTemp*(lossTemp >= tim[1] & lossTemp <= tim[2])'
    if(!remnant){
        lga <- paste0(lga,'>0)')
    }else{
        lga <- paste0(lga,'<=0)')}
    if(keep){
        lga <- paste0('treeTemp*',lga)}
    out <- eval(parse(text = lga))
    if(!all(is.na(noData)))
    out[out==noData] <- NA
    names(out) <- paste0('eco_',max(tim))
    return(out)
}

msk_sp_ <- function(treeTemp, lossTemp, tim = c(1,1)){
    lossTemp[!(lossTemp >= tim[1] & lossTemp <= tim[2])] <- NA
    out <- treeTemp*lossTemp
    names(out) <- names(treeTemp)
    return(out)
}

nm2yr <- function(ebv){
        if(any(grepl('[[:digit:]]+',
                     names(ebv)))){
            loss.vals <- as.numeric(
                unique(unlist(
                    regmatches(names(ebv),
                               gregexpr("[[:digit:]]+",
                                        names(ebv))))))}
        else{stop("'names(ebv)' must include digits")}
loss.vals <- scaleYear(loss.vals)
        return(loss.vals)}

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

rem_temp_pol <- function(
                     path_alt = tempdir(),
                    rexp2rem = '.vrt|.txt|.dbf|.prj|.shp|.shx|.cpg'){
dr1 <- dir(path_alt)
torem1 <- file.path(tempdir(),dr1[grepl(rexp2rem,dr1)])
file.remove(torem1)}

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
maing <- 'SpatialPolygons'
ep. <- as(extent(pol),maing)
pl <- as(extent(c(-180, 180, -90, 90)), maing)
crs(pl) <- crs(ep.) <- crs(pol)
ep. <- st_as_sf(ep.)
pl <- st_as_sf(pl)
grd <- sf::st_make_grid(pl, cellsize = 10, what = 'polygons',
                            square = TRUE)
pred <- st_intersects(ep., grd)[[1L]]
mpred <- Map(function(x)
    st_bbox(as.data.frame(grd)[x,]), pred)
nmord <- names(mpred[[1L]])[c(c(1,3),c(2,4))]
exts <- Map(function(x)
    extent(x[nmord]), mpred)
return(exts)}

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
        cat('Verifying in webs ...\n')
        layers <- rnm.lyrs0(layers)
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
#<==================================================================

ecochange_figlet <- function(){
msg <- cat(
"                      
      _/_/      _/_/_/   
   _/_/_/_/  _/          
  _/        _/           
   _/_/_/    _/_/_/      \n")

vrs <- paste0('ecochange version ',packageVersion("ecochange"),'\n')
cat(vrs)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- ecochange_figlet()
  if(!interactive())
    msg[1] <- paste("Package 'ecochange' version ", packageVersion("ecochange"))
  packageStartupMessage(msg)
  invisible()
}

## .onAttach <- function(lib, pkg)
## {
##     ## unlockBinding("ecochange", asNamespace("ecochange"))
##     version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
##     if(interactive())
##     { # > figlet ecochange
##         packageStartupMessage(
##             "ecochange
## version: ", version)
##     }
##     else
##     { packageStartupMessage(
##           "Package 'ecochange' version ", version) }
##     packageStartupMessage("Type 'citation(\"ecochange\")' for citing this R package in publications.")
##     invisible()
## }

.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(gfc = "https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.6.html",
                    wrs = "https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/atoms/files/WRS2_descending_0.zip",
                  apis = c(gapi = "https://storage.googleapis.com",
                           usapi = "https://e4ftl01.cr.usgs.gov"),
                           ## gsw = "http://storage.googleapis.com/global-surface-water/downloads2",
                           ## gfc = "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2021-v1.9",
                           ## daac = "https://e4ftl01.cr.usgs.gov/MEASURES/"),
                  webs = c(gsw = "https://global-surface-water.appspot.com/download",
                           gfc = "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2021-v1.9/download.html",
                           gfcc = "https://e4ftl01.cr.usgs.gov/MEASURES/GFCC30TC.003"),
                  inh = c('SpatialPolygonsDataFrame','Extent','character','NULL','logical','sf'),
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
