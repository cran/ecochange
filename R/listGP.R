listGP <- structure(function #List of global products
### This function prints information about ecosystem remote sensing
### products that can be downloaded with \code{\link{getrsp}}.
                    
                    ##references<< {Pekel, J. F., Cottam, A.,
                    ##Gorelick, N., & Belward,
                    ##A. S. (2016). High-resolution mapping of global
                    ##surface water and its long-term changes. Nature,
                    ##540(7633), 418-422.}
                    ## 
                    ##{Hansen, M. C., Potapov, P. V., Moore, R.,
                    ##Hancher, M., Turubanova, S. A., Tyukavina, A.,
                    ##... & Kommareddy, A. (2013). High-resolution
                    ##global maps of 21st-century forest cover
                    ##change. science, 342(6160), 850-853.}
                    ## 
                    ## {Sexton, J. O., Song, X. P., Feng, M.,
                    ##Noojipady, P., Anand, A., Huang, C., ... &
                    ##Townshend, J. R. (2013). Global, 30-m
                    ##resolution continuous fields of tree cover:
                    ##Landsat-based rescaling of MODIS vegetation
                    ##continuous fields with lidar-based estimates
                    ##of error. International Journal of Digital
                    ##Earth, 6(5), 427-448.}



(
    layer = TRUE, ##<<\code{character}. Add column \code{'layer'} to
                  ##the data.
    Algorithm = TRUE, ##<<\code{character}. Add column
                      ##\code{'Algorithm'} to the data.
    author = TRUE, ##<<\code{character}. Add column \code{'author'} to
                   ##the data.
    funs = FALSE, ##<<\code{character}. Add column \code{'funs'} to
                  ##the data.
    api.code = FALSE ##<<\code{character}. Add column
                     ##\code{'api.code'} to the data.
) {
    fls <- c("global_products.RData")
    rasNm <- system.file(fls, package = "ecochange")
    load(rasNm)
    dt <- get(gsub('.RData','',fls))
    dt <- dt[,colnames(dt)[c(layer, Algorithm, author, funs, api.code)]]
    ## dt <- suppressMessages(readr::type_convert(as_tibble(dt)))
    dt <- suppressMessages(utils::type.convert(as_tibble(dt),as.is =TRUE))
    return(dt)
###\code{tibble}.
} , ex=function() {
    lst <- listGP()
    
})
