#' Install the vkr package
#'
#' @export
install_vkr <- function() {

    ## * Set working directory
    setwd("/datadisk/Utah/Kladblok/20171214_R/MyPackage/vkr")

    ## * Unload previous version of vkr
    unloadNamespace("vkr")

    ## * Generate documentation for your functions
    devtools::document()

    ## * Install the package
    devtools::install(upgrade = "never")

}
