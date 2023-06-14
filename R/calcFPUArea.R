#' calc FPU area based on LUH2 datase
#'
#' @param x An output from readStates function of this package
#' @importFrom terra vect extract values
#'
#' @return list. Contains both a dataframe and a SpatVector with FPU level info.
#' @export
#'
#' @examples
#' \dontrun{
#' calcFPUArea(readStates())
#' }
#' @author Abhijeet Mishra
calcFPUArea <- function(x) {

    values <- NULL

    target_name <- "fpu2015_polygons_v3_multipart_polygons"
    fpath <-
        dirname(system.file("extdata", paste0(target_name, ".shp"),
                            package = "STATEROOM"))
    s <- terra::vect(fpath)

    # Extract
    area_recalc <- terra::extract(x,
                                  s,
                                  na.rm = TRUE,
                                  fun = sum,
                                  method = "bilinear", weights = TRUE)

    area_recalc$ID <- unlist(as.vector(values(s)[1]))
    terra::values(s) <- area_recalc

    out <- list()
    out$area_df  <- area_recalc
    out$area_shp <- s
    return(out)
}
