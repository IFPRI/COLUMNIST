#' calc FPU area based on LUH2 datase
#'
#' @param luh2_states An output from readStates function of this package
#' @importFrom terra vect extract values global
#'
#' @return list. Contains both a dataframe and a SpatVector with FPU level info.
#' @export
#'
#' @examples
#' \dontrun{
#' calcFPUArea(readStates())
#' }
#' @author Abhijeet Mishra
calcFPUArea <- function(luh2_states = NULL) {

    if (is.null(luh2_states)) {
        message("No LUH2 data provided. Calling readStates")
        luh2_states <- readStates()
    }

    if (class(luh2_states)[1] != "SpatRaster") {
        message("Invalid data type for LUH2 data. Has to be a SpatRaster.")
        stop()
    }

    values <- NULL

    message("Reading IMPACT FPU shapes ....")
    target_name <- "fpu2015_polygons_v3_multipart_polygons"
    fpath <-
        dirname(system.file("extdata", paste0(target_name, ".shp"),
                            package = "STATEROOM"))
    s <- terra::vect(fpath)

    # Extract
    message("Extracting LUH2 data based on IMPACT FPUs ....")
    area_recalc <- terra::extract(luh2_states,
                                  s,
                                  na.rm = TRUE,
                                  fun = sum,
                                  method = "bilinear",
                                  weights = TRUE,
                                  exact = TRUE,
                                  ID = TRUE)

    area_recalc$ID <- unlist(as.vector(values(s)[1]))

    message("Feeding values back to the FPUs ....")
    terra::values(s) <- area_recalc

    for (i in names(s)[!names(s) %in% "ID"]) {
        diff <-
            sum(area_recalc[, i], na.rm = TRUE) /
            terra::global(luh2_states[i], "sum", na.rm = TRUE)

        message(
            i, " --> ", "Missing area:", round(100 * (1 - diff), 1), "%")
    }

    out <- list()
    out$area_df  <- area_recalc
    out$area_shp <- s
    return(out)
}
