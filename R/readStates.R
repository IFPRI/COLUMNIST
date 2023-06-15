#' readStates
#'
#' @return Cell area for LUHv2 land use types on 0.25 degree grid
#' @importFrom ncdf4 nc_open
#' @importFrom terra rast time
#' @export
#'
#' @examples
#' \dontrun{
#' readStates()
#' }
#' @author Abhijeet Mishra
readStates <- function() {
    # Basic ----
    ncpath <- getOption("luhv2_loc")
    ncname <- "states"
    ncfname <- paste(ncpath, ncname, ".nc", sep = "")

    area_name <- paste(ncpath, "staticData_quarterdeg", ".nc", sep = "")

    varnames <- names(nc_open(ncfname)[["var"]])

    # TERRA ----

    temp <- c()
    exclude <- c("lat_bounds", "lon_bounds", "secma", "secmb")
    for (i in varnames[!(varnames %in% exclude)]){
        rast_obj <- terra::rast(ncfname, subds = i)[[1166]]
        names(rast_obj) <- terra::varnames(rast_obj)
        temp <- c(temp, rast_obj)
        message(i, " ---> ", time(rast_obj))
    }

    raster_stack <- terra::rast(temp)

    cell_area <-  rast(area_name, subds = "carea")

    land_use_area <- raster_stack * cell_area

    land_use_area <- land_use_area * 1e6 #km2 to m2
    land_use_area <- land_use_area / 1e4 #m2 to ha
    land_use_area <- land_use_area / 1e3 #ha to 000 ha

    return(land_use_area)
}
