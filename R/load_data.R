# Load data


#' Load FAIR data for analysis
#'
#' @return (data.frame) FAIR data to be analyzed
#' @import contentid
#' @import vroom
#' @export
#'
#' @examples
#' fair_data <- load_data()
load_data <- function() {

    # Load FAIR data from KNB data package
    # Matthew Jones, Peter Slaughter, and Ted Habermann. 2019. Quantifying FAIR: metadata improvement and guidance in the DataONE repository network. Knowledge Network for Biocomplexity. doi:10.5063/F14T6GP0.
    # Hash identifier is:
    fair_data_hash <- "hash://sha256/77eaa2aa2037f2bd43ad5185d204ad12fba68f315a46c4b0d59bb303512288a5"
    fair_data_file <- contentid::resolve(fair_data_hash, store=TRUE)
    fair_data <- vroom(fair_data_file)
    return(fair_data)
}
