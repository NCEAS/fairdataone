utils::globalVariables(c("."))

#' Get example DataCite documents
#'
#' This function grabs a list of example documents for the DataCite dialect from
#' the KNB, the Arctic Data Center, and Dryad.
#'
#' @param dir Local directory to write files to
#' @param n Number of example documents from each node to retrieve
#'
#' @import dataone
#' @import dplyr
#' @import jsonlite

get_datacite_examples <- function(dir, n){

    cn <- dataone::CNode("PROD")
    adc <- dataone::query(cn, list(q="formatType:METADATA+AND+authoritativeMN:*ARCTIC",
                                   fl = "identifier,authoritativeMN",
                                   rows="500"),
                             as = "data.frame") %>%
        dplyr::filter(grepl("doi", .$identifier)) %>%
        dplyr::slice_head(n = n)

    knb <- dataone::query(cn, list(q="formatType:METADATA+AND+authoritativeMN:*KNB",
                                   fl = "identifier,authoritativeMN",
                                   rows="500"),
                          as = "data.frame") %>%
        dplyr::filter(grepl("doi", .$identifier)) %>%
        dplyr::slice_head(n = n)
    # the API seems to not listen to the results per page, it returns 20 results regardless
    dry <- jsonlite::fromJSON(paste0("https://datadryad.org/api/v2/datasets?page=1&per_page=", as.character(n)))
    dry <- dry$`_embedded`[[1]]$identifier
    dry <- dry[1:n]


    ids <- c(dry, adc$identifier, knb$identifier) %>%
        gsub("doi:", "", .)


    p <- lapply(ids, function(x){
        t <- jsonlite::fromJSON(paste0("https://api.datacite.org/dois/", x))
        jsonlite::base64_dec(t$data$attributes$xml) %>%
        rawToChar() %>%
        writeLines(paste0(dir, gsub("/|\\.", "", x), ".xml"))
    }
        )


}





