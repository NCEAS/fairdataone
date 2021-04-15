utils::globalVariables(c("."))

#' Get a list of member nodes and dialects
#'
#' This function queries the DataONE member nodes and returns a table of
#' metadata dialects supported by each node.
#'
#' @return data.table A summary table of MNs
#'
#' @import dplyr
#' @import tidyr
#' @import jsonlite

get_mn_dialects <- function(){

    res <-  jsonlite::fromJSON("https://cn.dataone.org/cn/v2/query/solr/?q=formatType:METADATA&facet=true&facet.pivot=authoritativeMN,formatId&fl=formatId&authoritativeMN&wt=json")

    formatIds <- lapply(res$facet_counts$facet_pivot$`authoritativeMN,formatId`$pivot, function(x){x$value})
    counts <- lapply(res$facet_counts$facet_pivot$`authoritativeMN,formatId`$pivot, function(x){x$count})

    t <- data.frame(mn = res$facet_counts$facet_pivot$`authoritativeMN,formatId`$value)
    t$formatIds <- formatIds
    t$counts <- counts

    t_long <- tidyr::unnest(t, c(formatIds, counts))

    dialect_map <- dplyr::tribble(~formatIds, ~dialect,
                                 "http://www.isotc211.org/2005/gmd-pangaea", "ISO",
                                 "http://datadryad.org/profile/v3.1", "Dryad",
                                 "eml://ecoinformatics.org/eml-2.1.0", "EML",
                                 "eml://ecoinformatics.org/eml-2.1.1", "EML",
                                 "eml://ecoinformatics.org/eml-2.0.1", "EML",
                                 "eml://ecoinformatics.org/eml-2.0.0", "EML",
                                 "https://eml.ecoinformatics.org/eml-2.2.0", "EML",
                                 "http://www.isotc211.org/2005/gmd-noaa", "ISO",
                                 "http://ns.dataone.org/metadata/schema/onedcx/v1.0", "DublinCore",
                                 "FGDC-STD-001-1998", "FGDC",
                                 "FGDC-STD-001.1-1999", "FGDC",
                                 "http://www.isotc211.org/2005/gmd", "ISO",
                                 "FGDC-STD-001.2-1999", "FGDC",
                                 "http://purl.org/ornl/schema/mercury/terms/v1.0",  "Mercury",
                                 "http://www.openarchives.org/OAI/2.0/oai_dc/", "DublinCore")

    t_join <- dplyr::inner_join(t_long, dialect_map, by = "formatIds") %>%
        dplyr::group_by(.data$mn, .data$dialect) %>%
        dplyr::summarise(n = sum(.data$counts), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = .data$dialect, values_from = .data$n)


    d <- jsonlite::fromJSON("https://cn.dataone.org/cn/v2/query/solr/?q=formatType:METADATA&stats=true&stats.field={!tag=piv1%20min=true%20max=true}dateUploaded&facet=true&facet.pivot={!stats=piv1}authoritativeMN&fl=dateUploaded,authoritativeMN&wt=json")

    d_f <- data.frame(mn = d$facet_counts$facet_pivot$authoritativeMN$value,
                      min_date =  as.Date(d$facet_counts$facet_pivot$authoritativeMN$stats$stats_fields$dateUploaded$min),
                      max_date = as.Date(d$facet_counts$facet_pivot$authoritativeMN$stats$stats_fields$dateUploaded$max))

    t_final <- dplyr::full_join(t_join, d_f, by = "mn")  %>%
        dplyr::select(.data$mn, .data$EML, .data$ISO, .data$DublinCore, .data$FGDC, .data$Dryad, .data$Mercury, .data$min_date, .data$max_date) %>%
        dplyr::arrange(-.data$EML, -.data$ISO, -.data$DublinCore, -.data$FGDC, -.data$Dryad, -.data$Mercury, desc(.data$max_date)) %>%
        dplyr::mutate(active = ifelse(.data$max_date > as.Date("2021-01-01"), TRUE, FALSE))

    return(t_final)

}
