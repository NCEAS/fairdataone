library(xml2)
library(dplyr)
library(purrr)
library(tidyr)

# this script grabs a table of all xpaths, the check they correspond to, and
# their dialect for ease of review. It downloads the metadig-checks repo, gets
# summary information on the FAIR suite, and then parses the individual xml
# files for each check in the suite


# download the metadig-checks repo to a tempdir and unzip it
# currently, this gets the tip of the master branch. as xpaths for checks are
# reviewed and/or changed, this should will change to the tip of the branch
# where those changes are made. this file is deliberarely not cached since it
# is expected to change
t <- tempdir()
download.file(url = "https://github.com/NCEAS/metadig-checks/archive/master.zip",
              destfile = paste0(t, "/metadig.zip"))

z <- unzip(paste0(t, "/metadig.zip"), exdir = t)


# read in xml file listing the check names
fair_suite_def <- read_xml(paste0(t, "/metadig-checks-master/src/suites/FAIR-suite.xml"))

# extract check names and correct some inconsistent names
check_names <- fair_suite_def %>%
    xml_find_all("./check/id") %>%
    xml_text() %>%
    gsub(".1|.2", "", .) %>%
    gsub("resource.abstractLength.sufficient", "resource.abstractLength", .)

# extract check types
check_type <- fair_suite_def %>%
    xml_find_all("./check/type") %>%
    xml_text()

# extract check levels (required or optional)
check_level <- fair_suite_def %>%
    xml_find_all("./check/level") %>%
    xml_text()

# create data frame of checks
fair_checks <- data.frame(check_names, check_type, check_level, stringsAsFactors = F)

# define function to read individual check file and get
# check description
get_description <- function(check_name){
    base_path <- paste0(t, "/metadig-checks-master/src/checks/")
    t <- read_xml(paste0(base_path, check_name, ".xml")) %>%
        xml_find_all("./description") %>%
        xml_text()

}

# run get description function over all checks
fair_checks$description <- map_chr(check_names, get_description)

# define function to read check file in, then extract and clean
# all xpaths contained within the check
parse_xpath <- function(check_name){
    base_path <- paste0(t, "/metadig-checks-master/src/checks/")
    xps <- read_xml(paste0(base_path, check_name, ".xml")) %>%
        xml_find_all("./selector/xpath") %>%
        xml_text() %>%
        strsplit("\n") %>%
        unlist() %>%
        # clean up random characters not part of the xpath
        gsub("\\|", "", .) %>%
        gsub("boolean\\(", "", .) %>%
        gsub(" or", "", .) %>%
        trimws(.) %>%
        gsub("^\\(", "", .) %>%
        gsub("\\)$", "", .) %>%
        gsub("^//", "/", .) %>%
        grep("[a-z].*", ., value = T)
}

# extract xpaths for all checks
xpaths <- purrr::map(check_names, parse_xpath)

fair_checks$xpath <- xpaths
# expand data frame so each row corresponds to an xpath
fair_checks_ex <- fair_checks %>%
    unnest(cols = xpath)

# define function to assign metadata to dialect based
# on the start of the xpath
assign_dialect <- function(xpath){
    if (startsWith(xpath, "/*/")){
        return("ISO")
    } else if (startsWith(xpath, "/eml")){
        return("EML")
    } else if (startsWith(xpath, "/resource")){
        return("DataCite")
    } else if (startsWith(xpath, "/DryadDataFile")){
        return("Dryad")
    } else return(NA)

}
# run function to assign dialect to each xpath
fair_checks_ex$dialect <- map_chr(fair_checks_ex$xpath, assign_dialect)

