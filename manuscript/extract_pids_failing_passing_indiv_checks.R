#extract three example PIDs with passing and with failing individual for ISO docs only across multiple DataONE repositories/member nodes


library("here")
library("tidyverse")


#set path to data in P. Slaughter's  folder
path <- "/home/slaughter/FAIR-suite-0.3.1-20210121/check-data/fair-0.3.1-checks-joined.rda"

#load RDA file with all DataONE individual checks (NOTE: file is large with over 41 million rows; loading takes 3+ minutes)
load(file=path)
rm(path)



#confirm number of unique FAIR category types, levels, and individual checks
unique(checks_joined$check_type) #4
unique(checks_joined$check_level) #2
unique(checks_joined$check_name) #51

#check unique statuses (note these for filtering later)
unique(checks_joined$check_status)


#create vector of format IDs used to indicate ISO formats
ISO_formats <- c("http://www.isotc211.org/2005/gmd-noaa",
                 "http://www.isotc211.org/2005/gmd-pangaea",
                 "http://www.isotc211.org/2005/gmd")

#Filter out everything except for ISO. Remove ADC metadata.
checks_ISO_only <- checks_joined %>%
    filter(format_id %in% ISO_formats) %>%
    filter(origin_mn != "urn:node:ARCTIC")

#create summary table for the number of records in each check_status (NOTE: No FAILUREs for "Provenance Trace Present")
DataONE_indivChecks_ISO_status_summary <- checks_ISO_only %>%
    group_by(check_name, check_status) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = check_status, values_from = n) %>%
    arrange(check_name)



#set seed
set.seed(20210125)

#extract three examples for both passing/failing checks for ISO docs across all member nodes, using replace=TRUE to help bypass member nodes
#with zero SUCCESSES or FAILURES and then use `distinct()` to subset any duplicate entries.
intermediate_subset <- checks_ISO_only %>%
    filter(check_status %in% c("SUCCESS", "FAILURE")) %>%
    group_by(origin_mn, check_name, check_status) %>%
    sample_n(size=5, replace=TRUE) %>%
    distinct()

#sample all checks except two that have 3 or less checks (in either SUCCESS or FAILURE) across DataONE
DataONE_indivChecks_passFail_examplePIDs <- intermediate_subset %>%
    filter(!check_name %in% c("Resource License Present", "Entity Attribute Names Differ from Definitions")) %>%
    group_by(check_name, check_status) %>%
    sample_n(size=3, replace=FALSE) %>%
    select(check_name, check_id, check_status, format_id, origin_mn, pid) %>%
    arrange(check_name, check_status)



DataONE_indivChecks_passFail_examplePIDs <- intermediate_subset %>%
    filter(check_name == "Resource License Present" & check_status == "SUCCESS") %>%
    group_by(check_name, check_status) %>%
    sample_n(size=3, replace=FALSE) %>%
    select(check_name, check_id, check_status, format_id, origin_mn, pid) %>%
    full_join(DataONE_indivChecks_passFail_examplePIDs)

DataONE_indivChecks_passFail_examplePIDs <- intermediate_subset %>%
    filter(check_name == "Resource License Present" & check_status == "FAILURE") %>%
    group_by(check_name, check_status) %>%
    sample_n(size=3, replace=FALSE) %>%
    select(check_name, check_id, check_status, format_id, origin_mn, pid) %>%
    full_join(DataONE_indivChecks_passFail_examplePIDs)


DataONE_indivChecks_passFail_examplePIDs <- intermediate_subset %>%
    filter(check_name == "Entity Attribute Names Differ from Definitions", check_status == "SUCCESS") %>%
    group_by(check_name, check_status) %>%
    sample_n(size=3, replace=FALSE) %>%
    select(check_name, check_id, check_status, format_id, origin_mn, pid) %>%
    full_join(DataONE_indivChecks_passFail_examplePIDs)

DataONE_indivChecks_passFail_examplePIDs <- intermediate_subset %>%
    filter(check_name == "Entity Attribute Names Differ from Definitions", check_status == "FAILURE") %>%
    group_by(check_name, check_status) %>%
    sample_n(size=3, replace=FALSE) %>%
    select(check_name, check_id, check_status, format_id, origin_mn, pid) %>%
    full_join(DataONE_indivChecks_passFail_examplePIDs)

DataONE_indivChecks_passFail_examplePIDs <- DataONE_indivChecks_passFail_examplePIDs %>%
    arrange(check_name)



#check the number of individual checks
unique(DataONE_indivChecks_passFail_examplePIDs$check_name)



#write PIDs to CSV
write_csv(DataONE_indivChecks_passFail_examplePIDs, here("DataONE_indivChecks_passFail_examplePIDs.csv"))

#write summary stats to CSV
write_csv(DataONE_indivChecks_ISO_status_summary, here("DataONE_indivChecks_status_summary.csv"))
