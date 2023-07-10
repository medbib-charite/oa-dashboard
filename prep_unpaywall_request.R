#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Request Unpaywall API via roadoi ----
#+ This is only executed once per yearly survey
#+ to keep the state at a specific date.
#+ The result is saved as R object (Rds file) and CSV file
#+ containing the request date in file name
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load libraries ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(readxl)
library(roadoi)
library(tidyverse)
library(stringr)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions to request and save ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Function to request the Unpaywall API via roadoi.
#' file: Name of an xlsx file, that contains a column "DOI" with dois to request
#' (other columns are okay but will not be considered)
#' Sheet: name of the Worksheet in the xlsx file
request_unpaywall <- function(file, sheet) {
  raw <- read_xlsx(file, sheet = sheet)
  dois <- raw %>%
    select(doi = DOI) %>%
    filter(!str_detect(tolower(doi), "keine doi"),
           !is.na(doi))
  unpw <- roadoi::oadoi_fetch(dois = dois$doi,
                     email = "openaccess@charite.de",
                     .progress = "text")
  return(unpw)
}

#' Requests the Unpaywall API via roadoi using request_unpaywall()
#' and saves the results and the warnings each in an Rds file.
#' The file name includes the current date to avoid overwriting of existing
#' data for the requested year.
#' no return value (results will be saved directly to files)
request_and_save_unpaywall <- function(file, sheet, year) {
  unpw <- request_unpaywall(file, sheet)
  warn <- warnings()
  saveRDS(unpw, file = paste0("raw_data/unpaywall_", year, "_", Sys.Date(), ".Rds"))
  saveRDS(warn, file = paste0("raw_data/unpaywall_", year, "_", Sys.Date(), "_warnings", ".Rds"))

  # TODO: unnest cols c(best_oa_location, oa_locations, oa_locations_embargoed, authors) before writing to CSV
  unpw_as_character <- apply(unpw,2,as.character)  # not the best solution
  write.csv(unpw_as_character, file = paste0("raw_data/unpaywall_", year, "_", Sys.Date(), ".csv"))
}
