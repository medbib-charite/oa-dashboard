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

# 2022 ----
request_and_save_unpaywall(file = "raw_data/2022.xlsx", sheet = "Merge", year = 2022)

# 2023 ----
read_xlsx("raw_data/2023.xlsx", sheet = "Merge") %>%
  filter(!DOI == "10.1158/1078-0432.CCR-22-3790") %>% # remove this DOI because it causes a timeout every time
  mutate(Addresses = str_trunc(Addresses, 32767, side = "right")) %>%
  write_xlsx("raw_data/2023_for_unpw_request.xlsx")
request_and_save_unpaywall(file = "raw_data/2023_for_unpw_request.xlsx", sheet = "Sheet1", year = 2023)
request_and_save_unpaywall(file = "raw_data/2023.xlsx", sheet = "Merge", year = 2023)


## Test: request again the dois without result in the first time; result: they are not in Unpaywall at all ----
rest_of_2023 <- read_xlsx("raw_data/2023.xlsx", sheet = "Merge") %>%
  filter(!(tolower(DOI) %in% `unpaywall_2023_2024-09-25`$doi)) %>%
  filter(!DOI == "10.1158/1078-0432.CCR-22-3790") # remove this DOI because it causes a timeout every time
write_xlsx(rest_of_2023, "raw_data/rest_of_2023.xlsx")
request_and_save_unpaywall(file = "raw_data/rest_of_2023.xlsx", sheet = "Sheet1", year = "2023_rest_without_unpw_result")
