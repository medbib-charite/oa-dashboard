#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Manipulation and Visualization of Medbib dataset ----
# jan.taubitz@charite.de - 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Clean the workspace ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
gc()

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load libraries ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(gameofthrones)
library(highcharter)
library(htmlwidgets)
library(janitor)
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(uuid)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define color and oa-status variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed", "no result")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E", "#5F7036")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data input sets, ordered from oldest to newest ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

input_dataset_levels = c("2016_2017", "2018_2020", "2021", "2022")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load sources ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("prep_unpaywall.R", encoding = 'UTF-8')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data 2016 and 2017 ----
## Load data for 2016 and 2017 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2017_file <- "raw_data/2016-2018_merge charite_publikationen_article_review_wos_embase_corr_bih.xlsx"

total_2016_raw <- read_excel(data_2016_2017_file,
                             sheet = "merge_wos_embase_2016")

corr_2016_raw <- read_excel(data_2016_2017_file,
                            sheet = "merge_wos_embase_2016_corresp.")

total_2017_raw <- read_excel(data_2016_2017_file,
                             sheet = "merge_wos_embase_2017")

corr_2017_raw <- read_excel(data_2016_2017_file,
                            sheet = "merge_wos_embase_2017_corresp.")

total_2016_2017_raw <- rbind(total_2016_raw, total_2017_raw)

corr_2016_2017_raw <- rbind(corr_2016_raw, corr_2017_raw)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean 2016 and 2017 data, create some new variables ----
## Articles without doi will be deduplicated with PMID and WOS Number
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# create Charité corresponding author column
total_2016_2017 <- total_2016_2017_raw %>%
  clean_names() %>%
  mutate(corresponding_author_cha = FALSE)

corr_2016_2017 <- corr_2016_2017_raw %>%
  clean_names() %>%
  mutate(corresponding_author_cha = TRUE)

# combine data for corresponding and contributing authors and remove duplicate entries (introduced by xlsx sheets)
data_2016_2017_raw <- rbind(corr_2016_2017, total_2016_2017) %>%
  distinct(across(-corresponding_author_cha), .keep_all = TRUE) %>%
  mutate(datenbank = case_when(str_detect(identifier, "WOS:") ~ "WOS",
                               str_detect(identifier, "Embase") ~ "Embase")) %>%
  filter(datenbank != 0) # only keep entries from those databases

# assign UUID to articles without doi, then deduplicate dois but keep all articles without doi
data_2016_2017_doi_dedup <- data_2016_2017_raw %>%
  mutate(doi = tolower(doi)) %>%
  mutate(doi_existent = (doi != 0), .after = "doi") %>% # add column stating existence of doi
  mutate(doi = if_else(!doi_existent,
                       paste0("noDOI!!", replicate(n(), UUIDgenerate(n=1L, output = "string"))), doi)) %>% # Assign ids to articles without DOI
  distinct(doi, .keep_all = TRUE) # Remove duplicate dois. Articles without DOI not deduplicated here.

# deduplicate articles without doi using the PMID (found within all articles with or without doi)
data_2016_2017_noDOI_pmid_no_dup <- data_2016_2017_doi_dedup %>%
  filter(!doi_existent & pmid != 0) %>%
  distinct(pmid, .keep_all = TRUE) %>%
  filter(!pmid %in% (data_2016_2017_doi_dedup %>% filter(doi_existent) %>% pull(pmid)))

# re-combine articles with dois with the deduplicated articles without doi
data_2016_2017_no_pmid_dups <- data_2016_2017_doi_dedup %>%
  filter(!(!doi_existent & pmid != 0)) %>% # inverted condition of data_2016_2017_noDOI_pmid_no_dup
  rbind(data_2016_2017_noDOI_pmid_no_dup)

# remove articles with duplicate WOS Accession Number
data_2016_2017_no_dups <- data_2016_2017_no_pmid_dups %>%
  filter(datenbank == "WOS") %>%
  distinct(identifier, .keep_all = TRUE) %>%
  rbind(data_2016_2017_no_pmid_dups %>% filter(datenbank != "WOS"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean column names ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2017 <- data_2016_2017_no_dups %>%
  mutate(corresponding_author = NA,     # add columns for rbind with other years
         accession_number_embase = NA,
         accession_number_wos = NA,
         origin_dataset = "2016_2017") %>% # new column mainly for hierarchy during later deduplication and for join with Unpaywall data
  arrange(desc(doi_existent)) %>%
  select(doi,
         doi_existent,
         accession_number_embase,
         accession_number_wos,
         titel,
         zeitschrift = source,
         corresponding_author,
         issn,
         e_issn,
         jahr = publ_year,
         pmid,
         publisher,
         author_address = aut_affil,
         document_type = doc_type,
         e_mail_address = email_corr_author,
         open_access_indicator = oa,
         reprint_address = corresp_author,
         corresponding_author_cha,
         origin_dataset)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data 2018–2020 ----
## Load data for 2018–2020 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2018_2020_file <- "raw_data/2018-2020.xlsx"

data_2018_2020_raw <- read_excel(data_2018_2020_file,
                       sheet = "Worksheet",
                       guess_max = 20000) # extend to get correct column type also for accession number cols

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean data, create some new variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2018_2020_clean <- data_2018_2020_raw %>%
  clean_names() %>%
  rename(reprint_address = reprint_address_gelb_sind_korrespondenzautoren_der_charite,
         pmid = pub_med_id,
         accession_number_wos = accession_number_wo_s) %>%
  select(-autor_en, -oa_status)

# deduplicate dois (prefer WoS entries), but keep all articles without doi
data_2018_2020_doi_dedup <- data_2018_2020_clean %>%
  mutate(doi = tolower(doi)) %>%
  mutate(doi_existent = !str_detect(doi, "keine doi"), .after = "doi") %>% # new column stating existence of doi
  mutate(doi = if_else(!doi_existent,
                       paste0("noDOI!!", replicate(n(), UUIDgenerate(n=1L, output = "string"))), doi)) %>% # Assign ids to articles without DOI
  arrange(desc(doi_existent), desc(datenbank)) %>% # sort by database in descending order, so that WoS entries are first
  distinct(doi, .keep_all = TRUE) # Remove duplicate dois. Articles without DOI not deduplicated here.

# deduplicate articles without (!) doi using the PMID (found within all articles with or without doi)
data_2018_2020_noDOI_pmid_no_dup <- data_2018_2020_doi_dedup %>%
  filter(!doi_existent & !is.na(pmid)) %>%
  distinct(pmid, .keep_all = TRUE) %>%
  filter(!pmid %in% (data_2018_2020_doi_dedup %>% filter(doi_existent) %>% pull(pmid)))

# re-combine articles with dois with the deduplicated articles without doi
data_2018_2020_no_pmid_dups <- data_2018_2020_doi_dedup %>%
  filter(!(!doi_existent & !is.na(pmid))) %>%  # inverted condition of data_2018_2020_noDOI_pmid_no_dup
  rbind(data_2018_2020_noDOI_pmid_no_dup)

# remove articles with duplicate WOS Accession Number
data_2018_2020_no_wos_dups <- data_2018_2020_no_pmid_dups %>%
  arrange(desc(doi_existent)) %>% # sort by doi_existent to prefer article entries with doi #FIXME: refactor cause we only want to consider articles without doi for deduplication based on other identifiers
  filter(!is.na(accession_number_wos)) %>%
  distinct(accession_number_wos, .keep_all = TRUE) %>%
  rbind(data_2018_2020_no_pmid_dups %>% filter(is.na(accession_number_wos)))

# remove articles with duplicate EMBASE Accession Number
data_2018_2020_no_dups <- data_2018_2020_no_wos_dups %>%
  arrange(desc(doi_existent)) %>% # sort by doi_existent to prefer article entries with doi #FIXME: refactor cause we only want to consider articles without doi for deduplication based on other identifiers
  filter(!is.na(accession_number_embase)) %>%
  distinct(accession_number_embase, .keep_all = TRUE) %>%
  rbind(data_2018_2020_no_wos_dups %>% filter(is.na(accession_number_embase)))

data_2018_2020 <- data_2018_2020_no_dups %>%
  mutate(origin_dataset = "2018_2020") %>% # new column mainly for hierarchy during later deduplication and for join with Unpaywall data
  arrange(desc(doi_existent)) %>%
  select(doi,
         doi_existent,
         accession_number_embase,
         accession_number_wos,
         titel,
         zeitschrift,
         corresponding_author,
         issn,
         e_issn,
         jahr,
         pmid,
         publisher = verlag,
         author_address,
         document_type,
         e_mail_address,
         open_access_indicator,
         reprint_address,
         corresponding_author_cha,
         origin_dataset)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Combine dataframes of 2016-2017 data and 2018-2020 data with rbind ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2020 <- rbind(data_2016_2017, data_2018_2020) %>%
  distinct(doi, .keep_all = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Delete 71 datasets without Charité affiliation ----
#+ (corrects some WoS assignments)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrong_dois_input <- "raw_data/falsche_dois_wos_2016_2020.xls" # articles without Charité affiliation (incorrect assignment in WoS data)

wrong_dois <- read_excel(wrong_dois_input) %>%
  clean_names() %>%
  select(doi) %>%
  mutate(doi = str_to_lower(doi)) %>%
  pull(doi)

data_2016_2020 <- data_2016_2020 %>%
  filter(!doi %in% wrong_dois)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Data 2021 ----
##+ Load 2021 data ----
##+ (containing Unpaywall data retrieved September 2022)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

publications_charite_2016_2021_final <- "raw_data/2021.xlsx"

data_2021_raw <- read_excel(publications_charite_2016_2021_final,
                            sheet = "2021")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##+ Clean and enrich 2021 data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2021_clean <- data_2021_raw %>%
  clean_names() %>%
  rename(pmid = pub_med_id,
         reprint_address = reprint_address_gelb_sind_korrespondenzautoren_der_charite) %>%
  mutate(accession_number_embase = NA, # add columns for rbind with other years
         accession_number_wos = NA)

# deduplicate dois, but keep all articles without doi
data_2021_doi_dedup <- data_2021_clean %>%
  mutate(doi = tolower(doi)) %>%
  mutate(doi_existent = (doi != "keine doi")) %>% # new column stating existence of doi
  mutate(doi = if_else(!doi_existent,
                       paste0("noDOI!!", replicate(n(), UUIDgenerate(n=1L, output = "string"))), doi)) %>% # Assign ids to articles without DOI
  distinct(doi, .keep_all = TRUE)

# deduplicate articles without (!) doi using the PMID (found within all articles with or without doi)
data_2021_noDOI_pmid_no_dup <- data_2021_doi_dedup %>%
  filter(!doi_existent & !is.na(pmid)) %>%
  distinct(pmid, .keep_all = TRUE) %>%
  filter(!pmid %in% (data_2021_doi_dedup %>% filter(doi_existent) %>% pull(pmid)))

# re-combine articles with dois with the deduplicated articles without doi
data_2021_no_pmid_dups <- data_2021_doi_dedup %>%
  filter(!(!doi_existent & !is.na(pmid))) %>%  # inverted condition of data_2018_2020_noDOI_pmid_no_dup
  rbind(data_2021_noDOI_pmid_no_dup)

data_2021 <- data_2021_no_pmid_dups %>%
  arrange(desc(doi_existent)) %>%
  mutate(origin_dataset = "2021") %>% # new column mainly for hierarchy during later deduplication
  select(doi,
         doi_existent,
         accession_number_embase,
         accession_number_wos,
         titel,
         zeitschrift,
         corresponding_author,
         issn,
         e_issn,
         jahr,
         pmid,
         publisher = verlag,
         author_address,
         document_type,
         e_mail_address,
         open_access_indicator,
         reprint_address,
         corresponding_author_cha,
         origin_dataset)

#TODO: deduplicate for wos and embase accession number; data currently not included in 2021 raw data

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add 2021 data to existing data with rbind ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2021 <- rbind(data_2016_2020, data_2021)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Data 2022 ----
##+ Load 2022 data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2022_file <- "raw_data/2022.xlsx"

data_2022_raw <- read_excel(data_2022_file,
                            sheet = "Merge")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##+ Clean and enrich 2022 data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2022_clean <- data_2022_raw %>%
  clean_names() %>%
  rename(pmid = pubmed_id,
         reprint_address = reprint_addresses,
         author_address = addresses,
         corresponding_author_cha = ca,
         e_mail_address = email_addresses,
         open_access_indicator = open_access_designations,
         accession_number_embase = unique_identifier_emb,
         accession_number_wos = ut_unique_wos_id) %>%
  mutate(jahr = 2022,
         corresponding_author = reprint_address) %>%    # TODO: corresponding_author and reprint_address are the same: check in previous years
  mutate(corresponding_author_cha = case_when(corresponding_author_cha == "ja" ~ TRUE,
                                              TRUE ~ FALSE)) %>%
  mutate(datenbank = case_when(str_detect(accession_number_wos, "WOS:") ~ "WOS",
                               str_detect(accession_number_wos, "Embase") ~ "Embase")) %>%
  mutate(accession_number_wos = if_else(datenbank == "WOS", accession_number_wos, NA_character_)) %>%
  filter(datenbank != 0) # only keep entries from those databases

# deduplicate dois, but keep all articles without doi
data_2022_doi_dedup <- data_2022_clean %>%
  mutate(doi = tolower(doi)) %>%
  mutate(doi_existent = (doi != "keine doi")) %>% # new column stating existence of doi
  mutate(doi = if_else(!doi_existent,
                       paste0("noDOI!!", replicate(n(), UUIDgenerate(n=1L, output = "string"))), doi)) %>% # Assign ids to articles without DOI
  distinct(doi, .keep_all = TRUE)

# deduplicate articles without (!) doi using the PMID (found within all articles with or without doi)
data_2022_noDOI_pmid_no_dup <- data_2022_doi_dedup %>%
  filter(!doi_existent & !is.na(pmid)) %>%
  distinct(pmid, .keep_all = TRUE) %>%
  filter(!pmid %in% (data_2022_doi_dedup %>% filter(doi_existent) %>% pull(pmid)))

# re-combine articles with dois with the deduplicated articles without doi
data_2022_no_pmid_dups <- data_2022_doi_dedup %>%
  filter(!(!doi_existent & !is.na(pmid))) %>%  # inverted condition of data_2018_2020_noDOI_pmid_no_dup
  rbind(data_2022_noDOI_pmid_no_dup)

# remove articles with duplicate WOS Accession Number
data_2022_no_wos_dups <- data_2022_no_pmid_dups %>%
  arrange(desc(doi_existent)) %>% # sort by doi_existent to prefer article entries with doi #FIXME: refactor cause we only want to consider articles without doi for deduplication based on other identifiers
  filter(!is.na(accession_number_wos)) %>%
  distinct(accession_number_wos, .keep_all = TRUE) %>%
  rbind(data_2022_no_pmid_dups %>% filter(is.na(accession_number_wos)))

# remove articles with duplicate EMBASE Accession Number
data_2022_no_dups <- data_2022_no_wos_dups %>%
  arrange(desc(doi_existent)) %>% # sort by doi_existent to prefer article entries with doi #FIXME: refactor cause we only want to consider articles without doi for deduplication based on other identifiers
  filter(!is.na(accession_number_embase)) %>%
  distinct(accession_number_embase, .keep_all = TRUE) %>%
  rbind(data_2022_no_wos_dups %>% filter(is.na(accession_number_embase)))

data_2022 <- data_2022_no_dups %>%
  arrange(desc(doi_existent)) %>%
  mutate(origin_dataset = "2022") %>% # new column mainly for hierarchy during later deduplication
  select(doi,
         doi_existent,
         accession_number_embase,
         accession_number_wos,
         titel = article_title,
         zeitschrift = source_title,
         corresponding_author,
         issn,
         e_issn,
         jahr,
         pmid,
         publisher,
         author_address,
         document_type,
         e_mail_address,
         open_access_indicator,
         reprint_address,
         corresponding_author_cha,
         origin_dataset)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add 2022 data to existing data with rbind ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2022 <- rbind(data_2016_2020, data_2021, data_2022)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# All years combined ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Deduplicate: prefer data from previous years over newer data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Test: finding duplicate dois
# test <- data %>%
#   group_by(doi) %>%
#   summarise(n = n()) %>%
#   filter(n>1) %>%
#   select(doi)

data_clean <- data_2016_2022 %>%
  filter(jahr != 0) %>%
  mutate(pmid = na_if(pmid, 0)) %>%
  mutate(origin_dataset = factor(origin_dataset, levels = input_dataset_levels, ordered = TRUE)) %>%
  relocate(pmid, .after = doi_existent) %>%
  relocate(origin_dataset, .before = titel)

# deduplicate dois
data_doi_dedup <- data_clean %>%
  distinct(doi, .keep_all = TRUE)

# deduplicate articles without doi using the PMID (found within all articles with or without doi)
data_noDOI_pmid_no_dup <- data_doi_dedup %>%
  filter(!doi_existent & !is.na(pmid)) %>%
  arrange(origin_dataset) %>%     # to prefer older records before newer records
  distinct(pmid, .keep_all = TRUE) %>%
  filter(!pmid %in% (data_doi_dedup %>% filter(doi_existent) %>% pull(pmid)))

# re-combine articles with dois with the deduplicated articles without doi
data_no_pmid_dups <- data_doi_dedup %>%
  filter(!(!doi_existent & !is.na(pmid))) %>% # inverted condition of data_noDOI_pmid_no_dup
  rbind(data_noDOI_pmid_no_dup)

# deduplicate articles without doi using the WOS Accession Number (found within all articles with or without doi)
data_noDOI_wos_no_dup <- data_no_pmid_dups %>%
  filter(!doi_existent & !is.na(accession_number_wos)) %>%
  arrange(origin_dataset) %>%     # to prefer older records before newer records
  distinct(accession_number_wos, .keep_all = TRUE) %>%
  filter(!accession_number_wos %in% (data_no_pmid_dups %>% filter(doi_existent) %>% pull(accession_number_wos)))

# re-combine articles with dois with the deduplicated articles without doi
data_no_wos_dups <- data_no_pmid_dups %>%
  filter(!(!doi_existent & !is.na(accession_number_wos))) %>% # inverted condition of data_noDOI_wos_no_dup
  rbind(data_noDOI_wos_no_dup)

# deduplicate articles without doi using the Embase Accession Number (found within all articles with or without doi)
data_noDOI_embase_no_dup <- data_no_wos_dups %>%
  filter(!doi_existent & !is.na(accession_number_embase)) %>%
  arrange(origin_dataset) %>%     # to prefer older records before newer records
  distinct(accession_number_embase, .keep_all = TRUE) %>%
  filter(!accession_number_embase %in% (data_no_wos_dups %>% filter(doi_existent) %>% pull(accession_number_embase)))

# re-combine articles with dois with the deduplicated articles without doi
data_no_embase_dups <- data_no_wos_dups %>%
  filter(!(!doi_existent & !is.na(accession_number_embase))) %>% # inverted condition of data_noDOI_embase_no_dup
  rbind(data_noDOI_embase_no_dup)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Add Unpaywall data ----
## Also join by origin_unpaywall to use its data of the request date
## specifically stated for each year's dataset
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data <- data_no_embase_dups %>%
  mutate(doi = tolower(doi)) %>%
  mutate(origin_for_unpw_join = case_when(origin_dataset == "2016_2017" ~ "2016_2020",
                                          origin_dataset == "2018_2020" ~ "2016_2020",
                                          TRUE ~ as.character(origin_dataset))) %>%
  left_join(unpaywall_2016_2022_slim, by = c("doi", "origin_for_unpw_join" = "origin_unpaywall")) %>%
  rename(origin_unpaywall = origin_for_unpw_join) %>%
  mutate(license = replace_na(license, "no license found")) %>%
  mutate(oa_status_new = replace_na(oa_status_new, "no result")) %>%
  mutate(oa_status = oa_status_new) %>%
  select(-oa_status_new) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa_new = replace_na(is_oa_new, FALSE)) %>%
  mutate(is_oa = is_oa_new) %>%
  select(-is_oa_new)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Write data to xlsx ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_for_xlsx <- data %>%
  mutate(author_address = str_trunc(author_address, 32767, side = "right")) %>% # Truncate author_address to Excel's limit of 32,767 characters
  select(doi,
         titel,
         zeitschrift,
         corresponding_author,
         issn,
         e_issn,
         jahr,
         pmid,
         accession_number_embase,
         accession_number_wos,
         publisher,
         author_address,
         document_type,
         e_mail_address,
         open_access_indicator,
         reprint_address,
         corresponding_author_cha,
         is_oa,
         oa_status,
         license)

# write_xlsx(data_for_xlsx, "data/publications_charite_2016-2021.xlsx")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exploratory data analysis ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#sapply(data, function(x) length(unique(x)))
#sort(table(data$document_type), decreasing = TRUE)
#n_occur <- data.frame(table(data$titel))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of year and oa_status of contributing authors ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data %>%
  ggplot(aes(x = jahr, fill = oa_status)) +
  geom_bar()

summary_data_2017_2022 <- data %>%
  filter(jahr %in% c(2017, 2018, 2019, 2020, 2021, 2022)) %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n()) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

status_absolute <-
  hchart(summary_data_2017_2022,
         "column",
         hcaes(x = jahr, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(reversedStacks = FALSE, labels = list(format = '{value:.0f}')) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

status_absolute_spline <-
  summary_data_2017_2022 %>%
  mutate(jahr = factor(jahr)) %>%
  hchart("spline",
         hcaes(x = jahr, y = value, group = oa_status)) %>%
  hc_colors(color) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Number")) %>%
  hc_exporting(
    enabled = TRUE,
    # always enabled
    filename = "status_absolute_spline",
    buttons = list(contextButton = list(
      menuItems = c('downloadJPEG', 'separator', 'downloadCSV')
    ))
  )

# saveWidget(status_absolute, file = "status_absolute.html") # , selfcontained = TRUE
# %>% hc_title(text = "Open access status in absolute numbers", align = "left", style = list(fontSize = "12px"))
# %>% hc_subtitle(text = text, align = "left", style = list(fontSize = "12px"))

status_percent <-
  hchart(summary_data_2017_2022,
         "column",
         hcaes(x = jahr, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Open access status"),
           labels = list(format = '{value} %'),
           max = 100, reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_percent",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

# reversed bar stacks https://www.highcharts.com/forum/viewtopic.php?t=10916

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of year and oa_status corresponding authors ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary_corresponding_2017_2022 <- data %>%
  filter(corresponding_author_cha == TRUE) %>%
  filter(jahr %in% c(2017, 2018, 2019, 2020, 2021, 2022)) %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n()) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

status_corresponding_absolute <-
  hchart(
    summary_corresponding_2017_2022,
    "column",
    hcaes(x = jahr, y = value, group = oa_status)
  ) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(reversedStacks = FALSE) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_corresponding_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

status_corresponding_absolute_spline <-
  summary_corresponding_2017_2022 %>%
  mutate(jahr = factor(jahr)) %>%
  hchart("spline",
         hcaes(x = jahr, y = value, group = oa_status)) %>%
  hc_colors(color) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Number")) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_corresponding_absolute_spline",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

status_corresponding_percent <-
  hchart(
    summary_corresponding_2017_2022,
    "column",
    hcaes(x = jahr, y = percent, group = oa_status)
  ) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Open access status"),
           labels = list(format = '{value} %'),
           max = 100, reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_corresponding_percent",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of journals and oa_status ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2020 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary_journal_2020 <- data %>%
  filter(jahr == 2020) %>%
  group_by(zeitschrift, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_zs = sum(value)) %>%
  ungroup() %>%
  filter(value_zs >= 5) %>%
  mutate(zeitschrift = forcats::fct_reorder(zeitschrift, -value_zs))

summary_journal_2020_2 <- summary_journal_2020 %>%
  group_by(zeitschrift) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

journal_2020_absolute <- summary_journal_2020_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_xAxis(title = list(text = "Journal")) %>%
  hc_yAxis(title = list(text = "Number"),
           reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_zs} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "journal_2020_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

journal_2020_percent <- summary_journal_2020_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(labels = list(format = '{value} %'),
                         max = 100, reversedStacks = FALSE) %>%
  hc_size(height = 500) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_zs} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "journal_2020_percent",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2021 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary_journal_2021 <- data %>%
  filter(jahr == 2021) %>%
  group_by(zeitschrift, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_zs = sum(value)) %>%
  ungroup() %>%
  filter(value_zs >= 5) %>%
  mutate(zeitschrift = forcats::fct_reorder(zeitschrift, -value_zs))

summary_journal_2021_2 <- summary_journal_2021 %>%
  group_by(zeitschrift) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

journal_2021_absolute <- summary_journal_2021_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_xAxis(title = list(text = "Journal")) %>%
  hc_yAxis(title = list(text = "Number"),
           reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_zs} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "journal_2021_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

journal_2021_percent <- summary_journal_2021_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100, reversedStacks = FALSE) %>%
  hc_size(height = 500) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_zs} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "journal_2021_percent",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2022 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary_journal_2022 <- data %>%
  filter(jahr == 2022) %>%
  group_by(zeitschrift, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_zs = sum(value)) %>%
  ungroup() %>%
  filter(value_zs >= 5) %>%
  mutate(zeitschrift = forcats::fct_reorder(zeitschrift, -value_zs))

summary_journal_2022_2 <- summary_journal_2022 %>%
  group_by(zeitschrift) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

journal_2022_absolute <- summary_journal_2022_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_xAxis(title = list(text = "Journal")) %>%
  hc_yAxis(title = list(text = "Number"),
           reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_zs} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "journal_2022_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

journal_2022_percent <- summary_journal_2022_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100, reversedStacks = FALSE) %>%
  hc_size(height = 500) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_zs} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "journal_2022_percent",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of publishers (from Unpaywall) and oa_status ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2020 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_publisher_2020 <- data %>%
  filter(jahr == 2020) %>%
  select(doi,
         oa_status,
         publisher = unpw_publisher)

summary_publisher_2020 <- data_publisher_2020 %>%
  group_by(publisher, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_pub = sum(value)) %>%
  ungroup() %>%
 # filter(value_pub >= 5) %>%
  mutate(publisher = forcats::fct_reorder(publisher, -value_pub))

summary_publisher_2020_2 <- summary_publisher_2020 %>%
  group_by(publisher) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  filter(value_pub >= 3) %>%
  drop_na()

table_publishers_2020 <- summary_publisher_2020_2 %>%
  select(-value_pub, -percent) %>%
  pivot_wider(names_from = oa_status, values_from = value) %>%
  select(-`no result`)

publisher_2020_absolute <- summary_publisher_2020_2 %>%
  hchart("bar", hcaes(x = publisher, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_xAxis(title = list(text = "Publisher")) %>%
  hc_yAxis(title = list(text = "Number"),
           reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_pub} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_2020_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

pal <- got(4, direction = 1, option = "Jon_Snow")

publisher_donut <- data_publisher_2020 %>%
  group_by(publisher) %>%
  summarise(value = n()) %>%
  mutate(publisher_2 = if_else(value <= 500, "other Publishers", publisher)) %>%
  group_by(publisher_2) %>%
  summarise(value = sum(value)) %>%
  mutate(perc = round(value / sum(value) * 100, 1)) %>%
  arrange(value) %>%
  hchart("pie",
         hcaes(x = publisher_2, y = value),
         size = "65%",
         innerSize = "50%") %>%
  hc_colors(pal) %>%
  hc_tooltip(pointFormat = "{point.value} Artikel ({point.perc} %)") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_donut",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2021 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_publisher_2021 <- data %>%
  filter(jahr == 2021) %>%
  select(doi,
         oa_status,
         publisher = unpw_publisher)

summary_publisher_2021 <- data_publisher_2021 %>%
  group_by(publisher, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_pub = sum(value)) %>%
  ungroup() %>%
  # filter(value_pub >= 5) %>%
  mutate(publisher = forcats::fct_reorder(publisher, -value_pub))

summary_publisher_2021_2 <- summary_publisher_2021 %>%
  group_by(publisher) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  filter(value_pub >= 3) %>%
  drop_na()

table_publishers_2021 <- summary_publisher_2021_2 %>%
  select(-value_pub, -percent) %>%
  pivot_wider(names_from = oa_status, values_from = value) %>%
  select(-`no result`)

publisher_2021_absolute <- summary_publisher_2021_2 %>%
  hchart("bar", hcaes(x = publisher, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_xAxis(title = list(text = "Publisher")) %>%
  hc_yAxis(title = list(text = "Number"),
           reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_pub} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_2021_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2022 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_publisher_2022 <- data %>%
  filter(jahr == 2022) %>%
  select(doi,
         oa_status,
         publisher = unpw_publisher)

summary_publisher_2022 <- data_publisher_2022 %>%
  group_by(publisher, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_pub = sum(value)) %>%
  ungroup() %>%
  # filter(value_pub >= 5) %>%
  mutate(publisher = forcats::fct_reorder(publisher, -value_pub))

summary_publisher_2022_2 <- summary_publisher_2022 %>%
  group_by(publisher) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  filter(value_pub >= 3) %>%
  drop_na()

table_publishers_2022 <- summary_publisher_2022_2 %>%
  select(-value_pub, -percent) %>%
  pivot_wider(names_from = oa_status, values_from = value) %>%
  select(-`no result`)

publisher_2022_absolute <- summary_publisher_2022_2 %>%
  hchart("bar", hcaes(x = publisher, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_xAxis(title = list(text = "Publisher")) %>%
  hc_yAxis(title = list(text = "Number"),
           reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} articles ({point.percent} %)<br>{point.value_pub} total articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_2022_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

pal <- got(4, direction = 1, option = "Jon_Snow")

publisher_donut <- data_publisher_2022 %>%
  group_by(publisher) %>%
  summarise(value = n()) %>%
  mutate(publisher_2 = if_else(value <= 500, "other Publishers", publisher)) %>%
  group_by(publisher_2) %>%
  summarise(value = sum(value)) %>%
  mutate(perc = round(value / sum(value) * 100, 1)) %>%
  arrange(value) %>%
  hchart("pie",
         hcaes(x = publisher_2, y = value),
         size = "65%",
         innerSize = "50%") %>%
  hc_colors(pal) %>%
  hc_tooltip(pointFormat = "{point.value} Artikel ({point.perc} %)") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_donut",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data of OA costs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

oa_costs <- "raw_data/oa_costs.xlsx"

data_costs <- read_excel(oa_costs,
                         sheet = "Sheet1")

data_costs_long <- data_costs %>%
  select(-total) %>%
  pivot_longer(cols = c("2018", "2019", "2020", "2021"), names_to = "year")

# pal <- got(3, direction = 1, option = "Jon_Snow")
 pal <- c("#858688", "#B2C1DD", "#004ecc")
# pal <- c("#8797AE", "#B2C1DD", "#2C74B4")


publisher_costs <-
  data_costs_long %>%
  hchart("column",
         hcaes(x = publisher, y = value, group = year)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(title = list(text = "Publisher")) %>%
  hc_yAxis(title = list(text = "Funding amount"),
           labels = list(format = '{value:,0f} €'), reversedStacks = FALSE) %>%
  hc_colors(pal) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_costs",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#pal <- got(10, direction = 1, option = "Daenerys")
#pal <- topo.colors(n=10)
#library("viridis")
# pal <- viridis(n=10, direction = 1, option = "D")
#rainbow(n=7)

# pal <- colorRampPalette(c("#8797AE", "#B2C1DD", "#2C74B4"))
pal <- colorRampPalette(c("#858688", "#B2C1DD", "#004ecc"))
pal <- pal(10)

publisher_costs_year <-
  data_costs_long %>%
  hchart("bar",
         hcaes(x = year, y = value, group = publisher)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Funding amount"),
           labels = list(format = '{value:,0f} €'), reversedStacks = FALSE) %>%
  hc_colors(pal) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_costs",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exploratory data analysis of bih dataset ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

raw_data_publications_cha_dashboard <- "raw_data/publications_cha_dashboard.csv"
publications_cha_dashboard <- read_csv(raw_data_publications_cha_dashboard)

bih_data <- publications_cha_dashboard %>%
  filter(year >= 2018 & year <= 2020)

sapply(data, function(x) length(unique(x)))
sort(table(publications_cha_dashboard$OA_color), decreasing = TRUE)

n_occur <- data.frame(table(data$titel))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
