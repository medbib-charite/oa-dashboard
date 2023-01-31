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
# Clean 2016 and 2017 data, create some new variables ----
# Articles without doi will be deduplicated with PMID and WOS Number
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

# deduplicate dois, keep all articles without doi
data_2016_2017_doi_dedup <- data_2016_2017_raw %>%
  mutate(doi = tolower(doi)) %>%
  mutate(doi_existent = (doi != 0), .after = "doi") %>% # new column stating existence of doi
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
## Add oa status from unpaywall to data and clean column names ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("data/data_unpaywall.Rda") # Unpaywall data 2018-2020

data_unpaywall_2016_2017 <- data_unpaywall %>%
  distinct(doi, .keep_all = TRUE) %>%
  select(doi, oa_status)

data_2016_2017 <- left_join(data_2016_2017_no_dups, data_unpaywall_2016_2017, by = "doi") %>%
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  mutate(corresponding_author = NA,     # add columns for rbind with other years
         accession_number_embase = NA,
         accession_number_wos = NA) %>%
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
         oa_status,
         is_oa)


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
  mutate(oa_status = tolower(oa_status)) %>%
  mutate(oa_status = str_replace(oa_status, "kein ergebnis", "no result")) %>%
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  rename(reprint_address = reprint_address_gelb_sind_korrespondenzautoren_der_charite,
         pmid = pub_med_id,
         accession_number_wos = accession_number_wo_s) %>%
  select(!autor_en)

# deduplicate dois (prefer WoS entries), keep all articles without doi
data_2018_2020_doi_dedup <- data_2018_2020_clean %>%
  mutate(doi = tolower(doi)) %>%
  mutate(doi_existent = (doi != "keine doi"), .after = "doi") %>% # new column stating existence of doi # FIXME: use str_detect()!!
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
  arrange(desc(doi_existent)) %>% # sort by doi_existent to prefer article entries with doi
  filter(!is.na(accession_number_wos)) %>%
  distinct(accession_number_wos, .keep_all = TRUE) %>%
  rbind(data_2018_2020_no_pmid_dups %>% filter(is.na(accession_number_wos)))

# remove articles with duplicate EMBASE Accession Number
data_2018_2020_no_dups <- data_2018_2020_no_wos_dups %>%
  arrange(desc(doi_existent)) %>% # sort by doi_existent to prefer article entries with doi
  filter(!is.na(accession_number_embase)) %>%
  distinct(accession_number_embase, .keep_all = TRUE) %>%
  rbind(data_2018_2020_no_wos_dups %>% filter(is.na(accession_number_embase)))

data_2018_2020 <- data_2018_2020_no_dups %>%
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
         oa_status,
         is_oa)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine dataframes of 2016-2017 data and 2018-2020 data with rbind ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2020 <- rbind(data_2016_2017, data_2018_2020) %>%
  distinct(doi, .keep_all = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# New manipulation 2021-12-01; change oa_status to green for bronze articles
# with repository copy, delete 71 false datasets ----                           # FIXME: update or remove number of false datasets (71 before the 2021 articles)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_unpaywall_oa_status <- data_unpaywall %>%  # Unpaywall data 2018-2020
  distinct(doi, .keep_all = TRUE) %>%
  select(doi, oa_status, has_repository_copy) %>%                               # TODO: could line be removed as unnecessary? will be selected later anyway
  mutate(oa_status_new = case_when(oa_status == "bronze" & has_repository_copy == TRUE ~ "green",
                                   TRUE ~ oa_status)) %>%     # TODO: warum case_when und nicht: if_else(oa_status == "bronze" & has_repository_copy == TRUE, "green", oa_status)
  select(doi, oa_status_new)

# Control test (column oa_status in data_unpaywall_oa_status needed)
# test <- data_unpaywall_oa_status %>%
#   filter(oa_status != oa_status_new)


data_2016_2020 <- data_2016_2020 %>% left_join(data_unpaywall_oa_status, by = "doi") %>%
  select(!oa_status) %>%
  rename(oa_status = oa_status_new) %>%
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = case_when(oa_status == "green" ~ TRUE,
                           TRUE ~ is_oa))     # TODO: warum nicht: if_else(oa_status == "green", TRUE, is_oa)

# Control test
# test <- data %>%
#   filter(is_oa == FALSE & oa_status == "green")

wrong_dois_input <- "raw_data/falsche_dois_wos_2016_2020.xls" # articles without Charité affiliation (incorrect assignment in WoS data)

wrong_dois <- read_excel(wrong_dois_input) %>%
  clean_names() %>%
  select(doi) %>%
  mutate(doi = str_to_lower(doi)) %>%
  pull(doi)

data_2016_2020 <- data_2016_2020 %>%
  filter(!doi %in% wrong_dois)

# write_xlsx(data, "data/publications_charite_2016-2020.xlsx")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data 2021 ----
## Load 2021 data (containing unpaywall data, retrieved September 2022 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

publications_charite_2016_2021_final <- "raw_data/publications_charite_2016-2021_final.xlsx"

data_2021_raw <- read_excel(publications_charite_2016_2021_final,
                            sheet = "2021")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean and enrich 2021 data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2021_clean <- data_2021_raw %>%
  clean_names() %>%
  mutate(oa_status = tolower(oa_status)) %>%
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  rename(pmid = pub_med_id,
         reprint_address = reprint_address_gelb_sind_korrespondenzautoren_der_charite) %>%
  mutate(accession_number_embase = NA, # add columns for rbind with other years
         accession_number_wos = NA)

# deduplicate dois, keep all articles without doi
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
         oa_status,
         is_oa)

#TODO: deduplicate for wos and embase accession number; data currently not included in 2021 raw data

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add 2021 data to existing data with rbind ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2021 <- rbind(data_2016_2020, data_2021)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Deduplicate dois: prefer data from previous years over newer data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Test: finding duplicate dois
# test <- data %>%
#   group_by(doi) %>%
#   summarise(n = n()) %>%
#   filter(n>1) %>%
#   select(doi)

data_clean <- data_2016_2021 %>%
  filter(jahr != 0) %>%
  distinct(doi, .keep_all = TRUE)
# FIXME: deduplicate also for other identifiers

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add additional unpaywall data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data <- data_clean %>%
  left_join(unpaywall_2016_2021_slim, by = "doi")

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

data_sum <- data %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n()) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

status_absolute <-
  hchart(data_sum,
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
  data_sum %>%
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
  hchart(data_sum,
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

data_corresponding_sum <- data %>%
  filter(corresponding_author_cha == TRUE) %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n()) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

status_corresponding_absolute <-
  hchart(
    data_corresponding_sum,
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
  data_corresponding_sum %>%
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
    data_corresponding_sum,
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

journal_data <- data %>%
  filter(jahr == 2020) %>%
  group_by(zeitschrift, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_zs = sum(value)) %>%
  ungroup() %>%
  filter(value_zs >= 5) %>%
  mutate(zeitschrift = forcats::fct_reorder(zeitschrift, -value_zs))

journal_data_2 <- journal_data %>%
  group_by(zeitschrift) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

journal_absolute <- journal_data_2 %>%
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
    filename = "journal_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

journal_percent <- journal_data_2 %>%
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
    filename = "journal_percent",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of publishers and oa_status ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("data/data_unpaywall.Rda") # get publisher names from unpaywall dataset    # FIXME: use data for 2016-2021 (instead of data 2016-2020)

data_publisher <- data_unpaywall %>%
  select(doi, publisher)

data_publisher_join <- data %>%
  filter(jahr == 2020) %>%
  select(doi, oa_status) %>%
  left_join(data_publisher, by = "doi")

data_publisher_join_sum <- data_publisher_join %>%
  group_by(publisher, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_pub = sum(value)) %>%
  ungroup() %>%
 # filter(value_pub >= 5) %>%
  mutate(publisher = forcats::fct_reorder(publisher, -value_pub))

data_publisher_join_sum_2 <- data_publisher_join_sum %>%
  group_by(publisher) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  filter(value_pub >= 3) %>%
  drop_na()

data_publisher_table <- data_publisher_join_sum_2 %>%
  select(-value_pub, -percent) %>%
  pivot_wider(names_from = oa_status, values_from = value) %>%
  select(-`no result`)

publisher_absolute <- data_publisher_join_sum_2 %>%
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
    filename = "publisher_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

pal <- got(4, direction = 1, option = "Jon_Snow")

publisher_donut <- data_publisher_join %>%
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
  pivot_longer(cols = c("2018", "2019", "2020"), names_to = "year")

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
