#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Manipulation and Visualization of Medbib dataset ----
# jan.taubitz@charite.de - 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean the workspace ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
gc()

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Load libraries ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(gameofthrones)
library(highcharter)
library(htmlwidgets)
library(janitor)
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data for 2018–2020 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

endfassung <- "raw_data/Endfassung.xlsx"

raw_data <- read_excel(endfassung,
                       sheet = "Worksheet")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define color and oa-status variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed", "no result")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E", "#5F7036")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Clean data, create some new variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_clean <- raw_data %>%
  clean_names() %>%
  mutate(doi = tolower(doi),
         oa_status = tolower(oa_status)) %>% # Convert dois and oa_status to lower case
  distinct(doi, .keep_all = TRUE) %>%
  mutate(oa_status = str_replace(oa_status, "kein ergebnis", "no result")) %>%
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  mutate(corresponding_author_cha = if_else(row_number() %in% c(1:6292), TRUE, FALSE), .after = "is_oa") %>%   # FIXME: remove usage of row numbers, i.e. replace with filter using corr. author column in the relevant analyses (column not yet/originally not existent in 2018-2020 data but in 2021 data!)
  select(!c(datenbank, autor_en))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data for 2016 and 2017 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2017 <- "raw_data/2016-2018_merge charite_publikationen_article_review_wos_embase_corr_bih.xlsx"

total_2016 <- read_excel(data_2016_2017,
                         sheet = "merge_wos_embase_2016")

corr_2016 <- read_excel(data_2016_2017,
                        sheet = "merge_wos_embase_2016_corresp.")

total_2017 <- read_excel(data_2016_2017,
                         sheet = "merge_wos_embase_2017")

corr_2017 <- read_excel(data_2016_2017,
                        sheet = "merge_wos_embase_2017_corresp.")

total_2016_2017 <- rbind(total_2016, total_2017)

corr_2016_2017 <- rbind(corr_2016, corr_2017)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Clean 2016 and 2017 data, create some new variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

total_2016_2017_clean <- total_2016_2017 %>%
  clean_names() %>%
  mutate(doi = tolower(doi)) %>% # Convert dois to lower case
  distinct(doi, .keep_all = TRUE) %>%
  mutate(corresponding_author_cha = FALSE)

corr_2016_2017_clean <- corr_2016_2017 %>%
  clean_names() %>%
  mutate(doi = tolower(doi)) %>% # Convert dois to lower case
  distinct(doi, .keep_all = TRUE) %>%
  mutate(corresponding_author_cha = TRUE)

# combine data for corresponding and contributing authors
data_2016_2017_clean <- rbind(corr_2016_2017_clean, total_2016_2017_clean)

# deduplicate dois (corresponding authors before contributing authors)
data_2016_2017 <- data_2016_2017_clean %>%
  group_by(doi) %>%    # FIXME: couldn't this be done without grouping and slicing but with distinct() as this will preserve the *first* row anyway? The order of the datasets rbinded before is relevant in both cases
  slice(1) %>%
  filter(doi != 0) %>%
  rename(jahr = publ_year, zeitschrift = source) %>%
  ungroup()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add oa status from unpaywall to data and clean column names ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("data/data_unpaywall.Rda")

data_unpaywall_2016_2017 <- data_unpaywall %>%
  distinct(doi, .keep_all = TRUE) %>%
  select(doi, oa_status)

data_2016_2017_oa <- left_join(data_2016_2017, data_unpaywall_2016_2017, by = "doi") %>%
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  select(doi,
         titel,
         zeitschrift,
         corresponding_author = corresp_author,
         issn,
         e_issn,
         jahr,
         pub_med_id = pmid,
         verlag = publisher,
         author_address = aut_affil,
         document_type = doc_type,
         e_mail_address = email_corr_author,
         open_access_indicator = oa,
         reprint_address_gelb_sind_korrespondenzautoren_der_charite = corresp_author,
         oa_status,
         is_oa,
         corresponding_author_cha)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine dataframes of 2016-2017 data and 2018-2020 data with rbind ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2020 <- rbind(data_2016_2017_oa, data_clean) %>%
  distinct(doi, .keep_all = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# New manipulation 2021-12-01; change oa_status to green for bronze articles
# with repository copy, delete 71 false datasets ----                           # FIXME: update or remove number of false datasets (71 before the 2021 articles)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_unpaywall_oa_status <- data_unpaywall %>%
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
# Load 2021 data (containing unpaywall data, retrieved September 2022 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

raw_publications_charite_2016_2021_final <- "raw_data/publications_charite_2016-2021_final.xlsx"

raw_2021_data <- read_excel(raw_publications_charite_2016_2021_final,
                            sheet = "2021")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Clean and enrich 2021 data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2021 <- raw_2021_data %>%
  clean_names() %>%
  mutate(doi = tolower(doi),
         oa_status = tolower(oa_status)) %>%
  distinct(doi, .keep_all = TRUE) %>%       # FIXME: damit das ordnungsgemäß funktioniert, muss "keine doi" vorher differenziert worden sein, z.B. durch Ergänzen der PMID/WOS-Nummer/EMBASE-Nummer; temporär dafür Bearbeitung der doi-losen Artikel darunter eingefügt (data_2021_kein_doi)
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  select(doi,
         titel,
         zeitschrift,
         corresponding_author,
         issn,
         e_issn,
         jahr,
         pub_med_id,
         verlag,
         author_address,
         document_type,
         e_mail_address,
         open_access_indicator,
         reprint_address = reprint_address_gelb_sind_korrespondenzautoren_der_charite,
         oa_status,
         is_oa,
         corresponding_author_cha)


data_2021_kein_doi <- raw_2021_data %>%
  clean_names() %>%
  mutate(doi = tolower(doi),
         oa_status = tolower(oa_status)) %>%
  filter(doi == "keine doi") %>%
  mutate(oa_status = replace_na(oa_status, "no result")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  select(doi,
         titel,
         zeitschrift,
         corresponding_author,
         issn,
         e_issn,
         jahr,
         pub_med_id,
         verlag,
         author_address,
         document_type,
         e_mail_address,
         open_access_indicator,
         reprint_address = reprint_address_gelb_sind_korrespondenzautoren_der_charite,
         oa_status,
         is_oa,
         corresponding_author_cha)

data_2021 <- rbind(data_2021, data_2021_kein_doi) %>%
  distinct() # damit der erste "keine doi"-Artikel von distinct(doi, .keep_all = TRUE) nicht doppelt erscheint

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add 2021 data to existing data with rbind ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_2016_2020 <- data_2016_2020 %>%
  rename(reprint_address = reprint_address_gelb_sind_korrespondenzautoren_der_charite)
data <- rbind(data_2016_2020, data_2021)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Deduplicate dois: data from previous years before 2021 data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Test: finding duplicate dois
# test <- data %>%
#   group_by(doi) %>%
#   summarise(n = n()) %>%
#   filter(n>1) %>%
#   select(doi)

data <- data %>%
  distinct(doi, .keep_all = TRUE)   #FIXME: "keine doi" muss dafür erst noch spezifiziert werden, s.o.! Klären, ob Deduplizierung anhand der bereits in vorigen Jahren berücksichtigten DOIs erfolgen soll.


# Only for temporary purposes: overwrite data for creating an updated html with only 2016-2020 data
data <- data_2016_2020


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

load("data/data_unpaywall.Rda") # get publisher names from unpaywall dataset

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
