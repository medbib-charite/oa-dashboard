#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Crossref data ----
# jan.taubitz@charite.de - 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load sources and libraries ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("data_1.R", encoding = 'UTF-8')
#source("datenvergleich.Rmd", encoding = 'UTF-8')

library(rcrossref)

data_doi <- data %>%
  filter(!str_detect(doi, "keine doi"))

# sample_data <- data_doi %>%
#   sample_n(size = 10)

 data_doi_2018 <- data_doi %>%
   filter(jahr == 2018)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data from crossref ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Documentation: https://docs.ropensci.org/rcrossref/articles/rcrossref.html

#data_citation <- cr_citation_count(doi = data_doi_2018$doi)
#save(data_citation, file = "data/data_citation_2018.Rda")
load("data/data_citation_2018.Rda")

#sample_data <- cr_citation_count(doi = sample_data$doi)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data with medbib data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_citation_join <- data_doi_2018 %>%
  select(doi, oa_status) %>%
  left_join(data_citation, by = "doi")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualization ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

median_citation <-
  data_citation_join %>%
  group_by(oa_status) %>%
  summarise(median = median(count, na.rm = TRUE)) %>%
  mutate(median = replace_na(median, 0)) %>%
  hchart("column",
         hcaes(x = oa_status, y = median, color = color)) %>%
  hc_title(text = "Median") %>%
  hc_tooltip(pointFormat = "Der Median liegt bei {point.median} Zitierungen") %>%
   hc_exporting(
     enabled = TRUE, # always enabled
     filename = "median_citation",
     buttons = list(contextButton = list(menuItems = c('downloadPNG', 'downloadJPEG', 'separator', 'downloadCSV')))
   )

mean_citation <-
  data_citation_join %>%
  group_by(oa_status) %>%
  summarise(mean = mean(count, na.rm = TRUE)) %>%
  mutate(mean = replace_na(mean, 0)) %>%
  hchart("column",
         hcaes(x = oa_status, y = mean, color = color)) %>%
  hc_title(text = "Mittelwert") %>%
  hc_tooltip(pointFormat = "Der Mittelwert liegt bei {point.mean:.1f} Zitierungen") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "mean_citation",
    buttons = list(contextButton = list(menuItems = c('downloadPNG', 'downloadJPEG', 'separator', 'downloadCSV')))
  )


#ggplot(data_citation_join, aes(oa_status, count)) +
#  geom_violin()

#ggplot(data_citation_join, aes(oa_status, count)) +
#  geom_boxplot() +
#  ylim(0,50)

#ggplot(data_citation_join, aes(oa_status, fill=count)) +
#  geom_bar()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load license data from crossref ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Unpaywall data is better
# https://github.com/subugoe/metacheck/blob/main/R/metrics_cc.R


#req <- get_cr_md(sample_data$doi)

#cr_licenses(query = sample_data$doi)

#cr_citation_count(doi = sample_data$doi)

# out <- cr_compliance_overview(req)
