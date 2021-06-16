########################### Load sources ###########################

source("data_1.R", encoding = 'UTF-8')
#source("datenvergleich.Rmd", encoding = 'UTF-8')

library(rcrossref)

data_doi <- data %>%
  filter(!str_detect(doi, "keine DOI"))

sample_data <- data_doi %>%
  sample_n(size = 50) %>%
  mutate(doi = tolower(doi))

data_doi_2018 <- data_doi %>%
  filter(jahr == 2018) %>%
  mutate(doi = tolower(doi))

########################### Load data from crossref ###########################

# Documentation: https://docs.ropensci.org/rcrossref/articles/rcrossref.html

########################### Load citation data from crossref ###########################

#data_citation <- cr_citation_count(doi = data_doi_2018$doi)
#save(data_citation, file = "data/data_citation_2018.Rda")
load("data/data_citation_2018.Rda")

#sample_data <- cr_citation_count(doi = sample_data$doi)

########################### Combine data with medbib data ###########################

data_citation_join <- data_doi_2018 %>%
  select(doi, oa_status) %>%
  left_join(data_citation, by = "doi")

median_citation <-
  data_citation_join %>%
  group_by(oa_status) %>%
  summarise(median = median(count, na.rm = TRUE)) %>%
  mutate(median = replace_na(median, 0)) %>%
  hchart("column",
         hcaes(x = oa_status, y = median, color = color)) %>%
  hc_title(text = "Median") %>%
  hc_tooltip(pointFormat = "Der Median liegt bei {point.median} Zitierungen")

mean_citation <-
  data_citation_join %>%
  group_by(oa_status) %>%
  summarise(mean = mean(count, na.rm = TRUE)) %>%
  mutate(mean = replace_na(mean, 0)) %>%
  hchart("column",
         hcaes(x = oa_status, y = mean, color = color)) %>%
  hc_title(text = "Mittelwert") %>%
  hc_tooltip(pointFormat = "Der Mittelwert liegt bei {point.mean:.1f} Zitierungen")


#ggplot(data_citation_join, aes(oa_status, count)) +
#  geom_violin()

#ggplot(data_citation_join, aes(oa_status, count)) +
#  geom_boxplot() +
#  ylim(0,50)

#ggplot(data_citation_join, aes(oa_status, fill=count)) +
#  geom_bar()

