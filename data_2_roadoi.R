#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Unpaywall api via roadoi ----
# jan.taubitz@charite.de - 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load sources and libraries ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("data_1.R", encoding = 'UTF-8')
source("datenvergleich.Rmd", encoding = 'UTF-8')

library(roadoi)

data_doi <- data %>%
  filter(!str_detect(doi, "keine doi"))

#sample_data <- data_doi %>%
#  sample_n(size = 100)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data from unpaywall ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html

# data_doi_2020 <- data_doi %>%
#   filter(jahr == 2020) %>%
#   select(doi)
#
# data_doi_2019 <- data_doi %>%
#   filter(jahr == 2019) %>%
#   select(doi)
#
# data_doi_2018 <- data_doi %>%
#   filter(jahr == 2018) %>%
#   select(doi)
#
# data_unpaywall_2018 <- roadoi::oadoi_fetch(dois = data_doi_2018$doi,
#             email = "jan.taubitz@charite.de",
#             .progress = "text")
#
# warnings_2020 <- warnings()
# save(data_unpaywall_2020, file = "data/data_unpaywall_2020.Rda")
# save(warnings_2020, file = "data/warnings_2020.Rda")
# # load("data_unpaywall_2020.Rda")
#
# warnings_2019 <- warnings()
# save(data_unpaywall_2019, file = "data/data_unpaywall_2019.Rda")
# save(warnings_2019, file = "data/warnings_2019.Rda")
# # load("data_unpaywall_2019.Rda")
#
# warnings_2018 <- warnings()
# save(data_unpaywall_2018, file = "data/data_unpaywall_2018.Rda")
# save(warnings_2018, file = "data/warnings_2018.Rda")
# # load("data/data_unpaywall_2019.Rda")
#
# data_unpaywall <- rbind(data_unpaywall_2018, data_unpaywall_2019, data_unpaywall_2020)
# save(data_unpaywall, file = "data/data_unpaywall.Rda")

load("data/data_unpaywall.Rda")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Exploratory data analysis ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# sort(table(data_unpaywall$publisher), decreasing = TRUE)
# sort(table(data_unpaywall$journal_is_in_doaj), decreasing = TRUE)
# sapply(data_unpaywall, function(x) length(unique(x)))

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Proxy settings ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# https://support.rstudio.com/hc/en-us/articles/200488488-Configuring-R-to-Use-an-HTTP-or-HTTPS-Proxy
# file.edit('~/.Renviron')
# http_proxy=http://proxy.charite.de:8080/
# https_proxy=http://proxy.charite.de:8080/
# 2x http://...

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Literature documentations ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html
# https://subugoe.github.io/scholcomm_analytics/posts/unpaywall_evidence/


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analysis ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Percent of article that were oa

# Percent of articles that were published in a doaj-journal

# Percent of article with Corresponding author

is_oa <- data %>%
  group_by(jahr, is_oa) %>%
  summarise(value = n()) %>%
  mutate(perc = round(value / sum(value) * 100, 1))

is_doaj <- sort(table(data_unpaywall$journal_is_in_doaj), decreasing = TRUE)
is_doaj
is_doaj <- round(4152/sum(is_doaj)*100, 1)

is_corresponding <- sort(table(data$corresponding_author_cha), decreasing = TRUE)
is_corresponding
is_corresponding <- round(6292/sum(is_corresponding)*100, 1)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse Licences ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_license_unnest <- data_unpaywall %>%
  # 1. Unnest the list structure
  unnest(oa_locations, keep_empty = TRUE) %>%
  # 2. Clean the license strings and create "others" category
  mutate(license = case_when(grepl("specific|cc0|implied|pd", license, ignore.case = TRUE) ~ "sonstige Lizenz",
                             TRUE ~ license)) %>%
  select(doi, license)

data_license_unnest_2 <- data_license_unnest %>%
  drop_na() %>%
  # 3. Keep only distinct doi-license rows
  distinct(doi, license, .keep_all = TRUE) %>%
  # 4. Create column with logical operator that shows dois > 1
  group_by(doi) %>%
  mutate(dupe = n() > 1) %>%
  filter(dupe == TRUE) %>%
  # 5. Select only rows with shortest (== best) license
  mutate(min = min(nchar(license))) %>%
  mutate(best = if_else(nchar(license) == min, TRUE, FALSE)) %>%
  filter(best == TRUE) %>%
  select(doi, license)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine lists ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_license_unnest_distinct <- data_license_unnest %>%
  distinct(doi, .keep_all = TRUE)

data_license_final <- rbind(data_license_unnest_2, data_license_unnest_distinct) %>%
  distinct(doi, .keep_all = TRUE) %>%
  mutate(license = replace_na(license, "kein Ergebnis")) %>%
  mutate(license = factor(license, levels = c("cc-by", "cc-by-nc", "cc-by-sa", "cc-by-nc-sa", "cc-by-nd", "cc-by-nc-nd", "sonstige Lizenz", "kein Ergebnis")))

data_license_final_count <- data_license_final %>%
  group_by(license) %>%
  summarise(count = n())

# https://stackoverflow.com/questions/6986657/find-duplicated-rows-based-on-2-columns-in-data-frame-in-r

# 740 Artikel haben mindestens zwei unterschiedliche Lizenzen

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add oa_status to data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_doi <- data %>%
  filter(!str_detect(doi, "keine doi"))

data_medbib_license <- data_doi %>%
  select(doi, oa_status)

data_license_oa_status_final <- data_medbib_license %>%
  inner_join(data_license_final, by = "doi")

data_license_oa_status_final_count <- data_license_oa_status_final %>%
  group_by(license, oa_status) %>%
  summarise(count = n())

data_license_oa_status_final_count_2 <- data_license_oa_status_final_count %>%
  group_by(license) %>%
  spread(oa_status, count, fill = 0) %>% # to solve order problem
  gather(oa_status, count, 2:7) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  arrange(license)

save(data_license_oa_status_final_count_2, file = "data/data_license_oa_status_final_count_2.Rda")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualize licenses ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

chart_lizenzen <- data_license_oa_status_final_count_2 %>%
  hchart("column",
         hcaes(x = license, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_yAxis(labels = list(format = '{value:.0f}')) %>%
  hc_colors(color) %>%
  hc_tooltip(pointFormat = "{point.count} Artikel")

save(chart_lizenzen, file = "charts/chart_lizenzen.Rda")

chart_lizenzen_oa <- data_license_oa_status_final_count_2 %>%
  filter(oa_status %in% c("gold", "hybrid", "green")) %>%
  hchart("column",
         hcaes(x = license, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_yAxis(labels = list(format = '{value:.0f}')) %>%
  hc_colors(color) %>%
  hc_tooltip(pointFormat = "{point.count} Artikel")

save(chart_lizenzen_oa, file = "charts/chart_lizenzen_oa.Rda")

oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed", "kein ergebnis")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E", "#5F7036")

color_treemap <- c("#2C405E", "#F4C244", "#D85DBF", "#A0CBDA", "#4FAC5B", "#5F7036")

hchart(
  data_to_hierarchical(
    data = data_license_oa_status_final_count_2,
    group_vars = c(oa_status, license),
    size_var = count,
    colors = color_treemap),
  type = "treemap",
  levelIsConstant = FALSE,
  animation = list(defer = 1000, duration = 0),
  allowDrillToNode = TRUE,
  #  allowTraversingTree = TRUE,
  layoutAlgorithm = "squarified",
  levels = list(
    list(
      level = 1,
      dataLabels = list(enabled = TRUE),
      borderWidth = 4,
      colorVariation = list(key = 'brightness', to = 0.2)
    ),
    list(
      level = 2,
      dataLabels = list(enabled = FALSE),
      borderWidth = 1,
      colorVariation = list(key = 'brightness', to = 0.2)
    )
  )
) %>%
  hc_title(text = "Lizenzen - Treemap Chart") %>%
  hc_subtitle(text = "Daten zu Lizenzen von Unpaywall")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

