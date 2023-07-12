#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Unpaywall api via roadoi ----
# jan.taubitz@charite.de - 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load sources and libraries ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("data_1.R", encoding = 'UTF-8')

library(roadoi)

data_doi <- data %>%
  filter(!str_detect(doi, "keine doi"))

#sample_data <- data_doi %>%
#  sample_n(size = 100)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data from Unpaywall ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html
# split the data for performance reasons

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
# # load("data/data_unpaywall_2020.Rda")
#
# warnings_2019 <- warnings()
# save(data_unpaywall_2019, file = "data/data_unpaywall_2019.Rda")
# save(warnings_2019, file = "data/warnings_2019.Rda")
# # load("data/data_unpaywall_2019.Rda")
#
# warnings_2018 <- warnings()
# save(data_unpaywall_2018, file = "data/data_unpaywall_2018.Rda")
# save(warnings_2018, file = "data/warnings_2018.Rda")
# # load("data/data_unpaywall_2018.Rda")
#
#
# data_doi_2017 <- data_2016_2017 %>%
#   filter(jahr == 2017) %>%
#   select(doi)
#
# data_unpaywall_2017 <- roadoi::oadoi_fetch(dois = data_doi_2017$doi,
#             email = "jan.taubitz@charite.de",
#             .progress = "text")
#
# warnings_2017 <- warnings()
# save(data_unpaywall_2017, file = "data/data_unpaywall_2017.Rda")
# save(warnings_2017, file = "data/warnings_2017.Rda")
# load("data/data_unpaywall_2017.Rda")
#
# data_doi_2016 <- data_2016_2017 %>%
#   filter(jahr == 2016) %>%
#   select(doi)
#
# data_unpaywall_2016 <- roadoi::oadoi_fetch(dois = data_doi_2016$doi,
#                                            email = "jan.taubitz@charite.de",
#                                            .progress = "text")
#
# warnings_2016 <- warnings()
# save(data_unpaywall_2016, file = "data/data_unpaywall_2016.Rda")
# save(warnings_2016, file = "data/warnings_2016.Rda")
# load("data/data_unpaywall_2016.Rda")
#
# data_unpaywall <- rbind(data_unpaywall_2016, data_unpaywall_2017, data_unpaywall_2018, data_unpaywall_2019, data_unpaywall_2020)
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
# Analyse Licences ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_license <- select(data, doi, license)

# UNUSED?
# data_license_final_count <- data_license %>%
#   group_by(license) %>%
#   summarise(count = n())

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add oa_status to data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_doi <- data %>%
  filter(doi_existent) %>%
  filter(jahr %in% c(2016, 2017, 2018, 2019, 2020, 2021, 2022))

# data_medbib_license <- data_doi %>%
#   select(doi, oa_status)
#
# data_license_oa_status_final <- data_medbib_license %>%
#   inner_join(data_license, by = "doi")

data_license_oa_status_final_count <- data_doi %>%
  group_by(license, oa_status) %>%
  summarise(count = n())

data_license_oa_status_final_count_2 <- data_license_oa_status_final_count %>%
  group_by(license) %>%
  spread(oa_status, count, fill = 0) %>% # to solve order problem
  gather(oa_status, count, -license) %>%
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
  hc_xAxis(title = list(text = "License")) %>%
  hc_yAxis(title = list(text = "Number"),
           labels = list(format = '{value:.0f}')) %>%
  hc_colors(color) %>%
  hc_tooltip(pointFormat = "{point.count} articles")  %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "chart_lizenzen",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

save(chart_lizenzen, file = "charts/chart_lizenzen.Rda")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualize licenses ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## only for 2017 ----

data_doi_2017 <- data %>%
  filter(doi_existent) %>%
  filter(jahr == 2017)

data_medbib_license_2017 <- data_doi_2017 %>%
  select(doi, oa_status)

data_license_oa_status_final_2017 <- data_medbib_license_2017 %>%
  inner_join(data_license, by = "doi")

data_license_oa_status_final_count_2017 <- data_license_oa_status_final_2017 %>%
  group_by(license, oa_status) %>%
  summarise(count = n())

data_license_oa_status_final_count_2_2017 <- data_license_oa_status_final_count_2017 %>%
  group_by(license) %>%
  spread(oa_status, count, fill = 0) %>% # to solve order problem
  gather(oa_status, count, -license) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  arrange(license)

save(data_license_oa_status_final_count_2_2017, file = "data/data_license_oa_status_final_count_2_2017.Rda")

chart_lizenzen_year_2017 <- data_license_oa_status_final_count_2_2017 %>%
  hchart("column",
         hcaes(x = license, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(title = list(text = "License")) %>%
  hc_yAxis(title = list(text = "Number"),
           labels = list(format = '{value:.0f}')) %>%
  hc_colors(color) %>%
  hc_tooltip(pointFormat = "{point.count} articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "chart_lizenzen_oa",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

save(chart_lizenzen_year_2017, file = "charts/chart_lizenzen_year_2017.Rda")

## only for 2022 ----

data_doi_2022 <- data %>%
  filter(doi_existent) %>%
  filter(jahr == 2022)

data_medbib_license_2022 <- data_doi_2022 %>%
  select(doi, oa_status)

data_license_oa_status_final_2022 <- data_medbib_license_2022 %>%
  inner_join(data_license, by = "doi")

data_license_oa_status_final_count_2022 <- data_license_oa_status_final_2022 %>%
  group_by(license, oa_status) %>%
  summarise(count = n())

data_license_oa_status_final_count_2_2022 <- data_license_oa_status_final_count_2022 %>%
  group_by(license) %>%
  spread(oa_status, count, fill = 0) %>% # to solve order problem
  gather(oa_status, count, -license) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  arrange(license)

save(data_license_oa_status_final_count_2_2022, file = "data/data_license_oa_status_final_count_2_2022.Rda")

chart_lizenzen_year_2022 <- data_license_oa_status_final_count_2_2022 %>%
  hchart("column",
         hcaes(x = license, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(title = list(text = "License")) %>%
  hc_yAxis(title = list(text = "Number"),
           labels = list(format = '{value:.0f}')) %>%
  hc_colors(color) %>%
  hc_tooltip(pointFormat = "{point.count} articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "chart_lizenzen_oa",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

save(chart_lizenzen_year_2022, file = "charts/chart_lizenzen_year_2022.Rda")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualize licenses only for OA status gold, green, hybrid ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

chart_lizenzen_oa <- data_license_oa_status_final_count_2 %>%
  filter(oa_status %in% c("gold", "hybrid", "green")) %>%
  hchart("column",
         hcaes(x = license, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(title = list(text = "License")) %>%
  hc_yAxis(title = list(text = "Number"),
           labels = list(format = '{value:.0f}')) %>%
  hc_colors(color) %>%
  hc_tooltip(pointFormat = "{point.count} articles") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "chart_lizenzen_oa",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

save(chart_lizenzen_oa, file = "charts/chart_lizenzen_oa.Rda")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

