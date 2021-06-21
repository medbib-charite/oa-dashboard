########################### Load source ###########################

source("data_1.R", encoding = 'UTF-8')
source("datenvergleich.Rmd", encoding = 'UTF-8')

library(roadoi)

data_doi <- data %>%
  filter(!str_detect(doi, "keine doi"))

sample_data <- data_doi %>%
  sample_n(size = 100)


########################### Load data from unpaywall ###########################

# https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html

data_doi_2020 <- data_doi %>%
  filter(jahr == 2020) %>%
  select(doi)

data_doi_2019 <- data_doi %>%
  filter(jahr == 2019) %>%
  select(doi)

data_doi_2018 <- data_doi %>%
  filter(jahr == 2018) %>%
  select(doi)

data_unpaywall_2018 <- roadoi::oadoi_fetch(dois = data_doi_2018$doi,
            email = "jan.taubitz@charite.de",
            .progress = "text")

warnings_2020 <- warnings()
save(data_unpaywall_2020, file = "data/data_unpaywall_2020.Rda")
save(warnings_2020, file = "data/warnings_2020.Rda")
# load("data_unpaywall_2020.Rda")

warnings_2019 <- warnings()
save(data_unpaywall_2019, file = "data/data_unpaywall_2019.Rda")
save(warnings_2019, file = "data/warnings_2019.Rda")
# load("data_unpaywall_2019.Rda")

warnings_2018 <- warnings()
save(data_unpaywall_2018, file = "data/data_unpaywall_2018.Rda")
save(warnings_2018, file = "data/warnings_2018.Rda")
# load("data/data_unpaywall_2019.Rda")

data_unpaywall <- rbind(data_unpaywall_2018, data_unpaywall_2019, data_unpaywall_2020)
save(data_unpaywall, file = "data/data_unpaywall.Rda")
load("data/data_unpaywall.Rda")


sort(table(data_unpaywall$publisher), decreasing = TRUE)
sort(table(data_unpaywall$journal_is_in_doaj), decreasing = TRUE)
sapply(data_unpaywall, function(x) length(unique(x)))

########################### Proxy settings ###########################
# https://support.rstudio.com/hc/en-us/articles/200488488-Configuring-R-to-Use-an-HTTP-or-HTTPS-Proxy
# file.edit('~/.Renviron')
# http_proxy=http://proxy.charite.de:8080/
# https_proxy=http://proxy.charite.de:8080/
# 2x http://...

########################### Literature documentations ###########################
# https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html
# https://subugoe.github.io/scholcomm_analytics/posts/unpaywall_evidence/


########################### Analysis ###########################

# Percent of article that were oa
# Percent of articles that were published in a doaj-journal

# Percent of article with Corresponding author

is_oa <- sort(table(data_unpaywall$is_oa), decreasing = TRUE)
is_oa
is_oa <- round(9427/sum(is_oa)*100, 1)

is_doaj <- sort(table(data_unpaywall$journal_is_in_doaj), decreasing = TRUE)
is_doaj
is_doaj <- round(4152/sum(is_doaj)*100, 1)

is_corresponding <- sort(table(data$corresponding_author_cha), decreasing = TRUE)
is_corresponding
is_corresponding <- round(6292/sum(is_corresponding)*100, 1)


########################### Analyse Licences  ###########################


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

########################### Combine lists  ###########################

data_license_unnest_distinct <- data_license_unnest %>%
  distinct(doi, .keep_all = TRUE)

data_license_final <- rbind(data_license_unnest_2, data_license_unnest_distinct) %>%
  distinct(doi, .keep_all = TRUE) %>%
  mutate(license = replace_na(license, "kein Ergebnis")) %>%
  mutate(license = factor(license, levels = c("cc-by", "cc-by-nc", "cc-by-sa", "cc-by-nd", "cc-by-nc-sa", "cc-by-nc-nd", "sonstige Lizenz", "kein Ergebnis")))

data_license_final_count <- data_license_final %>%
  group_by(license) %>%
  summarise(count = n())

# test

test <- data_license_unnest_2 %>%
  left_join(data_license_final, by = "doi") %>%
  mutate(test = if_else(license.x == license.y, TRUE, FALSE))

# https://stackoverflow.com/questions/6986657/find-duplicated-rows-based-on-2-columns-in-data-frame-in-r

# 740 Artikel haben mindestens zwei unterschiedliche Lizenzen

########################### Add oa_status to data  ###########################

data_doi <- data %>%
  filter(!str_detect(doi, "keine doi"))

data_medbib_license <- data_doi %>%
  select(doi, oa_status)

data_license_oa_status_final <- data_medbib_license %>%
  inner_join(data_license_final, by = "doi")

data_license_oa_status_final_count <- data_license_oa_status_final %>%
  group_by(license, oa_status) %>%
  summarise(count = n())


########################### Visualize licenses  ###########################

chart_lizenzen <- data_license_oa_status_final_count %>%
  hchart("column",
         hcaes(x = license, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_title(text = "Lizenzen") %>%
  hc_subtitle(text = "Daten zu Lizenzen von Unpaywall") %>%
  hc_tooltip(pointFormat = "{point.count} Artikel")

save(chart_lizenzen, file = "charts/chart_lizenzen.Rda")
