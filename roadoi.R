########################### Load source ###########################

source("data_1.R", encoding = 'UTF-8')
source("datenvergleich.Rmd", encoding = 'UTF-8')

library(roadoi)

data_doi <- data %>%
  filter(!str_detect(doi, "keine DOI"))

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
