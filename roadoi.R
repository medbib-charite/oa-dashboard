########################### Load source ###########################

source("data_1.R", encoding = 'UTF-8')

library(roadoi)

data_doi <- data %>%
  filter(!str_detect(doi, "keine DOI"))

sample_data <- data_doi %>%
  sample_n(size = 100)


########################### Load data from unpaywall ###########################

# https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html

data_unpaywall <- roadoi::oadoi_fetch(dois = data$doi,
            email = "jan.taubitz@charite.de",
            .progress = "text")

########################### Proxy settings ###########################
# https://support.rstudio.com/hc/en-us/articles/200488488-Configuring-R-to-Use-an-HTTP-or-HTTPS-Proxy
# file.edit('~/.Renviron')
# http_proxy=http://proxy.charite.de:8080/
# https_proxy=http://proxy.charite.de:8080/
# 2x http://...

########################### Literature documentations ###########################
# https://subugoe.github.io/scholcomm_analytics/posts/unpaywall_evidence/
