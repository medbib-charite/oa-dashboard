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
library(writexl)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

endfassung <- "raw_data/Endfassung.xlsx"

raw_data <- read_excel(endfassung,
                       sheet = "Worksheet")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define color and oa-status variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed", "kein ergebnis")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E", "#5F7036")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Clean data, create some new variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data <- raw_data %>%
  clean_names() %>%
  mutate(doi = tolower(doi),
         oa_status = tolower(oa_status)) %>% # Convert dois and oa_status to lower case
  distinct(doi, .keep_all = TRUE) %>%
  mutate(oa_status = replace_na(oa_status, "kein ergebnis")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status") %>%
  mutate(corresponding_author_cha = if_else(row_number() %in% c(1:6292), TRUE, FALSE), .after = "is_oa")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exploratory data analysis ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#sapply(data, function(x) length(unique(x)))
#sort(table(data$document_type), decreasing = TRUE)
#n_occur <- data.frame(table(data$titel))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of year and oa_status ----
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
hchart(data_sum,
       "spline",
       hcaes(x = factor(jahr), y = value, group = oa_status)) %>%
  hc_colors(color) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_absolute_spline",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
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
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100, reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)") %>%
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
  hchart(data_corresponding_sum,
         "spline",
         hcaes(x = factor(jahr), y = value, group = oa_status)) %>%
  hc_colors(color) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_corresponding_absolute_spline",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  ) %>%
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
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100, reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "status_corresponding_percent",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of year and oa_status item chart ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

item_2018 <- data_corresponding_sum %>%
  filter(jahr == 2018) %>%
  hchart(
    "item",
    hcaes(name = oa_status, y = value),
  #  name = "Anzahl",
    tooltip = list(pointFormat = "{point.value} Artikel ({point.percent} %)"),
    marker = list(symbol = "round"),
    showInLegend = FALSE
  ) %>%
  hc_colors(color)

item_2019 <- data_corresponding_sum %>%
  filter(jahr == 2019) %>%
  hchart(
    "item",
    hcaes(name = oa_status, y = value),
  #  name = "Anzahl",
    tooltip = list(pointFormat = "{point.value} Artikel ({point.percent} %)"),
    marker = list(symbol = "round"),
    showInLegend = FALSE
  ) %>%
  hc_colors(color)

item_2020 <- data_corresponding_sum %>%
  filter(jahr == 2020) %>%
  hchart(
  "item",
  hcaes(name = oa_status, y = value),
#  name = "Anzahl",
  tooltip = list(pointFormat = "{point.value} Artikel ({point.percent} %)"),
  marker = list(symbol = "round"),
  showInLegend = TRUE
) %>%
  hc_colors(color)

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
  hc_yAxis(reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)<br>{point.value_zs} Artikel insgesamt") %>%
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
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)<br>{point.value_zs} Artikel insgesamt") %>%
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
  select(-`kein ergebnis`)

publisher_absolute <- data_publisher_join_sum_2 %>%
  hchart("bar", hcaes(x = publisher, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_yAxis(reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)<br>{point.value_pub} Artikel insgesamt") %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_absolute",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

pal <- got(4, direction = 1, option = "Jon_Snow")

publisher_donut <- data_publisher_join %>%
  group_by(publisher) %>%
  summarise(value = n()) %>%
  mutate(publisher_2 = if_else(value <= 500, "andere Verlage", publisher)) %>%
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

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean publisher column from medbib dataset ----
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_publisher <- data %>%
  mutate(publisher = tolower(verlag)) %>%
  mutate(publisher = case_when(str_detect(publisher, regex("wiley", ignore_case=TRUE)) ~ "wiley",
                               str_detect(publisher, regex("springer", ignore_case=TRUE)) ~ "springer",
                               str_detect(publisher, regex("elsevier", ignore_case=TRUE)) ~ "elsevier",
                               str_detect(publisher, regex("nature", ignore_case=TRUE)) ~ "nature",
                               str_detect(publisher, regex("thieme", ignore_case=TRUE)) ~ "thieme",
                               str_detect(publisher, regex("lippincott", ignore_case=TRUE)) ~ "lww",
                               str_detect(publisher, regex("^bmj", ignore_case=TRUE)) ~ "bmj",
                               str_detect(publisher, regex("frontiers", ignore_case=TRUE)) ~ "frontiers",
                               str_detect(publisher, regex("oxford", ignore_case=TRUE)) ~ "oxford up",
                               TRUE ~ publisher
                               ))

data_publisher_sum <- data_publisher %>%
  #filter(oa_status == "closed") %>%
  group_by(jahr, publisher) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  filter(value >= 100) %>%
  arrange(-jahr, -value) %>%
  ungroup() %>%
  mutate(publisher = forcats::fct_reorder(publisher, value)) %>%
  arrange(-value, .by_group = TRUE)

# ordering bar plots https://github.com/jbkunst/highcharter/issues/363

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
# Load data of OA costs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

oa_costs <- "raw_data/oa_costs.xlsx"

data_costs <- read_excel(oa_costs,
                         sheet = "Sheet1")

data_costs_long <- data_costs %>%
  select(-total) %>%
  pivot_longer(cols = c("2018", "2019", "2020"), names_to = "year")

pal <- got(3, direction = 1, option = "Jon_Snow")
pal <- c("#8797AE", "#B2C1DD", "#2C74B4")

publisher_costs <-
  data_costs_long %>%
  hchart("column",
         hcaes(x = publisher, y = value, group = year)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(title = FALSE) %>%
  hc_yAxis(title = FALSE,
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

pal <- colorRampPalette(c("#8797AE", "#B2C1DD", "#2C74B4"))
pal <- pal(10)

publisher_costs_year <-
  data_costs_long %>%
  hchart("bar",
         hcaes(x = year, y = value, group = publisher)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(title = FALSE) %>%
  hc_yAxis(title = FALSE,
           labels = list(format = '{value:,0f} €'), reversedStacks = FALSE) %>%
  hc_colors(pal) %>%
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "publisher_costs",
    buttons = list(contextButton = list(menuItems = c('downloadJPEG', 'separator', 'downloadCSV')))
  )

rainbow(n=7)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
