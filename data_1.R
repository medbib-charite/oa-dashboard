########################### Load in libraries ###########################

library(tidyverse)
library(ggplot2)
library(highcharter)
library(janitor)
library(readr)
library(readxl)


########################### Load in data ###########################

raw_data <- read_excel("Endfassung.xlsx",
                       sheet = "Worksheet")

# raw_data <- read_excel("T:/OA-Dashboard/oa-dashboard/Endfassung.xlsx",
#                         sheet = "Worksheet")

########################### Clean data ###########################
data <- raw_data %>%
  clean_names() %>%
  mutate(oa_status = tolower(oa_status)) %>%
  mutate(oa_status = replace_na(oa_status, "kein ergebnis")) %>%
  mutate(oa_status = factor(oa_status, levels = c("green", "gold", "hybrid", "bronze", "closed", "kein ergebnis")))

########################### Only corresponding authors ###########################

data_corresponding <- data %>%
  slice(1:6292)


########################### Exploratory data analysis ###########################

sapply(data, function(x) length(unique(x)))
sort(table(data$document_type), decreasing = TRUE)

n_occur <- data.frame(table(data$titel))

########################### Exploratory visualizations ###########################

data %>%
  ggplot(aes(x = jahr, fill = oa_status)) +
  geom_bar()


data_sum <- data %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n()) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

color <- c("#5cff0a", "#ffdc0a", "#0ac7ff", "#cd7f32", "#262c2f", "darkgray")
text <- "In hac habitasse platea dictumst. Nam hendrerit elementum lacus. Suspendisse potenti. Vestibulum id aliquet neque. Praesent vel est est. Integer molestie consequat erat nec facilisis. Pellentesque scelerisque posuere nulla eu interdum."

status_absolute <-
  hchart(
  data_sum,
  "column",
  hcaes(x = jahr, y = value, group = oa_status)
  ) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color)

# %>% hc_title(text = "Open access status in absolute numbers", align = "left", style = list(fontSize = "12px"))
# %>% hc_subtitle(text = text, align = "left", style = list(fontSize = "12px"))

status_percent <-
  hchart(
  data_sum,
  "column",
  hcaes(x = jahr, y = percent, group = oa_status)
) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100)

# hc_title(text = "Open access status of articles in %", align = "left", style = list(fontSize = "12px")) %>%
# hc_subtitle(text = text, align = "left", style = list(fontSize = "12px")) %>%
# hc_tooltip(shared = TRUE, split = TRUE)

########################### Plot corresponding authors ###########################


data_corresponding_sum <- data_corresponding %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n()) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

color <- c("#5cff0a", "#ffdc0a", "#0ac7ff", "#cd7f32", "#262c2f", "darkgray")
text <- "In hac habitasse platea dictumst. Nam hendrerit elementum lacus. Suspendisse potenti. Vestibulum id aliquet neque. Praesent vel est est. Integer molestie consequat erat nec facilisis. Pellentesque scelerisque posuere nulla eu interdum."

status_corresponding_absolute <-
  hchart(
    data_corresponding_sum,
    "column",
    hcaes(x = jahr, y = value, group = oa_status)
  ) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color)


# hc_title(text = "Open access status of articles of which a Charité scientists was the corresponding author(in absolute numbers)", align = "left", style = list(fontSize = "28px")) %>%
# hc_subtitle(text = text, align = "left", style = list(fontSize = "12px"))

status_corresponding_percent <-
  hchart(
    data_corresponding_sum,
    "column",
    hcaes(x = jahr, y = percent, group = oa_status)
  ) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100)

# hc_title(text = "Open access status of articles of which a Charité scientists was the corresponding author (in %)", align = "left", style = list(fontSize = "28px")) %>%
# hc_subtitle(text = text, align = "left", style = list(fontSize = "12px")) %>%



########################### Publishers ###########################


data_publisher <- data %>%
  mutate(publisher = tolower(verlag)) %>%
  mutate(publisher = case_when(str_detect(publisher, regex("wiley", ignore_case=TRUE)) ~ "wiley",
                               str_detect(publisher, regex("springer", ignore_case=TRUE)) ~ "springer",
                               str_detect(publisher, regex("elsevier", ignore_case=TRUE)) ~ "elsevier",
                               TRUE ~ publisher
                               ))


data_publisher_sum <- data_publisher %>%
  group_by(jahr, publisher) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  filter(value >= 100) %>%
  arrange(-jahr, -value) %>%
  mutate(publisher = forcats::fct_reorder(publisher, value)) %>%
  arrange(-value, .by_group = TRUE) %>%
  ungroup()


data_publisher_sum %>%
  hchart(
  "column",
  hcaes(x = jahr, y = percent, group = publisher)
) %>%
  hc_plotOptions(series = list(stacking = "normal", showInLegend = TRUE))

# ordering bar plots https://github.com/jbkunst/highcharter/issues/363

########################### To do ###########################
#Sheet Unpaywall vergessen
#Charite Metrics Dashboard
#Göttingen anschauen
#Treffen 24. Juni
#Compare data with cha-dashboard











########################### publications_cha_dashboard ###########################

publications_cha_dashboard <- read_csv("publications_cha_dashboard.csv")


bih_data <- publications_cha_dashboard %>%
  filter(year >= 2018)

#doi_na schauen
#dois bih anzeigen
#dois medbib anzeigen

#oa_status vergleichen


sapply(data, function(x) length(unique(x)))
sort(table(publications_cha_dashboard$OA_color), decreasing = TRUE)

n_occur <- data.frame(table(data$titel))


########################### End ###########################
