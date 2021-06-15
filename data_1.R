########################### Load in libraries ###########################


library(highcharter)
library(janitor)
library(readxl)
library(tidyverse)
library(writexl)


########################### Load in data ###########################

raw_data <- read_excel("Endfassung.xlsx",
                       sheet = "Worksheet")

# raw_data <- read_excel("T:/OA-Dashboard/oa-dashboard/Endfassung.xlsx",
#                         sheet = "Worksheet")

########################### Clean data ###########################

oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed", "kein ergebnis")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E", "#5F7036")
#color <- c("#5cff0a", "#ffdc0a", "#0ac7ff", "#cd7f32", "#262c2f", "darkgray")


data <- raw_data %>%
  clean_names() %>%
  mutate(oa_status = tolower(oa_status)) %>%
  mutate(oa_status = replace_na(oa_status, "kein ergebnis")) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(corresponding_author_cha = if_else(row_number() %in% c(1:6292), TRUE, FALSE), .after = "oa_status")


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


text <- "In hac habitasse platea dictumst. Nam hendrerit elementum lacus. Suspendisse potenti. Vestibulum id aliquet neque. Praesent vel est est. Integer molestie consequat erat nec facilisis. Pellentesque scelerisque posuere nulla eu interdum."

status_absolute <-
  hchart(data_sum,
         "column",
         hcaes(x = jahr, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(reversedStacks = FALSE)

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
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)")

# reversed bar stacks https://www.highcharts.com/forum/viewtopic.php?t=10916

# hc_title(text = "Open access status of articles in %", align = "left", style = list(fontSize = "12px")) %>%
# hc_subtitle(text = text, align = "left", style = list(fontSize = "12px")) %>%
# hc_tooltip(shared = TRUE, split = TRUE)

########################### Plot corresponding authors ###########################


data_corresponding_sum <- data %>%
  filter(corresponding_author_cha == TRUE) %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n()) %>%
  mutate(percent = round(value / sum(value) * 100, 1))

text <- "In hac habitasse platea dictumst. Nam hendrerit elementum lacus. Suspendisse potenti. Vestibulum id aliquet neque. Praesent vel est est. Integer molestie consequat erat nec facilisis. Pellentesque scelerisque posuere nulla eu interdum."

status_corresponding_absolute <-
  hchart(
    data_corresponding_sum,
    "column",
    hcaes(x = jahr, y = value, group = oa_status)
  ) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(reversedStacks = FALSE)


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
           max = 100, reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)")

# hc_title(text = "Open access status of articles of which a Charité scientists was the corresponding author (in %)", align = "left", style = list(fontSize = "28px")) %>%
# hc_subtitle(text = text, align = "left", style = list(fontSize = "12px")) %>%



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
#  hc_title(
#    text = "2018"
#  ) %>%
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
#  hc_title(
#    text = "2019"
#  ) %>%
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
#  hc_title(
#    text = "2020"
#  ) %>%
  hc_colors(color)


########################### Journals ###########################

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

########################### Plot journals ###########################

journal_absolute <- journal_data_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 20,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_yAxis(reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)<br>{point.value_zs} Artikel insgesamt")

journal_percent <- journal_data_2 %>%
  hchart("bar", hcaes(x = zeitschrift, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 20,
           scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(labels = list(format = '{value} %'),
                         max = 100, reversedStacks = FALSE) %>%
  hc_size(height = 500) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)<br>{point.value_zs} Artikel insgesamt")


########################### End journals ###########################

########################### Publishers ###########################


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


data_publisher_sum %>%
  hchart(
  "column",
  hcaes(x = jahr, y = value, group = publisher)
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
  filter(year >= 2018 & year <= 2020)

#doi_na schauen
#dois bih anzeigen
#dois medbib anzeigen

#oa_status vergleichen


sapply(data, function(x) length(unique(x)))
sort(table(publications_cha_dashboard$OA_color), decreasing = TRUE)

n_occur <- data.frame(table(data$titel))


########################### End ###########################
