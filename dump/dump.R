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

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean publisher column from medbib dataset ----
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# data_publisher <- data %>%
#   mutate(publisher = tolower(verlag)) %>%
#   mutate(publisher = case_when(str_detect(publisher, regex("wiley", ignore_case=TRUE)) ~ "wiley",
#                                str_detect(publisher, regex("springer", ignore_case=TRUE)) ~ "springer",
#                                str_detect(publisher, regex("elsevier", ignore_case=TRUE)) ~ "elsevier",
#                                str_detect(publisher, regex("nature", ignore_case=TRUE)) ~ "nature",
#                                str_detect(publisher, regex("thieme", ignore_case=TRUE)) ~ "thieme",
#                                str_detect(publisher, regex("lippincott", ignore_case=TRUE)) ~ "lww",
#                                str_detect(publisher, regex("^bmj", ignore_case=TRUE)) ~ "bmj",
#                                str_detect(publisher, regex("frontiers", ignore_case=TRUE)) ~ "frontiers",
#                                str_detect(publisher, regex("oxford", ignore_case=TRUE)) ~ "oxford up",
#                                TRUE ~ publisher
#                                ))
#
# data_publisher_sum <- data_publisher %>%
#   #filter(oa_status == "closed") %>%
#   group_by(jahr, publisher) %>%
#   summarise(value = n(), .groups = "drop_last") %>%
#   mutate(percent = round(value / sum(value) * 100, 1)) %>%
#   filter(value >= 100) %>%
#   arrange(-jahr, -value) %>%
#   ungroup() %>%
#   mutate(publisher = forcats::fct_reorder(publisher, value)) %>%
#   arrange(-value, .by_group = TRUE)

# ordering bar plots https://github.com/jbkunst/highcharter/issues/363

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Treemap for license data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

