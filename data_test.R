########################### Start publisher test ###########################

data_publisher %>%
  filter(publisher == "elsevier") %>%
  group_by(jahr, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  hchart("column",
         hcaes(x = jahr, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100)

data_publisher_bar <- data_publisher %>%
  filter(publisher %in% c("elsevier", "springer", "wiley")) %>%
  group_by(jahr, publisher, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(percent = round(value / sum(value) * 100, 1))

data_publisher_bar %>%
  filter(jahr == 2020) %>%
  hchart("bar",
         hcaes(x = publisher, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color)


data_publisher_bar %>%
  hchart("bar",
         hcaes(y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(categories = df_list)


hchart("bar",
       hcaes(x = jahr, y = percent, group = oa_status, color = publisher)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100)



mpgg <- mpg %>%
  filter(class %in% c("suv", "compact", "midsize")) %>%
  group_by(class, manufacturer) %>%
  summarize(count = n())

categories_grouped <- mpgg %>%
  group_by(name = class) %>%
  summarise(categories = .$manufacturer) %>%
  list_parse()

highchart() %>%
  hc_xAxis(categories = categories_grouped) %>%
  hc_add_series(data = mpgg, type = "bar", hcaes(y = count, color = manufacturer),
                showInLegend = FALSE)


df <- data.frame(jahr = c(2018, 2018, 2018, 2019, 2019, 2019, 2020, 2020, 2020),
                 publisher = c("elsevier", "springer", "wiley", "elsevier", "springer", "wiley", "elsevier", "springer", "wiley"))

df_list <- df %>%
  group_by(name = jahr) %>%
  do(categories = .$publisher) %>%
  list_parse()

test <- data_publisher_bar %>%
  select(-percent) %>%
  group_by(name = jahr) %>%
  do(categories = .$publisher) %>%
  list_parse()

highchart() %>%
  hc_xAxis(categories = df_list) %>%
  hc_add_series(data = data_publisher_bar, type = "bar", hcaes(y = value, color = oa_status),
                showInLegend = FALSE)


%>%
  hc_plotOptions(series = list(stacking = "normal"))

mpgg <- mpg %>%
  filter(class %in% c("suv", "compact", "midsize")) %>%
  group_by(class, manufacturer) %>%
  summarize(count = n())

categories_grouped <- mpgg %>%
  group_by(name = class) %>%
  do(categories = .$manufacturer) %>%
  list_parse()

highchart() %>%
  hc_xAxis(categories = categories_grouped) %>%
  hc_add_series(data = mpgg, type = "bar", hcaes(y = count, color = manufacturer),
                showInLegend = FALSE)


data_publisher_long <- data_publisher %>%
  filter(jahr == 2020) %>%
  group_by(publisher, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(percent = round(value / sum(value) * 100, 1))

hchart(
  data_publisher_long,
  "bar",
  hcaes(x = publisher, y = value, group = oa_status)
  # dataLabels = list(enabled = TRUE, style = list(fontWeight = "normal"))
) %>%
  hc_xAxis(
    min = 0,
    max = 30,
    scrollbar = list(enabled = TRUE)
  ) %>%
  hc_size(height = 700)



########################### End publisher test ###########################
########################### Start journal test ###########################

test_journal <- data %>%
  filter(oa_status != "kein ergebnis") %>%
  filter(jahr == 2020) %>%
  group_by(zeitschrift, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  group_by(oa_status) %>%
  slice_max(value, n = 10)

test_journal %>%
#  filter(oa_status == "gold") %>%
  hchart("bar",
         hcaes(x = zeitschrift, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal"))


test_journal_2 <- data %>%
  filter(jahr == 2020) %>%
  group_by(zeitschrift, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(percent = round(value / sum(value) * 100, 1)) %>%
  group_by(zeitschrift) %>%
  mutate(value_zs = sum(value)) %>%
  ungroup() %>%
#  slice_max(value_zs, n = 100) %>%
  filter(value_zs >= 5) %>%
  mutate(zeitschrift = forcats::fct_reorder(zeitschrift, -value_zs)) %>%
  arrange(-value_zs)

test_journal_3 <- test_journal_2 %>%
  select(-percent) %>%
  group_by(zeitschrift) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:7) %>%
  mutate(oa_status = factor(oa_status, levels = c("green", "gold", "hybrid", "bronze", "closed", "kein ergebnis"))) %>%
  mutate(percent = round(value / sum(value) * 100, 1))


test_journal_2 <- data %>%
  filter(jahr == 2020) %>%
  group_by(zeitschrift, oa_status) %>%
  summarise(value = n(), .groups = "drop_last") %>%
  mutate(value_zs = sum(value)) %>%
  ungroup() %>%
  filter(value_zs >= 5) %>%
  mutate(zeitschrift = forcats::fct_reorder(zeitschrift, -value_zs))

test_journal_3 <- test_journal_2 %>%
  group_by(zeitschrift) %>%
  spread(oa_status, value, fill = 0) %>%
  gather(oa_status, value, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = c("green", "gold", "hybrid", "bronze", "closed", "kein ergebnis"))) %>%
  mutate(percent = round(value / sum(value) * 100, 1))


test_journal_3 %>%
  hchart("bar", hcaes(x = zeitschrift, y = value, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 20,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500)

########################### End journal test ###########################
