########################### Load in libraries ###########################

library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(readxl)





########################### Load in data ###########################

raw_data <- read_excel("T:/OA-Dashboard/oa-dashboard/Endfassung.xlsx",
                         sheet = "Worksheet")


########################### Clean data ###########################
data <- raw_data %>%
  clean_names() %>%
  mutate(oa_status = tolower(oa_status))

########################### Exploratory data analysis ###########################

sapply(data, function(x) length(unique(x)))
sort(table(data$datenbank), decreasing = TRUE)

n_occur <- data.frame(table(data$titel))

########################### Exploratory visualizations ###########################


data %>%
  ggplot(aes(x = jahr, fill = oa_status)) +
  geom_bar()


########################### Load in data ###########################
#Sheet Unpaywall vergessen
#Charite Metrics Dashboard
#Göttingen anschauen
#Treffen 24. Juni









########################### End ###########################
