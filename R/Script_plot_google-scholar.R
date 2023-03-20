
library(ggplot2)
library(readr)
library(dplyr)

theme_set(
  theme_classic() +
    theme(legend.position = "top", text = element_text(family ="serif"))
)

data <- read_csv("googlescholar.csv")





data_grouped <- data %>%
  group_by(year) %>%
  summarize(total_occurrences = sum((results)))


ggplot(data_grouped, aes(x = year, y = total_occurrences)) +
  geom_bar(stat = "identity",) +
  scale_x_continuous(breaks = data_grouped$year[seq(1, length(data_grouped$year), by = 2)]) + 
  labs(title = paste0("Autreten des Wortes 'Digitalpolitik' in der wissenschafltichen Literatur auf Google Scholar"), x = "Jahr", y = "Vorkommen") +
  geom_col(fill = "#4472C4")

