# Load necessary libraries
library(ggplot2)

# Assume 'combined_filtered_data' is the combined dataset with data from all countries
# Group the data by Country and Collection Year, calculate ST131 percentage for human sources

# Create a summarized dataset for plotting
summary_data <- combined_filtered_data %>%
  filter(Source.Type == "Human", Collection.Year >= 2010, Collection.Year <= 2024) %>%
  group_by(Country, Collection.Year) %>%
  summarise(
    total_human = n(),
    st131_count = sum(ST == "ST131"),
    st131_percentage = (st131_count / total_human) * 100
  )

# Create the line plot using ggplot
ggplot(summary_data, aes(x = Collection.Year, y = st131_percentage, color = Country, group = Country)) +
  geom_line(size = 1.2) +
  labs(title = "ST131 Percentage in Human Sources (2010-2024)",
       x = "Year",
       y = "ST131 Percentage",
       color = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
