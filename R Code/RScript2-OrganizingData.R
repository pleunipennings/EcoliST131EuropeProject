
#setting the working directory 
setwd("/Users/Kaylee/Desktop/EcoliST131EuropeProject")
getwd()

# List all CSV files in the directory
file_list <- list.files(pattern = "*.csv")

# Stop if no CSV files are found
if (length(file_list) == 0) {
  stop("No CSV files found in the directory.")
}

# Create an empty list to store data frames
data_list <- list()

# Loop through each file in file_list, read it, and add a country column
for (file in file_list) {
  data <- read.csv(file)
  
  # Add a column to track the country (based on file name)
  data$Country <- gsub(".csv","", file)
  
  # Append to data_list
  data_list[[file]] <- data
}

# Combine all data frames into one large data frame
combined_data <- do.call(rbind, data_list)

# Verify if ST131 entries exist in the filtered data
st131_check <- combined_data %>%
  filter(Source.Type == "Human", Collection.Year >= 2010, Collection.Year <= 2024, ST == "ST131")

print("Number of ST131 records in human sources (2010-2024):")
print(nrow(st131_check))  # Check if any ST131 records exist

# Summarize data for plotting
library(dplyr)
summary_data <- combined_data %>%
  filter(Source.Type == "Human", Collection.Year >= 2010, Collection.Year <= 2024) %>%
  group_by(Country, Collection.Year) %>%
  summarise(
    total_human = n(),
    st131_count = sum(ST == "ST131", na.rm = TRUE),  # Counts "ST131" cases
    st131_percentage = ifelse(total_human > 0, (st131_count / total_human) * 100, 0)
  )

# Print summary data to verify calculations before plotting
print("Summary Data for Plotting:")
print(head(summary_data))

# Create the line plot using ggplot
library(ggplot2)
ggplot(summary_data, aes(x = Collection.Year, y = st131_percentage, color = Country, group = Country)) +
  geom_line(linewidth = 1.2) +
  labs(title = "ST131 Percentage in Human Sources (2010-2024)",
       x = "Year",
       y = "ST131 Percentage",
       color = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
