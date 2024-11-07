# Set working directory
setwd("C:/Users/patron/Desktop/R Studio")
setwd("/Users/tarnampreetkaur/Desktop/Research data/Fall 2024/GitHub/EcoliST131EuropeProject/CountryData")
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
  
  # Add diagnostic print statement
  print(paste("Reading file:", file))
  print(paste("Number of rows:", nrow(data)))
  
  # Add a column to track the country (based on file name)
  data$Country <- gsub(".csv","", file)
  
  # Append to data_list
  data_list[[file]] <- data
}

# Combine all data frames into one large data frame
combined_data <- do.call(rbind, data_list)

# Add debugging checks
print("Checking unique ST values:")
print(unique(combined_data$ST))

print("Data structure:")
str(combined_data)

print("Checking Source.Type values:")
print(unique(combined_data$Source.Type))

print("First few rows of combined data:")
print(head(combined_data))

# Load required libraries
library(dplyr)
library(ggplot2)

# Create summary data with debugging prints
summary_data <- combined_data %>%
  filter(Source.Type == "Human", 
         Collection.Year >= 2010, 
         Collection.Year <= 2024) %>%
  group_by(Country, Collection.Year) %>%
  summarise(
    total_human = n(),
    st131_count = sum(ST == 131, na.rm = TRUE),
    st131_percentage = (st131_count / total_human) * 100
  ) %>%
  ungroup()

# Print detailed summary
print("Detailed summary of calculations:")
print(summary_data %>% 
        select(Country, Collection.Year, total_human, st131_count, st131_percentage))

# Create the plot
ggplot(summary_data, aes(x = Collection.Year, y = st131_percentage, color = Country, group = Country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  # Added points to better see the data
  scale_y_continuous(
    limits = c(0, max(summary_data$st131_percentage) * 1.1),  # Set reasonable y-axis limits
    breaks = seq(0, 100, by = 10)  # Create breaks every 10%
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  labs(title = "ST131 Percentage in Human Sources (2010-2024)",
       x = "Year",
       y = "ST131 Percentage (%)",
       color = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )+
  facet_wrap(~Country, ncol=5)

# plotting graph ST131 ECOLI/ countries in bar.
ggplot(summary_data, aes(x = Collection.Year, y = st131_percentage, fill = Country)) +
  geom_col(position = position_dodge(width = 0.7), linewidth = 1.5) +  
  scale_y_continuous(
    limits = c(0, max(summary_data$st131_percentage) * 1.1),  
    breaks = seq(0, 100, by = 10)  
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  labs(title = "ST131 Percentage in Human Sources (2010-2024)",
       x = "Year",
       y = "ST131 Percentage (%)",
       fill = "Country") +
  theme_minimal()






