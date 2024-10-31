# Set the working directory to where the CSV files are housed

# Check to make sure working directory is correct
getwd()

# List all the files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Making sure the files are found
print(file_list)

# Create an emopty list to store the data frames
data_list <- list()

# Loop through each file in file_list, read it, and add a country column
for (file in file_list) {
  
  # Skip the file if it's empty
  if (file.size(file) == 0) {
    message("Skipping empty file: ", file)
    next
  }
  
  # Attempt to read the file; skip if there’s an error
  data <- tryCatch({
    read.csv(file)
  }, error = function(e) {
    message("Error reading file: ", file, " - ", e$message)
    return(NULL)  # Skip this file
  })
  
  # If data was successfully read, add the Country column and store in data_list
  if (!is.null(data)) {
    data$Country <- gsub(".csv", "", file)
    data_list[[file]] <- data
  }
}

# Combine all non-empty data frames into one large data frame
combined_data <- do.call(rbind, data_list)

# Check the first few rows and count data per country
head(combined_data)
table(combined_data$Country)

# Verify if ST131 entries exist in the filtered data
st131_check <- combined_data$ST
head(st131_check)

# Load libary
library(dplyr)

# Isolate data to collums/topics needed
ST131_filtered_data <- combined_data %>%
  # Filter to ST!31 and collection year
  filter(Source.Type == 131) %>% 
  
  # Group Country and Collection.Year
  group_by(Country, Collection.Year) %>%
  
  # Summarize the data
  summarise(
    total_human = n(),
    st131_count = sum(ST == "ST131", na.rm = TRUE),  # Counts "ST131" cases
    st131_percentage = ifelse(total_human > 0, (st131_count / total_human) * 100, 0)
  )
table(ST131_filtered_data)