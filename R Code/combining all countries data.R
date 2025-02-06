# Set working directory
setwd("/Users/codelab/Desktop/GitHub/EcoliST131EuropeProject/CountryData")
getwd()

# List all csv files of the countries
file_list <- list.files(pattern = "*.csv")
print(file_list)
