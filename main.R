#load 
library(readr)
library(dplyr)

# functions
# Error handling when reading a CSV file
read_csv_safe <- function(file_path) {
  tryCatch(
    {
      # Attempt to read the CSV file
      data <- read.csv(file_path)
      cat("File successfully loaded!\n")
      return(data)
    },
    error = function(e) {
      # Error handling message
      cat("Error: The file could not be found or loaded.\n")
      cat("Error message:", e$message, "\n")
      return(NULL)  # Return NULL if an error occurs
    }
  )
}

# load Data
file_path <- "/Users/dacoriesmith/Developer/GitHub/Interview assignment/data_analyst_r/assets/AE_data.csv"
AE_data  <- read_csv_safe(file_path)

# Check if data was loaded successfully
if (!is.null(AE_data)) {
  head(AE_data)
} else {
  cat("No data was loaded.\n")
}


# Define the site names for mapping
site_names <- c(
  "01" = "Hospital A", "02" = "Hospital B", "03" = "Hospital C",
  "04" = "Hospital D", "05" = "Hospital E", "06" = "Hospital F",
  "07" = "Hospital G", "08" = "Hospital H", "09" = "Hospital I",
  "10" = "Hospital J", "11" = "Hospital K", "12" = "Hospital L",
  "13" = "Hospital M", "14" = "Hospital N", "15" = "Hospital O",
  "16" = "Hospital P", "17" = "Hospital Q", "18" = "Hospital R"
)


# Filter out rows where Form Status is 6 and only include 1, 2, 3, 4, 5, and 7
filtered_data <- AE_data %>% filter(!(Form.Status %in% c(6)))

# Add Site Names based on Site ID
# Add Site ID and Site Name columns
filtered_data <- filtered_data %>% mutate(SiteID = substr(participant.ID, 1, 2), SiteName = site_names[SiteID])

# Total Number of AEs
Total_Number_of_AES <- nrow(filtered_data)
print(Total_Number_of_AES)

#Number of Related AEs 
filtered_number_related_ad <- filtered_data %>% filter(aerelat %in% c(1, 2))
filtered_number_related_ad <- filtered_data %>% filter(saeyn %in% c(2))
Total_Number_of_Related_AEs <- nrow(filtered_number_related_ad)
print(Total_Number_of_Related_AEs)


# Number of  SAEs (where only saeyn == 1 and Form.Status not 6)
Number_of_Related_SAEs <- filtered_data %>% filter(saeyn == 1) %>% nrow()
print(paste("Number of Related SAEs:", Number_of_Related_SAEs))


# Number of Related SAEs
filtered_data_related_se <- filtered_data_related %>% filter(saeyn == 2)
Total_Number_of_SAEs <- nrow(filtered_data_related_se)
print(Total_Number_of_SAEs)

# Number of Related AE
filtered_data_related_ae<- filtered_data_related %>% filter(saeyn == 1)
Total_Number_AEs <- nrow(filtered_data_related_ae)
print(Total_Number_AEs)

# Number of Related umber of Related AEs and SAEs
filtered_data_related <- filtered_data %>% filter(aerelat %in% c(1, 2, 3))
Total_Number_of_Related_AEs_and_SAEs <- nrow(filtered_data_related)
print(Total_Number_of_Related_AEs_and_SAEs)



# Group by Site and calculate metrics
summary_data <- filtered_data %>% 
  group_by(SiteID, SiteName) %>% 
  summarise(
    Total_AEs = n(),
    Number_of_AEs = sum(saeyn == 2),
    Number_of_SAEs = sum(saeyn == 1),
    Number_of_Related_AEs = sum(aerelat %in% c(1, 2, 3) & saeyn == 2),
    Number_of_Related_SAEs = sum(aerelat %in% c(1, 2, 3) & saeyn == 1),
    Number_of_Related_AEs_and_SAEs = sum(aerelat %in% c(1, 2, 3))
  )

# Add missing sites with zero counts
missing_sites <- setdiff(names(site_names), summary_data$SiteID)
if(length(missing_sites) > 0) {
  missing_data <- data.frame(
    SiteID = missing_sites,
    SiteName = site_names[missing_sites],
    Total_AEs = 0,
    Number_of_AEs = 0,
    Number_of_SAEs = 0,
    Number_of_Related_AEs = 0,
    Number_of_Related_SAEs = 0,
    Number_of_Related_AEs_and_SAEs = 0
  )
  summary_data <- bind_rows(summary_data, missing_data)
}

# Sort the data by Site ID
summary_data <- summary_data %>% arrange(SiteID)

# Add a Total Row using column references correctly
total_row <- data.frame(
  SiteID = "Total",
  SiteName = "Total",
  Total_AEs = sum(summary_data$Total_AEs),
  Number_of_AEs = sum(summary_data$Number_of_AEs),
  Number_of_SAEs = sum(summary_data$Number_of_SAEs),
  Number_of_Related_AEs = sum(summary_data$Number_of_Related_AEs),
  Number_of_Related_SAEs = sum(summary_data$Number_of_Related_SAEs),
  Number_of_Related_AEs_and_SAEs = sum(summary_data$Number_of_Related_AEs_and_SAEs)
)


# Combine site-level and total summaries
final_summary <- bind_rows(summary_data, total_row)


# Write results to CSV
write.csv(final_summary, "AE_output.csv", row.names = FALSE)

# Save results to CSV with error handling
tryCatch(
  {
    write.csv(final_summary, file_path_AE_output, row.names = FALSE)
    cat("AE analysis completed and results saved to AE_output.csv\n")
  },
  error = function(e) {
    cat("Error saving the file. Please check write permissions.\n")
    cat("Error message:", e$message, "\n")
  }
)

# Display the final summary for verification
print(final_summary)


# Print the final summary and completion message
print(final_summary)
cat("AE analysis completed and results saved to AE_output.csv\n")



