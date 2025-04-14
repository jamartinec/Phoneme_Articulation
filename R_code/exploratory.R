renv::activate()
renv::status()

library(dplyr)
library(readr)

# Set the file path (assuming your data is in a folder named "data")
#file_path <- "data/probabilities-middle-frame.csv"
file_path <- "data/probabilities-max-frame.csv"
# Get the file size (in bytes)
file_size <- file.info(file_path)$size
cat("File Size: ", file_size, "bytes\n")

# Read a sample of the data (e.g., 100 rows)
data_sample <- read_csv(file_path, n_max = 1000000)

# Display the first few rows of the sample
head(data_sample)

# Display column names and their data types
str(data_sample)

# Calculate the percentage of missing values for each column
missing_percentage <- sapply(data_sample, function(x) sum(is.na(x)) / length(x) * 100)

# Display the missing percentage for each column
cat("Missing Data Percentage:\n")
print(missing_percentage)

# Summary statistics for numeric columns
summary(data_sample)



#######################################################################################################################
# exploring prob column ##
# Load necessary libraries
library(dplyr)   # For data manipulation
library(tidyr)   # For transforming data to wide format
library(psych)   # For factor analysis
library(ggplot2) # For plotting histograms
# Step 1: Apply natural log transformation to the probability variable
df = data_sample 
df <- df %>%
  mutate(log_prob = log(prob))  # Assuming 'probability' is the column name
# Step 2: Aggregate repeated measures for a given phoneme/user by taking the mean
df_aggregated <- df %>%
  group_by(speaker, phoneme) %>%
  summarise(mean_log_prob = mean(log_prob, na.rm = TRUE), .groups = "drop")
print("aggregated")
print(head(df_aggregated))





# Assuming your data frame is named df

# Create a histogram for each phoneme and save as a PNG
# Step 3: Plot histograms for each phoneme
#phoneme_columns <- colnames(df_wide)[-1]  # Exclude 'user' column
phoneme_columns <- unique(df_aggregated$phoneme)
for (phoneme in phoneme_columns) {
  #plot <- ggplot(df_aggregated, aes_string(x = mean_log_prob)) +
    plot <- ggplot(df_aggregated %>% filter(phoneme == phoneme), aes(x = mean_log_prob))+
    geom_histogram(bins = 29, fill = "blue", alpha = 0.6, color = "black") +
    labs(title = paste("Histogram of", phoneme), x = "Mean Log Probability", y = "Count") +
    theme_minimal() 
ggsave(paste0("output/plots/histogram_", phoneme, ".png"),plot=plot)  # Saves each histogram as an image file
}


# Step 2: Transform data into wide format (Each user as a row, each phoneme as a column)
df_wide <- df_aggregated %>%
  pivot_wider(names_from = phoneme, values_from = mean_log_prob)
print("dfwide:")
print(head(df_wide))

df_wide_clean <- df_wide %>%
  select(-speaker) %>%     # Remove the non-numeric 'speaker' column
  na.omit() %>%            # Remove rows with any NA values
  apply(2, function(x) ifelse(is.finite(x), x, NA)) %>%   # Remove infinite values
  na.omit()                 # Remove rows with remaining NA values
  #apply(2,as.numeric)
print("clean:")
str(df_wide_clean)

# Convert all columns to numeric
#df_wide_clean <- df_wide_clean %>%
  #mutate(across(everything(), as.numeric))

# Step 4: Apply Factor Analysis
# 4a: Compute correlation matrix and eigenvalues
cor_matrix <- cor(as.matrix(df_wide_clean) , use = "complete.obs")
# Check if the correlation matrix has NA or infinite values
print(cor_matrix)
print(any(is.na(cor_matrix)))   # Check for NA values
any(is.infinite(cor_matrix))  # Check for infinite values

eigenvalues <- eigen(cor_matrix)$values
# 4b: Plot scree plot to visualize eigenvalues
scree_plot <-plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Factor Number", ylab = "Eigenvalue")
output_dir <- "output"
# Save scree plot as PNG
scree_plot_path <- file.path(output_dir, "scree_plot.png")
png(scree_plot_path)
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Factor Number", ylab = "Eigenvalue")
dev.off()

# 4c: Identify the "elbow" (manually inspect the plot)
# The student should visually determine where the eigenvalues show a marked drop.
# 4d: Run factor analysis with the chosen number of factors
num_factors <- 3  # <-- Student should manually enter the chosen number of factors here
fa_result <- fa(cor_matrix, nfactors = num_factors, rotate = "varimax")



# Export loading matrix as CSV

loading_matrix <- as.data.frame(unclass(fa_result$loadings))

write.csv(loading_matrix, file = file.path(output_dir, "loading_matrix.csv"), row.names = TRUE)

# 4e: Report the loading matrix
print(fa_result$loadings)

# (Optional) If the student wants a clearer view of factor loadings
fa_result$loadings[,]  # Convert to matrix for easier interpretation
