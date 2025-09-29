# Load necessary library
library(tidyverse)

# Define the file names
file_names <- c("Alice.csv", "Bob.csv", "Charlie.csv", "David.csv", "Eve.csv")
members <- c("Alice", "Bob", "Charlie", "David", "Eve")
# Create a data frame without row names or column names
df <- data.frame(V1 = members, V2 = rep(3, length(members)), stringsAsFactors = FALSE)

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each file and read the data
for (i in seq_along(file_names)) {
    # Read the CSV file without headers
    data <- read.csv(file_names[i], header = FALSE)

     # Check if the first column matches the corresponding member
    if (all(data$V1 == members)  && ncol(data)==2 &&  all(data$V2 >= 0 & data$V2 <= 5)) {
        # Add the data frame to the list
        data_list[[members[i]]] <- data
    } else {
        data_list[[members[i]]] <- df
    }

}

# Merge all data frames into a single data frame
merged_data <- bind_rows(data_list, .id = "Rater")

# Spread the merged data frame
spread_data <- merged_data %>%
    spread(key = V1, value = V2)

# Print the spread data frame
print(spread_data)

# Print the merged data frame
