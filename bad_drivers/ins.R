library(readr)
print(sys.frame(1)$ofile)
setwd(dirname(normalizePath(sys.frame(1)$ofile)))
ins_df <- read_csv("../insurance.csv")
print(head(ins_df))
print(colnames(ins_df))

# Create an empty data frame for results
ins_results <- data.frame(
    colName = character(),
    colFeature = character(),
    colValue = numeric(),
    stringsAsFactors = FALSE
)

# Get numeric columns
num_cols <- sapply(ins_df, is.numeric)
numeric_names <- names(ins_df)[num_cols]

# Iterate and calculate statistics
for (col in numeric_names) {
    values <- ins_df[[col]]
    stats <- list(
        mean = mean(values, na.rm = TRUE),
        `standard deviation` = sd(values, na.rm = TRUE),
        median = median(values, na.rm = TRUE),
        minimum = min(values, na.rm = TRUE),
        maximum = max(values, na.rm = TRUE)
    )
    for (feature in names(stats)) {
        ins_results <- rbind(
            ins_results,
            data.frame(
                colName = col,
                colFeature = feature,
                colValue = stats[[feature]],
                stringsAsFactors = FALSE
            )
        )
    }
}



# Get character columns
char_cols <- sapply(ins_df, is.character)
character_names <- names(ins_df)[char_cols]

# Iterate and count unique values
for (col in character_names) {
    unique_count <- length(unique(ins_df[[col]]))
    ins_results <- rbind(
        ins_results,
        data.frame(
            colName = col,
            colFeature = "Number of Unique Entries",
            colValue = unique_count,
            stringsAsFactors = FALSE
        )
    )
}

for (col in character_names) {
    value_counts <- table(ins_df[[col]])
    for (unique_value in names(value_counts)) {
        ins_results <- rbind(
            ins_results,
            data.frame(
                colName = col,
                colFeature = unique_value,
                colValue = value_counts[[unique_value]],
                stringsAsFactors = FALSE
            )
        )
    }
}

print(ins_results)

quiz_questions =""
for (i in 1:nrow(ins_results)) {
    
        quiz_questions <- paste0(quiz_questions, 
				i, ". What is the ", ins_results$colFeature[i], 
				" of ", ins_results$colName[i], "?\n= ", 
				ins_results$colValue[i], " +-0.1", "\n")
    } 



# Print the quiz_questions string
cat(quiz_questions)
writeLines(quiz_questions, "ins_results_quiz.qtitext")