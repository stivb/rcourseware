library(readr)
setwd(dirname(normalizePath(sys.frame(1)$ofile)))

# Read in the CSV file using the first row as column names
bad_drivers <- read_csv("bad-drivers.csv")
colnames(bad_drivers)[2] <- "nDeadDriversPerBillMiles"
colnames(bad_drivers)[3] <- "pcDeadSpeeding"
colnames(bad_drivers)[4] <- "pcDeadAlcohol"
colnames(bad_drivers)[5] <- "pcDeadUndistracted"
colnames(bad_drivers)[6] <- "pcDeadFirstAccident"
colnames(bad_drivers)[7] <- "premiums"
colnames(bad_drivers)[8] <- "lossesPerDriver"

# Create an empty data frame to store results
baddriverresults <- data.frame(
    colName = character(),
    colFeature = character(),
    colValue = character(),
    colMargin = character(),
    colType = character(),
    stringsAsFactors = FALSE
)

# Iterate over columns 2 to 8
for (i in 2:8) {
    col_data <- bad_drivers[[i]]
    col_name <- colnames(bad_drivers)[i]
    
    # Calculate features
    mean_val <- mean(col_data, na.rm = TRUE)
    sd_val <- sd(col_data, na.rm = TRUE)
    median_val <- median(col_data, na.rm = TRUE)
    min_val <- min(col_data, na.rm = TRUE)
    max_val <- max(col_data, na.rm = TRUE)
    state_min <- bad_drivers$State[which.min(col_data)]
    state_max <- bad_drivers$State[which.max(col_data)]
    
    # Add rows to results
    baddriverresults <- rbind(
        baddriverresults,
        data.frame(colName = col_name, colFeature = "mean", colValue = as.character(mean_val), colMargin = " +-0.1", colType = "numeric"),
        data.frame(colName = col_name, colFeature = "standard deviation", colValue = as.character(sd_val), colMargin = " +-0.1", colType = "numeric"),
        data.frame(colName = col_name, colFeature = "median", colValue = as.character(median_val), colMargin = " +-0.1", colType = "numeric"),
        data.frame(colName = col_name, colFeature = "minimum", colValue = as.character(min_val), colMargin = " +-0.1", colType = "numeric"),
        data.frame(colName = col_name, colFeature = "maximum", colValue = as.character(max_val), colMargin = " +-0.1", colType = "numeric"),
        data.frame(colName = col_name, colFeature = "State with minimum", colValue = as.character(state_min), colMargin = "", colType = "string"),
        data.frame(colName = col_name, colFeature = "State with maximum", colValue = as.character(state_max), colMargin = "", colType = "string")
    )
}


quiz_questions =""
for (i in 1:nrow(baddriverresults)) {
    if (baddriverresults$colType[i] == "numeric") {
        quiz_questions <- paste0(quiz_questions, 
                                i, ". What is the ", baddriverresults$colFeature[i], 
                                " of ", baddriverresults$colName[i], "?\n= ", 
                                baddriverresults$colValue[i], baddriverresults$colMargin[i], "\n")
    } else {
        quiz_questions <- paste0(quiz_questions, 
                                i, ". What is the ", baddriverresults$colFeature[i], 
                                " of ", baddriverresults$colName[i], "?\n* ", 
                                baddriverresults$colValue[i], "\n")
    }

}

# Print the quiz_questions string
cat(quiz_questions)
writeLines(quiz_questions, "bad_drivers_quiz.qtitext")