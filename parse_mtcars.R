#parsing mtcars
library(dplyr)
library(plyr)
library(tidyr)

# Load the mtcars dataset
data(mtcars)

# Create a new dataframe by removing columns with less than 5 distinct values
new_mtcars <- mtcars %>%
    select_if(~ n_distinct(.) >= 7)

# Print the new dataframe
print(new_mtcars)

# Calculate pairwise Pearson correlation
cor_results <- data.frame()

numeric_cols <- sapply(new_mtcars, is.numeric)
numeric_data <- new_mtcars[, numeric_cols]

for (i in 1:(ncol(numeric_data) - 1)) {
    for (j in (i + 1):ncol(numeric_data)) {
        col1 <- names(numeric_data)[i]
        col2 <- names(numeric_data)[j]
        cor_test <- cor.test(numeric_data[[col1]], numeric_data[[col2]], method = "pearson")
        cor_results <- rbind(cor_results, data.frame(
            pair = paste(col1, col2, sep = "-"),
            correlation = cor_test$estimate,
            p_value = cor_test$p.value
        ))
    }
}

# Print the correlation results
print(cor_results)
write.csv(cor_results, "correlation_results.csv", row.names = FALSE)

# Perform pairwise t-tests for the first 7 columns based on the "am" column
t_test_results <- data.frame()

for (col in names(mtcars)[1:7]) {
    t_test <- t.test(mtcars[[col]] ~ mtcars$am)
    t_test_results <- rbind(t_test_results, data.frame(
        column = col,
        factor = "am",
        mean_difference = diff(t_test$estimate),
        p_value = t_test$p.value
    ))
}

# Print the t-test results
print(t_test_results)
write.csv(t_test_results, "t_test_results_am.csv", row.names = FALSE)

t_test_results_vs <- data.frame()

for (col in names(mtcars)[1:7]) {
    t_testv <- t.test(mtcars[[col]] ~ mtcars$vs)
    print(col)
    print(t_testv)
    t_test_results_vs <- rbind(t_test_results_vs, data.frame(
        column = col,
        factor = "vs",
        mean_difference = diff(t_testv$estimate),
        p_value = t_testv$p.value
    ))
}

# Print the t-test results
print(t_test_results_vs)
write.csv(t_test_results_vs, "t_test_results_vs.csv", row.names = FALSE)

# Calculate the mean of the mpg column factored by the am column

# Calculate the means of all columns with more than 4 unique values factored by all columns with 4 or less unique values
mean_results <- data.frame()

# Identify columns with more than 4 unique values
cols_more_than_4 <- names(mtcars)[sapply(mtcars, function(x) n_distinct(x) > 4)]

# Identify columns with 4 or less unique values
cols_4_or_less <- names(mtcars)[sapply(mtcars, function(x) n_distinct(x) <= 4)]

for (col in cols_more_than_4) {
    for (factor_col in cols_4_or_less) {
        factor_levels <- unique(mtcars[[factor_col]])
        for (level in factor_levels) {
            mean_value <- mean(mtcars[[col]][mtcars[[factor_col]] == level], na.rm = TRUE)
            mean_results <- rbind(mean_results, data.frame(
                column = col,
                factored_by = factor_col,
                factor_value = level,
                mean = mean_value
            ))
        }
    }
}

# Print the mean results
print(mean_results)
write.csv(mean_results, "mean_results.csv", row.names = FALSE)

qtimean=""

for (i in 1:nrow(mean_results)) {
    qtimean <- paste0(qtimean, 
                      i, ". What is the mean of ", mean_results$column[i], 
                      " when the value of ", mean_results$factored_by[i], 
                      " is ", mean_results$factor_value[i], "\n=", 
                      mean_results$mean[i], "  +- 0.1\n")
}

# Print the qtimean string
cat(qtimean)
writeLines(qtimean, "factored_means.qti")

corr_mean=""
for (i in 1:nrow(cor_results)) {
    corr_mean <- paste0(corr_mean, 
                        "What is the correlation between the columns ", cor_results$pair[i], 
                        " in the mtcars dataset?\n= ", 
                        cor_results$correlation[i], "\n")
}

# Print the corr_mean string
cat(corr_mean)

writeLines(corr_mean, "correlation_questions.qti")

# Calculate summary statistics for columns with more than 6 distinct values
summary_stats <- data.frame()

cols_more_than_6 <- names(mtcars)[sapply(mtcars, function(x) n_distinct(x) > 6)]

for (col in cols_more_than_6) {
    mean_value <- mean(mtcars[[col]], na.rm = TRUE)
    sd_value <- sd(mtcars[[col]], na.rm = TRUE)
    median_value <- median(mtcars[[col]], na.rm = TRUE)
    iqr_value <- IQR(mtcars[[col]], na.rm = TRUE)
    max_value <- max(mtcars[[col]], na.rm = TRUE)
    min_value <- min(mtcars[[col]], na.rm = TRUE)
    
    summary_stats <- rbind(summary_stats, data.frame(
        column = col,
        mean = mean_value,
        sd = sd_value,
        median = median_value,
        iqr = iqr_value,
        max = max_value,
        min = min_value
    ))
}

# Gather summary statistics into a long format dataframe
summary_stats_long <- summary_stats %>%
    gather(key = "statistic", value = "value", -column)

# Print the long format summary statistics
print(summary_stats_long)
write.csv(summary_stats_long, "summary_stats_long.csv", row.names = FALSE)

statsqti=""

for (i in 1:nrow(summary_stats_long)) {
    statsqti <- paste0(statsqti, 
                       i, ". What is the ", summary_stats_long$statistic[i], 
                       " of ", summary_stats_long$column[i], 
                       " in the mtcars dataset?\n= ", 
                       summary_stats_long$value[i], " +-0.1\n")
}

# Print the statsqti string
cat(statsqti)
writeLines(statsqti, "summary_stats_questions.qti")