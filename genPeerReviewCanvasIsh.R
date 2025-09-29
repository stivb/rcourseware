library(dplyr)

# Set seed for reproducibility
set.seed(456)

# Define individuals
individuals <- c("Alice", "Bob", "Charlie", "David", "Eve")
# Define another set of individuals
more_individuals <- c("Frank", "Grace", "Hannah", "Ivy", "Jack")
# Create a dataframe where each row involves a rating by one of the individuals of each of the others (including themselves)
ratings <- data.frame(
    rater = character(),
    ratee1 = character(),
    rating1 = integer(),
    ratee2 = character(),
    rating2 = integer(),
    ratee3 = character(),
    rating3 = integer(),
    ratee4 = character(),
    rating4 = integer(),
    ratee5 = character(),
    rating5 = integer(),
    stringsAsFactors = FALSE
)

# Generate random ratings for each individual
generate_ratings <- function(individuals) {
    for (rater in individuals) {
        ratees <- sample(individuals)  # Randomize the order of ratees
        ratings <<- rbind(ratings, data.frame(
            rater = rater,
            ratee1 = ratees[1],
            rating1 = sample(0:5, 1),
            ratee2 = ratees[2],
            rating2 = sample(0:5, 1),
            ratee3 = ratees[3],
            rating3 = sample(0:5, 1),
            ratee4 = ratees[4],
            rating4 = sample(0:5, 1),
            ratee5 = ratees[5],
            rating5 = sample(0:5, 1)
        ))
    }
}
generate_ratings(individuals)
generate_ratings(more_individuals)

print(ratings)

# Function to convert ratings to 5x5 dataframe
convert_to_matrix <- function(ratings) {
    unique_raters <- unique(ratings$rater)
    matrices <- list()
    for (i in seq(1, nrow(ratings), by = 5)) {
        group <- ratings[i:(i+4), ]
        individuals <- sort(unique(group$rater))
        rating_matrix <- matrix(0, nrow = 5, ncol = 5, dimnames = list(individuals, individuals))
        for (j in 1:5) {
            rater <- group$rater[j]
            rating_matrix[rater, group$ratee1[j]] <- group$rating1[j]
            rating_matrix[rater, group$ratee2[j]] <- group$rating2[j]
            rating_matrix[rater, group$ratee3[j]] <- group$rating3[j]
            rating_matrix[rater, group$ratee4[j]] <- group$rating4[j]
            rating_matrix[rater, group$ratee5[j]] <- group$rating5[j]
        }
        matrices[[length(matrices) + 1]] <- rating_matrix
    }
    return(matrices)
}

matrices <-convert_to_matrix(ratings)
print(matrices)

for (matrix in matrices) {
    avg_sum <-sum(matrix) / 5
    col_sums <- colSums(matrix)
    col_sums_df <- data.frame(col_sums)
    rownames(col_sums_df) <- matrix %>% rownames()
    factor_df <- col_sums_df / avg_sum
    print("Here are the weightings we will apply to the work of each individual based on the average of all the scores")    
    print(factor_df)
}


