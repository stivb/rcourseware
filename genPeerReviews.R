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




# Create a dataframe with random values between 1 and 5
# Create a dataframe with random values between 0 and 5 for each individual rating themselves and others
ratings <- data.frame(
    rater = rep(individuals, each = length(individuals)),
    ratee = rep(individuals, times = length(individuals)),
    rating = sample(0:5, length(individuals) * length(individuals), replace = TRUE)
)

# Save each individual's ratings to a separate CSV file
for (individual in individuals) {
    individual_ratings <- ratings %>% filter(rater == individual)
    write.csv(individual_ratings, paste0(individual, ".csv"), row.names = FALSE)
}