library(dplyr)

# Set seed for reproducibility
set.seed(456)

# Define individuals
individuals <- c("Alice", "Bob", "Charlie", "David", "Eve")
# Define another set of individuals
more_individuals <- c("Frank", "Grace", "Hannah", "Ivy", "Jack")

small_group <- c("Karl", "Liam", "Mary")
# Combine all individuals into a single list with an ID marker
all_individuals <- list(
    list(id = 1, names = individuals),
    list(id = 2, names = more_individuals),
    list(id = 3, names = small_group)
)

# Convert the list to a dataframe
individuals_df <- do.call(rbind, lapply(all_individuals, function(group) {
    data.frame(
        id = group$id,
        t(sapply(1:5, function(i) ifelse(i <= length(group$names), group$names[i], NA)))
    )
}))

# Rename columns
colnames(individuals_df) <- c("id", paste0("name", 1:5))
print ("INDIVIDUALS" )
print(individuals_df)


#need to change this such that 1) there is a look up to see if the individual is in the group
#2) the dataframe of simulated survey ratings will miss certain raters who should have filled one in (in which case dummies have to b created)
#3) maybe with not every ratee included in a some raters ratings (in which case their ratings are invalid and replaced with 3s)
#4) the output at the end will need to have weightings per participant as well as data about whether they filled in the form correctly or at all







# Create a dataframe where each row involves a rating by one of the individuals of each of the others (including themselves)
ratings <- data.frame(
    groupId = integer(),
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
generate_ratings <- function(individuals, groupId) {

    for (rater in individuals) {
        ratees <- sample(individuals)  # Randomize the order of ratees
        if (rater %in% ratees) {    
            ratings <<- rbind(ratings, data.frame(
                groupId = groupId,
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
}


# Process each group based on groupId
unique_group_ids <- unique(individuals_df$id)

for (group_id in unique_group_ids) {
    group_individuals <- individuals_df %>% filter(id == group_id) %>% select(-id) %>% unlist() %>% na.omit() %>% as.character()
    generate_ratings(group_individuals, group_id)
}


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


