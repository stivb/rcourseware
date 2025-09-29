# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(456)

# Define individuals
individuals <- c("Alice", "Bob", "Charlie", "David", "Eve")

# Create a dataframe with random values between 1 and 5
peerEval <- data.frame(matrix(sample(1:5, 25, replace = TRUE), nrow = 5, ncol = 5))

# Set column and row names
colnames(peerEval) <- individuals
rownames(peerEval) <- individuals

# Print the dataframe

# Set diagonal values to 5
diag(peerEval) <- 5

# Print the modified dataframe
print("Here is a matrix of peer evaluations - note everyone has given themselves maximum 5/5 but the others are random")
print("Each row represents a rating given BY the named individual on the left TO the named individual on the top")
print(peerEval)

# Calculate the sum of all values in the dataframe


avg_sum <-sum(peerEval) / 5

# Calculate the sum of each column

col_sums <- colSums(peerEval)

# Create a new dataframe with column sums and use individuals as the row names
col_sums_df <- data.frame(col_sums)


rownames(col_sums_df) <- individuals



# Create a new dataframe with values divided by avg_sum
factor_df <- col_sums_df / avg_sum

print("Here are the weightings we will apply to the work of each individual based on the average of all the scores")    

# Print the new dataframe
print(factor_df)


print("Now we will create a new dataframe with the following values for two geeks, one lazy person and two wastemen")

df_two_geeks_1_lazy_2_wastemen <- data.frame(matrix(c
                                   (5, 5, 3, 1, 1,  # ratings given by alice
                                    5, 5, 3, 1, 1,  # ratings given by bob
                                    4, 4, 4, 3, 3,  # ratings given by charlie
                                    5, 5, 5, 5, 5,  # ratings given by david
                                    5, 5, 5, 5, 5),  # ratings given by eve
                                    nrow = 5, ncol = 5))
df_two_geeks_1_lazy_2_wastemen <- t(df_two_geeks_1_lazy_2_wastemen)

avg_sum <-sum(df_two_geeks_1_lazy_2_wastemen ) / 5

individuals <- c("Geek1", "Geek2", "Lazy1", "Wasteman1", "Wasteman2")

colnames(df_two_geeks_1_lazy_2_wastemen) <- individuals    
rownames(df_two_geeks_1_lazy_2_wastemen) <- individuals

print(df_two_geeks_1_lazy_2_wastemen) 



# Calculate the sum of each column

col_sums <- colSums(df_two_geeks_1_lazy_2_wastemen)
col_sums_df <- data.frame(col_sums)
rownames(col_sums_df) <- individuals



# Create a new dataframe with values divided by avg_sum
factor_df <- col_sums_df / avg_sum

# Print the new dataframe
print("Here are the weightings we will apply to the work of each individual based on the average of all the scores")    
print(factor_df)




