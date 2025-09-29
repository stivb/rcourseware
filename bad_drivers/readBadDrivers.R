library(readr)
setwd(dirname(normalizePath(sys.frame(1)$ofile)))
# Print the initial column names before renaming
bad_drivers <- read_csv("bad-drivers.csv")
print(colnames(bad_drivers))
print(bad_drivers$`Number of drivers involved in fatal collisions per billion miles`)
#colnames(bad_drivers) <- c("State", "nDeadDriversPerBillMiles", "nDeadAlcohol", "nDeadSpeeding", "nDeadDistracted", "nDeadFirstAccident", "premiums", "lossesPerDriver")

# Read in the CSV file using the first row as column names
bad_drivers <- read_csv("bad-drivers.csv")
colnames(bad_drivers)[2] <- "nDeadDriversPerBillMiles"
colnames(bad_drivers)[3] <- "pcDeadSpeeding"
colnames(bad_drivers)[4] <- "pcDeadAlcohol"
colnames(bad_drivers)[5] <- "pcDeadUndistracted"
colnames(bad_drivers)[6] <- "pcDeadFirstAccident"
colnames(bad_drivers)[7] <- "premiums"
colnames(bad_drivers)[8] <- "lossesPerDriver"
# Draw a barplot of all the values in bad_drivers$pcDeadAlcohol in ascending order
# Order the values and corresponding states
ordered_indices <- order(bad_drivers$pcDeadAlcohol)
ordered_values <- bad_drivers$pcDeadAlcohol[ordered_indices]
ordered_states <- bad_drivers$State[ordered_indices]

# Draw the barplot
bar_positions <- barplot(ordered_values,
    names.arg = ordered_states,
    las = 2,
    main = "Percentage of Fatal Collisions Involving Alcohol (Ascending)",
    ylab = "pcDeadAlcohol",
    col = "lightcoral",
    border = "black")

# Calculate statistics
q1 <- quantile(ordered_values, 0.25)
mean_val <- mean(ordered_values)
q3 <- quantile(ordered_values, 0.75)

# Add lines for quartiles and mean
abline(h = q1, col = "blue", lty = 2, lwd = 2)
abline(h = mean_val, col = "darkgreen", lty = 2, lwd = 2)
abline(h = q3, col = "purple", lty = 2, lwd = 2)

# Add legend
legend("topleft",
       legend = c("1st Quartile", "Mean", "3rd Quartile"),
       col = c("blue", "darkgreen", "purple"),
       lty = 2,
       lwd = 2,
       bty = "n")


print(colnames(bad_drivers))

# Print the first few rows to verify
print(head(bad_drivers,10))

# Find the state with the lowest nDeadDriversPerBillMiles
min_state <- bad_drivers$State[which.min(bad_drivers$nDeadDriversPerBillMiles)]
cat("State with lowest nDeadDriversPerBillMiles:", min_state, "\n")

# Find the state with the highest nDeadDriversPerBillMiles
max_state <- bad_drivers$State[which.max(bad_drivers$nDeadDriversPerBillMiles)]
cat("State with highest nDeadDriversPerBillMiles:", max_state, "\n")

# Find the state with the lowest premiums
min_premium_state <- bad_drivers$State[which.min(bad_drivers$premiums)]
cat("State with lowest premiums:", min_premium_state, "\n")

# Find the state with the highest premiums
max_premium_state <- bad_drivers$State[which.max(bad_drivers$premiums)]
cat("State with highest premiums:", max_premium_state, "\n")
# Find the state with the lowest lossesPerDriver
min_losses_state <- bad_drivers$State[which.min(bad_drivers$lossesPerDriver)]
cat("State with lowest lossesPerDriver:", min_losses_state, "\n")           
# Find the state with the highest lossesPerDriver
max_losses_state <- bad_drivers$State[which.max(bad_drivers$lossesPerDriver)]
cat("State with highest lossesPerDriver:", max_losses_state, "\n")
# Draw a histogram of the nDeadDriversPerBillMiles column
hist(bad_drivers$nDeadDriversPerBillMiles,
    main = "Histogram of nDeadDriversPerBillMiles",
    xlab = "nDeadDriversPerBillMiles",
    col = "lightblue",
    border = "black")


