set.seed(23)

#create data frame
df <- data.frame(x=c(3, 5, 6, 6, 8, 12, 14),
                 y=c(12, 6, 4, 23, 25, 8, 9),
                 z=c(2, 7, 8, 8, 15, 17, 29))

#select random sample of three rows from data frame
rand_df <- df[sample(nrow(df), size=3), ]

#display randomly selected rows
rand_df

