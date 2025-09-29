import pandas as pd

# Load the mtcars dataset
mtcars = pd.read_csv('https://raw.githubusercontent.com/selva86/datasets/master/mtcars.csv')

# Remove columns with less than 5 distinct values
filtered_mtcars = mtcars.loc[:, mtcars.nunique() >= 5]

print(filtered_mtcars)