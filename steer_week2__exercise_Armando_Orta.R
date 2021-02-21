# --- Exercise 2 - Data wrangling with R --- #

# Tasks to complete:
# 1. Import the Good Relations dataset from 'grel.sav' file. Note this file
# is in SPSS file format, therefore you need to use a specific package 
# and/or method to import it.

setwd("C:/Users/AOrta/Documents/00 Steer R Training/week2/data")
library(Hmisc)
Good_Relations <- spss.get("grel.sav")

# 2. Check the structure and dimensionality of this dataset.
# What functions did you use?
str(Good_Relations) #To check the dataset structure
dim(Good_Relations) #To check for dimensionality of dataset
names(Good_Relations) #To check for column names

# How many columns and rows does this dataset contain?
# The dataset contains 133 columns, 1204 rows

# Print the first 10 rows of this dataset. 
head(Good_Relations, n=10)


# 3. Check how many missing values are ...: 
# a.) in the entire dataset
sum(is.na(Good_Relations))
#13,927 values

# b.) in each column of this dataset
sapply(Good_Relations, function(x) sum(is.na(x)))

# 4. How many occurrences of value 'Self-employed' can you find in this dataset?
which (Good_Relations == "Self-employed", arr.ind=TRUE)
 #128 occurrences

# Where are these values located in terms of their row/column indices
 # See printed list from previous snippet
 

# Are they located only in one specific column? If yes, which one?
 #Yes, column 41
 
# What is/are the name(s) of the column(s) in which these values occur?
 names(Good_Relations)[41]
 #"Work"

# 5. What are the unique values within the 'ruhappy' column?
unique(Good_Relations$ruhappy)

# How are those values coded (do they have numeric or character values)?
str(Good_Relations$ruhappy)
table (Good_Relations$ruhappy)
# They are factors with numeric and character values



# What is the class and mode of this variable? What does it mean?
class(Good_Relations$ruhappy)

library(modeest)  
mlv(as.numeric(Good_Relations$ruhappy), method="mfv", na.rm=TRUE)
#You have to estimate your mlv function using the numeric values of the variable. If not it will not show the mode


# 6. Create a subset called "subset1" with only the following columns:
# - "rage", "rsex", "nadult", "nkids", "highqual", "work", "ruhappy".
# How did you do it?
subset1 <- Good_Relations %>% 
  select (rage, rsex, nadult, nkids, highqual, work, ruhappy)

# Print the structure of this subset.
str(subset1)

# 7. Using 'subset1' object created in Task 6, convert "nadult" and "nkids" columns
# into numeric type. Do not create a new object, simply overwrite the existing 
# two variables with their numeric types within the 'subset1' data frame.
# Check the structure of 'subset1' after the convertion of these 2 variables
# to confirm they have been altered 
subset1$nadult <- as.numeric(subset1$nadult)  
subset1$nkids <- as.numeric(subset1$nkids)  
  #I am sure there is a way to do this in one snippet!
str(subset1)

# 8. Using 'subset1' object processed in Task 7, create a subset called 'subset2'
# where 'rsex' is equal to 'Female', and 'nkids' is greater than 2 and smaller or equal to 5.
subset2 <- subset1 %>% filter( rsex == 'Female', nkids >2 & nkids <= 5) 

# How many rows does 'subset2' contain?
str(subset2)
  #98

# 9. Using 'subset1' object processed in Task 7, create a subset called 'subset3'
# where 'work' is equal to either 'Employee' or 'Manager' and 'ruhappy' is 
# equal to either 'Fairly happy' or 'Very happy'.
# Additionally, select only the following variables: 'work', 'ruhappy', 'rage'.
subset3 <- subset1 %>% 
          filter (work == 'Employee' | work == 'Manager', ruhappy == 'Fairly happy' | ruhappy == 'Very happy') %>% 
              select (work, ruhappy,rage)

# How many rows and how many individual values does 'subset3' contain?
str (subset3)
unique (subset3)
 #584 rows
#568 unique values

# 10. Export 'subset3' created in Task 9 into 'subset3.csv' file. 
# How did you do it?

write_csv(subset3, file = "subset3.csv", col_names = TRUE )

# --- The End of Exercise 1 --- #
