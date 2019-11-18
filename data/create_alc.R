# Pekka Päärni

# 11.11.2019

#Data wrangling Exercise set 3

#https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(dplyr)

#First let's read and check structure and dimensions of the data 

student_mat <- read.csv("~/IODS-project/data/student-mat.csv", header = T, sep = ";")

str(student_mat)

dim(student_mat)

summary(student_mat)

#
student_por <- read.csv("~/IODS-project/data/student-por.csv", header = T, sep = ";")

str(student_por)

dim(student_por)

summary(student_por)

#Now we join the two datasets with variables "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet"

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
math_por <- inner_join(student_mat, student_por, by = join_by, suffix = c(".math", ".por"))

colnames(math_por)

str(math_por)

dim(math_por)

summary(math_por)


# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(student_mat)[!colnames(student_mat) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data

glimpse(alc)


# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

glimpse(alc)

setwd("~/IODS-project/data")

write.csv(alc, file = "alc.csv")

