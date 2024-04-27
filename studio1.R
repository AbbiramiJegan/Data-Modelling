# Q.1 Implement a loop that prints out all the odd numbers from 1 to 15.
# Solution 1:
for (i in 1:15)
{
  if (i %% 2 == 1)
  {
    cat(i,"\n")
  }
}

# Q.2 Write a function that takes an argument n, calculates the factorial of n, and returns it.
myfactorial <- function(n)
{
  # Error checking -- must be integer >= 0
  if (n < 0 || floor(n) != n)
  {
    stop("n must be non-negative integer")
  }
  # Base-case
  if (n == 0)
  {
    return(1)
  }
  else
  {
    return(n*myfactorial(n-1))
  }
}

# Q.3 Write a function that takes a vector of numbers and returns the minimum and maximum number,
# and the difference between the maximum and minimum (i.e., the range) of the vector.
# (hint: the length() function lets you find the length of a vector).
# (hint: remember to return multiple return values using a list as described above).

findminmax <- function(x)
{
  # Error checking
  if (!is.numeric(x) || !is.vector(x))
  {
    stop("Input must be a numeric vector")
  }
  
  retval = list()
  retval$min = Inf
  retval$max = -Inf
  for (i in 1:length(x))
  {
    if (x[i] < retval$min)
    {
      retval$min = x[i]
    }
    if (x[i] > retval$max)
    {
      retval$max = x[i]
    }
  }
  
  # compute the range
  retval$rng = retval$max - retval$min
  
  
  return(retval)
}
findminmax(c(4,3,10,33,-2,8))

# Q.4 Create a pie chart for each variable to show the relative frequencies of the values

mush = read.csv("Mushroom.csv",header=T,stringsAsFactors=T)
tab = table(mush$cap.shape)
pie(tab)

# List of categorical column names to analyze
#categorical_cols <- c("cap.shape", "cap.surface", "cap.color", "odor", "population", "habitat")
categorical_cols <- c("cap.shape", "cap.surface")

# Function to generate and plot pie charts for each column
generate_pie_chart <- function(column) {
  tab <- table(mush[[column]])
  pie(tab, main = column, col = rainbow(length(tab)))
}

# Setting up the layout for multiple pie charts in a grid
par(mfrow = c(2, 3))  # Change the values (2, 3) to adjust the grid layout

# Loop through the list and generate pie charts for each column
for (col in categorical_cols) {
  generate_pie_chart(col)
}

# Q.5 Cross-tabulate the mush$class variable against each of the other variables. Which variable
# appears most strong associated with class?

mush = read.csv("Mushroom.csv",header=T,stringsAsFactors=T)

## class vs cap.shape
table(mush$class,mush$cap.shape)
# having a bell shape seems to be associated with less poisonous varieties, having a knobbed shape
# appears to be associated with more poisonous varieties

## class vs cap.surface
table(mush$class,mush$cap.surface)
# some mild association between fibrous and the class, and being smooth and the class

## class vs. cap.color
table(mush$class,mush$cap.color)
# again, some mild association as some categories (i.e. brown, gray) seem to lead to less poisonous 
# varieties but others (buff, yellow) lead to more poisonous varieties

## class vs. odor
table(mush$class, mush$odor)
# This variable seems very, very strong associated with class, as mushrooms are either (almost) all edible
# or all poisonous depending on the type of odor. Knowing the odor would lead to a very good prediction
# of the mushroom's class

## class vs population
table(mush$class, mush$population)
# again, some reasonably strong changes in distribution associated with some of the categories

## class vs habitat
table(mush$class, mush$habitat)
# some habitats quite predictive of class, for example, growing along paths or in urban areas
# you would believe a mushroom to be poisonous, while growing in the meadows suggests
# its much more likely to be edible

# In general most of the variables show some association with class, but odor is
# by far the strongest associated variable

##################### Using For Loop ######################

# List of categorical variables to analyze
categorical_vars <- c("cap.shape", "cap.surface", "cap.color", "odor", "population", "habitat")

# Function to generate and print tables for each variable
generate_tables <- function(variable) {
  cat("Table for class vs.", variable, ":\n")
  tbl <- table(mush$class, mush[[variable]])
  print(tbl)
  cat("\n")
}

# Loop through the list and generate tables for each variable
for (var in categorical_vars) {
  generate_tables(var)
}

##################################################################################

# Q.6 Write a loop that computes the correlation coefficient between each of the wine attributes and
# the wine quality variable. Your loop should display the information to the console in the form
# Correlation between fixed.acidity and quality = -0.1136628
# for each of the variables. (hint: you can use the names(wine)[i] to access the name of the i-th
# variable in a dataset).

wine <- read.csv("wine.csv")
wine_corr = c()
for (i in 1:11)
{
  wine_corr[i] = cor(wine[,i], wine$quality)
  cat("Correlation between", names(wine)[i], "and quality = ", wine_corr[i], "\n")
}

cat("Variable with maximum correlation is:", names(wine)[which.max(abs(wine_corr))], "\n")