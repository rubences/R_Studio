############################################################
# BASIC R SCRIPT
############################################################

############################################################
# 1. COMMENTS
############################################################

# Anything after # is a comment.
# Comments are ignored by R and are used to explain the code.


############################################################
# 2. VARIABLES
############################################################

# Create variables
x <- 10
y <- 5

# Print values
x
y

# Basic operations
x + y      # addition
x - y      # subtraction
x * y      # multiplication
x / y      # division
x^2        # power


############################################################
# 3. DATA TYPES
############################################################

# Numeric
a <- 3.14

# Character (text)
name <- "Bioinformatica"

# Logical (TRUE/FALSE)
is_student <- TRUE

# Check data type
class(a)
class(name)
class(is_student)


############################################################
# 4. VECTORS
############################################################

# A vector is a collection of values of the same type

numbers <- c(1,2,3,4,5)

numbers

# Access elements
numbers[1]     # first element
numbers[3]     # third element

# Vector operations
numbers * 2
numbers + 10

# Mean value
mean(numbers)

# Length
length(numbers)


############################################################
# 5. MATRICES
############################################################

# Create a matrix with 2 rows and 3 columns

mat <- matrix(c(1,2,3,4,5,6),
              nrow = 2,
              ncol = 3)

mat

# Access elements
mat[1,2]   # row 1 column 2


############################################################
# 6. DATA FRAMES
############################################################

# Data frames are tables (like Excel)

data <- data.frame(
  gene = c("TP53","BRCA1","EGFR","MYC"),
  expression = c(5.3, 8.1, 2.4, 6.7),
  mutated = c(TRUE, FALSE, FALSE, TRUE)
)

data

# View structure
str(data)

# Access columns
data$gene
data$expression

# Filter rows
data[data$expression > 5, ]


############################################################
# 7. BASIC STATISTICS
############################################################

mean(data$expression)
median(data$expression)
sd(data$expression)


############################################################
# 8. INSTALL AND LOAD LIBRARIES
############################################################

# Install a package (only once)
# install.packages("ggplot2")

# Load library
library(ggplot2)


############################################################
# 9. BASIC PLOT
############################################################

# Simple scatter plot
plot(data$expression,
     main="Gene expression values",
     ylab="Expression",
     xlab="Gene index")


############################################################
# 10. PLOT WITH GGPLOT2
############################################################

ggplot(data, aes(x = gene, y = expression)) +
  geom_bar(stat="identity") +
  ggtitle("Gene Expression") +
  xlab("Gene") +
  ylab("Expression")


############################################################
# 11. IMPORT DATA
############################################################

# Read a CSV file
# (file must be in working directory)

# dataset <- read.csv("data.csv")

# Check first rows
# head(dataset)


############################################################
# 12. EXPORT DATA
############################################################

# Write a dataframe to CSV

# write.csv(data, "output.csv", row.names = FALSE)


############################################################
# 13. WORKING DIRECTORY
############################################################

# Show current directory
getwd()

# Change directory
# setwd("C:/your_folder")

