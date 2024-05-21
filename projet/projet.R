
# **********************************************************************************
# ***********           Part 1 : Preliminary analysis       ************************
# **********************************************************************************

# Load dataset
data <- read.csv("./abalone_data.csv",sep=",")

# ==================================================================================
# 1/ how many observations abalones are described ? How many variables are there ?
# ==================================================================================
# numbers of observations abalones
len <- nrow(data) -1

# variables
data_variables <- colnames(data)
print(data_variables)
# ==================================================================================
# 2/ Are there any missing values in the dataset ?
# ==================================================================================
has_missing_values <- any(is.na(data))
print( paste("missing values ? ", has_missing_values))


# ==================================================================================
# 3/ Calculate descriptive statistics for all the variables
# ==================================================================================
# data's summary
data_summary <-
print(summary(data))

# Graphic for "Sex" column
Sex_category <- unique(data$Sex)
Sex_quantity <-table(data$Sex)
barplot(Sex_quantity, names.arg = Sex_category, 
        xlab = "Category", ylab = "Quantity",
        main = "Sex in dataset")


# **********************************************************************************
# ******************       Part 2 : PCA        *************************************
# **********************************************************************************

# ==================== Theoretical question ========================================
# 1/ If two variables are perfectly correlated in the dataset, would it be suitable to
#include both of them in the analysis when performing PCA? Justify your answer. In contrast,
#what if the variables are completely uncorrelated?
# ==================================================================================

# ===================== Practical questions =========================================
# 1/ Calculate the variance of each variable and interpret the results. Do you think
# it is necessary to standardize the variables before performing PCA for this dataset ? Why ?
# ==================================================================================

# data's standard deviation
numeric_data <- data[,2:length(data_variables)]

cols_var <- sapply(numeric_data,var)
print(cols_var)
#standardization would be necessary before doing PCA on the dataset because it can maximize 
#the variance along the principal components and will reduce the importance of large variances 
#from secondary components

# ==================================================================================
# 2/  Perform PCA using the appropriate function with the appropriate arguments
#and options considering your answer to the previous question. Analyze the output of the function. 
#Interpret the values of the two first principal component loading vectors.
# ==================================================================================

#standardize the data for PCA
norm_numeric_data <- as.data.frame(scale(numeric_data))
summary(norm_numeric_data)

#Perform PCA on standardized variables of the dataset:
pca_result <- prcomp(norm_numeric_data)
summary(pca_result)

head(pca_result)
screeplot(pca_result, type = "lines", main = "Scree Plot")
#biplot(pca_result, scale = 0)
#interpret

# ==================================================================================
# 3/  Calculate the percentage of variance explained (PVE) by each component?
#Plot the PVE explained by each component, as well as the cumulative PVE. 
#How many components would you keep? Why?
# ==================================================================================

# Extract the standard deviations of the PCs
sd <- pca_result$sd

# Calculate the variance explained by each PC
var_explained <- sd^2

# Calculate the proportion of variance explained
proportion_var_explained <- var_explained / sum(variance_explained)

# Convert to percentage
percentage_var_explained <- proportion_var_explained * 100

# View the percentage of variance explained by each PC
percentage_var_explained

# Calculate cumulative percentage of variance explained
cumul_percentage_var_explained <- cumsum(percentage_var_explained)

par(mfrow = c(2, 1))
barplot(percentage_var_explained, type = "o", main = "Percentage of Variance Explained by each PC",
     xlab = "Principal Components", ylab = "Percentage of Variance Explained", 
     ylim = c(0, 100), col = "blue", pch = 16)
plot(cumul_percentage_var_explained, type = "o", main = "Cumulative Percentage of Variance Explained",
     xlab = "Principal Components", ylab = "Cumulative Percentage of Variance Explained", 
     ylim = c(0, 100), col = "red", pch = 16)

# ==================================================================================
# 4/  Use a biplot with a correlation circle to display both the principal component
#scores and the loading vectors in a single plot. Interpret the results.
# ==================================================================================

install.packages("factoextra")
library(factoextra)

# Create a basic biplot
biplot(pca_result, scale = 0, main = "PCA Biplot with Correlation Circle")

# Add a correlation circle
# Calculate the radius of the correlation circle
circle_radius <- sqrt(2)

# Draw the circle
symbols(0, 0, circles = circle_radius, inches = FALSE, add = TRUE, fg = "blue")

# Add axis lines
abline(h = 0, v = 0, col = "gray", lty = 2)

#Xstd = (X-matrix(rep(mean,4), nrow=4,byrow =T))/matrix(rep(sd,4),nrow=4,byrow=T)
