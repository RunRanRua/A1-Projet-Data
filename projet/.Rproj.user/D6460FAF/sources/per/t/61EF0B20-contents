
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

# ==================================================================================
# 1/ Calculate the variance of each variable and interpret the results. Do you think
# it is necessary to standardize the variables before performing PCA for this dataset ? Why ?
  
# ==================================================================================
# data's standard deviation
numeric_data <- data[,2:length(data_variables)]

cols_var <- sapply(numeric_data,var)
print(cols_var)



#Xstd = (X-matrix(rep(mean,4), nrow=4,byrow =T))/matrix(rep(sd,4),nrow=4,byrow=T)



