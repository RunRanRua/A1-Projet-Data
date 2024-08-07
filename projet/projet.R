
# **********************************************************************************
# ***********           Part 1 : Preliminary analysis       ************************
# **********************************************************************************
# Load dataset
data <- read.csv("./abalone_data.csv",sep=",")
# ==================================================================================
# 1/ how many observations abalones are described ? How many variables are there ?
# ==================================================================================
# numbers of observations abalones
len <- nrow(data)

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
# Quantitatif variable
summary(data)
# Qualitatif variable
Sex_quantity <-table(data$Sex)
print(Sex_quantity)

# ---------------------------------------------------------------------------------
# General Graphic
par(mfrow=c(2,4))
for(i in 2:length(data_variables)){
  mean_v <- mean(data[,i])
  sd_v <- round(sd(data[,i]), digits=2)
  
  boxplot(data[,i], 
          main= data_variables[i], 
          xlab= paste("sd =", sd_v, sep = " "), cex.lab = 1.5,
          col="lightblue",
          outline = TRUE)
  abline(h=mean_v,  col = "red", lty = 2, lwd = 2) # mean line
  
  stats <- summary(data[,i])
  text(1, stats[1], paste("Min =", stats[1]), pos = 3, offset = 0.5, col = "purple",cex=1.3)
  text(0.8, stats[2], paste("Q1 =", stats[2]), pos = 3, offset = -1, col = "blue",cex=1.3)
  text(1, stats[3], paste("Median =", stats[3]), pos = 3, offset = 0.5, col = "blue",cex=1.3)
  text(1.3, stats[4], paste("Mean =", round(mean_v, digits=2)), pos = 1, offset = 0.5, col = "red",cex=1.3)
  text(0.8, stats[5], paste("Q3 =", stats[5]), pos = 3, offset = 1, col = "blue",cex=1.3)
  text(1, stats[6], paste("Max =", stats[6]), pos = 3, offset = -0.8, col = "purple",cex=1.3)
  
}

# Graphic with sex
par(mfrow=c(2,4))
for(i in 2:length(data_variables)){
  means_sex <- tapply(data[,i], data$Sex, mean)
  SDs_sex <- round( tapply(data[,i],data$Sex,sd), digits = 2)
  
  boxplot(data[,i] ~ data$Sex, 
          main= data_variables[i],
          xlab = paste(SDs_sex[1], SDs_sex[2], SDs_sex[3], sep = " | "),
          col="lightblue")
  
  
  # means
  for(j in 1:length(means_sex)){
    lines(c(j-0.5, j+0.5), c(means_sex[j], means_sex[j]), col = "red", lty = 2, lwd = 2)
  }
}


# **********************************************************************************
# ******************       Part 2 : PCA        *************************************
# **********************************************************************************

# ==================================================================================
# 1/ Varianc of each variable  +  interpret + need Standardize before PCA?
# ==================================================================================
# Numeric data
numeric_data <- data[,2:length(data_variables)]
# ---------------------------------------------------------------------------------
# Variances
vars <- round( sapply(numeric_data,var), digits=3 )
# ---------------------------------------------------------------------------------
# interpretation
var_sd <- round( sd(vars), digits = 2)

# graph
par(mfrow=c(1,1))
barplot_vars <- barplot(vars, main = "Variances for each vairable",
                       xlab = "Variables", ylab = "Variance",
                       col = "skyblue", border = "white")
text(x=barplot_vars, y=vars, label=vars, pos=3, col="black")
legend("topleft",
       legend=c(paste(names(vars),vars,sep=" : "), paste("standard deviation", var_sd, sep=" : ")),
       title="result")

# ==================================================================================
# 2/  Perform PCA using the appropriate function with the appropriate arguments
# and options considering your answer to the previous question.
# Analyze the output of the function.
# Interpret the values of the two first principal component loading vectors.
# ==================================================================================
# Standardize dataset + Perform PCA
pca_result <- prcomp(numeric_data, scale. = TRUE)
pca_result
names(pca_result)
loadings1_2 <- pca_result$rotation[,1:2] # PC1-2 loading vectors
loadings1_2[,1]

# Graph
plot(loadings1_2[,1], loadings1_2[,2], 
     xlab = "PC1 loadings", xlim = c(0,0.5),
     ylab="PC2 loadings", ylim= c(-1,0.4),
     main="Loadings Plot for PC1 & PC2")
grid()
text(loadings1_2[,1], loadings1_2[,2], labels=rownames(loadings1_2),pos=4,col='red')
# ==================================================================================
# 3/  Calculate the percentage of variance explained (PVE) by each component?
#Plot the PVE explained by each component, as well as the cumulative PVE. 
#How many components would you keep? Why?
# ==================================================================================
# Summary + PVE + cumultative PVE
pca_summary_info <-summary(pca_result)
pca_summary_info
# Calcul : pve <- pca_result$sdev^2/sum(pca_result$sdev^2)
pve <- round(pca_summary_info$importance[2,] * 100, digits=2)
cpve <- round(pca_summary_info$importance[3,]*100, digits=2)

# Graphs
plot(pve, 
     xlab = "PC", 
     ylab="PVE (%)", ylim=c(0,100), 
     type="b", col = "blue")
text(x=1:length(pve), y= pve, labels=paste(pve," %"), pos=3, col="blue")

plot(cumsum(pve), 
     xlab="PC", 
     ylab="Cumulative PVE (%)", ylim=c(0,100), 
     type="b", col = "red")
text(x=1:length(pve), y= cpve, labels=paste(cpve," %"), pos=1, col="red")


# ==================================================================================
# 4/ Use a biplot with a correlation circle to display both the principal component
#scores and the loading vectors in a single plot. Interpret the results.
# ==================================================================================
# Graph
# 1st Way
# biplot(pca_result,scale=0)
# symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "blue")

# 2nd Way : make graph better
library(ggplot2) #install.packages('ggplot2')
library(ggforce) #install.packages('ggforce')

plotdata <- as.data.frame(pca_result$x[,1:2]) #  PC1/PC2's data
plotdata$abalone <- rownames(plotdata)  # add names
rotdata <- as.data.frame(pca_result$rotation[,1:2]) # PC1/PC2's rotation
rotdata$variables <- rownames(rotdata) # add names


ggplot() +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) + # remove grid
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + # horizontal line
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed") + # vertical line
  geom_point(data = plotdata, aes(x = PC1, y = PC2), size = 1) + # data represented as points
  scale_y_continuous(sec.axis = sec_axis(~./6)) + scale_x_continuous(sec.axis = sec_axis(~./10)) + # 2nd axis
  geom_segment(data = rotdata,aes(x=0, xend= PC1*10, y=0, yend= PC2*6), arrow = arrow(length = unit(0.03, "npc")), colour = 'red') +   # vectors
  geom_text(data = rotdata, aes(x = PC1*10.4, y = PC2*6.4, label = variables), size = 4, colour = 'red') + # add vector names
  geom_circle(aes(x0=0, y0=0, r=7), color="blue", linewidth=1)


# **********************************************************************************
# ******************       Part 3 : Linear Regression        ***********************
# **********************************************************************************
# new column : age = 1.5 + Card(rings)
data$age <-1.5+data$Rings


# ==================================================================================
# SIMPLE LINEAR REGRESSION
# ==================================================================================
# Calculate correlation coefficient : r --------------------------------------------
all_r <- data[, !names(data) %in% c('Rings','Sex')] # remove ring column
all_r <- data.frame(cor(all_r))   # get all coefficient
r_age <- all_r$age[1:length(all_r)-1] # obtain data
r_age

# Graph to analyze ----------------------------------------------------------------
r_barplot <- barplot(r_age, names.arg = rownames(all_r)[1:length(all_r)-1], 
                     main="correlation coefficient between Age and each of the other variables",
                     xlab="variables", ylab="coefficient")
text(x = r_barplot, y=r_age, label=round(r_age, digits=2),pos=1 )


# Fit the Simple Regression Linear model ----------------------------------------
Y <- data$age    # target variable
X <- data$Shell.weight  # feature (most correlated)
model_simple <- lm(Y ~ X)  # model

summary(model_simple) # description

# get coefficient estimate : Y = beta1 * X + beta0 + eps -------------------------
coefficients <- coef(model_simple)
beta0 <- coefficients[1] # intercep or b in (Y = aX+b) [the starting point in y axis]
beta1 <- coefficients[2] # slope or a in (Y = aX+b)  [pente]

beta0 # 7.962117
beta1 # 13.53568 >0 so proportional between X and Y


# Model
plot(data$Shell.weight,data$age, main="Simple Linear Regression", xlab="Shell_Weight", ylab="Age")
abline(model_simple, col="red",lwd=3)


# 95% Confidence Interval -------------------------------------------------------
se_beta1 <- summary(model_simple)$coefficients["X","Std. Error"] # standard error of beta1
alpha <- 0.05
df <- model_simple$df.residual # degree of freedom
critical_value <- qt(1-alpha/2, df=df) # t_alpha/2
margin_err <- critical_value * se_beta1

interval_95 <- c(beta1 - margin_err, beta1 + margin_err)
interval_95

# zero slope hypothesis test ----------------------------------------------------
t_stat <- beta1 / se_beta1 # t-statistic
t_stat
p <-2*pt(abs(t_stat), df, lower.tail = FALSE) # ~0 so reject hypothesis

# coefficient of determination R^2
R2 <- round( all_r["Shell.weight","age"]^2, digits=2)
R2

# ==================================================================================
# MULTIPLE LINEAIR REGRESSION
# ==================================================================================
# 1 best subset selection --------------------------------------------
library(leaps)

# Graph : adj_R2
best_subset <- regsubsets(age~Length+Diameter+Height+Whole.weight+Shucked.weight+Viscera.weight+Shell.weight,
                          data=data,nvmax=4)
adj_R2 <- summary(best_subset)$adjr2
bar_adjR2 <-barplot(adj_R2, names.arg = 1:4,xlab="number of variables",ylab="adjusted R^2", main="adjusted R^2 visualization")
text(x=bar_adjR2, y=adj_R2,label=round(adj_R2, digits=2),pos=1,col="red")


# get best model
best_model <-which.max(adj_R2)
best_model

# 4 -----------------------------------------------------------------------------
# coef of estimates
best_model_coef <- coef(best_subset,best_model)
best_model_coef


# model

# coef of determination R2
R2 <- summary(best_subset)$rsq[best_model]
R2

# 5 -----------------------------------------------------------------------------
# multiple linear regression model
multiple_model <- lm(age~Diameter+Whole.weight+Shucked.weight+Shell.weight,data=data)

# p-values
p_values_coef <- summary(multiple_model)$coefficients[,4]
p_values_coef

# F-test
ssr <- sum((multiple_model$fitted.values - mean(multiple_model$fitted.values))^2)
sse <- sum(multiple_model$residuals^2)

df_ssr <- length(multiple_model$coefficients) -1
df_sse <- length(multiple_model$residuals) - length(multiple_model$coefficients)

f_stat_multiple <- (ssr/df_ssr) / (sse / df_sse)
f_stat_multiple # 1107.491 >1

# ==================================================================================
# MULTIPLE LINEAIR REGRESSION WITH A QUALITATIVE VARIABLE (BONUS)
# ==================================================================================

# ==================================================================================
# 1/ How many observations are in each category of the variable Sex ?
# ==================================================================================

# Get the count of each category in the "Sex" variable
sex_counts <- table(data$Sex)

# Print the counts
print(sex_counts)

# ==================================================================================
# 2/ Plot a boxplot of the target variable Age versus the Sex. Comment on the output
# ==================================================================================

boxplot(age ~ Sex, data = data, 
        main = "Boxplot of Age by Sex",
        xlab = "Sex",
        ylab = "Age",
        col = c("lightblue", "lightpink"))

# ==================================================================================
# 3/ Perform multiple linear regression by adding the Sex as explanatory variable
# to the model selected in the previous section. Interpret the coefficient estimates of the variable
# Sex (for each category),perform the zero slope test and conclude.
# ==================================================================================

# Fit the model including the Sex variable
multiple_model_with_sex <- lm(age ~ Diameter + Whole.weight + Shucked.weight + Shell.weight + Sex, data = data)

# Summarize the model
summary(multiple_model_with_sex)

#Coefficients for Sex:
#  SexI (-0.81997): Infants are estimated to have an age 0.81997 years less than the reference category (Females). 
#  SexM (0.07133): Males are estimated to have an age 0.07133 years more than the reference category (Females)

# Zero Slope Test for Sex
# To determine the significance of the Sex variable, you look at the p-values:
#   SexI: The p-value is 2.15e-15, which is highly significant, indicating that the age difference between Infants and Females is significant.
#   SexM: The p-value is 0.398, which is not significant, indicating that the age difference between Males and Females is not significant.

# F-test
ssr_w_sex <- sum((multiple_model_with_sex$fitted.values - mean(multiple_model_with_sex$fitted.values))^2)
sse_w_sex <- sum(multiple_model_with_sex$residuals^2)

df_ssr_w_sex <- length(multiple_model_with_sex$coefficients) -1
df_sse_w_sex <- length(multiple_model_with_sex$residuals) - length(multiple_model_with_sex$coefficients)

f_stat_multiple_w_sex <- (ssr_w_sex/df_ssr_w_sex) / (sse_w_sex / df_sse_w_sex)
f_stat_multiple_w_sex 

anova_result <- anova(multiple_model, multiple_model_with_sex)
anova_result

#Since the p-value is much smaller than the common significance level of 0.05, we reject the null hypothesis
#that the simpler model (without Sex) is as good as the more complex model (with Sex). This indicates that 
#adding the Sex variable to the model significantly improves the fit.


# ==================================================================================
# 4/ For the fitted model make a prediction for an infant abalone with the following
# characteristics : Length = 0.4 mm, Diameter = 0.35 mm , Height = 0.12 mm, 
# Whole weight= 0.40 g, Shucked weight =0.15g, Viscera weight = 0.08 g and Shell weight = 0.13 g.
# ==================================================================================

# New data for prediction
new_abalone <- data.frame(
  Diameter = 0.35,
  Whole.weight = 0.40,
  Shucked.weight = 0.15,
  Shell.weight = 0.13,
  Sex = factor("I", levels = c("F", "M", "I"))
)

# Predict the age for the new abalone
predicted_age <- predict(multiple_model_with_sex, newdata = new_abalone)

# Print the predicted age
predicted_age

