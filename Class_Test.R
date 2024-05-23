install.packages("readxl")
library(readxl)

#loading the data
data<-read_excel("Dataset_2024.xlsx")
data
str(data)
summary(data)
colnames(data)[colnames(data) == "Body fat (%)"] <- "Body_fat"
colnames(data)[colnames(data) == "Age (years)"] <- "Age"
colnames(data)[colnames(data) == "Knee circumference (cm)"] <- "Knee_circumference"
colnames(data)[colnames(data) == "Chest circumference (cm)"] <- "Chest_circumference"
colnames(data)[colnames(data) == "Density (g/cm³)"] <- "Density"
colnames(data)[colnames(data) == "Weight (lbs)"] <- "Weight"

#checking for the linearity of the data
install.packages("e1071")
library(e1071)
windows(20,12)
pairs(data, smooth=FALSE, scale=FALSE, density=TRUE,ellipses=FALSE, method="spearman",
      pch=21, lm=FALSE,cor=TRUE, jiggle =FALSE, factor=2, hist.col=4,stars=TRUE,ci =TRUE)

windows(20,12)
par(mfrow= c(4,2))

scatter.smooth(x = data$Age,
               y = data$Body_fat,
               xlab = "Body fat",
               ylab = "Age", main = "Correlation of Body Fat~Age")

scatter.smooth(x=data$Chest_circumference,
               y=data$Body_fat,
               xlab = "Chest Circumference",
               ylab ="Age",main="correlation of Body_fat~Chest Circumference")

scatter.smooth(x = data$Density,
               y = data$Body_fat,
               xlab = "Density",
               ylab = "Age", main = "Correlation of Body Fat~Density")

scatter.smooth(x=data$Knee_circumference,
               y=data$Body_fat,
               xlab = "Knee Circumference",
               ylab ="Age",main="correlation of Body Fat~Knee Circumference")
scatter.smooth(x=data$Weight,
               y=data$Body_fat,
               xlab = "Knee Circumference",
               ylab ="Age",main="correlation of Body Fat~Weight")

# Examining correlation between murder and Independent variables

cor(data)

attach(data)
# Examining the other variables
paste("Correlation for Body_Fat and Age: ", round(cor(Body_fat, Age),2))
paste("Correlation for Body_Fat and Chest_circumstances: ", round(cor(Body_fat, Chest_circumference),2))
paste("Correlation for Body_Fat and Density: ", round(cor(Body_fat, Density),2))
paste("Correlation for Body_Fat and Knee_circumference ", round(cor(Body_fat, Knee_circumference),2))
paste("Correlation for Body_Fat and Weight ", round(cor(Body_fat, Weight),2))

windows(20,10)
par(mfrow = c(3, 2)) # divide graph area in 3 rows by 2 columns
attach(data)
boxplot(Age,main = "Age")
boxplot(Body_fat,main = "Body_fat")
boxplot(Knee_circumference,main = "Knee_circumference")
boxplot(Chest_circumference,main = "Chest_circumference")
boxplot(Density,main = "Density")
boxplot(Weight,main = "Weight")

# Body_fat variable contains outliers.
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Body_fat)$out
paste("Body_fat outliers: ", paste(outlier_values, sep =", "))
# Remove Body_fat_in_percentage outliers
data <- subset(data,data$Body_fat != 47.5)
# chest_circumference variable contains outliers.
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Chest_circumference)$out # outlier values.
paste("Chest_circumference outliers: ", paste(outlier_values, sep =", "))
# Removing the  Chest_circumference outliers
data <- subset(data,data$Chest_circumference != 128.3
               &data$Chest_circumference != 136.2 )
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Density)$out # outlier values.
paste("Density  outliers: ", paste(outlier_values, sep =", "))
# Remove Density  outliers
data <- subset(data,data$Density != 0.995)
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Knee_circumference)$out # outlier values.
paste("Knee_circumference  outliers: ", paste(outlier_values, sep =", "))
# Remove Knee_circumference outliers
data <- subset(data,
               data$Knee_circumference  != 49.1
               &data$Knee_circumference  !=45
               &data$Knee_circumference  != 46)
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Weight )$out # outlier values.
paste("Weight   outliers: ", paste(outlier_values, sep =", "))
# Remove Weight  outliers
data <- subset(data,
               data$Weight  != 363.15
               &data$Weight   != 262.75
)

# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
windows(30,20)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical


plot(density(data$Age),
     main = "Density plot : Age",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(data$Age), 2)))
polygon(density(data$Age), col = "red")

plot(density(data$Body_fat),
     main = "Density plot : Body_Fat",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(data$Body_fat), 2)))
polygon(density(data$Body_fat), col = "red")

plot(density(data$Knee_circumference),
     main = "Density plot : Knee_circum",
     ylab = "Frequency", xlab = "Knee_Circum",
     sub = paste("Skewness : ", round(e1071::skewness(data$Knee_circumference), 2)))
polygon(density(data$Knee_circumference), col = "red")

plot(density(data$Density),
     main = "Density plot : Density",
     ylab = "Frequency", xlab = "Density",
     sub = paste("Skewness : ", round(e1071::skewness(data$Density), 2)))
polygon(density(data$Density), col = "red")

plot(density(data$Chest_circumference),
     main = "Density plot : Chest_Circum",
     ylab = "Frequency", xlab = "Chest_Circum",
     sub = paste("Skewness : ", round(e1071::skewness(data$Chest_circumference), 2)))
polygon(density(data$Chest_circumference), col = "red")

paste("Skewness for Age : ", round(e1071::skewness(data$Age), 2))
paste("Skewness for Body_fat : ", round(e1071::skewness(data$Body_fat), 2))
paste("Skewness for Chest_Circumference : ", round(e1071::skewness(data$Chest_circumference), 2))
paste("Skewness for Knee_circumference : ", round(e1071::skewness(data$Knee_circumference), 2))
paste("Skewness for Density : ", round(e1071::skewness(data$Density), 2))
paste("Skewness for Weight : ", round(e1071::skewness(data$Weight), 2))

# Chest_circumference = 0.67 => Moderatly skewed
# Weight = 1.19 => highly skewed

#Check linearity using histogram
windows(16,10)
par(mfrow=c(1,2))
hist(data$Chest_circumference)
hist(data$Weight)

# Check normality of all variables using normality test
shapiro.test(data$Age)
shapiro.test(data$Body_fat)
shapiro.test(data$Chest_circumference)
shapiro.test(data$Density)
shapiro.test(data$Knee_circumference)
shapiro.test(data$Weight)

# If p-value < 0.05 then variable is not normally distributed
# Age is not normally distributed (p-value = 0.001082)
# Body_fat is  normally distributed (p-value =0.06)
# Chest_circumference is not normally distributed (p-value = 0.01234)
# Density is  normally distributed (p-value = 0.2158)
# Knee_circumference is  normally distributed (p-value =  0.26)
# Weight is  normally distributed (p-value = 0.06792)

# Need to transform Age, Chest_circumference
attach(data)
library(MASS)
View(data)
box_cox_transform <- boxcox(Body_fat~Age)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_age<-(Body_fat^lamda-1)/lamda
normalised_age
hist(normalised_age)
shapiro.test(normalised_age)

##Modify the variable Age
data$Age_new <- normalised_age
shapiro.test(data$Age_new)

box_cox_transform <- boxcox(Body_fat~Chest_circumference)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_chest_circumference<-(Body_fat^lamda-1)/lamda
normalised_chest_circumference
hist(normalised_chest_circumference)
shapiro.test(normalised_chest_circumference)

##Modify the variable Age
data$chest_circum_new <- normalised_chest_circumference
shapiro.test(data$chest_circum_new)


# Fit the full model
model <- lm(Body_fat ~ Age + Chest_circumference + Density + Knee_circumference + Weight, data = data)

# Summary of the full model
summary(model)
vif(model)
# Residuals analysis
windows(20,12)
par(mfrow = c(2, 2))
plot(model)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(model))
AIC(model)
BIC(model)
