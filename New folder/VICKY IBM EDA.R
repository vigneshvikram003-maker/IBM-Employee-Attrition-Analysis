#TASK 1
# Load Dataset
ibm <- read.csv("IBM.csv")

# First 10 records
head(ibm, 10)

# Last 10 records
tail(ibm, 10)

# Number of rows and columns
dim(ibm)

# Rows
nrow(ibm)

# Columns
ncol(ibm)

# Structure
str(ibm)

# Summary
summary(ibm)
# TASK 2
# Missing values
colSums(is.na(ibm))

# Duplicate rows
sum(duplicated(ibm))

# Convert categorical columns to factor

ibm$Attrition <- as.factor(ibm$Attrition)
ibm$Department <- as.factor(ibm$Department)
ibm$EducationField <- as.factor(ibm$EducationField)
ibm$MaritalStatus <- as.factor(ibm$MaritalStatus)

#TASK 3
numeric_columns <- sapply(ibm, is.numeric)

# Mean
sapply(ibm[,numeric_columns], mean)

# Median
sapply(ibm[,numeric_columns], median)

# Standard Deviation
sapply(ibm[,numeric_columns], sd)

# Minimum
sapply(ibm[,numeric_columns], min)

# Maximum
sapply(ibm[,numeric_columns], max)

# Quartiles
sapply(ibm[,numeric_columns], quantile)

#TASK 4
# Age Distribution
hist(ibm$Age,
     col="skyblue",
     main="Age Distribution",
     xlab="Age")

# Monthly Income Distribution
hist(ibm$MonthlyIncome,
     col="orange",
     main="Monthly Income Distribution",
     xlab="Monthly Income")

# Years At Company Distribution
hist(ibm$YearsAtCompany,
     col="green",
     main="Years At Company Distribution",
     xlab="Years At Company")

# Attrition
barplot(table(ibm$Attrition),
        col=c("lightgreen","red"),
        main="Employee Attrition")

# Department Distribution
barplot(table(ibm$Department),
        col="steelblue",
        las=2,
        main="Department Distribution")

# Education Field Distribution
barplot(table(ibm$EducationField),
        col="purple",
        las=2,
        main="Education Field Distribution")

# Marital Status Distribution
barplot(table(ibm$MaritalStatus),
        col="pink",
        main="Marital Status Distribution")

#TASK 5
boxplot(ibm$Age,
        main="Age",
        col="skyblue")

boxplot(ibm$MonthlyIncome,
        main="Monthly Income",
        col="orange")

boxplot(ibm$YearsAtCompany,
        main="Years At Company",
        col="green")

boxplot(ibm$DistanceFromHome,
        main="Distance From Home",
        col="yellow")

boxplot(ibm$NumCompaniesWorked,
        main="Number of Companies Worked",
        col="pink")
#TASK 6
# Attrition vs Department
table(ibm$Department, ibm$Attrition)

barplot(table(ibm$Department, ibm$Attrition),
        beside=TRUE,
        legend=TRUE,
        col=c("green","red"),
        main="Department vs Attrition")

# Attrition vs Education Field
table(ibm$EducationField, ibm$Attrition)

barplot(table(ibm$EducationField, ibm$Attrition),
        beside=TRUE,
        legend=TRUE,
        col=c("green","red"),
        las=2,
        main="Education Field vs Attrition")

# Attrition vs Marital Status
table(ibm$MaritalStatus, ibm$Attrition)

barplot(table(ibm$MaritalStatus, ibm$Attrition),
        beside=TRUE,
        legend=TRUE,
        col=c("green","red"),
        main="Marital Status vs Attrition")
#TASK 7
# Age vs Monthly Income
plot(ibm$Age,
     ibm$MonthlyIncome,
     col=ifelse(ibm$Attrition=="Yes","red","blue"),
     pch=19,
     xlab="Age",
     ylab="Monthly Income",
     main="Age vs Monthly Income")

legend("topleft",
       legend=c("Yes","No"),
       col=c("red","blue"),
       pch=19)

# Years At Company vs Monthly Income
plot(ibm$YearsAtCompany,
     ibm$MonthlyIncome,
     col=ifelse(ibm$Attrition=="Yes","red","blue"),
     pch=19,
     xlab="Years At Company",
     ylab="Monthly Income",
     main="Years At Company vs Monthly Income")

legend("topleft",
       legend=c("Yes","No"),
       col=c("red","blue"),
       pch=19)

# Number of Companies Worked vs Monthly Income
plot(ibm$NumCompaniesWorked,
     ibm$MonthlyIncome,
     col=ifelse(ibm$Attrition=="Yes","red","blue"),
     pch=19,
     xlab="Companies Worked",
     ylab="Monthly Income",
     main="Companies Worked vs Monthly Income")

legend("topleft",
       legend=c("Yes","No"),
       col=c("red","blue"),
       pch=19)
#TASK 8

# Overall Attrition Rate

prop.table(table(ibm$Attrition))*100

# Department with Highest Attrition
table(ibm$Department, ibm$Attrition)

# Education Field with Highest Attrition
table(ibm$EducationField, ibm$Attrition)

# Marital Status with Highest Attrition
table(ibm$MaritalStatus, ibm$Attrition)

# Top 10 Highest Paid Employees
head(ibm[order(-ibm$MonthlyIncome), ],10)

# Top 10 Employees with Longest Tenure
head(ibm[order(-ibm$YearsAtCompany), ],10)

# Department with Highest Average Salary
aggregate(MonthlyIncome~Department,
          data=ibm,
          mean)

# Education Field with Highest Average Salary
aggregate(MonthlyIncome~EducationField,
          data=ibm,
          mean)

# Correlation Matrix
num_data <- ibm[sapply(ibm,is.numeric)]
cor(num_data)