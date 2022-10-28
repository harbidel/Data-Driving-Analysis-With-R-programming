#installing the needed library to import excel data
#install.packages("readxl")
library("readxl")

#importing the customer data
DDM22_26_customer <- read_excel("DDM22_26_customer.xlsx")
View(DDM22_26_customer)
#importing the score data
DDM22_26_score <- read_excel("DDM22_26_score.xlsx")
View(DDM22_26_score)

#checking the number rows and columns in the first dataset
nrow(DDM22_26_customer)
ncol(DDM22_26_customer)

#checking the number rows and columns in the second dataset
ncol(DDM22_26_score)
nrow(DDM22_26_score)

#checking the number of missing variables and the ratio in each of the dataset
sum(is.na(DDM22_26_customer))
mean(is.na(DDM22_26_score))

# checkng the statistic summary of the customer data
View(DDM22_26_customer)
summary(DDM22_26_customer)

sum(is.na(DDM22_26_customer$click))
mean(is.na(DDM22_26_customer$click))

#since the ratio of missing data is now really low we can ommit the missing variables
DDM22_26_customer <- na.omit(DDM22_26_customer)

# Deleting the outlier on the data_allowance noticed on the dataset entered as drunkard
DDM22_26_customer<-subset(DDM22_26_customer, data_allowance!="drunkard")

# checking the summary of the dataset after omitting the missing data
summary(DDM22_26_customer)
sapply(DDM22_26_customer, n_distinct)

#joining the customer and score data together
#install.packages("readr")
library(dplyr)
library(readr)

df <- left_join(DDM22_26_customer, DDM22_26_score)
View(df)

#importing dplyr to check the number of unique values we have in each column
n_distinct(df$cid)


#importing pivottabler to make pivot tables
#install.packages("pivottabler")
library(pivottabler)

#import ggplot2 a data visualization library
library(ggplot2)

# visualizing the gender using Bar chart
ggplot(DDM22_26_customer, aes(x=reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill='Blue') +  labs(x='Gender')

# Visualizing age vs gender using box plot
ggplot(DDM22_26_customer, aes(x=gender, y=age)) +  geom_boxplot(fill='green')

# Pivot table for age and gender
qhpvt(DDM22_26_customer, "age", "gender", "n()")

# visualizing the citizenship using Bar chart
ggplot(DDM22_26_customer, aes(x=citizenship)) + 
  geom_bar(fill='Purple')

# Visualizing the Gender vs Citizenship using mosaic plot
counts <- table(DDM22_26_customer$citizenship, DDM22_26_customer$gender)
mosaicplot(counts, xlab='Citizenship', ylab='Gender',main='customer gender by country of origin', col='orange')

# creating Pivot table for citizenship and gender
qhpvt(DDM22_26_customer, "citizenship", "gender", "n()")

# plotting viz for contract_type using bar plot
ggplot(DDM22_26_customer, aes(x=reorder(contract_type, contract_type, function(x)-length(x)))) +
  geom_bar(fill='green') +  labs(x='contract_type')

# Visualizing the Gender vs contract using mosaic plot
counts <- table(DDM22_26_customer$contract_type, DDM22_26_customer$gender)
mosaicplot(counts, xlab='contract_type', ylab='Gender',main='customer gender by contrct type', col='blue')

# creating Pivot table for contract_type and gender
qhpvt(DDM22_26_customer, "contract_type", "gender", "n()")

# plotting viz for age using histogram chart
ggplot(DDM22_26_customer, aes(x=age)) + 
  geom_histogram(fill='blue')

# plotting viz for tenure using histogram chart
ggplot(DDM22_26_customer, aes(x=tenure)) + 
  geom_histogram(fill='orange')

# plotting viz for monthly_charges using histogram chart
ggplot(DDM22_26_customer, aes(x=monthly_charges)) + 
  geom_histogram(fill='gray')

# plotting viz for payment_arrears using histogram chart
ggplot(DDM22_26_customer, aes(x=payment_arrears)) + 
  geom_histogram(fill='black')

# plotting viz for data_allowance using Bar chart
ggplot(DDM22_26_customer, aes(x=data_allowance)) + 
  geom_bar()

# plotting viz for payment_method using Bar chart
ggplot(DDM22_26_customer, aes(x=payment_method)) + 
  geom_bar()

# plotting viz for payment_arrears using Bar chart
ggplot(DDM22_26_customer, aes(x=payment_arrears)) + 
  geom_bar()

# plotting viz for contract_terminated using Bar chart
ggplot(DDM22_26_customer, aes(x=contract_terminated)) + 
  geom_bar()

# plotting viz fore number_transferred using Bar chart
ggplot(DDM22_26_customer, aes(x=number_transferred)) + 
  geom_bar()

# Visualizing the contract_type vs payment_method using mosaic plot
counts <- table(DDM22_26_customer$contract_type, DDM22_26_customer$payment_method)
mosaicplot(counts, xlab='contract_type', ylab='Payment method',main='customer payment method by contract type', col='orange')

qhpvt(DDM22_26_customer, "contract_type", "payment_method", "n()")

ggplot(DDM22_26_customer, aes(x=contract_type, y=monthly_charges)) +  geom_boxplot(fill='blue')

qhpvt(DDM22_26_customer, "monthly_charges", "contract_type", "n()")  

ggplot(DDM22_26_customer, aes(x=contract_type, y=payment_arrears)) +  geom_boxplot(fill='green')
qhpvt(DDM22_26_customer, "payment_arrears", "contract_type", "n()")

counts <- table(DDM22_26_customer$contract_type, DDM22_26_customer$contract_terminated)
mosaicplot(counts, xlab='contract_type', ylab='Contract terminated',main='Contract terminated by contrct type', col='orange')

qhpvt(DDM22_26_customer, "contract_terminated", "contract_type", "n()") 


counts <- table(DDM22_26_customer$contract_type, DDM22_26_customer$number_transferred)
mosaicplot(counts, xlab='contract_type', ylab='number_transferred',main='number_transferred by contrct type', col='orange')

qhpvt(DDM22_26_customer, "number_transferred", "contract_type", "n()")  

ggplot(DDM22_26_customer, aes(x=contract_type, y=tenure)) +  geom_boxplot(fill='green')
qhpvt(DDM22_26_customer, "tenure", "contract_type", "n()")


ggplot(DDM22_26_customer, aes(x=tenure, y=monthly_charges)) + geom_point()

ggplot(DDM22_26_customer, aes(x=tenure, y=monthly_charges)) +
  geom_point(aes(size=payment_arrears))

ggplot(DDM22_26_customer, aes(x=tenure, y=monthly_charges, shape=contract_type, color=contract_type)) +
  geom_point()

ggplot(data, aes(x=tenure, y=score, shape=contract_type, color=contract_type)) +
  geom_point()

qhpvt(data, "score", "contract_type", "n()")

counts <- table(data$contract_type, data$score)
mosaicplot(counts, xlab='contract_type', ylab='score',main='score by contrct type', col='black')
