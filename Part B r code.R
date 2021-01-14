# Load the library

library(caret)
library(dplyr)
library(xlsx)
library(ggplot2)
library(fastDummies)
library(modelr)
library(rpart)


# load the data

data = read.xlsx('ANZ%20synthesised%20transaction%20dataset.xlsx; filename%2A.xlsx', as.data.frame = T,sheetIndex = 1)


# Cleaning and pre-processing the data


data_ml <- filter(data, txn_description == 'PAY/SALARY')


# Finding number of unique values 
rapply(data_ml, function(x) length(unique(x)))

# Drop all the columns with just one entry and other irrelevant columns

data_ml <- data_ml[, c('account', 'first_name', 'balance', 'date', 'gender', 'age',
                      "amount", 'customer_id')]



# Calculate annual salary of each individual account holder

data_ml_sal <- data_ml[, c('account', 'balance', 'date',
                           "amount", 'customer_id','gender')]


data_ml_sal$date_diff <- 0
data_ml_sal$An_Salary <- 0


for (i in seq(nrow(data_ml_sal))){
  for (j in ((i+1): seq(nrow(data_ml_sal)))){
    if(i == j){
      next
    }
    if( j > 883 ){
      next
    }
    if(i !=j){
       if (data_ml_sal[i, 1]  == data_ml_sal[j,1]){
          data_ml_sal[i,7] <- abs(as.numeric(data_ml_sal[i,3]-data_ml_sal[j,3]))
      
          data_ml_sal[i,8] = data_ml_sal[i,4]*(364/abs(data_ml_sal[i,7]))
       }
      else{
        next
      }
        
    }
  }
  
}

# some account had some instance of recurring amount of salary on the same day, which resulted in Inf output in 
# 8 accounts, even though the the salary remained the same for previous time-frame and the next time-frame 
# hence assuming that the recurring amount to be a clerical error or other early withdrawal of future salary
# rather than a salary bump


## extract the annual salary of every account (ignore the Inf and zero value)

data_ml_sal[data_ml_sal == 0 | data_ml_sal == Inf] <-  NA

data_ml_sal_cleaned <- data_ml_sal[complete.cases(data_ml_sal),]

data_ml_sal_cleaned <- data_ml_sal_cleaned[!duplicated(data_ml_sal_cleaned$account),]

# data_ml_sal_cleaned now contains the frequency of pay i.e, whether the payment of salary
# is done in weekly basis (7 days), bi weekly basis (14 days) etc.  and the annual salary

## checking all the various frequencies of pay

unique(data_ml_sal_cleaned$date_diff)

## the values contains 31, 30 and 32, this is because of the uneven distribution of number
## of days in a month. 

# frequency distribution of Salary

hist(data_ml_sal_cleaned$An_Salary, xlab = 'Annual Salary',
     main = 'Frequency Distribution of Annual Salary')



## extracting spending from each account

data_ml_spend <- filter(data, txn_description != 'PAY/SALARY')

data_ml_spend_sum <- aggregate(amount~account+age+txn_description+gender, data_ml_spend, sum)


# Merging spending pattern with annual salary

data_ml_Sal_Sp <- merge(data_ml_sal_cleaned, data_ml_spend_sum,  by = 'account')

# drop costumer ID, Balance, and date and amount.x columns

data_ml_Sal_Sp <- data_ml_Sal_Sp %>%
                            select(-date, -amount.x, -balance, -customer_id, -gender.y,-account)

head(data_ml_Sal_Sp)


## Build a predictive model


## Linear Model

fit <- lm(formula = An_Salary~., data = data_ml_Sal_Sp)

summary(fit)

rmse(fit, data_ml_Sal_Sp)

## Build a decision-tree based model


data_ml_Sal_Sp <- dummy_cols(data_ml_Sal_Sp, select_columns = c('txn_description', 'gender.x'),
                                  remove_selected_columns = T)


inTrain <- createDataPartition(data_ml_Sal_Sp, p = 0.70, list = F)

data_train <- data_ml_Sal_Sp[inTrain,]
data_test <- data_ml_Sal_Sp[-inTrain,]


fit_rpart <- rpart(An_Salary~., data = data_test, control = rpart.control(minsplit = 1, minbucket = 1,cp = 0))


rmse(fit_rpart, data_train)










