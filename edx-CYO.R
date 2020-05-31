
################################
# Download the data Set
################################
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse",repos="http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",repos="http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table",repos="http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate",repos="http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally",repos="http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart",repos="http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot",repos="http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle",repos="http://cran.us.r-project.org")
if(!require(descr)) install.packages("descr",repos="http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(GGally)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(descr)
############ Bank Marketing Data Set: ############ 
# Location of Data Set in UCI Machine Learning Repository
# https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
# Dropbox location of data zipped file
#https://www.dropbox.com/s/u54po56i9acmbm2/bank-additional.zip?dl=0

dl <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip",dl)
bankData <- read.csv(unzip(dl,"bank-additional/bank-additional-full.csv"),
                     stringsAsFactors = FALSE, header = T,sep = ";")
head(bankData)

glimpse(bankData)

#Observations: 41,188
#Variables: 21
#$ age            <int> 56, 57, 37, 40, 56, 45, 59, 41, 24, 25, 41, 25, 29, 57, 35, 54, 35, 46, 50, 39, 30, 55, 55, 41, 37, 35, 59, 39, 54, 55, 46, 59, 49, 54, 54, 55, 34, 52, 41, 56, 58, 32, 3…
#$ job            <chr> "housemaid", "services", "services", "admin.", "services", "services", "admin.", "blue-collar", "technician", "services", "blue-collar", "services", "blue-collar", "hous…
#$ marital        <chr> "married", "married", "married", "married", "married", "married", "married", "married", "single", "single", "married", "single", "single", "divorced", "married", "marrie…
#$ education      <chr> "basic.4y", "high.school", "high.school", "basic.6y", "high.school", "basic.9y", "professional.course", "unknown", "professional.course", "high.school", "unknown", "high…
#$ default        <chr> "no", "unknown", "no", "no", "no", "unknown", "no", "unknown", "no", "no", "unknown", "no", "no", "no", "no", "unknown", "no", "unknown", "no", "unknown", "no", "unknown…
#$ housing        <chr> "no", "no", "yes", "no", "no", "no", "no", "no", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "no", "no", "yes", "yes", "yes", "yes", "no",…
#$ loan           <chr> "no", "no", "no", "no", "yes", "no", "no", "no", "no", "no", "no", "no", "yes", "no", "no", "yes", "no", "yes", "yes", "no", "no", "no", "no", "no", "no", "yes", "no", "…
#$ contact        <chr> "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", "telephone", …
#$ month          <chr> "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "may", "…
#$ day_of_week    <chr> "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "…
#$ duration       <int> 261, 149, 226, 151, 307, 198, 139, 217, 380, 50, 55, 222, 137, 293, 146, 174, 312, 440, 353, 195, 38, 262, 342, 181, 172, 99, 93, 233, 255, 362, 348, 386, 73, 230, 208, …
#$ campaign       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1…
#$ pdays          <int> 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999, 999,…
#$ previous       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#$ poutcome       <chr> "nonexistent", "nonexistent", "nonexistent", "nonexistent", "nonexistent", "nonexistent", "nonexistent", "nonexistent", "nonexistent", "nonexistent", "nonexistent", "non…
#$ emp.var.rate   <dbl> 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1,…
#$ cons.price.idx <dbl> 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 93.994, 9…
#$ cons.conf.idx  <dbl> -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -36.4, -…
#$ euribor3m      <dbl> 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4.857, 4…
#$ nr.employed    <dbl> 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5191, 5…
#$ y              <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "…


## Checking for NAs
sapply(bankData, {function(x) any(is.na(x))}) %>% knitr::kable()

########################## Data Exporation of variables in the dataset########################## 

#Distribution of Output variable
ggplot(bankData)+geom_bar(aes(y,fill=y))
prop.table(table(bankData$y))
#       no       yes 
#0.8873458 0.1126542 

# Distribution of age for term deposit subscription visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, age,fill=y))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))

# Density plot of age variable for term deposit subscription
ggplot(bankData)+geom_density(aes(age,fill=y),alpha=1/3)+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))

#Distiribution of job visualization
ggplot(bankData)+geom_bar(aes(job,fill=job)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Distiribution of job variable for term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(job,fill=y))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Distiribution of marital visualization
ggplot(bankData)+geom_bar(aes(marital,fill=marital))

#Distiribution of marital variable to term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(marital,fill=y))


#Distiribution of education category visualization
ggplot(bankData)+geom_bar(aes(education,fill=education))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#Distiribution of education variable to term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(education,fill=y))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#Distiribution of credit default visualization
ggplot(bankData)+geom_bar(aes(default,fill=default))

#Distiribution of credit default on term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(default,fill=y))


#Distiribution of housing variable  visualization
ggplot(bankData)+geom_bar(aes(housing,fill=housing))


#Distiribution of housing variable to term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(housing,fill=y))


#Distiribution of loan category visualization
ggplot(bankData)+geom_bar(aes(loan,fill=loan))


#Distiribution of loan variable to  term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(loan,fill=y))

#Distiribution of contact variable visualization
ggplot(bankData)+geom_bar(aes(contact,fill=contact))


#Distiribution of contact variable to term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(contact,fill=y))

#Distiribution of month variable visualization
ggplot(bankData)+geom_bar(aes(month,fill=month))


#Distiribution of  last contct month variable to term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(month,fill=y))

#Distiribution of day_of_week variable visualization
ggplot(bankData)+geom_bar(aes(day_of_week,fill=day_of_week))


#Distiribution of last contact day_of_week variable to term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(day_of_week,fill=y))

#Distiribution of duration variable visualization
ggplot(bankData) +
  geom_boxplot(aes(y, duration,fill=y))

# Distribution of contact duration to term deposit subscription visualization 
ggplot(bankData)+geom_bar(aes(duration,fill=y))

# Distribution of number of contacts in this campaign variable visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, campaign,fill=y))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80))

# Distribution of number of contacts in this campaign  to term deposit subscription visualization 
ggplot(bankData)+geom_bar(aes(campaign,fill=y))

# Distribution of number of days since last contact pdays variable visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, pdays,fill=y))

# Distribution of number of days since last contact pdays for term deposit subscription visualization 
ggplot(bankData)+geom_histogram(aes(pdays),bins = 3)

# Distribution of number of contacts for previous campaign previous variable  visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, previous,fill=y))

# Distribution of  number of contacts for previous campaign previous variable to term deposit subscription visualization 
ggplot(bankData)+geom_bar(aes(previous,fill=y))+scale_x_continuous(breaks = c(0,1,2,3,4,5,6))

#Distiribution of outcome of the previous marketing campaign poutcome variable  visualization
ggplot(bankData)+geom_bar(aes(poutcome,fill=poutcome))

#Distiribution of outcome of the previous marketing campaign poutcome to term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(poutcome,fill=y))


# Distribution of employment variation rate - quarterly indicator emp.var.rate visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, emp.var.rate,fill=y))

# Distribution of employment variation rate - quarterly indicator emp.var.rate for term deposit subscription visualization 
ggplot(bankData)+geom_bar(aes(emp.var.rate,fill=y))

# Distribution of consumer price index - monthly indicator cons.price.idx visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, cons.price.idx,fill=y))

# Distribution of consumer price index - monthly indicator cons.price.idx for term deposit subscription visualization 
ggplot(bankData)+geom_bar(aes(cons.price.idx,fill=y))+scale_x_continuous(limits =c(92,95),breaks = seq(92,95,by=0.5))


# Distribution of consumer confidence index - monthly indicator cons.conf.idx visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, cons.conf.idx,fill=y))

# Distribution of consumer confidence index - monthly indicator cons.conf.idx for term deposit subscription visualization
ggplot(bankData)+geom_bar(aes(cons.conf.idx,fill=y))

# Distribution of euribor 3 month rate - daily indicator euribor3m visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, euribor3m,fill=y))

# Distribution of euribor 3 month rate - daily indicator euribor3m for term deposit subscription visualization 
ggplot(bankData)+geom_histogram(aes(euribor3m,fill=y),bins = 10,alpha =0.6)

# Distribution of number of employees - quarterly indicator nr.employed visualization 
ggplot(bankData) +
  geom_boxplot(aes(y, nr.employed,fill=y))

# Distribution of number of employees - quarterly indicator nr.employed for term deposit subscription visualization 
ggplot(bankData)+geom_bar(aes(nr.employed,fill=y))

###############################Correlation visualization####################

ggcorr(bankData,  label = TRUE, hjust = 0.75,size = 3)

aa <-bankData %>%select(nr.employed, euribor3m, cons.conf.idx,cons.price.idx,emp.var.rate) 


ggpairs(aa,columns = 1:5 , lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)),
        upper = list(continuous = wrap("cor", method= "spearman")))

################################Data Processing##############################

#Check for Duplicate Rows
sum(duplicated(bankData))
#12

#create a new dataset bankData_dist with distinct dataset only i.e remove the duplicates
bankData_dist <- bankData %>% distinct()

nrow(bankData_dist)
str(bankData_dist)
#converting the output variable y from yes/no to 1/0 
bankData_dist$y = ifelse(bankData_dist$y=='yes',1,0)
prop.table(table(bankData_dist$y))
# converting character variables to factor variables
bankData_dist$y <- as.factor(bankData_dist$y)
bankData_dist$job <- as.factor(bankData_dist$job)
bankData_dist$marital <- as.factor(bankData_dist$marital)
bankData_dist$education <- as.factor(bankData_dist$education)
bankData_dist$default <- as.factor(bankData_dist$default)
bankData_dist$housing <- as.factor(bankData_dist$housing)
bankData_dist$loan <- as.factor(bankData_dist$loan)
bankData_dist$contact <- as.factor(bankData_dist$contact)
bankData_dist$month <- as.factor(bankData_dist$month)
bankData_dist$day_of_week <- as.factor(bankData_dist$day_of_week)
bankData_dist$poutcome <- as.factor(bankData_dist$poutcome)

#converting the columns to numeric
bankData_dist$age <- as.numeric(bankData_dist$age)
bankData_dist$duration <- as.numeric(bankData_dist$duration)
bankData_dist$campaign <- as.numeric(bankData_dist$campaign)
bankData_dist$pdays <- as.numeric(bankData_dist$pdays)
bankData_dist$previous <- as.numeric(bankData_dist$previous)
bankData_dist$emp.var.rate <- as.numeric(bankData_dist$emp.var.rate)
bankData_dist$cons.price.idx <- as.numeric(bankData_dist$cons.price.idx)
bankData_dist$cons.conf.idx <- as.numeric(bankData_dist$cons.conf.idx)
bankData_dist$nr.employed <- as.numeric(bankData_dist$nr.employed)

#checking the variables in the dataset
str(bankData_dist)

# Splitting the dataset into two parts (training set and test set)
# The test set will be 10% of the data while 90% will be training set

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(bankData_dist$y, times = 1, p = 0.1, list = FALSE)
train_bankData_dist <- bankData_dist[-test_index,]
test_bankData_dist <- bankData_dist[test_index,]

############################################################
###   Model 1: Classification and Regression Tree [CART]
############################################################

# Classification and Regression Trees
 bank_cart<-rpart(y ~ ., train_bankData_dist , method = 'class')

#Plotting the tree
par(mfrow=c(1,1))
fancyRpartPlot( bank_cart , digits=2 , palettes = c("Blues", "Oranges"))

#prediction on the test set
cart_pred <- predict(  bank_cart , test_bankData_dist , type = "class")
cart_prob <- predict(  bank_cart , test_bankData_dist , type = "prob")

# Confusion matrix
confusionMatrix(cart_pred , test_bankData_dist$y)

#Storing the result of the confusion matrix 
cm_cart <-confusionMatrix(cart_pred , test_bankData_dist$y)

### Cross table validation for CART
CrossTable(test_bankData_dist$y, cart_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#########################[CART ENDS]########################

############################################################
###   Model 2: K- Nearest Neighbor [KNN]
############################################################

bank_knn <- train(y ~ ., data = train_bankData_dist, method = "knn",
                  maximize = TRUE,
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess=c("center", "scale"))

# Let's check the best value for tuning parameter
bank_knn$bestTune
# Plotting the tuning parameter
ggplot(bank_knn, highlight = TRUE)
# Final model details
bank_knn$finalModel
# Prediction on the test set
predictedkNN <- predict(bank_knn , newdata = test_bankData_dist)

# Confusion matrix
confusionMatrix(predictedkNN , test_bankData_dist$y)
#Storing the result of the confusion matrix 
cm_knn <- confusionMatrix(predictedkNN , test_bankData_dist$y)

# Checking the important variables
varImp(bank_knn)

#Plotting the imortant variables
plot(varImp(bank_knn),main="Top variables - KNN")

### Cross table validation for KNN

CrossTable(test_bankData_dist$y, predictedkNN,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#########################[KNN ENDS]########################

############################################################
###   Model 3: Random Forest [RF]
############################################################
set.seed(8)
bank_rf <- train(y ~ ., method = 'rf', tunegrid = data.frame(mtry = seq(1,7,1)),
                   data = train_bankData_dist, ntree = 100)

# Let's check the best value for tuning parameter
bank_rf$bestTune

# Plotting the tuning parameter
ggplot(bank_rf, highlight = TRUE)

# Final model details
bank_rf$finalModel

# Prediction on the test set
y_hat_rf1 <- predict(bank_rf, test_bankData_dist)

# Checking the important variables
varImp(bank_rf)

#Plotting the imortant variables
plot(varImp(bank_rf),main="Top variables - RF",top = 10)

# Confusion matrix
confusionMatrix(y_hat_rf1 , test_bankData_dist$y)
#Storing the result of the confusion matrix 
cm_rf <- confusionMatrix(y_hat_rf1 , test_bankData_dist$y)


#########################[RF ENDS]########################

################Results Comparison ###########################
models_list <- list(CART= bank_cart,
                    Random_Forest=bank_rf,
                    KNN=bank_knn)

models_list

cm_list <- list(
  CART=cm_cart,
  Random_Forest=cm_rf,
  KNN=cm_knn)

#Output of Confusion Matix
cm_list_results <- sapply(cm_list, function(x) x$byClass)
cm_list_results %>% knitr::kable()

#Accuracy of CART
cm_cart$overall['Accuracy']
#Accuracy of KNN
cm_knn$overall['Accuracy']
#Accuracy of Random Forest
cm_rf$overall['Accuracy']


########Creating a results table to store results of different models ####
accuracy_results <- data_frame(MODEL ="CART", ACCURACY = cm_cart$overall['Accuracy'])
accuracy_results <- bind_rows(accuracy_results,data_frame(MODEL ="Random Forest", ACCURACY = cm_rf$overall['Accuracy']))
accuracy_results <- bind_rows(accuracy_results,data_frame(MODEL ="KNN", ACCURACY = cm_knn$overall['Accuracy']))
accuracy_results %>% knitr::kable()
##############################################################
