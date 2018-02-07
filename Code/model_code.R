
### Model Building ####
#########################################################################################
### DATA - CLEANING & PRE-PROCESSING###
#data sets can be found at the link below:
#https://drive.google.com/drive/folders/1DlqE5DgZ22W4h7Ma_snJ2907otgsC_ZO?usp=sharing

Jan<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr1.csv")
Feb<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr2.csv")
Mar<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr3.csv")
Apr<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr4.csv")
May<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr5.csv")
Jun<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr6.csv")
Jul<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr7.csv")
Aug<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr8.csv")
Sep<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr9.csv")
Oct<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr10.csv")
Nov<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr11.csv")
Dec<-read.csv("C:\\Users\\Lokesh Sharma\\Desktop\\Data Science Project\\Data_16\\Tr12.csv")

#We have selected 20 critical variables from the original dataset based on their predictive utility for our model building process.

data_16<-rbind(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
data_16<-subset(data_16,select = c(MONTH,DAY_OF_MONTH,DAY_OF_WEEK,UNIQUE_CARRIER,ORIGIN,DEST,DEP_TIME,DEP_DELAY,DEP_DEL15,DEP_TIME_BLK,ARR_TIME,ARR_DELAY,ARR_DEL15,ARR_TIME_BLK,DISTANCE,CARRIER_DELAY,WEATHER_DELAY,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY
))

#For NA's in Delay Reason Column - Convert to '0'
data_16[,"CARRIER_DELAY"][is.na(data_16[,"CARRIER_DELAY"])]=0
data_16[,"WEATHER_DELAY"][is.na(data_16[,"WEATHER_DELAY"])]=0
data_16[,"NAS_DELAY"][is.na(data_16[,"NAS_DELAY"])]=0
data_16[,"SECURITY_DELAY"][is.na(data_16[,"SECURITY_DELAY"])]=0
data_16[,"LATE_AIRCRAFT_DELAY"][is.na(data_16[,"LATE_AIRCRAFT_DELAY"])]=0

#Check NA's
sum(is.na(data_16))

#Remove NA rows
data_16<-na.omit(data_16)

write.csv(data_16,"data_16.csv")

###DATA-WRANGLING###

#Check for X or X1 column after loading data and remove
view(data_16)
data_16<-subset(data_16,select = -c(X1))

data_16$MONTH<-as.factor(data_16$MONTH)
data_16$DAY_OF_WEEK <- as.factor(data_16$DAY_OF_WEEK)
data_16$DAY_OF_MONTH <- as.factor(data_16$DAY_OF_MONTH)
data_16$DISTANCE <- as.integer(data_16$DISTANCE)
data_16$ARR_DEL15 <- as.factor(data_16$ARR_DEL15)
data_16$DEP_DEL15 <-as.factor(data_16$DEP_DEL15)
data_16$UNIQUE_CARRIER<-as.factor(data_16$UNIQUE_CARRIER)
data_16$ORIGIN<-as.factor(data_16$ORIGIN)
data_16$DEST<-as.factor(data_16$DEST)
data_16$DEP_TIME_BLK<-as.factor(data_16$DEP_TIME_BLK)
data_16$ARR_TIME_BLK<-as.factor(data_16$ARR_TIME_BLK)

#Correlation Matrix

#Check and convert dataframe into a numeric matrix
is.numeric(data_16)
matrix=data.matrix(data_16, rownames.force = NA)
matrix1=cor(matrix)
gc()

#Draw heatmap

install.packages(pheatmap)
install.packages(gplots)

library(gplots)
library(pheatmap)

heatmap <- pheatmap(matrix1, color = colorpanel(3,160,85,60))

#In the entire heatmap we are only interested in ARR_DEL15 & DEP_DELAY15 rows. Visually we can see interesting correlations.

library(caret)

##Data partition into training and test sets

train_new_list<-createDataPartition(y=data_16$MONTH,p=0.75,list = FALSE)

train_new<-data_16[train_new_list,]
test_new<-data_16[-train_new_list,]

#Remove near zero variance predictors

nzv=nearZeroVar(train_new)
str(nzv)
train_new=train_new[,-nzv]
test_new=test_new[,-nzv]

##Now we are left with 15 predictors 
##Interestingly all 5 delays due to weather,security, NAS, late aricraft & carrier reasons are removed

#For computational reasons, we will consider two busiest airports for model building
#data_16_busy.csv : Consider data for the busiest airports in US i.e Atlanta and LosAngeles
#Busiest Airports were selected using data for 2016 airport traffic data in the US obtained from Wikipedia .

train_new1a<-subset(train_new,ORIGIN=="ATL"& DEST=="LAX")
train_new1b<-subset(train_new,ORIGIN=="LAX"& DEST=="ATL")
train_new1<-rbind(train_new1a,train_new1b)

test_new1a<-subset(test_new,ORIGIN=="ATL"& DEST=="LAX")
test_new1b<-subset(test_new,ORIGIN=="LAX"& DEST=="ATL")
test_new1<-rbind(test_new1a,test_new1b)

# Now we will use PCAmix package to perform PCA on mixed data types - 
# where qualititative & quantitative predictors are combined

set.seed(888)

install.packages("PCAmixdata")
library(PCAmixdata)

#Split Qualitative an Quantative predictors to perform mixed PCA. Also remove ARR_DEL15 and DEP_DEL15 as they are our response variables.
#Since we are taking one set of cities- Origin and Dest predictors also dont make sense. Remove them.

#Train data split for PCA

split <- splitmix(train_new1)
X1 <- split$X.quanti 
X2 <- split$X.quali 
X2<-subset(X2,select = -c(ORIGIN,DEST,ARR_DEL15,DEP_DEL15))

#Test data split for PCA

split<-splitmix(test_new1)
X11<-split$X.quanti  
X22<-split$X.quali
X22<-subset(X22,select = -c(ORIGIN,DEST,ARR_DEL15,DEP_DEL15))

#Create numbered list for count.number of rows in test and training data
te<-sample(1:nrow(test_new1))
tr<-sample(1:nrow(train_new1))  

set.seed(111)

train.pcamix <- PCAmix(X1[tr,],X2[tr,],graph=FALSE,rename.level = TRUE)
pred_tr <- predict(train.pcamix,X1,X2)
pred_tr<-cbind(pred_tr,train_new1$ARR_DEL15,train_new1$DEP_DEL15)
pred_tr1<-as.data.frame(pred_tr)
pred_tr1$V6<-as.integer(pred_tr1$V6)
pred_tr1$V7<-as.integer(pred_tr1$V7)
pred_tr1$V6[pred_tr1$V6==1]<-0
pred_tr1$V6[pred_tr1$V6==2]<-1
pred_tr1$V7[pred_tr1$V7==1]<-0
pred_tr1$V7[pred_tr1$V7==2]<-1
pred_tr1$V6<-as.factor(pred_tr1$V6)
pred_tr1$V7<-as.factor(pred_tr1$V7)


test.pcamix <- PCAmix(X11[te,],X22[te,],graph=FALSE,rename.level = TRUE)
pred_te <- predict(test.pcamix,X11,X22)
pred_te<-cbind(pred_te,test_new1$ARR_DEL15,test_new1$DEP_DEL15)
pred_te1<-as.data.frame(pred_te)
pred_te1$V6<-as.integer(pred_te1$V6)
pred_te1$V7<-as.integer(pred_te1$V7)
pred_te1$V6[pred_te1$V6==1]<-0
pred_te1$V6[pred_te1$V6==2]<-1
pred_te1$V7[pred_te1$V7==1]<-0
pred_te1$V7[pred_te1$V7==2]<-1
pred_te1$V6<-as.factor(pred_te1$V6)
pred_te1$V7<-as.factor(pred_te1$V7)


######################################################################

################### CLASSIFICATION MODEL #########################

# METHOD 1: Logistic Regression

library(DBI)
library(biglm)
library(e1071)

# For Both Arrival and Departure delays

# Model Building

# Arrival Delay Prediction Model

log_reg_mod1 <- train( V6~ ., data = pred_tr1, method = "glm", model = FALSE, family = "binomial",trControl=trainControl(method = "cv", number = 5, repeats = 5)) 

# Departure Delay Prediction Model

log_reg_mod2 <- train(V7~ ., data = pred_tr1, method = "glm", model = FALSE, family = "binomial",trControl=trainControl(method = "cv", number = 5, repeats = 5)) 

# Prediction and confusion matrix

log_reg_predict1 <- predict(log_reg_mod1, pred_te1)

confusion_matrix_reg1 <- confusionMatrix(log_reg_predict1, test_new1$ARR_DEL15)
confusion_matrix_reg1

log_reg_predict2 <- predict(log_reg_mod2, pred_te1)

confusion_matrix_reg2 <- confusionMatrix(log_reg_predict2, test_new1$DEP_DEL15)
confusion_matrix_reg2


#Save confusion matrix into a csv file to use for visualization. Change column header names to ON-TIME & DELAYED

## Confusion matrix Visualization - Log Regression

library(ggplot2)

##ARR DELAY
input1 <- read.delim(file.choose(), header=TRUE, sep=",")
input1.matrix <- data.matrix(input1)
colnames(input1.matrix)
rownames(input1.matrix) = colnames(input1.matrix)
rownames(input1.matrix)

confusion1 <- as.data.frame(as.table(input1.matrix))

plot1 <- ggplot(confusion1)
plot1 + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_color_gradient(breaks=seq(from=0, to=2000000, by=500)) + labs(fill="Frequency")

##DEP DELAY
input2 <- read.delim(file.choose(), header=TRUE, sep=",")
input2.matrix <- data.matrix(input2)
colnames(input2.matrix)
rownames(input2.matrix) = colnames(input2.matrix)
rownames(input2.matrix)

confusion2 <- as.data.frame(as.table(input2.matrix))

plot2 <- ggplot(confusion2)
plot2 + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_color_gradient(breaks=seq(from=0, to=2000000, by=500)) + labs(fill="Frequency")

# METHOD 2: Random Forest

#Model Building

library(randomForest) 
random_forest1 <- randomForest(pred_tr1[,-6], pred_tr1$V6, proximity = TRUE, importance = TRUE)

random_forest2 <- randomForest(pred_tr1[,-7], pred_tr1$V7, proximity = TRUE, importance = TRUE)

random_forest_validation1 <- predict(random_forest1, pred_te1)

random_forest_validation2 <- predict(random_forest2, pred_te1)

# Confusion matrix 

confusion_matrix_rf1 <- confusionMatrix(random_forest_validation1, pred_te1$V6)
confusion_matrix_rf1

confusion_matrix_rf2 <- confusionMatrix(random_forest_validation2, pred_te1$V7)
confusion_matrix_rf2

#Save confusion matrix into a csv file to use for visualization. Change column header names to ON-TIME & DELAYED

## Confusion matrix Visualization - Random Forest

library(ggplot2)

##ARR DELAY
input1 <- read.delim(file.choose(), header=TRUE, sep=",")
input1.matrix <- data.matrix(input1)
colnames(input1.matrix)
rownames(input1.matrix) = colnames(input1.matrix)
rownames(input1.matrix)

confusion1 <- as.data.frame(as.table(input1.matrix))

plot1 <- ggplot(confusion1)
plot1 + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_color_gradient(breaks=seq(from=0, to=1000, by=50)) + labs(fill="Frequency")

##DEP DELAY
input2 <- read.delim(file.choose(), header=TRUE, sep=",")
input2.matrix <- data.matrix(input2)
colnames(input2.matrix)
rownames(input2.matrix) = colnames(input2.matrix)
rownames(input2.matrix)

confusion2 <- as.data.frame(as.table(input2.matrix))

plot2 <- ggplot(confusion2)
plot2 + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_color_gradient(breaks=seq(from=0, to=1000, by=50)) + labs(fill="Frequency")

# METHOD 3: Support Vector Machines

#Model Building
set.seed(111)

#Arrival Delay

#We can optimize the model by tuning the best combinations of parameters
svmfit.tune1<-tune(svm,V6~., data=pred_tr1,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:10)))
svmfit1<-svmfit.tune1$best.model
svmfit1
prediction.svm1<-predict(svmfit1,pred_te1)
confusion_matrix_svm1<-confusionMatrix(prediction.svm1,pred_te1$V6)
confusion_matrix_svm1

#Departure Delay

#We can optimize the model by tuning the best combinations of parameters
svmfit.tune2<-tune(svm,V7~., data=pred_tr1,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:10)))
svmfit2<-svmfit.tune2$best.model
svmfit2
prediction.svm2<-predict(svmfit2,pred_te1)
confusion_matrix_svm2<-confusionMatrix(prediction.svm2,pred_te1$V7)
confusion_matrix_svm2

#### Confusion Matrix Visualization

##ARR DELAY
input1 <- read.delim(file.choose(), header=TRUE, sep=",")
input1.matrix <- data.matrix(input1)
colnames(input1.matrix)
rownames(input1.matrix) = colnames(input1.matrix)
rownames(input1.matrix)

confusion1 <- as.data.frame(as.table(input1.matrix))

plot1 <- ggplot(confusion1)
plot1 + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_color_gradient(breaks=seq(from=0, to=1000, by=50)) + labs(fill="Frequency")

##DEP DELAY
input2 <- read.delim(file.choose(), header=TRUE, sep=",")
input2.matrix <- data.matrix(input2)
colnames(input2.matrix)
rownames(input2.matrix) = colnames(input2.matrix)
rownames(input2.matrix)

confusion2 <- as.data.frame(as.table(input2.matrix))

plot2 <- ggplot(confusion2)
plot2 + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_color_gradient(breaks=seq(from=0, to=1000, by=50)) + labs(fill="Frequency")


#####################PREDICTION - MODEL######################

#We will use 2 prediction models and choose the best one-

#First we will improve prediction accuracy by one hot encoding for multilevel categorical variables
#One -Hot Encoding for converting categorical variables into dummy variables with binary outcomes

m_tr<-subset(train_new1,select = -c(ORIGIN,DEST))
m_tr <- model.matrix( ~ MONTH+DAY_OF_MONTH+DAY_OF_WEEK+UNIQUE_CARRIER+ARR_DEL15+DEP_DEL15+ARR_TIME_BLK+DEP_TIME_BLK, data = train_new1)
head(m_tr)

m_te<-subset(test_new1,select = -c(ORIGIN,DEST))
m_te <- model.matrix( ~ MONTH+DAY_OF_MONTH+DAY_OF_WEEK+UNIQUE_CARRIER+ARR_DEL15+DEP_DEL15+ARR_TIME_BLK+DEP_TIME_BLK, data = test_new1)
head(m_te)

#Combine qualitative and quantitative variables
m_tr<-cbind(m_tr,train_new1$DEP_TIME,train_new1$DEP_DELAY,train_new1$ARR_TIME,train_new1$ARR_DELAY,train_new1$DISTANCE)

m_te<-cbind(m_te,test_new1$DEP_TIME,test_new1$DEP_DELAY,test_new1$ARR_TIME,test_new1$ARR_DELAY,test_new1$DISTANCE)

#convert to data frame

m_tr1<-as.data.frame(m_tr)
m_te1<-as.data.frame(m_te)

set.seed(738)

#Method-1: OLS Regression

#Arrival Delay
m1<-lm(V101~.,data=m_tr1)
pred1<-predict(m1,m_te1)
plot(sqrt((pred1-m_te1$V101)^2),type = "b",col="purple",xlab = "Observations",ylab= "RMSE",main = "OLS Root Mean Square Error Plot - Arr Delay")
summary(m1)
mean(sqrt((pred1-m_te1$V101)^2))
median(sqrt((pred1-m_te1$V101)^2))

#Departure Delay
m2<-lm(V99~.,data=m_tr1)
pred2<-predict(m2,m_te1)
plot(sqrt((pred2-m_te1$V99)^2),type = "b",col="purple",xlab = "Observations",ylab= "RMSE",main = "OLS Root Mean Square Error Plot - Dep Delay")
summary(m2)
mean(sqrt((pred2-m_te1$V99)^2))
median(sqrt((pred2-m_te1$V99)^2))

#Method-2: Support Vector Regression

#Arrival Delay
m3.tune<-tune(svm,V101~.,data=m_tr1,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:10)))
m3<-m3.tune$best.model
pred3<-predict(m3,m_te1)
plot(sqrt((pred3-m_te1$V101)^2),type = "b",col="purple",xlab = "Observations",ylab= "RMSE",main = "SVM Root Mean Square Error Plot - Arr Delay")
summary(m3)
mean(sqrt((pred3-m_te1$V101)^2))
median(sqrt((pred3-m_te1$V101)^2))

#Departure Delay
m4.tune<-tune(svm,V99~.,data=m_tr1,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:10)))
m4<-m4.tune$best.model
pred4<-predict(m4,m_te1)
plot(sqrt((pred4-m_te1$V99)^2),type = "b",col="purple",xlab = "Observations",ylab= "RMSE",main = "SVM Root Mean Square Error Plot - Dep Delay")
summary(m4)
mean(sqrt((pred4-m_te1$V99)^2))
median(sqrt((pred4-m_te1$V99)^2))