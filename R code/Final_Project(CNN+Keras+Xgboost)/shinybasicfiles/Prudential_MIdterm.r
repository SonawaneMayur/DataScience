
# Import train data

train.data <- read.csv("C:/Users/Mayur/Documents/Advance Data Science/MidTerm_Project_Prudential/Data/train.csv")

str(train.data)

head(train.data)

#2. Import test dataset

test.data <- read.csv("C:/Users/Mayur/Documents/Advance Data Science/MidTerm_Project_Prudential/Data/test.csv")

str(test.data)


head(test.data)

#3. Remove columns that has too much missing value

ValueMissingVecTrain <- NA
for (i in (1:127)) ValueMissingVecTrain[i] <- sum(is.na(train.data[,i]))/nrow(train.data)
inedxMatRemove1 <- which(ValueMissingVecTrain >= 0.55)
ValueMissingVecTest <- NA
for (i in (1:127)) ValueMissingVecTest[i] <- sum(is.na(test.data[,i]))/nrow(test.data)
inedxMatRemove2 <- which(ValueMissingVecTest >= 0.55)



inedxMatRemove1
inedxMatRemove2



train.data.processed <- train.data[,-inedxMatRemove1]
test.data.processed <- test.data[,-inedxMatRemove1]

head(train.data.processed)

head(test.data.processed)

#4. Get rows that have NA values

ValueMissingColTrain <- NA
for (i in (1:122)) ValueMissingColTrain[i] <- sum(is.na(train.data.processed[,i]))
inedxMatKeep1 <- which(ValueMissingColTrain > 0)

ValueMissingColText <- NA
for (i in (1:121)) ValueMissingColText[i] <- sum(is.na(test.data.processed[,i]))
inedxMatKeep2 <- which(ValueMissingColText > 0)

inedxMatKeep1
inedxMatKeep2

#5. Filling NA value in 13 Column for train.data.processed

colnames(train.data.processed)[13]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,13])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,13])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,13], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,13], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,13], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,13], na.rm = TRUE))

# most value is around 0.06, less than 0.1
# we choose Average as missing value
train.data.processed.filling <- train.data.processed
test.data.processed.filling <- test.data.processed
train.data.processed.filling[,13][is.na(train.data.processed.filling[,13])] <- 0.078
test.data.processed.filling[,13][is.na(test.data.processed.filling[,13])] <- 0.078


#6. Filling NA value in 16 Column for train.data.processed

colnames(train.data.processed)[16]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,16])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,16])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,16], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,16], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,16], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,16], na.rm = TRUE))
percentage_0 <- length(which(train.data.processed[,16] == 0)) / length(which(!is.na(train.data.processed[,16])))
paste("Percentage of Value 0:", paste(percentage_0*100, "%", sep=''))

# more than 84% of column 16 is 0, we choose 0 as missing value
train.data.processed.filling[,16][is.na(train.data.processed.filling[,16])] <- 0
test.data.processed.filling[,16][is.na(test.data.processed.filling[,16])] <- 0

#7. Filling NA value in 18 Column for train.data.processed

colnames(train.data.processed)[18]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,18])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,18])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,18], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,18], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,18], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,18], na.rm = TRUE))

# 18 column is Continuous from 0 to 1, we choose average as missing value
train.data.processed.filling[,18][is.na(train.data.processed.filling[,18])] <- 0.36
test.data.processed.filling[,18][is.na(test.data.processed.filling[,18])] <- 0.36

#8. Filling NA value in 30 Column for train.data.processed

colnames(train.data.processed)[30]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,30])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,30])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,30], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,30], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,30], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,30], na.rm = TRUE))
percentage_0.01 <- length(which(train.data.processed[,30] < 0.01)) / length(which(!is.na(train.data.processed[,30])))
paste("Percentage of Value 0:", paste(percentage_0.01*100, "%", sep=''))

# more than 98% of column 30 is less than 0.01, we choose 0 as missing value
train.data.processed.filling[,30][is.na(train.data.processed.filling[,30])] <- 0
test.data.processed.filling[,30][is.na(test.data.processed.filling[,30])] <- 0

#9. Filling NA value in 35 Column for train.data.processed

colnames(train.data.processed)[35]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,35])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,35])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,35], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,35], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,35], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,35], na.rm = TRUE))

# MEAN and MEDIAN is similiar, column 35 has Continuous data between 0 to 1
# we choose median as missing value
train.data.processed.filling[,35][is.na(train.data.processed.filling[,35])] <- 0.46
test.data.processed.filling[,35][is.na(test.data.processed.filling[,35])] <- 0.46

#10. Filling NA value in 36 Column for train.data.processed

colnames(train.data.processed)[36]    # Continuous
paste("Number of missing value:", sum(is.na(train.data.processed[,35])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,36])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,36], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,36], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,36], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,36], na.rm = TRUE))

# MEAN and MEDIAN is similiar, column 36 has Continuous data between 0 to 1
# we choose median as missing value
train.data.processed.filling[,36][is.na(train.data.processed.filling[,36])] <- 0.44
test.data.processed.filling[,36][is.na(test.data.processed.filling[,36])] <- 0.44

#11. Filling NA value in 37 Column for train.data.processed

colnames(train.data.processed)[37]    # Discrete
paste("Number of missing value:", sum(is.na(train.data.processed[,37])))
print("--------------------------------------------------------------------------------------")
str(train.data.processed[,37])
print("--------------------------------------------------------------------------------------")

paste("MEAN  :", mean(train.data.processed[,37], na.rm = TRUE))
paste("MEDIAN:", median(train.data.processed[,37], na.rm = TRUE))
paste("MAX   :", max(train.data.processed[,37], na.rm = TRUE))
paste("MIN   :", min(train.data.processed[,37], na.rm = TRUE))
b <- table(train.data.processed[,37])
paste("MODE  :", as.numeric(names(b)[b == max(b)]))

# Column 37 is discrete, range between 0 to 240
# we choose to set Mode "1" as the missing value, because its repetition rate is high
train.data.processed.filling[,37][is.na(train.data.processed.filling[,37])] <- 1
test.data.processed.filling[,37][is.na(test.data.processed.filling[,37])] <- 1

#12. Check the new data frame

str(train.data.processed.filling)



#12. Convert all categorical variables to factor and remove id column


train.data.processed.convert <- train.data.processed.filling
train.data.processed.convert<-train.data.processed.convert[,-1]

train.data.processed.convert

colnames(train.data.processed.convert)

train.data.processed.convert[,'Product_Info_2']<-as.numeric(train.data.processed.convert[,'Product_Info_2'])
str(train.data.processed.convert)

fit <- lm(Response~. ,data=train.data.processed.convert)
summary(fit)



significant_colnames<-rownames(data.frame(summary(fit)$coef[summary(fit)$coef[,4] <= .05,4])[,0])
significant_colnames

categorical.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""),"Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""))
categorical.names


cat.names<-intersect(significant_colnames,categorical.names)
cat.names


for (i in cat.names) train.data.processed.convert[,i]<-as.factor(train.data.processed.convert[,i])
str(train.data.processed.convert)

head(train.data.processed.convert)

predictors <- ''

#binary <- model.matrix(~Product_Info_6,data=train.data.processed.convert)

for( i in cat.names){
                          predictors   <- paste( predictors, i, sep=' + ')
                        #cbind(binary,model.matrix(~ i,data=train.data.processed.convert))
                        }
predictors <- substring(predictors, 4)
#predictors
form <- as.formula(paste(paste("~", predictors), '-1'))

binary <- model.matrix(form,data=train.data.processed.convert)


str(binary)

x <- setdiff(significant_colnames, categorical.names)
x

#train.data.processed.convert <- cbind(train.data.processed.convert,model.matrix(form,data=train.data.processed.convert))
# drop the cat.names columns 
#train.data.processed.convert.NC <- train.data.processed.convert[ , !(names(train.data.processed.convert) %in% cat.names)]
train.data.processed.convert.significant <- train.data.processed.convert[ , (names(train.data.processed.convert) %in% x)]
str(train.data.processed.convert.significant)
# update the binary columns of cat.names columns and bind with the dataframe
#train.data.processed.convert.binary <- cbind(train.data.processed.convert , binary)

train.data.processed.convert.significant.binary <- cbind(train.data.processed.convert.significant, binary)

str(train.data.processed.convert.significant.binary)

p <- train.data.processed.convert.significant.binary
length(names(p))
#col <- as.character(colnames(p))
#for(i in colnames(p)){paste(i,1)}


for(i in names(p)){
            IndexList <- which(colnames(p) == i)
            #IndexList
            totalCount = length(IndexList)
            #paste(i, totalCount)
            if(totalCount > 1){
                #i
                for(j in IndexList){
                    cnt = 1
                    colnames(p)[j] <- paste(i,cnt,sep ='_')
                    cnt = cnt + 1
                }
            }
    
}



p['Response'] <- train.data.processed$Response
length(names(p))

names(p)


model <- lm (Response~., data= p)
summary(model)

significant_colnames_model<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] <= .05,4])[,0])
significant_colnames_model

q <- p[,significant_colnames_model]
q['Response'] <- p['Response']
length(colnames(q))

model <- lm (Response~., data= q)
summary(model)
significant_colnames_model1<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] <= .05,4])[,0])
length(significant_colnames_model1)

r <- q[,significant_colnames_model]
r['Response'] <- p['Response']
length(colnames(r))

model <- lm (Response~., data= r)
summary(model)
significant_colnames_model2<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] <= .05,4])[,0])
length(significant_colnames_model2)
significant_colnames_model2

n <- length(significant_colnames_model2)
significant_colnames_model2 <- significant_colnames_model2[2:n]
s <- r[,significant_colnames_model2]
s['Response'] <- r['Response']
length(colnames(s))

model <- lm (Response~., data= s)
summary(model)
significant_colnames_model3<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] < .01,4])[,0])
length(significant_colnames_model3)

n <- length(significant_colnames_model3)
significant_colnames_model3 <- significant_colnames_model3[2:n]
t <- s[,significant_colnames_model3]
t['Response'] <- s['Response']
length(colnames(t))

model <- lm (Response~., data= t)
summary(model)
significant_colnames_model4<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] < .05,4])[,0])
length(significant_colnames_model4)

n <- length(significant_colnames_model4)
significant_colnames_model4 <- significant_colnames_model4[2:n]
u <- t[,significant_colnames_model3]
u['Response'] <- t['Response']
length(colnames(u))

model <- lm (Response~., data= u)
summary(model)
significant_colnames_model5<-rownames(data.frame(summary(model)$coef[summary(model)$coef[,4] < .01,4])[,0])
length(significant_colnames_model5)

n <- length(significant_colnames_model5)

significant <- intersect(significant_colnames_model5[2:n], significant_colnames_model)
length(significant)


#For Neural Network Go to the Neural network section where I am using t as a Train data set 

#this data is XGb model

 XGB <- p[,significant]
#XGB <- p
#XGB <- train.data.processed.filling[-1]
#for (i in significant_colnames_model) train.data.processed.convert[,i]<-as.factor(train.data.processed.convert[,i])
str(XGB)
XGB_columns <- colnames(XGB)


XGB['Response'] <- train.data.processed$Response
names(XGB)

# XGB works on only Numeric dataset so here convering all columns to integer 

for(i in colnames(XGB)){
                        
    XGB[i]<-as.numeric(unlist(XGB[i]))
}

for (f in colnames(XGB)) {
  if (class(XGB[[f]])=="character") {
    levels <- unique(c(XGB[[f]], XGB[[f]]))
    XGB[[f]] <- as.integer(factor(XGB[[f]], levels=levels))
    XGB[[f]]  <- as.integer(factor(XGB[[f]],  levels=levels))
  }
}

str(XGB)

# To check the accuracy of model we have to apply prediction on the data which we have the output parameter

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(XGB), size = floor(.80*nrow(XGB)), replace = F)
XGB_Ttrain <- XGB[sample, ]
XGB_Ttest  <- XGB[-sample, ]

nrow(XGB)
nrow(XGB_Ttrain)
nrow(XGB_Ttest)

rem <- which(colnames(XGB) == 'Response')
rem
head(XGB_Ttrain[-rem])

require(xgboost)

data(XGB_Ttrain, package='xgboost')
data(XGB_Ttest, package='xgboost')
train <- XGB_Ttrain
test <- XGB_Ttest

feature.names <- names(train)[1:(ncol(train)-1)]
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

feature.names

library(Matrix)

regMat <- data.matrix(train[,feature.names])

test_regMat <- data.matrix(test[,feature.names])


sparseMat <- as(regMat, "sparseMatrix")      
test_sparseMat <- Matrix(test_regMat, sparse = TRUE)    

identical(sparseMat, test_sparseMat)

dtrain <- xgb.DMatrix(data = sparseMat, label = train$Response)
dtest <- xgb.DMatrix(data = test_regMat, label=test$Response)
watchlist <- list(train=dtrain, test=dtest)

#rem <- which(colnames(XGB) == 'Response')

bstSparse <- xgboost(data = data.matrix(train[,feature.names]), label = train$Response, max.depth = 2, eta = 0.01, nthread = 10, nround = 500, objective = "reg:linear")

pre <- predict(bstSparse, data.matrix(test[,feature.names]))

test['Predicted_Response'] <- as.integer(round(pre))


#subset(test, select =c('Predicted_Response','Response'))

test[test$Predicted_Response<1, "Predicted_Response"] <- 1
test[test$Predicted_Response>8, "Predicted_Response"] <- 8
print("----------------Root Mean Square of Predicted value ----------------------------------")

error <- test$Predicted_Response - test$Response
rmse <- sqrt(mean(error^2))
mae <- mean(abs(error))
paste("Root Mean Square of Response & Predicted_Response:", rmse)
paste("Mean Absolute Error of Response & Predicted_Response:", mae)
print("--------------------------------------------------------------------------------------")

print("------------------Accuracy of the XGBOOST Model----------------------------------------")
library('forecast')

accuracy(test$Response,test$Predicted_Response)

acc <- table(pred = test$Predicted_Response, true = test$Response)
acc
cols <- ncol(acc)
cols
rows <- nrow(acc)
rows
sum <- 0
for(i in cols){    
        sum = sum + acc[i,i]   
}

accuracy <- sum/nrow(test)

paste("--------------accuracy--------",accuracy)


importance_matrix <- xgb.importance(colnames(test),model = bstSparse)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


#xgb.plot.tree(model = bstSparse)

Gain is the improvement in accuracy brought by a feature to the branches it is on. The idea is that before adding a new split on a feature X to the branch there was some wrongly classified elements, after adding the split on this feature, there are two new branches, and each of these branch is more accurate (one branch saying if your observation is on this branch then it should be classified as 1, and the other branch saying the exact opposite).

Cover measures the relative quantity of observations concerned by a feature.

Frequency is a simpler way to measure the Gain. It just counts the number of times a feature is used in all generated trees. You should not use it (unless you know why you want to use i
library(Matrix)

regMat <- data.matrix(train[,feature.names])

test_regMat <- data.matrix(test[,feature.names])


sparseMat <- as(regMat, "sparseMatrix")      
test_sparseMat <- Matrix(test_regMat, sparse = TRUE)    

identical(sparseMat, test_sparseMat)

test_sparseMat

dtrain <- xgb.DMatrix(data = sparseMat, label = train$Response)
dtest <- xgb.DMatrix(data = test_regMat, label=test$Response)
watchlist <- list(train=dtrain, test=dtest)

#watchlist <- list(train=dtrain, test=dtest)

bstSparse_Dense <- xgb.train(data = dtrain, max.depth = 2, eta = 0.04, nthread = 10, nround = 108, max_delta_step = 10, lambda = 100,tree_method='exact', watchlist = watchlist, objective = "reg:linear")

bstSparse_Dense

pre <- predict(bstSparse_Dense, test_regMat)

test['Predicted_Dense_Response'] <- as.integer(round(pre))



test[,c('Response','Predicted_Dense_Response')] 

#subset(test, select =c('Predicted_Response','Response'))

test[test$Predicted_Dense_Response<1, "Predicted_Dense_Response"] <- 1
test[test$Predicted_Dense_Response>8, "Predicted_Dense_Response"] <- 8
print("----------------Root Mean Square of Predicted value ----------------------------------")

error <- test$Predicted_Dense_Response - test$Response
rmse <- sqrt(mean(error^2))
mae <- mean(abs(error))
paste("Root Mean Square of Response & Predicted_Response:", rmse)
paste("Mean Absolute Error of Response & Predicted_Response:", mae)
print("--------------------------------------------------------------------------------------")

print("------------------Accuracy of the XGBOOST Model----------------------------------------")
library('forecast')

accuracy(test$Response,test$Predicted_Dense_Response)

acc <- table(pred = test$Predicted_Dense_Response, true = test$Response)
acc
cols <- ncol(acc)
cols
rows <- nrow(acc)
rows
sum <- 0
for(i in cols){    
        sum = sum + acc[i,i]   
}

accuracy <- sum/nrow(test)
accuracy

importance_matrix <- xgb.importance(colnames(test),model = bstSparse_Dense)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
paste("------- Tree spilts on above Features-----")


importance_matrix$Feature[1:15]

#watchlist <- list(train=dtrain, test=dtest)

bstSparse_lBoost <- xgb.train(data = dtrain, max.depth = 2, booster = "gbtree", nthread = 10, nround = 10, watchlist = watchlist, objective = "reg:linear")

bstSparse_lBoost

prelb <- predict(bstSparse_lBoost, test_regMat)

test['Predicted_DLBoost_Response'] <- as.integer(round(prelb))



test[,c('Response','Predicted_DLBoost_Response')] 

#subset(test, select =c('Predicted_Response','Response'))

test[test$Predicted_DLBoost_Response<1, "Predicted_DLBoost_Response"] <- 1
test[test$Predicted_DLBoost_Response>8, "Predicted_DLBoost_Response"] <- 8
print("----------------Root Mean Square of Predicted value ----------------------------------")

error <- test$Predicted_DLBoost_Response - test$Response
rmse <- sqrt(mean(error^2))
mae <- mean(abs(error))
paste("Root Mean Square of Response & Predicted_Response:", rmse)
paste("Mean Absolute Error of Response & Predicted_Response:", mae)
print("--------------------------------------------------------------------------------------")

print("------------------Accuracy of the XGBOOST Model----------------------------------------")
library('forecast')

accuracy(test$Response,test$Predicted_DLBoost_Response)

acc <- table(pred = test$Predicted_DLBoost_Response, true = test$Response)
acc
cols <- ncol(acc)
cols
rows <- nrow(acc)
rows
sum <- 0
for(i in cols){    
        sum = sum + acc[i,i]   
}

accuracy <- sum/nrow(test)
accuracy

importance_matrix <- xgb.importance(colnames(test),model = bstSparse_lBoost)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)











bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

dtrain <- xgb.DMatrix(data = train$data, label = train$label)
# verbose = 0, no message
bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic", verbose = 0)

# verbose = 1, print evaluation metric
bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic", verbose = 1)

pred <- predict(bst, test$data)

# size of the prediction vector
print(length(pred))

# limit display of predictions to the first 10
print(head(pred))

prediction <- as.numeric(pred > 0.5)
print(head(prediction))

#To measure the model performance, we will compute a simple metric, the average error.
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

xgb.dump(bst, with.stats = T)



xgb.plot.tree(feature_names = colnames(train$data),model = bst)

binary

str(train.data.processed)

NN <- u
str(NN)
NN <- data.matrix(NN)



set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(NN), size = floor(.80*nrow(NN)), replace = F)
NN_Ttrain <- NN[sample, ]
NN_Ttest  <- NN[-sample, ]

nrow(NN)
nrow(NN_Ttrain)
nrow(NN_Ttest)

nTrain <- colnames(NN_Ttrain)
f <- as.formula(paste("Response ~", paste(nTrain[!nTrain %in% "Response"], collapse = " + ")))
f

library(neuralnet)
nn <- neuralnet(f,data=NN_Ttrain,hidden=c(30,10), act.fct = "logistic",linear.output=T)



library(dplyr)
important_features <- c('BMI','Medical_History_233_1.1','Medical_History_42','Medical_Keyword_3','Product_Info_4','Wt','Ins_Age','Medical_History_403','Medical_History_133','Product_Info_215','InsuredInfo_62','Insurance_History_23','InsuredInfo_53','Medical_History_1','InsuredInfo_73')

NN <- u[,important_features]
NN$Response <- u[,"Response"]
glimpse(NN)
NN <- data.matrix(NN)



set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(NN), size = floor(.80*nrow(NN)), replace = F)
NN_Ttrain <- NN[sample, ]
NN_Ttest  <- NN[-sample, ]

nrow(NN)
nrow(NN_Ttrain)
nrow(NN_Ttest)

#nTrain <- colnames(NN_Ttrain)
#f <- as.formula(paste("Response ~", paste(nTrain[!nTrain %in% "Response"], collapse = " + ")))
#f


ind <- c(colnames(NN_Ttest[,-which(colnames(NN_Ttest) == 'Response')]))

X_Train<-NN_Ttrain[, -which(colnames(NN_Ttest) == 'Response')]
Y_Train<-NN_Ttrain[, which(colnames(NN_Ttest) == 'Response')]



X_Test<-NN_Ttest[, -which(colnames(NN_Ttest) == 'Response')]
Y_Test<-NN_Ttest[, which(colnames(NN_Ttest) == 'Response')]

X_Train <- data.matrix(X_Train)
Y_Train <- data.matrix(Y_Train)
X_Test <- data.matrix(X_Test)
Y_Test <- data.matrix(Y_Test)

dim(X_Train)

library(keras)

#calculating time to measure the model run time using KERAS
start_time <- Sys.time()


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape =  dim(X_Train)[2]) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 9, activation = 'softmax')
summary(model)
model %>% compile(
  loss ="sparse_categorical_crossentropy" ,# "binary_crossentropy",#,      #"categorical_crossentropy"
  optimizer =optimizer_rmsprop(), # 'adam',   # optimizer_rmsprop(),
  metrics = c("accuracy")
)
history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 250, batch_size = 125, 
  validation_split = 0.01,
    verbose= 2,
    shuffle='True'
)
plot(history)
model %>% evaluate(X_Test, Y_Test,verbose = 1)
predicted_Y_Test <- model %>% predict_classes(X_Test)
table(predicted_Y_Test, Y_Test)

end_time <- Sys.time()

run_time <- end_time - start_time
print("--------------------------------------------------------------------------------------")
paste("Total run time for Keras model", run_time)

#for all 76 variables 

library(dplyr)
#important_features <- c('BMI','Medical_History_233_1.1','Medical_History_42','Medical_Keyword_3','Product_Info_4','Wt','Ins_Age','Medical_History_403','Medical_History_133','Product_Info_215','InsuredInfo_62','Insurance_History_23','InsuredInfo_53','Medical_History_1','InsuredInfo_73')

NN <- u
#NN$Response <- u[,"Response"]
glimpse(NN)
NN <- data.matrix(NN)



set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(NN), size = floor(.80*nrow(NN)), replace = F)
NN_Ttrain <- NN[sample, ]
NN_Ttest  <- NN[-sample, ]

nrow(NN)
nrow(NN_Ttrain)
nrow(NN_Ttest)

#nTrain <- colnames(NN_Ttrain)
#f <- as.formula(paste("Response ~", paste(nTrain[!nTrain %in% "Response"], collapse = " + ")))
#f


X_Train<-NN_Ttrain[, -which(colnames(NN_Ttest) == 'Response')]
Y_Train<-NN_Ttrain[, which(colnames(NN_Ttest) == 'Response')]



X_Test<-NN_Ttest[, -which(colnames(NN_Ttest) == 'Response')]
Y_Test<-NN_Ttest[, which(colnames(NN_Ttest) == 'Response')]

# convert into matrix form
X_Train <- data.matrix(X_Train)
Y_Train <- data.matrix(Y_Train)
X_Test <- data.matrix(X_Test)
Y_Test <- data.matrix(Y_Test)

#
library(keras)

#calculating time to measure the model run time using KERAS
start_time <- Sys.time()


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 70, activation = 'relu', input_shape =  dim(X_Train)[2]) %>% 
  layer_dense(units = 9, activation = 'softmax')
summary(model)
model %>% compile(
  loss ="sparse_categorical_crossentropy" ,# "binary_crossentropy",#,      #"categorical_crossentropy"
  optimizer =optimizer_rmsprop(), # 'adam',   # optimizer_rmsprop(),
  metrics = c("accuracy")
)
history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 250, batch_size = 125, 
  validation_split = 0.01,
    verbose= 2,
    shuffle='True'
)
plot(history)
model %>% evaluate(X_Test, Y_Test,verbose = 1)
predicted_Y_Test <- model %>% predict_classes(X_Test)
table(predicted_Y_Test, Y_Test)

end_time <- Sys.time()

run_time <- end_time - start_time
print("--------------------------------------------------------------------------------------")
paste("Total run time for Keras model", run_time)

unique(predicted_Y_Test)

acc <- table(predicted_Y_Test, Y_Test)
#acc[-1,]
cols <- ncol(acc)
cols
rows <- nrow(acc)
rows 


acc 

sum <- 0
for(i in 1:8){    
        sum = sum + acc[i,i]   
}
paste("sum--", sum)
accuracy <- sum/(nrow(Y_Test))
paste("accuracy--",accuracy)

sqrt(mean((as.vector(Y_Test) - as.vector(predicted_Y_Test) )^2))

library(dplyr)
#important_features <- c('BMI','Medical_History_233_1.1','Medical_History_42','Medical_Keyword_3','Product_Info_4','Wt','Ins_Age','Medical_History_403','Medical_History_133','Product_Info_215','InsuredInfo_62','Insurance_History_23','InsuredInfo_53','Medical_History_1','InsuredInfo_73')

NN <- t
#NN$Response <- u[,"Response"]
glimpse(NN)
NN <- data.matrix(NN)



set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(NN), size = floor(.80*nrow(NN)), replace = F)
NN_Ttrain <- NN[sample, ]
NN_Ttest  <- NN[-sample, ]

nrow(NN)
nrow(NN_Ttrain)
nrow(NN_Ttest)

#nTrain <- colnames(NN_Ttrain)
#f <- as.formula(paste("Response ~", paste(nTrain[!nTrain %in% "Response"], collapse = " + ")))
#f
X_Train<-NN_Ttrain[, -which(colnames(NN_Ttest) == 'Response')]
Y_Train<-NN_Ttrain[, which(colnames(NN_Ttest) == 'Response')]



X_Test<-NN_Ttest[, -which(colnames(NN_Ttest) == 'Response')]
Y_Test<-NN_Ttest[, which(colnames(NN_Ttest) == 'Response')]

# convert into matrix form
X_Train <- data.matrix(X_Train)
Y_Train <- data.matrix(Y_Train)
X_Test <- data.matrix(X_Test)
Y_Test <- data.matrix(Y_Test)

#
library(keras)

#calculating time to measure the model run time using KERAS
start_time <- Sys.time()


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 157, activation = 'relu', input_shape =  dim(X_Train)[2]) %>% 
  layer_dense(units = 9, activation = 'softmax')
summary(model)
model %>% compile(
  loss ="sparse_categorical_crossentropy" ,# "binary_crossentropy",#,      #"categorical_crossentropy"
  optimizer =optimizer_rmsprop(), # 'adam',   # optimizer_rmsprop(),
  metrics = c("accuracy")
)
history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 250, batch_size = 125, 
  validation_split = 0.01,
    verbose= 2,
    shuffle='True'
)
plot(history)
model %>% evaluate(X_Test, Y_Test,verbose = 1)
predicted_Y_Test <- model %>% predict_classes(X_Test)
table(predicted_Y_Test, Y_Test)

end_time <- Sys.time()

run_time <- end_time - start_time
print("--------------------------------------------------------------------------------------")
paste("Total run time for Keras model", run_time)

print("--------------------------------------------------------------------------------------")
sum <- 0
for(i in 1:8){    
        sum = sum + acc[i,i]   
}
#paste("sum--", sum)
accuracy <- sum/(nrow(Y_Test))
paste("accuracy--",accuracy)
print("--------------------------------------------------------------------------------------")
paste("RMSE :", sqrt(mean((as.vector(Y_Test) - as.vector(predicted_Y_Test) )^2)))


