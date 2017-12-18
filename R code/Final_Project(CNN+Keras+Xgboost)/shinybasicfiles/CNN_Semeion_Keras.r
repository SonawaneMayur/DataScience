
semeion <- read.table("C://Users//Mayur//Documents//Advance Data Science//Assignments_SDas//ADS_FINAL//Data_Semeion//semeion.txt", 
                  header = TRUE)

head(df)

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.80*nrow(df)), replace = F)
train <- semeion[sample, ]
test  <- semeion[-sample, ]

nrow(train)
nrow(test)

library(data.table)


mydat <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/semeion/semeion.data')
head(mydat)

semeion<-mydat[,1:256]
dataVal<-mydat[,257:266]



semeion[,which(is.na(semeion))]


semeion$digit <- NA
semeion$digit[which(mydat$V257 == 1)] <- 0
semeion$digit[which(mydat$V258 == 1)] <- 1
semeion$digit[which(mydat$V259 == 1)] <- 2
semeion$digit[which(mydat$V260 == 1)] <- 3
semeion$digit[which(mydat$V261 == 1)] <- 4
semeion$digit[which(mydat$V262 == 1)] <- 5
semeion$digit[which(mydat$V263 == 1)] <- 6
semeion$digit[which(mydat$V264 == 1)] <- 7
semeion$digit[which(mydat$V265 == 1)] <- 8
semeion$digit[which(mydat$V266 == 1)] <- 9
#semeion


#dataVal

#labels(semeion)

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(semeion), size = floor(.80*nrow(semeion)), replace = F)
CNN_Ttrain <- semeion[sample, ]
CNN_Ttest  <- semeion[-sample, ]



X_Train<-CNN_Ttrain[, -257]
Y_Train<-CNN_Ttrain[, 257]



X_Test<-CNN_Ttest[, -257]
Y_Test<-CNN_Ttest[, 257]

library(reshape2)
sapply(Y_Train, function(x) sum(is.na(x)))

# reshape
head(Y_Train)
dim(Y_Train)
#dim(X_Train) <- c(nrow(X_Train),dim(X_Train)[2])
Y_Train <- data.matrix(Y_Train)
X_Train <- data.matrix(X_Train)

ncol(X_Train)
dim(X_Train)[2]

library(keras)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 128, activation = "relu", input_shape = dim(X_Train)[2]) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax")

summary(model)

model %>% compile(
  loss = "sparse_categorical_crossentropy" ,      #"categorical_crossentropy"
  optimizer = 'adam',   # optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 7, batch_size = 101, 
  validation_split = 0.2
)

plot(history)

#before running the model need to convert data to matrix form for keras model
X_Test <- data.matrix(X_Test)
Y_Test <- data.matrix(Y_Test)

model %>% evaluate(X_Test, Y_Test,verbose = 1)

predicted_Y_Test <- model %>% predict_classes(X_Test)

table(predicted_Y_Test, Y_Test)

#tunning by using epochs= 200 and batch_size = 30

history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 200, batch_size = 30, 
  validation_split = 0.2
)

plot1<-plot(history)

model %>% evaluate(X_Test, Y_Test,verbose = 1)

predicted_Y_Test <- model %>% predict_classes(X_Test)
table(predicted_Y_Test, Y_Test)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape =  dim(X_Train)[2]) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)
model %>% compile(
  loss = "sparse_categorical_crossentropy" ,      #"categorical_crossentropy"
  optimizer = 'adam',   # optimizer_rmsprop(),
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

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape =  dim(X_Train)[2]) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)
model %>% compile(
  loss = "sparse_categorical_crossentropy" ,      #"categorical_crossentropy"
  optimizer = 'adam',   # optimizer_rmsprop(),
  metrics = c("accuracy")
)
history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 25, batch_size = 128, 
  validation_split = 0.011,
 verbose = 2,
    shuffle="True"
)
plot(history)
model %>% evaluate(X_Test, Y_Test,verbose = 1)
predicted_Y_Test <- model %>% predict_classes(X_Test)
table(predicted_Y_Test, Y_Test)

#calculating time to measure the model run time using KERAS
start_time <- Sys.time()
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 1024, activation = 'relu', input_shape =  dim(X_Train)[2]) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)
model %>% compile(
  loss = "sparse_categorical_crossentropy" ,      #"categorical_crossentropy"
  optimizer = 'adam',   # optimizer_rmsprop(),
  metrics = c("accuracy")
)
history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 200, batch_size = 128, 
  validation_split = 0.02,
 verbose = 2,
    shuffle="True"
)
plot(history)
model %>% evaluate(X_Test, Y_Test,verbose = 1)
predicted_Y_Test <- model %>% predict_classes(X_Test)
table(predicted_Y_Test, Y_Test)

end_time <- Sys.time()

run_time <- end_time - start_time
print("--------------------------------------------------------------------------------------")
paste("Total run time for Keras model", run_time, " seconds")

#calculating time to measure the model run time using KERAS
start_time <- Sys.time()
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 1024, activation = 'relu', input_shape =  dim(X_Train)[2]) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)
model %>% compile(
  loss = "sparse_categorical_crossentropy" ,      #"categorical_crossentropy"
  optimizer = 'adam',   # optimizer_rmsprop(),
  metrics = c("accuracy")
)
history <- model %>% fit(
  X_Train, Y_Train,
  epochs = 200, batch_size = 129, 
  validation_split = 0.02,
 verbose = 2,
    shuffle="True"
)
plot(history)
model %>% evaluate(X_Test, Y_Test,verbose = 1)
predicted_Y_Test <- model %>% predict_classes(X_Test)
table(predicted_Y_Test, Y_Test)

end_time <- Sys.time()

run_time <- end_time - start_time
print("--------------------------------------------------------------------------------------")
paste("Total run time for Keras model", run_time, " seconds")

library('forecast')


predicted <- data.frame(predicted_Y_Test)
actual <- data.frame(Y_Test)
error <- predicted - actual
rmse <- sqrt(mean(error^2))
mae <- mean(abs(error))

rmse

ls()



