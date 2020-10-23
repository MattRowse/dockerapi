library(dplyr)
library(lubridate)
library(plumber)
library(SnowballC)
library(httr)
library(tm)
library(gsheet)
# Chatbot for guiding customers to required documentation

# Methdology
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
# 3. Train Support Vector Machines model with the training matrix
# 4. Propose a testing question
# 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
# 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
# 7. Predict the answer with the trained SVM model

# read data
#data = gsheet2tbl("https://docs.google.com/spreadsheets/d/1lq3tOwDrxD9ZEuKc_oYCI-2lb9octmcgbN2P274D6wk/edit#gid=0")
#saveRDS(data, file = "data")
data = readRDS("data")
data1 <- data %>% filter(Area=="design")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
corpus1 = VCorpus(VectorSource(data1$Question))
corpus1 = tm_map(corpus1, content_transformer(tolower))
corpus1 = tm_map(corpus1, removeNumbers)
corpus1 = tm_map(corpus1, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
corpus1 = tm_map(corpus1, stemDocument)
corpus1 = tm_map(corpus1, stripWhitespace)

# convert to DTM
dtm1 = DocumentTermMatrix(corpus1)

# convert to dataframe
dataset1 = as.data.frame(as.matrix(dtm1))


# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
data_train1 = cbind(data1['Answers'], dataset1)

# 3. Train SVM model with the training matrix, specify type
library("e1071")
svmfit1 = svm(Answers ~., data_train1, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
designpred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  corpus1 = VCorpus(VectorSource(x))
  corpus1 = tm_map(corpus1, content_transformer(tolower))
  corpus1 = tm_map(corpus1, removeNumbers)
  corpus1 = tm_map(corpus1, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  corpus1 = tm_map(corpus1, stemDocument)
  corpus1 = tm_map(corpus1, stripWhitespace)
  
  # convert to DTM
  dtm1 = DocumentTermMatrix(corpus1)
  
  # convert to dataframe
  data_test1 = as.data.frame(as.matrix(dtm1))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  add_data1 = dataset1[1,]
  add_data1[add_data1 == 1] = 0
  data_test1=cbind(data_test1,add_data1)
  
  # 7. Predict the answer with the trained SVM model
  p1 = predict(svmfit1, data_test1)
  answer1 <- as.character(p1[2])
}

# Predict
#pred("Barcode scanner")
data2 <- data %>% filter(Area=="payments")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
corpus2 = VCorpus(VectorSource(data2$Question))
corpus2 = tm_map(corpus2, content_transformer(tolower))
corpus2 = tm_map(corpus2, removeNumbers)
corpus2 = tm_map(corpus2, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
corpus2 = tm_map(corpus2, stemDocument)
corpus2 = tm_map(corpus2, stripWhitespace)

# convert to DTM
dtm2 = DocumentTermMatrix(corpus2)

# convert to dataframe
dataset2 = as.data.frame(as.matrix(dtm2))


# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
data_train2 = cbind(data2['Answers'], dataset2)

# 3. Train SVM model with the training matrix, specify type
library("e1071")
svmfit2 = svm(Answers ~., data_train2, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
paymentspred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  corpus2 = VCorpus(VectorSource(x))
  corpus2 = tm_map(corpus2, content_transformer(tolower))
  corpus2 = tm_map(corpus2, removeNumbers)
  corpus2 = tm_map(corpus2, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  corpus2 = tm_map(corpus2, stemDocument)
  corpus2 = tm_map(corpus2, stripWhitespace)
  
  # convert to DTM
  dtm2 = DocumentTermMatrix(corpus2)
  
  # convert to dataframe
  data_test2 = as.data.frame(as.matrix(dtm2))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  add_data2 = dataset2[1,]
  add_data2[add_data2 == 1] = 0
  data_test2=cbind(data_test2,add_data2)
  
  # 7. Predict the answer with the trained SVM model
  p2 = predict(svmfit2, data_test2)
  answer2 <- as.character(p2[2])
}

data3 <- data %>% filter(Area=="ebay")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
corpus3 = VCorpus(VectorSource(data3$Question))
corpus3 = tm_map(corpus3, content_transformer(tolower))
corpus3 = tm_map(corpus3, removeNumbers)
corpus3 = tm_map(corpus3, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
corpus3 = tm_map(corpus3, stemDocument)
corpus3 = tm_map(corpus3, stripWhitespace)

# convert to DTM
dtm3 = DocumentTermMatrix(corpus3)

# convert to dataframe
dataset3 = as.data.frame(as.matrix(dtm3))


# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
data_train3 = cbind(data3['Answers'], dataset3)

# 3. Train SVM model with the training matrix, specify type
library("e1071")
svmfit3 = svm(Answers ~., data_train3, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
ebaypred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  corpus3 = VCorpus(VectorSource(x))
  corpus3 = tm_map(corpus3, content_transformer(tolower))
  corpus3 = tm_map(corpus3, removeNumbers)
  corpus3 = tm_map(corpus3, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  corpus3 = tm_map(corpus3, stemDocument)
  corpus3 = tm_map(corpus3, stripWhitespace)
  
  # convert to DTM
  dtm3 = DocumentTermMatrix(corpus3)
  
  # convert to dataframe
  data_test3 = as.data.frame(as.matrix(dtm3))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  add_data3 = dataset3[1,]
  add_data3[add_data3 == 1] = 0
  data_test3=cbind(data_test3,add_data3)
  
  # 7. Predict the answer with the trained SVM model
  p3 = predict(svmfit3, data_test3)
  answer3 <- as.character(p3[2])
}


data4 <- data %>% filter(Area=="inventory")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
corpus4 = VCorpus(VectorSource(data4$Question))
corpus4 = tm_map(corpus4, content_transformer(tolower))
corpus4 = tm_map(corpus4, removeNumbers)
corpus4 = tm_map(corpus4, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
corpus4 = tm_map(corpus4, stemDocument)
corpus4 = tm_map(corpus4, stripWhitespace)

# convert to DTM
dtm4 = DocumentTermMatrix(corpus4)

# convert to dataframe
dataset4 = as.data.frame(as.matrix(dtm4))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
data_train4 = cbind(data4['Answers'], dataset4)

# 3. Train SVM model with the training matrix, specify type
library("e1071")
svmfit4 = svm(Answers ~., data_train4, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
inventorypred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  corpus4 = VCorpus(VectorSource(x))
  corpus4 = tm_map(corpus4, content_transformer(tolower))
  corpus4 = tm_map(corpus4, removeNumbers)
  corpus4 = tm_map(corpus4, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  corpus4 = tm_map(corpus4, stemDocument)
  corpus4 = tm_map(corpus4, stripWhitespace)
  
  # convert to DTM
  dtm4 = DocumentTermMatrix(corpus4)
  
  # convert to dataframe
  data_test4 = as.data.frame(as.matrix(dtm4))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  add_data4 = dataset4[1,]
  add_data4[add_data4 == 1] = 0
  data_test4=cbind(data_test4,add_data4)
  
  # 7. Predict the answer with the trained SVM model
  p4 = predict(svmfit4, data_test4)
  answer4 <- as.character(p4[2])
}
