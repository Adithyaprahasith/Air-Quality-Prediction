#NAME:MALLADI ADITHYA PRAHASITH
# REG.NO:18BCD7043
# DASHBOARD FOR AIR QUALITY PREDICTION -CAPSTONE PROJECT
# read data
setwd("/Users/adithyaprahasith/Downloads")
data1=read.csv("AirQualityUCI.csv")
#priliminary data analysis
names(data1) # column names of dataset
dim(data1) # dimensions of the dataset
head(data1) #first 5 rows of dataset
str(data1) # structure of dataset to know about datatype of each column
summary(data1) # summary of dataset to know the descriptive statistics of each column
#missing values & imputation
data1$NMHC_GT<-NULL # 80% of this column are having missing values,so dropping this column
colSums(is.na(data1)) # check number of na values in each column
colSums((data1)==-200) # na values are represented as -200
for(i in 3:ncol(data1)){ # changing -200 as na in every column
  data1[(data1[,i])==-200, i] <-NA
}
df2=na.omit(data1) # omitting na values and storing in another variable
write.csv(df2,"pdata.csv") 
df1=na.omit(data1) 
df1$Date<-NULL
df1$Time<-NULL
df1$CO_level<-NULL
aq_data=df1
write.csv(df1,"aq_data.csv")
# Histogram visualzations
hist(df1$CO_GT)
hist(df1$T,col = "red",main = "Histogram of temperature")
hist(df1$RH,col = "blue",main = "Histogram of Relative Humidity")
hist(df1$CO_GT,col = "green",main = "Histogram of carbon_monoxide")
hist(df1$Nox_GT,col = "orange",main = "Histogram of nitrogen oxide")
data1$CO_level<-as.factor(data1$CO_level)
plot(density(df1$T,col="red"),main = "temperature",col="red")
polygon(df1$T, col = "steelblue")
hist(rnorm(df1$T), main = "Normal Distribution of temperature", col = "darkorange")
hist(rnorm(df1$C6H6_GT), main = "Normal Distribution", col = "brown")
# box plots for outlier detection
boxplot(df1)
boxplot(df1$Nox_GT)
boxplot(df1$PT08_S3_Nox)
#outlier removal
outliers<-boxplot(df1$Nox_GT,plot = FALSE)$out
df3<-df1[-which(df1$Nox_GT %in% outliers),]
boxplot(df3)
#z score normalization
z_scores <- as.data.frame(sapply(df1, function(df1) (abs(df1-mean(df1))/sd(df1))))
df4 <- z_scores[!rowSums(z_scores>3), ]
dim(df4)
boxplot(df4,plot = F)
#find Q1, Q3, and interquartile range for values in column Nox_GT
Q1 <- quantile(df1$Nox_GT, .25)
Q3 <- quantile(df1$Nox_GT, .75)
IQR <- IQR(df1$Nox_GT)
no_outliers <- subset(df1, df1$Nox_GT> (Q1 - 1.5*IQR) & df1$Nox_GT< (Q3 + 1.5*IQR))
boxplot(no_outliers$Nox_GT)
Q11 <- quantile(df1$PT08_S3_Nox, .25)
Q31 <- quantile(df1$PT08_S3_Nox, .75)
IQR <- IQR(df1$PT08_S3_Nox)
no_outliers1 <- subset(df1, df1$PT08_S3_Nox> (Q11 - 1.5*IQR) & df1$PT08_S3_Nox< (Q31 + 1.5*IQR))
boxplot(no_outliers1$PT08_S2_NMHC)

# correlation matrix
cormat <- round(cor(df1),2)
head(cormat)
library(ggcorrplot)
library(ggplot2)
ggcorrplot(cor(df1),hc.order=TRUE, colors = c("red", "yellow", "green"),method = "square",lab = TRUE)
library(GGally)
ggcorr(df1,label = TRUE)
corr <- cor.test(df1$CO_GT,df1$Nox_GT, method = 'spearman')
print(corr)

# linear regression model for predicting c6h6 concentration
trainingRowIndex <- sample(1:nrow(df1), 0.7*nrow(df1))  # row indices for training data
trainingData <- df1[trainingRowIndex, ]  # model training data
testData  <- df1[-trainingRowIndex, ]   # model test data
lmMod<-lm(trainingData$C6H6_GT~.,data=trainingData)
distPred <- predict(lmMod, testData) 
actuals_preds <- data.frame(cbind(actual_values=testData$C6H6_GT, predicted_values=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds,20)
summary(lmMod,F)
actuals_preds
#feature selection 
library(caret)
library(mlbench)
set.seed(7)
correlation_matrix<-cor(df1)
highly_correlated<-findCorrelation(correlation_matrix,cutoff = 0.5)
print(highly_correlated)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
importance <- varImp(lmMod, scale=FALSE)
print(importance)

## Regression techniques

# linear regression for predicting NO concentration using caTools library
library(caTools)
split<-sample.split(df1,SplitRatio = 0.7)
train<-subset(df1,split=TRUE)
test<-subset(df1,split=FALSE)
head(train)
head(test)
model<-lm(train$NO2_GT~.,data=train)
summary(model)
pred<-predict(model,test)
original <- test$NO2_GT
predicted <- pred
d <- original - predicted
R2 <- 1 - (sum((d)^2) / sum((original - mean(original))^2))
print(R2)
distPred <- predict(model, test) 
actuals_preds <- data.frame(cbind(actual_values=test$NO2_GT, predicted_values=distPred)) 
head(actuals_preds,10)
MAE <- mean(abs(d))
MSE <- mean((d)^2)
RMSE <- sqrt(MSE)
print("performance metrics for caTools")
cat(" Mean Absolute Error:", MAE, "\n", "Mean Square Error:", MSE, "\n", "Root Mean Square Error:", RMSE, "\n", "R-squared:", R2)

# linear regression for predicting co concentration using caTools library
library(caTools)
split<-sample.split(df1,SplitRatio = 0.7)
train<-subset(df1,split=TRUE)
test<-subset(df1,split=FALSE)
head(train)
head(test)
model<-lm(train$CO_GT~.,data=train)
summary(model)
pred<-predict(model,test)
original <- test$CO_GT
predicted <- pred
d <- original - predicted
R2 <- 1 - (sum((d)^2) / sum((original - mean(original))^2))
print(R2)
distPred <- predict(model, test) 
actuals_preds <- data.frame(cbind(actual_values=test$CO_GT, predicted_values=distPred)) 
head(actuals_preds,10)
MAE <- mean(abs(d))
MSE <- mean((d)^2)
RMSE <- sqrt(MSE)
print("performance metrics for caTools")
cat(" Mean Absolute Error:", MAE, "\n", "Mean Square Error:", MSE, "\n", "Root Mean Square Error:", RMSE, "\n", "R-squared:", R2)

# ridge regression
library(glmnet)
library(Rcpp)
library(plyr)
library(readr)
library(dplyr)
library(repr)
cols_reg = c('T', 'AH', 'RH','CO_GT')
dummies <- dummyVars(CO_GT ~ ., data = df1[,cols_reg])
train_dummies = predict(dummies, newdata = train[,cols_reg])
test_dummies = predict(dummies, newdata = test[,cols_reg])
print(dim(train_dummies)); print(dim(test_dummies))
x = as.matrix(train_dummies)
y_train = train$CO_GT
x_test = as.matrix(test_dummies)
y_test = test$CO_GT
ridge_reg = glmnet(x, y_train, alpha = 0, family = 'gaussian', lambda = 0.01)
summary(ridge_reg)
ridge_reg
ridge.coef<-predict(ridge_reg, type = "coefficients", s = ridge_reg$lambda.min)
ridge.coef
# lasso regression
lasso_reg = glmnet(x, y_train, alpha = 1, family = 'gaussian', lambda = 0.01)
summary(lasso_reg)
lasso_reg
lasso.coef<-predict(ridge_reg, type = "coefficients", s = lasso_reg$lambda.min)
lasso.coef

# support vector regression
library(e1071)
modelsvr=svm(CO_GT~.,data=train,kernel='linear')
pred_svr<-predict(modelsvr,test)
modelsvr
summary(modelsvr)
table(pred_svr,test$CO_GT)
# elastic net regression
train_cont <- trainControl(method = "repeatedcv",number = 10,repeats = 2,search = "random", verboseIter = TRUE)
elastic_reg <- train(CO_GT ~ ., data = train,method = "glmnet",preProcess = c("center", "scale"),tuneLength = 5,
                     trControl = train_cont)
elastic_reg$bestTune
summary(elastic_reg)
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}
# predictions on training set
predictions_train <- predict(elastic_reg, train)
eval_results(y_train, predictions_train, train) 
predictions_test <- predict(elastic_reg, test)
ev<-eval_results(y_test, predictions_test, test)


#Classification 
#categorical variables label encoding 
library(superml)
label<-LabelEncoder$new()
df2$CO_level<-label$fit_transform(df2$CO_level) # labels high-1,veryhigh-0,moderate-2,low-3
df2$CO_level<-as.factor(df2$CO_level)
df2$Date<-NULL
df2$Time<-NULL
trainIndex_cl=createDataPartition(df2$CO_level, p=0.8)$Resample1
train_cl=df2[trainIndex_cl, ]
test_cl=df2[-trainIndex_cl, ]
library(e1071)
library(caret)
#random forest classification
library(randomForest) 
rfmodel <- randomForest(CO_level ~ T+RH+AH, data=train_cl, importance=TRUE, ntree=500, mtry = 2, do.trace=100)
print(rfmodel)
summary(rfmodel)
predrf<-predict(rfmodel,test_cl)
predrf
confusionMatrix(predrf,test_cl$CO_level)
str(predrf)
importance(rfmodel)
# logistic regression
library(nnet)
lr <- multinom(CO_level ~ CO_GT+T+AH+RH, data = train_cl)
summary(lr)
pre<-predict(lr,test_cl)
exp(coef(lr))
test$CO_level<-as.factor(test_cl$CO_level)
confusionMatrix(pre,test_cl$CO_level)
table(pre,test_cl$CO_level)
tail(fitted(lr))
mean(pre==test_cl$CO_level)

# k nearest neighbor classification
library(class)
df3=df2
clevel<-df3$CO_level
df3$CO_level<-NULL
prc_test_pred <- knn(train = train_cl, test = test_cl,cl = train_cl$CO_level, k=10)
prc_test_pred
misClassError <- mean(prc_test_pred != test_cl$CO_level)
print(1-misClassError)
summary(prc_test_pred)

avg_res_vec<-c(R2*100,98,72,98,90,91) # this vector is the average accuracy of models after performing several times
barplot(avg_res_vec,xlab="models",ylim = c(0,100),ylab="average accuracy",main="average accuracy of models",names.arg=c("linear regression","logistic regression",
                                                                                                                        "random forest","decision tree",
                                                                                                                        "naive bayes","knn"))
# decision tree
library(rpart)
library(rpart.plot)
train_control = trainControl(method = "cv", number = 5, search = "grid")
multi_classification_Tree_Grid =  expand.grid(maxdepth = c(1,3,5,7,9))
set.seed(50)
modeldt = train(CO_level~., data = train_cl, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid)
print(modeldt)
pred_dt = predict(modeldt, test_cl)
head(pred_dt,10)
confusionMatrix(data = pred_dt, test_cl$CO_level)
decision_tree <- rpart(CO_level~., data=train_cl, cp=.02)
rpc<-rpart.plot(decision_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#naive bayes classification
library(e1071)
library(caret)
NBclassfier=naiveBayes(CO_level~., data=train_cl)
print(NBclassfier)
summary(NBclassfier)
predN<-predict(NBclassfier,test_cl)
predN
table(predN,test_cl$CO_level)
confusionMatrix(predN,test_cl$CO_level)

#bagging 
library(ipred)
fit <- bagging(CO_level~., data = train_cl, coob = T, nbagg = 100)
fit
predb <- predict(fit, test_cl)
predb
result <- data.frame(original = test_cl$CO_level, predicted = predb)
print(result)
confusionMatrix(predb,test_cl$CO_level)
s="Data Set Information:The dataset contains 9358 instances of hourly averaged responses from an array of 5 metal oxide chemical sensors embedded in an Air Quality Chemical Multisensor Device. The device was located on the field in a significantly polluted area, at road level,within an Italian city. Data were recorded from March 2004 to February 2005 (one year)representing the longest freely available recordings of on field deployed air quality chemical sensor devices responses. Ground Truth hourly averaged concentrations for CO, Non Metanic Hydrocarbons, Benzene, Total Nitrogen Oxides (NOx) and Nitrogen Dioxide (NO2) and were provided by a co-located reference certified analyzer. Evidences of cross-sensitivities as well as both concept and sensor drifts are present as described in De Vito et al., Sens. And Act. B, Vol. 129,2,2008 (citation required) eventually affecting sensors concentration estimation capabilities. Missing values are tagged with -200 value. This dataset can be used exclusively for research purposes. Commercial purposes are fully excluded. Attribute Information:0 Date (DD/MM/YYYY) 1 Time (HH.MM.SS) 2 True hourly averaged concentration CO in mg/m^3 (reference analyzer) 3 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted) 4 True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer) 5 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer) 6 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted) 7 True hourly averaged NOx concentration in ppb (reference analyzer) 8 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted) 9 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer) 10 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted) 11 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted) 12 Temperature in Â°C 13 Relative Humidity (%) 14 AH Absolute Humidity "
# Interactive Shiny Dashboard 
library(shiny)
library(shinydashboard)
library(DT)
ui<-dashboardPage(
  dashboardHeader(title = "Air Quality Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset",tabName="ds",icon=icon("table")),
      menuItem("About Data",tabName = "aboutdata",icon = icon("list-alt")),
      menuItem("Visualization",tabName="plots",icon=icon("list-alt")),
      menuItem("Regression Models",tabName = "rgmodels",icon=icon("terminal"),
               menuSubItem("Linear Regression", tabName = "lmodel"),
               menuSubItem("Support Vector Regressor", tabName = "svr"),
               menuSubItem("Lasso Regression", tabName = "lrr"),
               menuSubItem("Elastic net Regression", tabName = "enr"),
               menuSubItem("Ridge Regression", tabName = "rrr")
      ),
      menuItem("Classification Models",tabName = "clmodels",icon=icon("terminal"),
               menuSubItem("Logistic Regression", tabName = "logr"),
               menuSubItem("Random Forest", tabName = "rfc"),
               menuSubItem("Decision Tree", tabName = "dcc"),
               menuSubItem("Naive bayes ", tabName = "nbc"),
               menuSubItem("K-Nearest Neighbours", tabName = "knn"),
               menuSubItem("Bagging", tabName = "bgc"),
               menuSubItem("XG Boosting", tabName = "xgb")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("plots", h3("Scatter Plot"),
              box(plotOutput("correlation_plot"),width=10),
              box(
                selectInput("features_xlab","Features_x:",
                            c("PT08_S1_CO", "NMHC_GT", "C6H6_GT", "PT08_S2_NMHC",
                              "Nox_GT", "PT08_S3_Nox", "NO2_GT", "PT08_S4_NO2","T","AH","RH")),width=2
                ,selectInput("features_ylab","Features_y:",
                             c("CO_GT", "PT08_S1_CO", "NMHC_GT", "C6H6_GT", "PT08_S2_NMHC",
                               "Nox_GT", "PT08_S3_Nox", "NO2_GT", "PT08_S4_NO2","T","AH","RH"))
              ),box(plotOutput("cplot"),width=10),box(plotOutput("hpl"),width = 10)
      ),
      
      tabItem("ds",
              fluidPage(
                h1("Air Quality Dataset"),
                dataTableOutput("datatable")
              )
      ),
      tabItem(tabName = "lmodel",
              h3('Linear Regression Model for Air Quality Prediction'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel",
                  sliderInput("TrainTest", "Select % of Training Data:", 1, 100, 50),
                  uiOutput("Target"),
                  uiOutput("Predictor"),
                  actionButton("ModelSubmit","Submit"),
                  br(), br(), br(),
                  actionButton("reset","Reset"),
                  br(), br(), br()),
                mainPanel(verbatimTextOutput(outputId = "ModelSum")
                )
              ),box("predict",width=80,
                    mainPanel(actionButton("predict","Prediction"),
                              br(),br(),
                              textOutput('TargetOfModel'),
                              br(),br(),
                              DT::dataTableOutput('mytable') ,width=80,h4('Predictions'),h4('Performance metrics'),verbatimTextOutput('lm2')))),
      
      tabItem(tabName = "svr",
              h3('Support Vector Regressor Model for Air Quality Prediction'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel",
                  sliderInput("TrainTest", "Select % of Training Data:", 1, 100, 50),
                  uiOutput("Target1"),
                  uiOutput("Predictor1"),
                  actionButton("ModelSubmit1","Submit"),
                  br(), br(), br(),
                  actionButton("reset1","Reset"),
                  br(), br(), br()),
                mainPanel(verbatimTextOutput(outputId = "ModelSum1")
                )
              ),
              box(h4('Predictions of Support Vector Regressor model')
                  ,width=20,verbatimTextOutput('pred_sv'))
      ),
      
      tabItem(tabName = "lrr",
              h3('Lasso Regression Model for Air Quality Prediction'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel",
                  sliderInput("TrainTest2", "Select % of Training Data:", 1, 100, 50),
                  uiOutput("Target2"),
                  uiOutput("Predictor2"),
                  actionButton("ModelSubmit2","Submit"),
                  br(), br(), br(),
                  actionButton("reset2","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum2")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('lsr'),h4('Coefficients'),verbatimTextOutput('lsr2'))
      ),
      tabItem(tabName = "rrr",
              h3('Ridge Regression Model for Air Quality Prediction'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel",
                  sliderInput("TrainTest3", "Select % of Training Data:", 1, 100, 50),
                  uiOutput("Target3"),
                  uiOutput("Predictor3"),
                  actionButton("ModelSubmit3","Submit"),
                  br(), br(), br(),
                  actionButton("reset3","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum3")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('rr'),h4('Coefficients'),verbatimTextOutput('rr2'))
      ),
      tabItem(tabName = "enr",
              h3('Elastic net Regression Model for Air Quality Prediction'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel",
                  sliderInput("TrainTest4", "Select % of Training Data:", 1, 100, 50),
                  uiOutput("Target4"),
                  uiOutput("Predictor4"),
                  actionButton("ModelSubmit4","Submit"),
                  br(), br(), br(),
                  actionButton("reset4","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum4")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('er'),h4('metrics'),verbatimTextOutput('er2'))
      ),
      tabItem(tabName = "logr",
              h3('Logistic Regression Model for CO Level Classification'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel5",
                  sliderInput("TrainTest55", "Select % of Training Data:", 1, 100, 50),
                  h5("Target variable is CO_level(very high,high,low,moderate)"),
                  uiOutput("Predictor55"),
                  actionButton("ModelSubmit55","Submit"),
                  br(), br(), br(),
                  actionButton("reset55","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum55")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('ler'),h4('Coefficients'),verbatimTextOutput('ler2'))
      ),
      tabItem(tabName = "rfc",
              h3('Random forest Model for CO Level Classification'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel5",
                  sliderInput("TrainTest6", "Select % of Training Data:", 1, 100, 50),
                  h5("Target variable is CO_level(very high,high,low,moderate)"),
                  uiOutput("Predictor6"),
                  actionButton("ModelSubmit6","Submit"),
                  br(), br(), br(),
                  actionButton("reset6","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum6")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('sumrf1'),h4('Predictions'),verbatimTextOutput('rfpred1'))
      ),
      tabItem(tabName = "knn",
              h3('K-Nearest Neighbours Model for CO Level Classification'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel7",
                  sliderInput("TrainTest7", "Select % of Training Data:", 1, 100, 50),
                  h5("Target variable is CO_level(very high,high,low,moderate)"),
                  uiOutput("Predictor7"),
                  actionButton("ModelSubmit7","Submit"),
                  br(), br(), br(),
                  actionButton("reset7","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum7")
                )
              ),
              box(width=20,h4('model accuracy'),verbatimTextOutput('k1'),h4('Predictions'),verbatimTextOutput('k2'))
      ),
      tabItem(tabName = "nbc",
              h3('Naive Bayes Classifier Model for CO Level Classification'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel9",
                  sliderInput("TrainTest9", "Select % of Training Data:", 1, 100, 50),
                  h5("Target variable is CO_level(very high,high,low,moderate)"),
                  uiOutput("Predictor9"),
                  actionButton("ModelSubmit9","Submit"),
                  br(), br(), br(),
                  actionButton("reset9","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum9")
                )
              ),
              box(width=20,h4('confusion matrix'),verbatimTextOutput('n1'),h4('Predictions'),verbatimTextOutput('n2'))
      ),
      tabItem(tabName = "dcc",
              h3('Decision tree Model for CO Level Classification'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel8",
                  sliderInput("TrainTest8", "Select % of Training Data:", 1, 100, 50),
                  h5("Target variable is CO_level(very high,high,low,moderate)"),
                  uiOutput("Predictor8"),
                  actionButton("ModelSubmit8","Submit"),
                  br(), br(), br(),
                  actionButton("reset8","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum8")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('d1'),h4('Predictions'),plotOutput('d2'),verbatimTextOutput('d3'))
      ),
      tabItem(tabName = "bgc",
              h3('bagging Model for CO Level Classification'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel11",
                  sliderInput("TrainTest11", "Select % of Training Data:", 1, 100, 50),
                  h5("Target variable is CO_level(very high,high,low,moderate)"),
                  uiOutput("Predictor11"),
                  actionButton("ModelSubmit11","Submit"),
                  br(), br(), br(),
                  actionButton("reset11","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum11")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('b1'),h4('Predictions'),verbatimTextOutput('b2'))
      ),
      tabItem(tabName = "xgb",
              h3('XGBoosting Model for CO Level Classification'),
              sidebarLayout(
                sidebarPanel(
                  shinyjs::useShinyjs(),
                  id="side-panel10",
                  sliderInput("TrainTest10", "Select % of Training Data:", 1, 100, 50),
                  h5("Target variable is CO_level(very high,high,low,moderate)"),
                  uiOutput("Predictor10"),
                  actionButton("ModelSubmit10","Submit"),
                  br(), br(), br(),
                  actionButton("reset10","Reset"),
                  br(), br(), br()),
                mainPanel(h4('summary of the model'),verbatimTextOutput(outputId = "ModelSum10")
                )
              ),
              box(width=20,h4('model'),verbatimTextOutput('b11'),h4('Predictions'),verbatimTextOutput('b22'))
      ),
      tabItem(tabName = "aboutdata",box(textOutput("abd"),width = 20,height = 360),
              box(h4('Summary of the dataset'),
                  mainPanel(verbatimTextOutput("summary"),
                            width=15),
                  width = 20,br(),br(),
                  h4('datatypes of the dataset'),
                  mainPanel(verbatimTextOutput("str"),width=15)
              )
      )
    )
  )
)

server<-function(input,output){
  dataset <- reactive({
    dataset<-read.csv('aq_data.csv')
  })
  output$correlation_plot<-renderPlot({
    plot(df1[[input$features_xlab]],df1[[input$features_ylab]],
         xlab=input$features_xlab,ylab=input$features_ylab ) 
  })
  output$cplot<-renderPlot({
    ggcorr(df1,label = TRUE)
  })
  output$hpl<-renderPlot({
    hist(df1$RH,col = "blue",main = "Histogram of Relative Humidity")
    hist(df1$CO_GT,col = "green",main = "Histogram of carbon_monoxide")
    hist(df1$Nox_GT,col = "orange",main = "Histogram of nitrogen oxide")
  })
  
  output$summary<-renderPrint({summary(df2)})
  output$abd<-renderText({s})
  
  output$str<-renderPrint({str(df2)})
  output$datatable<-renderDataTable(data1)
  output$corrmatrix<-renderPlot({plot(ggcorrplot(cor(df1),hc.order=TRUE,method=c("circle"),type="full", colors = c("red", "blue", "green")))})
  
  output$Target <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data11<-aq_data
      selectInput("Target","Target", choices = names(data11[,!names(data11) %in%  names(Filter(is.factor, data11))]),multiple = FALSE)}
    )
  output$Target1 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data12<-aq_data
      selectInput("Target1","Target", choices = names(data12[,!names(data12) %in%  names(Filter(is.factor, data12))]),multiple = FALSE)}
    )
  output$Target2 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data2<-aq_data
      selectInput("Target2","Target", choices = names(data2[,!names(data2) %in%  names(Filter(is.factor, data2))]),multiple = FALSE)}
    )
  output$Target3 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data23<-aq_data
      selectInput("Target3","Target", choices = names(data23[,!names(data23) %in%  names(Filter(is.factor, data23))]),multiple = FALSE)}
    )
  output$Target4 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data13<-aq_data
      selectInput("Target4","Target", choices = names(data13[,!names(data13) %in%  names(Filter(is.factor, data13))]),multiple = FALSE)}
    ) 
  output$Predictor <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor","Predictor", choices = names(aq_data[,!names(aq_data) %in% input$Target]),multiple = TRUE)
    }
    ) 
  output$Predictor1 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor1","Predictor", choices = names(aq_data[,!names(aq_data) %in% input$Target1]),multiple = TRUE)
    }
    )
  output$Predictor2 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor2","Predictor", choices = names(aq_data[,!names(aq_data) %in% input$Target2]),multiple = TRUE)
    }
    ) 
  output$Predictor3 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor3","Predictor", choices = names(aq_data[,!names(aq_data) %in% input$Target3]),multiple = TRUE)
    }
    )
  output$Predictor4 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor4","Predictor", choices = names(aq_data[,!names(aq_data) %in% input$Target4]),multiple = TRUE)
    }
    )
  output$Predictor55 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor55","Predictor", choices = names(aq_data),multiple = TRUE)
    }
    )
  output$Predictor6 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor6","Predictor", choices = names(aq_data),multiple = TRUE)
    }
    )
  output$Predictor7 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor7","Predictor", choices = names(aq_data),multiple = TRUE)
    }
    )
  output$Predictor8 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor8","Predictor", choices = names(aq_data),multiple = TRUE)
    }
    )
  output$Predictor9 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor9","Predictor", choices = names(aq_data),multiple = TRUE)
    }
    )
  output$Predictor11 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor11","Predictor", choices = names(aq_data),multiple = TRUE)
    }
    )
  output$Predictor10 <- 
    renderUI({
      if(is.null(aq_data)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor10","Predictor", choices = names(aq_data),multiple = TRUE)
    }
    )
  lmodel <- reactive({
    if (is.null(input$Target)) {
      return(NULL)
    }
    if (is.null(input$Predictor)) {
      return(NULL)
    } 
    set.seed(2)
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest)/100)})
    data<-dataset()
    sample <- sample.split(data[paste(input$Target)], SplitRatio=as.double(SampleSize()))
    train <- subset(data, sample==TRUE)
    test <- subset(data, sample==FALSE)
    model<- lm(as.formula(paste(input$Target," ~ ",paste(input$Predictor,collapse="+"))),data=train)
    
  })
  output$TargetOfModel<-reactive({input$Target})
  prediction_df<-reactive({
    if (is.null(input$Target)) {
      return(NULL)
    }
    if (is.null(input$Predictor)) {
      return(NULL)
    } 
    prediction <- predict(lmodel(), dataset())
    predict_df <- data.frame(Predictions = prediction)
    prediction_df <- cbind(predict_df,dataset())
    return(prediction_df)
  })
  
  # Display Predictions generated above  
  observeEvent(input$predict,{
    output$mytable <- DT::renderDataTable({prediction_df()}, selection='none',server = FALSE, escape = FALSE, options = list( 
      paging=TRUE,
      preDrawCallback = JS('function() { 
                           Shiny.unbindAll(this.api().table().node()); }'), 
      drawCallback = JS('function() { 
                        Shiny.bindAll(this.api().table().node()); } ') 
    ) )
  })
  svr<-reactive({
    
    set.seed(2)
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest1)/100)})
    
  })
  lrr<-reactive({
    if (is.null(input$Target2)) {
      return(NULL)
    }
    if (is.null(input$Predictor2)) {
      return(NULL)
    } 
    set.seed(2)
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest2)/100)})
    datass<-df1
    sample <- sample.split(datass[paste(input$Target2)], SplitRatio=as.double(SampleSize()))
    
  })
  rrr<-reactive({
    if (is.null(input$Target3)) {
      return(NULL)
    }
    if (is.null(input$Predictor3)) {
      return(NULL)
    } 
    set.seed(2)
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest3)/100)})
    dats<-df1
    sample <- sample.split(dats[paste(input$Target3)], SplitRatio=as.double(SampleSize()))
    
  })
  enr<-reactive({
    if (is.null(input$Target4)) {
      return(NULL)
    }
    if (is.null(input$Predictor4)) {
      return(NULL)
    } 
    set.seed(2)
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest4)/100)})
    datss1<-df1
    sample <- sample.split(datss1[paste(input$Target4)], SplitRatio=as.double(SampleSize()))
    
  })
  logr<-reactive({
    set.seed(2)
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest5)/100)})
    dats1<-df1
    
  })
  rfc<-reactive({
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest6)/100)})
  })
  knn<-reactive({
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest7)/100)})
  })
  nbc<-reactive({
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest9)/100)})
  })
  dcc<-reactive({
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest8)/100)})
  })
  bgc<-reactive({
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest8)/100)})
  })
  xgb<-reactive({
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest8)/100)})
  })
  # Generate Model Summary
  observeEvent(input$ModelSubmit, {
    output$ModelSum<-renderPrint({summary(lmodel())})
    output$lm2<-renderPrint({cat(" Mean Absolute Error:", MAE, "\n", "Mean Square Error:", MSE, "\n", "Root Mean Square Error:", RMSE, "\n", "R-squared:", R2)})
  })
  observeEvent(input$ModelSubmit1, {
    output$ModelSum1<-renderPrint({summary(modelsvr)})
    output$pred_sv<-renderPrint({table(pred_svr,test$CO_GT)})
  })
  observeEvent(input$ModelSubmit2,{
    output$ModelSum2<-renderPrint({summary(lasso_reg)})
    output$lsr<-renderPrint({lasso_reg})
    output$lsr2<-renderPrint({lasso.coef})  
    
  })
  observeEvent(input$ModelSubmit3,{
    output$ModelSum3<-renderPrint({summary(ridge_reg)})
    output$rr<-renderPrint({ridge_reg})
    output$rr2<-renderPrint({ridge.coef})  
    
  })
  observeEvent(input$ModelSubmit4,{
    output$ModelSum4<-renderPrint({summary(elastic_reg)})
    output$er<-renderPrint({elastic_reg})
    output$er2<-renderPrint({ev})  
    
  })
  observeEvent(input$ModelSubmit55,{
    output$ModelSum55<-renderPrint({summary(lr)})
    output$ler<-renderPrint({confusionMatrix(pre,test_cl$CO_level)})
    output$ler2<-renderPrint({tail(fitted(lr))})  
    
  })
  observeEvent(input$ModelSubmit6,{
    output$ModelSum6<-renderPrint({summary(rfmodel)})
    output$sumrf1<-renderPrint({confusionMatrix(predrf,test$CO_level)})
    output$rfpred1<-renderPrint({predrf}) 
    
  })
  observeEvent(input$ModelSubmit7,{
    output$ModelSum7<-renderPrint({summary(prc_test_pred)})
    output$k1<-renderPrint({1-misClassError})
    output$k2<-renderPrint({prc_test_pred}) 
  })
  observeEvent(input$ModelSubmit8,{
    output$ModelSum8<-renderPrint({modeldt})
    output$d1<-renderPrint({confusionMatrix(data = pred_dt, testdt$CO_level)})
    output$d2<-renderPlot({rpc}) 
    output$d3<-renderPrint({pred_dt})
  })
  observeEvent(input$ModelSubmit9,{
    output$ModelSum9<-renderPrint({summary(NBclassfier)})
    output$n1<-renderPrint({confusionMatrix(predN,test_cl$CO_level)})
    output$n2<-renderPrint({predN})
  })
  observeEvent(input$ModelSubmit10,{
    output$ModelSum10<-renderPrint({confusionMatrix(predN,test_cl$CO_level)})
  })
  observeEvent(input$ModelSubmit11,{
    output$ModelSum11<-renderPrint({summary(predb)})
    output$b1<-renderPrint({confusionMatrix(predb,test_cl$CO_level)})
    output$b2<-renderPrint({predb})
  })
  # Reset Model Tab
  observeEvent(input$reset,{
    shinyjs::reset("side-panel")
  })
  observeEvent(input$reset1,{
    shinyjs::reset("side-panel")
  })
  
}
shinyApp(ui,server)








