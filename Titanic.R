#installation of packages
install.packages('ggplot2') # visualization
install.packages('ggthemes') # visualization
install.packages('scales') # visualization
install.packages('dplyr') # data manipulation
install.packages('mice') # imputation
install.packages('randomForest')
install.packages('caTools')

#Loading of packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('caTools') # classification algorithm
library('rpart')

train1<-read.csv('C:\\Users\\rokkalav\\Desktop\\R\\train.csv',stringsAsFactors = FALSE, na.strings="")

  spl<-sample.split(train1$Survived,0.7);
  train<-subset(train1,spl==TRUE);
  test<-subset(train1,spl==FALSE);

str(train);
str(test);

model1<-glm(Survived~ Pclass+Sex+Age, data=train,family="binomial");

summary(model1);
  
  predictCart=predict(model1,type="response")  
  table(train$Survived)
as.factor(train$Sex)

    
  ROCRpred=prediction(predictCart,train$Survived)
  ROCRpref=performance(ROCRpred,"tpr","fpr")
  plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=-c(0.2,1.7))
  
  table(train$Survived,predictCart>0.6)
  table(predictTest>0.6)
  
  predictTest
  predictTest=predict(model,type="response",newdata = test) 
  summary(predictTest)
names(train);
model;

bestmodel=stepAIC(model,trace=FALSE)

predictTest=predict(model1,type="response",newdata = test)

table(test$Survived,predictTest>0.6)

actualTest<-read.csv('C:\\Users\\rokkalav\\Desktop\\R\\test.csv',stringsAsFactors = FALSE)
#Go to datamanipulation file from here



predictTest=predict(model1,type="response",newdata = actualTest)

MySubmission = data.frame(PassengerId = actualTest$PassengerId, Survived = as.numeric(predictTest>0.6))

MySubmission
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)


#decision tree modelling
  rmfrst = rpart(Survived~ Pclass+Sex+Embarked, data = train, method="class")
  rmpredict=predict(rmfrst,type="class")
  rmpredict
  table(train$Survived,rmpredict)
  rmpredictTest=predict(rmfrst,type="class",newdata = actualTest)
  table(rmpredictTest)
  
  
  MySubmission = data.frame(PassengerId = actualTest$PassengerId, Survived = rmpredictTest)
  write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)