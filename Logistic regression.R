-- Logistic regression

fedFunds<-read.csv("federalFundsRate.csv", stringsAsFactors=FALSE);

table(fedFunds$Chairman,fedFunds$RaisedFedFunds);

fedFunds$Chairman=as.factor(fedFunds$Chairman);
fedFunds$DemocraticPres=as.factor(fedFunds$DemocraticPres);
fedFunds$RaisedFedFunds=as.factor(fedFunds$RaisedFedFunds);

set.seed(201)



spl = sample.split(fedFunds$RaisedFedFunds, 0.7);

training=subset(fedFunds,spl==TRUE )
testing=subset(fedFunds,spl==FALSE )

model<-glm(RaisedFedFunds~ PreviousRate+Streak+Unemployment+ HomeownershipRate+DemocraticPres+MonthsUntilElection, data=fedFunds,family="binomial");

baseline model- 0.5025641

predictCart=predict(model,newdata = testing,type="response")
table(testing$RaisedFedFunds,predictCart>0.5)


pred = prediction(predictCart, testing$RaisedFedFunds)

as.numeric(performance(pred, "auc")@y.values)

install.packages("e1071")

library(e1071)

plot(performance(pred, "tpr", "fpr"),colorize=TRUE);

tc<-trainControl("cv",10);

cpGrid=expand.grid(.cp=seq(0.001,0.05,0.001))

train(RaisedFedFunds~ PreviousRate+Streak+Unemployment+ HomeownershipRate+DemocraticPres+MonthsUntilElection
                  ,data=training,
                method="rpart",trControl=tc,tuneGrid=cpGrid);

predictCart=predict(model2,newdata = testing,type="class")
table(testing$RaisedFedFunds,predictCart)

