movies<-read.csv("movies.csv");
train<-subset(movies,movies$Year<2010);
test<-subset(movies,movies$Year>=2010);
model<-lm(Worldwide ~ ., data = train[ , 3:ncol(train)] );
cor(train$Worldwide,train$Production.Budget);

model1<-lm(Worldwide~ Runtime+Crime+Horror+Animation+History+Nominations+Production.Budget, train);

result<-predict(model1,test);

SSE=sum((result-test$Worldwide)^2)
SST=sum((mean(train$Worldwide)-test$Worldwide)^2)
R2=1-SSE/SST
RMSE=sqrt(R2)
RMSE=sqrt(SSE/nrow(FluTest))table()

movies$Performance = factor(ifelse(movies$Worldwide > quantile(movies$Worldwide, .75), "Excellent", 
ifelse(movies$Worldwide > quantile(movies$Worldwide, .25), "Average", "Poor")));

table(movies$Performance);

movies$Worldwide=NULL

set.seed(15071);

split=sample.split(movies$Performance,SplitRatio = 0.7)
trainSparse=subset(movies,split==TRUE )
testSparse=subset(movies,split==FALSE )

tweetCart=rpart(Performance~.,data=trainSparse[ , 3:ncol(trainSparse)],method="class");

predictCart=predict(tweetCart,newdata = testSparse,type="class")
table(testSparse$Performance,predictCart)

ROCRpredTest = prediction(predictCart, testSparse$trial)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

predTrain = predict(tweetCart)[,2]

summary(predTrain)



