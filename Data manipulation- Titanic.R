model<-glm(Survived~ SibSp, data=train,family="binomial");

summary(model)

model<-glm(Survived~ Pclass+Sex+Age, data=train,family="binomial");

predictTest=predict(model,type="response",newdata = train) 

predictTest

table(predictTest>0.6)

#Data cleaning starts here
male_subset=subset(train1,train1$Sex=="male")
str(male_subset)

male_subset$Age[is.na(male_subset$Age)]=  mean(male_subset$Age,na.rm=TRUE)
#Check for NAs
is.na(male_subset$Age)

female_subset=subset(train1,train1$Sex=="female")
female_subset$Age[is.na(female_subset$Age)]=  mean(female_subset$Age,na.rm=TRUE)

#mean check
mean(female_subset$Age)
str(train)
train1=rbind(male_subset,female_subset)
table(is.na(train1$Age))


pcaData <- princomp(myData, scores = TRUE, cor = TRUE)
summary(pcaData) 


male_subset=subset(actualTest,actualTest$Sex=="male")
str(male_subset)

male_subset$Age[is.na(male_subset$Age)]=  mean(male_subset$Age,na.rm=TRUE)
#Check for NAs
is.na(male_subset$Age)

female_subset=subset(actualTest,actualTest$Sex=="female")
female_subset$Age[is.na(female_subset$Age)]=  mean(female_subset$Age,na.rm=TRUE)

#mean check
mean(female_subset$Age)
  str(train)
  actualTest=rbind(male_subset,female_subset)
  
#Data manipulation ends here

  
  MySubmission = data.frame(train1)
  write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)