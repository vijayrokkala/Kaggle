test<-read.csv("eBayiPadTest.csv")

train<-read.csv("eBayiPadTrain.csv")

str(test)

train$sold=as.factor(train$sold)



corpusAbstract=Corpus(VectorSource(train$description))
corpusAbstract=tm_map(corpusAbstract,tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract =tm_map(corpusAbstract ,removePunctuation)

corpusAbstract=tm_map(corpusAbstract,removeWords,stopwords("english"))

corpusAbstract=tm_map(corpusAbstract,stemDocument)

dtmtext=DocumentTermMatrix(corpusAbstract)

dtmtext=removeSparseTerms(dtmtext,0.99 )


dtmtext=as.data.frame(as.matrix(dtmtext))

colnames(dtmtext)=make.names(colnames(dtmtext))

train$description_flag=as.numeric(train$description=="")
test$description_flag=as.numeric(test$description=="")

train<-as.data.frame(c(train,dtmtext))


train1<-subset(train, select=-c(UniqueID,description))

bivariate = glm(sold~ ., data=train1, family="binomial")

bestmodel=glm(sold ~ biddable + startprice + condition + cellular + storage + 
                productline + description_flag, data=train1, family="binomial")

bestmodel=stepAIC(bivariate,trace=FALSE)

summary(bivariate)

corpusAbstract=Corpus(VectorSource(test$description))
corpusAbstract=tm_map(corpusAbstract,tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract =tm_map(corpusAbstract ,removePunctuation)

corpusAbstract=tm_map(corpusAbstract,removeWords,stopwords("english"))

corpusAbstract=tm_map(corpusAbstract,stemDocument)

dtmtext=DocumentTermMatrix(corpusAbstract)

dtmtext=removeSparseTerms(dtmtext,0.99 )


dtmtext=as.data.frame(as.matrix(dtmtext))

colnames(dtmtext)=make.names(colnames(dtmtext))

test<-as.data.frame(c(test,dtmtext))

test1<-subset(test, select=-c(description,UniqueID))

predictions = predict(bestmodel, newdata=test1, type="response")


MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = predictions)

write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)