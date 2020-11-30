titanic.train<-read.csv("train.csv",stringsAsFactors =FALSE,header=TRUE) 
titanic.test<-read.csv("test.csv",stringsAsFactors = FALSE,header=TRUE)


titanic.train$IsTrainSet=TRUE
titanic.test$IsTrainSet=FALSE

titanic.test$Survived=NA

titanic.full<-rbind(titanic.train,titanic.test)

titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'
table(titanic.full$Embarked)

titanic.full[is.na(titanic.full$Age),"Age"]

age.median<-median(titanic.full$Age,na.rm=TRUE)

titanic.full[is.na(titanic.full$Age),"Age"]=age.median

table(is.na(titanic.full$fare))

#fare.median<-median(titanic.full$Fare,na.rm=TRUE)
#titanic.full[is.na(titanic.full$Fare),"Fare"]=fare.median

#linear model for fare prediction
upper.whisker<-boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter<-titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]

fare.equation<-"Fare~Pclass+Sex+Age+SibSp+Parch+Embarked"

fare.model<-lm(formula=fare.equation,
   data=titanic.full[outlier.filter,]
   )

fare.row<-titanic.full[is.na(titanic.full$Fare),c("Pclass","Age","Sex","SibSp","Parch","Embarked")]

fare.predictions<-predict(fare.model,newdata = fare.row)
titanic.full[is.na(titanic.full$Fare),"Fare"]=fare.predictions

#categorical casting
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)


#split dataset back to train and test
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived<-as.factor(titanic.train$Survived)

library(randomForest)


survived.equation<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula<-as.formula(survived.equation)

titanic.model<-randomForest(formula=survived.formula,data=titanic.train,ntree=500,mtry = 3,nodesize = 0.01*nrow(titanic.test))

features.eqation<-"Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived<-predict(titanic.model,newdata=titanic.test)

PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived

write.csv(output.df,file="Kaggle_submission.csv",row.names=FALSE)
