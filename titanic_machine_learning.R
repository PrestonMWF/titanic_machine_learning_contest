#Loading Data  
titanic_train<- read.csv("titanic_train.csv", na.strings = "")
titanic_test<- read.csv("titanic_test.csv", na.strings = "")
titanic_test$Survived<- NA
titanic_combined<- rbind(titanic_train,titanic_test)

#Cleaning and Transformations
library(tidyverse)

str(titanic_combined)
summary(titanic_combined)
names(titanic_combined)<- tolower(names(titanic_combined))

titanic_combined$name<- as.character(titanic_combined$name)
mr<- grepl("Mr.|Rev.|Col.|Capt.|Don.|Jonkheer.|Major.", titanic_combined$name,ignore.case = F)
mrs<- grepl("Mrs.|Mme.|Countess.|Dona.", titanic_combined$name,ignore.case = F)
miss<- grepl("Miss.|Ms.|Mlle.",titanic_combined$name,ignore.case = F)
master<- grepl("Master.", titanic_combined$name,ignore.case = F)
dr<- grepl("Dr.", titanic_combined$name,ignore.case = F, fixed=T)

titanic_combined$cabin<- as.character(titanic_combined$cabin)
titanic_combined<- mutate(titanic_combined,
                          survived= as.factor(ifelse(survived=="0","Dead","Survived")),
                          pclass= as.factor(pclass),
                          title=ifelse(mr,"Mr",NA),
                          title=ifelse(mrs, "Mrs",title),
                          title=ifelse(miss, "Miss",title),
                          title=ifelse(master, "Master",title),
                          title=ifelse(dr, "Mr",title),
                          title=ifelse(titanic_combined$passengerid=="797", "Mrs",title),
                          title=ifelse(titanic_combined$passengerid=="545", "Mr",title),
                          with.sibling= factor(ifelse(titanic_combined$sibsp > 0,"1","0")),
                          with.parents= factor(ifelse(titanic_combined$parch > 0,"1","0")),
                          with.family= factor(ifelse(titanic_combined$sibsp > 0 & titanic_combined$parch > 0,
                                                     "1","0")),
                          family.size= (titanic_combined$sibsp + titanic_combined$parch) + 1,
                          cabin.status= factor(ifelse(is.na(titanic_combined$cabin),"unknown","deck")),
                          age= as.integer(ifelse(age < 1,"1", age)))
titanic_combined$title<- as.factor(titanic_combined$title)
titanic_combined$name<- NULL
titanic_combined$ticket<- NULL
titanic_combined$cabin<- NULL

#Exploring missing data and imputations
library(mice)
library(Hmisc)

md.pattern(titanic_combined)
set.seed(1017)
impute_run<- aregImpute(~ pclass+title+sex+age+embarked+fare,
                        data = titanic_combined, n.impute = 5)
age_impute<- data.frame(impute_run$imputed$age)
age_impute$impute.avg<- apply(age_impute,1,
                              FUN= function(x) sum(x/5))
titanic_combined$age<- round(ifelse(is.na(titanic_combined$age),
                                    age_impute$impute.avg, titanic_combined$age),0)
impute_run$imputed$embarked # suggest S for 62, C for 830 
titanic_combined$embarked[[62]]<- "S"
titanic_combined$embarked[[830]]<- "C"
impute_run$imputed$fare
titanic_combined$fare[[1044]]<- round(as.numeric(
  mean(impute_run$imputed$fare)),2)
titanic_combined$life.status<- factor(ifelse(titanic_combined$age >= 18,"Adult","Minor"))

#Splitting sets back into train and test
titanic_train<- titanic_combined[1:891,]
titanic_test<- titanic_combined[892:1309,]
titanic_test$survived<- NULL

#Exploring the data- individual variables
table(titanic_train$survived)
prop.table(table(titanic_train$survived))
ggplot(titanic_train) + geom_bar(aes(survived,fill=survived))
ggplot(titanic_train) + geom_bar(aes(x=survived,fill=sex))

ggplot(titanic_train) + geom_bar(aes(pclass, fill=pclass))
ggplot(titanic_train) + geom_bar(aes(sex, fill=sex))
ggplot(titanic_train) + geom_bar(aes(life.status, fill=life.status))
ggplot(titanic_train) + geom_bar(aes(cabin.status, fill=cabin.status))
ggplot(titanic_train) + geom_bar(aes(embarked, fill=embarked))
ggplot(titanic_train) + geom_bar(aes(title, fill=title))
ggplot(titanic_train) + geom_bar(aes(with.sibling, fill=with.sibling))
ggplot(titanic_train) + geom_bar(aes(with.parents, fill=with.parents))
ggplot(titanic_train) + geom_bar(aes(with.family,fill=with.family))

ggplot(titanic_train) + geom_histogram(aes(age),fill="blue")
ggplot(titanic_train) + geom_histogram(aes(sibsp),fill="blue")
ggplot(titanic_train) + geom_histogram(aes(parch),fill="blue")
ggplot(titanic_train) + geom_histogram(aes(fare),fill="blue")
ggplot(titanic_train) + geom_histogram(aes(family.size),fill="blue")
range(titanic_train$family.size)

#Exploring Data- bi and multi-variate
ggplot(titanic_train) + 
  geom_bar(aes(x=pclass, fill=survived), position = "fill")
ggplot(titanic_train) + 
  geom_bar(aes(x=sex, fill=survived), position = "fill")

ggplot(titanic_train) + geom_bar(aes(x=survived,fill=sex))
ggplot(titanic_train) + geom_bar(aes(x=survived,fill=life.status))
ggplot(titanic_train) + geom_bar(aes(x=survived,fill=pclass)) +
  facet_grid(.~sex)
ggplot(titanic_train) + geom_bar(aes(x=survived,fill=sex)) +
  facet_grid(.~pclass)
table(titanic_train$sex,titanic_train$survived)
round(prop.table(table(titanic_train$sex,titanic_train$survived)),2)

ggplot(titanic_train) + geom_bar(aes(x=survived,fill=life.status)) +
  facet_grid(.~sex)

ggplot(titanic_train) + 
  geom_bar(aes(x=embarked, fill=survived), position = "fill")

ggplot(titanic_train) + 
  geom_bar(aes(x=title, fill=survived), position = "fill")

ggplot(titanic_train) + 
  geom_bar(aes(x=cabin.status, fill=survived), position = "fill")

ggplot(titanic_train) + 
  geom_bar(aes(x=multiple.cabin, fill=survived), position = "fill")

ggplot(titanic_train) + 
  geom_bar(aes(x=family.size, fill=survived), position = "fill") +
  scale_x_discrete(limits=c(1:11))

ggplot(titanic_train,aes(x=fare,y=pclass, colour=survived)) +
  geom_point() +
  geom_jitter(position = position_jitter(height = .2))+
  facet_grid(sex~.)

ggplot(titanic_train,aes(x=family.size,y=pclass, colour=survived)) +
  geom_point()+
  geom_jitter(position = position_jitter(height = .2))+
  scale_x_discrete(limits=c(1:11))+
  geom_vline(xintercept = 5, linetype="dashed", colour="red")

ggplot(titanic_train,aes(x=family.size,y=pclass, colour=survived)) +
  geom_point() +
  geom_jitter(position = position_jitter(height = .2))+
  scale_x_discrete(limits=c(1:11))+
  facet_grid(sex~.)+
  geom_vline(xintercept = 5, linetype="dashed", colour="red")

ggplot(titanic_train,aes(x=age,y=pclass, colour=survived)) +
  geom_point() +
  geom_jitter(position = position_jitter(height = .2))+
  facet_grid(sex~.)

ggplot(titanic_train,aes(x=age,y=title, colour=survived)) +
  geom_point() +
  geom_jitter(position = position_jitter(height = .2))+
  facet_grid(sex~.)+
  ylab("")

ggplot(titanic_train,aes(x=age,y=life.status, colour=survived)) +
  geom_point() +
  geom_jitter(position = position_jitter(height = .2))+
  facet_grid(sex~.)+
  ylab("")

#Variable Importance- Boruta
library(Boruta)
bor.test<- Boruta(survived~., titanic_train[,-1],maxRuns = 100, doTrace = 0)
plot(bor.test,cex.axis=.7, las=2, xlab="", main="Titanic Variable Importance")
getSelectedAttributes(bor.test, withTentative = F)

#Variable Importance- earth
library(earth)
earth.test<- earth(survived~., data = titanic_train[,-1])
earth.importance<- evimp(earth.test)
plot(earth.importance)

#Modelling- evtree
library(caret)
library(evtree)

set.seed(1017) #83.5, 0.817 AUC
evtree_titanic<- evtree(survived~.,
                        data = titanic_train[,-1])
plot(evtree_titanic)
confusionMatrix(predict(evtree_titanic),titanic_train$survived)
evtree_pred<- predict(evtree_titanic, type="response")
roc.curve(titanic_train$survived, evtree_pred)

set.seed(1017) #83.5- no change
evtree_titanic2<- evtree(survived~age+sex+pclass+title+family.size,
                         data = titanic_train[,-c(1,16)],
                         control = evtree.control(minsplit=50, minbucket=25,niterations = 50000))
confusionMatrix(predict(evtree_titanic2),titanic_train$survived, dnn = c("Prediction","Actual"))

#Modelling- Random Forrest
library(randomForest)

set.seed(1017) #82.72
forest_titanic<- randomForest(survived~., data = titanic_train[,-c(1,16)])
confusionMatrix(predict(forest_titanic),titanic_train$survived, dnn = c("Prediction","Actual"))

#Logistic Regression
set.seed(1017) #83.95 accuracy, 0.878 AUC
log.reg_titanic<- glm(survived~., data = titanic_train[,-1], family = binomial)
log_predict<- predict(log.reg_titanic, type="response")
table(titanic_train$survived, log_predict > 0.5)
roc.curve(titanic_train$survived, log_predict, main="Titanic Prediction using Logistic Regression")

set.seed(1017) #83.95 accuracy, 0.878 AUC
log.reg_titanic2<- glm(survived~ sex+title+fare+cabin.status+family.size, data = titanic_train[,-1], family = binomial)
log_predict2<- predict(log.reg_titanic, type="response")
table(titanic_train$survived, log_predict2 > 0.5)
roc.curve(titanic_train$survived, log_predict2, main="Titanic Prediction using Logistic Regression")

#Modelling- Oversampled evtree w/ ROSE
library(ROSE)

set.seed(100) #85.63, 0.856 AUC
rose_titanic<- ROSE(survived~., data = titanic_train[,-1])$data
rose.evtree<- evtree(survived~., data = rose_titanic)
pred.rose <- predict(rose.evtree, data= rose_titanic, type="response")
roc.curve(rose_titanic$survived, pred.rose, main="Titanic Prediction using Randomly Over Sampling Examples")
confusionMatrix(predict(rose.evtree), rose_titanic$survived)

#Modelling- ADA (boosting algorithm)
library(ada)
set.seed(42)
boost_titanic <- ada(survived ~ .,
                     data=xg_data,
                     control=rpart.control(maxdepth=30,
                                           cp=0.010000,
                                           minsplit=20,
                                           xval=10),
                     iter=50)

#predict w/ Evtree- Score: 0.78947
set.seed(1017)
pred.titanic_evtree<- predict(evtree_titanic, newdata = titanic_test[,-1])
submission<- data.frame(PassengerID=c(892:1309),Survived=pred.titanic_evtree)
submission$Survived<- ifelse(submission$Survived=="Survived","1","0")
write.csv(submission,"Titanic_Kaggle_Submission.csv")

#predict w/ Logistic Regression- Score: 0.79426
set.seed(1017)
logistic_prediction<- predict(log.reg_titanic, newdata = titanic_test[,-1], type = "response")
submission2<- data.frame(PassengerID=c(892:1309),Log.Reg= logistic_prediction,Survived= ifelse(logistic_prediction > .5,"1","0"))
write.csv(submission2, "Titanic_Logistic_Kaggle_Submission.csv")

set.seed(1017)
logistic_prediction2<- predict(log.reg_titanic, newdata = titanic_test[,-1], type = "response")
submission<- data.frame(PassengerID=c(892:1309),Log.Reg= logistic_prediction,Survived= ifelse(logistic_prediction > .5,"1","0"))
write.csv(submission, "Titanic_Logistic_Kaggle_Submission-6 variables.csv")

#predict w/ ROSE- Score: 0.77033
set.seed(1017)
rose.titanic_evtree<- predict(rose.evtree,newdata = titanic_test[,-1])
submission3<- data.frame(PassengerID=c(892:1309),Survived=rose.titanic_evtree)
submission3$Survived<- ifelse(submission3$Survived=="Survived","1","0")
write.csv(submission3,"Titanic_ROSE_Kaggle_Submission.csv")

#predict w/ ADA- Score: 0.78947
set.seed(42)
boost_titanic_predict<- predict(boost$model, newdata = titanic_test)
submission4<- data.frame(PassengerID=892:1309,Survived=boost_titanic_predict)
submission4$Survived<- ifelse(submission4$Survived=="Survived",1,0)
write.csv(submission4,"Titanic_Boost_Kaggle_Submission.csv")
