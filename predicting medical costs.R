Wagetest=read.csv(file.choose(),header=T)

attach(Wagetest)
names(Wagetest)
dim(Wagetest)    
summary(Wagetest)

#testing and training dataset

test=Wagetest[!train,]          

test.truevalue=wage[!train]   

train.truevlue=wage[train]
dim(test) 

M1=glm(wage~year+age+maritl+education+jobclass+health+health_ins,data=Wagetest,subset=train,family=binomial)
summary(M1)   

glm.probs=predict(M1,Wagetest,type="response")  
M1.pred=rep("Low",2000) 
M1.pred[glm.probs>.5]="High"

#confusion matrix

table(M1.pred,test.truevalue)   
mean(M1.pred==Wagetest)  #0.06325
mean(M1.pred!=Wagetest)   # 0.93675

# 5-fold cross-validation

set.seed(1)
k=5
folds=sample(1:k,nrow(Weekly),replace=TRUE)

accuracy=rep(0,k)

for(i in 1:k)
{
  M1=glm(wage~year+age+maritl+education+jobclass+health+health_ins,data=Wagetest,subset=train,family=binomial)
  test=Weekly[folds==i, ]
  M1 =predict(M1,test, type="response")
  M1=rep("Low",nrow(Weekly[folds==i,]))
  M1[M1>.5]="High"
  
  test.truevalue=Direction[folds==i]
  table(test.truevalue,glm.pred3)       
  accuracy[i]=mean(M1==test.truevalue)  
}
mean(accuracy)  


#random forest 

library(randomForest)

set.seed(1)


rf.Wagetest=randomForest(wage~.,data=Wagetest,subset=train,mtry=2,importance=TRUE)  

yhat.rf=predict(rf.Wagetest,Wagetest.test,type="class")    

table(yhat.rf,wage.test)


importance(rf.Wagetest) 
varImpPlot(rf.Wagetest)






