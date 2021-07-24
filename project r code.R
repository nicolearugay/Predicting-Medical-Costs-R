#load data

rm(list=ls())

insurance=read.csv(file.choose(),header=T)


names(insurance)
attach(insurance)
summary(insurance)
dim(insurance)   #1338 observations and 9 variables



## ##Explatory
lmData1 <- insurance[,-9]
lmData <- lmData1[,-8]
View(lmData)
str(lmData)


summary(lmData$charges)
par("mar")
par(mar=c(5,5,5,5))
hist(lmData$charges, main = "Distribution of Charges", col="pink",xlim=c(0,64000),ylim=c(0,400),xlab="Charges",ylab="frequency",labels = TRUE)
cor(lmData[c("age","bmi","children","charges")])
library(psych)
pairs.panels(lmData[c("age","bmi","children","charges")])


##*******Linear Regression*************
linearModel <- lm(charges~age+sex+bmi+children+smoker+region,data=lmData)
summary(linearModel)




### Clustering 


#Unsupervised learning does not use class
#Used for comparison, the 8th column is the category of charges
Insurance_Data.labs=insurance[,8]

#Variables initially explored are BMI, Children, and Smoking, to find similarities
#through clustering
Insurance_Data.data=insurance[,3:5]
#Our subset of data has 3 variables and 1338 entries
dim(Insurance_Data.data)
#The table shows that there are 669 Below Average charges and 669 Above Average charges
table(Insurance_Data.labs)

####Hierarchical clustering###
#step 1:Calculate Distances
data.dist=dist(Insurance_Data.data)
#Step 2:Attemt the three types of hierarchial clustering
hc1=hclust(data.dist)
hc2=hclust(data.dist, method="average")
hc3=hclust(data.dist, method="single")
#Step3:Plotting
par(mfrow=c(1,3))
plot(hc1, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hc2, main="Average Linkage", xlab="", sub="",ylab="")
plot(hc3, main="Single Linkage", xlab="", sub="",ylab="")
###Analysis: Single Linkage is not readable well, so we will exclue Single Linkage
#Step 4: Cutting
#There are 2 classes, so let's cut into two to see how well this works
hc.clusters1=cutree(hc1,2)
hc.clusters2=cutree(hc2,2)
#Step 5: Comparing to determine the best method
table(hc.clusters1,Insurance_Data.labs)
table(hc.clusters2,Insurance_Data.labs)
###Analysis: Complete Linkage seems a bit better based on this information
#Step 6: Remove a variable to get a better focus
#New data subset will look at BMI and Children 
Insurance_Data.data2=insurance[,3:4]
data.dist2=dist(Insurance_Data.data2)
#observe the complete and average methods
newhc2=hclust(data.dist2, method="complete")
newhc3=hclust(data.dist2, method="average")
#Cutting
newhc.clusters2=cutree(newhc2,2)
newhc.clusters3=cutree(newhc3,2)
#Compare results 
table(newhc.clusters2,Insurance_Data.labs)
table(newhc.clusters3,Insurance_Data.labs)
###Analysis:Complete Linkage seems better
par(mfrow=c(1,1))
plot(newhc2, labels=Insurance_Data.labs)
abline(h=1.3, col="red")
newhc2
###Analysis: Complete Linkage method is better for this data; 
###however, it does not provide valuable feedback


###K-Means clustering###
set.seed(1)
km.out1 =kmeans (Insurance_Data.data,2, nstart =1)
#Step 1: Determine optimal number of K by testing around the data
km.out1
km.out1$betweenss
km.out1$withinss
km.out1$tot.withinss
km.out1$totss
#Result in a table form
table(km.out1$cluster,Insurance_Data.labs)
#Step 2: Testing out the data by manipulating set seed
set.seed(11)
km.out2=kmeans (Insurance_Data.data,2, nstart =1)
km.out2$betweenss
km.out2$withinss
km.out2$tot.withinss
km.out2$totss
#Result in a table form
table(km.out2$cluster,Insurance_Data.labs)
#Step 3: Testing out the data by manipulating nstart
set.seed(1)
km.out3 =kmeans (Insurance_Data.data,2, nstart =20)
km.out3$betweenss
km.out3$withinss
km.out3$tot.withinss
km.out3$totss
#Result in a table form
table(km.out3$cluster,Insurance_Data.labs)
#Step 4: Changing to less variables
#Variables observed (as before) BMI and Children
Insurance_Data.data2=insurance[,3:4]
set.seed(1)
sd.data=scale(Insurance_Data.data2)
km.out4 =kmeans (sd.data,2, nstart =20)
km.out4$betweenss
km.out4$withinss
km.out4$tot.withinss
km.out4$totss
#Result table
table(km.out4$cluster,Insurance_Data.labs)
#Step 5: Using more variables 
km.out5 =kmeans (Insurance_Data.data2,2, nstart =20)
km.out5$betweenss
km.out5$withinss
km.out5$tot.withinss
km.out5$totss
#Result table
table(km.out5$cluster,Insurance_Data.labs)
#Step 6: To determine the optimal number of clusters we plot the data explored
wss = km.out5$totss
for (i in 2:10) wss[i] = sum(kmeans(Insurance_Data.data2,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="find the optimal value of K")
###Analysis: Optimal value of k should be 2

#Step 7: Visualize the Data for Deeper Analysis
#Libraries used 
library(cluster)
library(factoextra)
#BMI vs Charges using 2 Clusters
Insurance_Data.data3=insurance[, c(3,7)]
set.seed(123)
km.res <- kmeans(Insurance_Data.data3, 2, nstart = 25)
fviz_cluster(km.res, data = Insurance_Data.data3, palette = "jco",
             ggtheme = theme_minimal())

#Children vs Charges using 2 Clusters
Insurance_Data.data4=insurance[, c(4,7)]
set.seed(123)
km.res <- kmeans(Insurance_Data.data4, 2, nstart = 25)
fviz_cluster(km.res, data = Insurance_Data.data4, palette = "jco",
             ggtheme = theme_minimal())
#Smoking vs Charges using 2 Clusters
Insurance_Data.data5=insurance[, c(5,7)]
set.seed(123)
km.res <- kmeans(Insurance_Data.data5, 2, nstart = 25)
fviz_cluster(km.res, data = Insurance_Data.data5, palette = "jco",
             ggtheme = theme_minimal())



##LogRegression - 



######################

logdata=insurance[, c("sex","bmi","children","smoker","region","Group","Amount")]
summary(logdata)
glm.fit=glm(Amount~sex+bmi+children+smoker+region+Group,family="binomial",data=logdata)
summary(glm.fit) #AIC= 734.73
coef(glm.fit) 
exp(coef(glm.fit)) ### Odds Ratio

#######

set.seed(1)
train=sample(nrow(logdata),nrow(logdata)*0.8) #80% train 
logdata.test=logdata[-train,]
test.truevalue=Amount[-train] #true value from testing data 20% into test 
train.truevalue=Amount[train]

glm.fit2= glm(Amount~children+Group,family="binomial",subset=train,data=logdata)
summary(glm.fit2)
exp(coef(glm.fit2)) #AIC: 1121.6

glm.probs2=predict(glm.fit2,logdata.test, type="response")
glm.pred2=rep("0",268) 
glm.pred2[glm.probs2>.5] = "Above Average"
glm.pred2[glm.probs2<.5]= "Below Average"
glm.pred2

#######

table(glm.pred2,test.truevalue) # 2201493 or 22.01%
mean(glm.pred2==test.truevalue) # 0.2201493
mean(glm.pred2!=test.truevalue) # 0.7798507

accuracy = mean(test.truevalue == glm.pred2)
accuracy
summary(test.truevalue)



#Classification tree

library(tree)
IN=insurance[,-1]   #delete age column
in1=IN[,-6]    #delete charges column
in1

set.seed(1)

train=sample(nrow(in1),nrow(in1)*0.8)  # divide the set into two part; 80% for train

tree.model=tree(Amount~.,in1,subset=train)

in1.test=in1[-train,]

Amount.test=Amount[-train]

dim(in1.test)    #268 observation, 7 variables

# - Applying a Tree
cv.model=cv.tree(tree.model,K=10,FUN = prune.misclass)
cv.model
#results claim that the lowest error (dev) is at size 3 and 4, Which is 100 

par(mfrow=c(1,1))
prune.model=prune.tree(tree.model,best=3)  # we choose 3 
plot(prune.model)
text(prune.model,pretty = 0)

# from the plot, for those people who smoke, the amount will be above average;
#those people who don't smoke and are in the senior adult group will be above average

prunetree.pred=predict(prune.model,in1.test,type="class")
table(prunetree.pred,Amount.test)


mean(prunetree.pred==Amount.test)  # accuracy 0.9216418
mean(prunetree.pred!=Amount.test)   # 0.07835821




#Random Forest
library(randomForest)                          

set.seed(1)

rf.in=randomForest(Amount~.,data=in1,subset=train,mtry=2,importance=TRUE)  #mtry square root 6 rounded down
yhat.rf=predict(rf.in,in1.test,type="class")   
table(yhat.rf,Amount.test)  

mean(yhat.rf==Amount.test)   #0.9216418 accuracy
mean(yhat.rf!=Amount.test)  #0.07835821 error

importance(rf.in) 
varImpPlot(rf.in)



