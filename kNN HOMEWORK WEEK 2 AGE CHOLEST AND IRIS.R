library(ggplot2)
library(dplyr)
install.packages("gcookbook")
library(gcookbook)
install.packages("ggrepel")
library(ggrepel)
agecholest<- age_cholest
names(agecholest)
agecholest$X<- NULL
agecholest$X.1<-NULL
agecholest$X.2<-NULL
head(agecholest)
agecholest_1<-agecholest[-c(21,22,23,24,25,26), ]
summary(agecholest_1)
agecholest_1

qplot(agecholest_1$cholest,agecholest_1$age,colour=agecholest_1$disease,
      xlab="cholesterol",ylab="age")+geom_text_repel(aes(label = agecholest_1$id), size = 2)+
      annotate(geom="text", x=300, y=40, label="+",color="black",size=4)
(age_min<-min(agecholest_1$age))
(age_max<-max(agecholest_1$age))
(age_range<-age_max-age_min)

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
age_cholest<-as.data.frame(age_cholest)
normalize(c(10,20,30,40,50))
names(agecholest_1)
age_cholest_n<-as.data.frame(lapply(agecholest_1[,c(3,4)],normalize))
head(age_cholest_n)
table<-cbind(agecholest_1$age,age_cholest_n$age,
             agecholest_1$cholest,age_cholest_n$cholest)
table
colnames(table)<-c("age", "age (n)","cholest","cholest (n)")
disease<-agecholest_1$disease
age_cholest_np<-cbind(agecholest_1,disease)
qplot(age_cholest_n$cholest,age_cholest_n$age,colour=age_cholest_np$disease,
            xlab="cholesterol (n)",ylab="age (n)")+
            geom_text_repel(aes(label=agecholest_1$id),size=2)+
            annotate(geom="text", x=.4877, y=.5,
            label="+",color="black",size=4)

newdata<-data.frame(c(age=40,cholesterol=300))
(new_age_n<-((40-age_min)/(age_range)))
(new_cholest_n<-((300-min(age_cholest_np$cholest))/
                   (max(age_cholest_np$cholest)-min(age_cholest_np$cholest))))
age_cholest_n$dist<-sqrt((age_cholest_n$age-.5)^2+
                        (age_cholest_n$cholest-.4877)^2)
colnames(agecholest_1)<-c("Patient ID","Age (N)","Cholesterol
(N)","Distance (N)","Disease?")
options(digits=4)
age_cholest_n$dist

#Define distances of all Data Points
age_cholest_n$dist<-sqrt((age_cholest_n$age-.5)^2+
                    (age_cholest_n$cholest-.4877)^2)
age_cholest_n<-cbind(age_cholest_np$id,age_cholest_n,age_cholest_np$disease)
age_cholest_nsort<-age_cholest_n[order(age_cholest_n$dist),]
age_cholest_nsort
install.packages("class")

head(age_cholest_nsort, 20)
library(class)
age_cholest_nsort_train_target<-age_cholest_np$disease  [1:10]#Trainig data labels
length(age_cholest_nsort_train_target)
age_cholest_nsort_test_target<-age_cholest_np$disease[11:20]#Test data labels
length(age_cholest_nsort_test_target)
age_cholest_nsort_train <- age_cholest_np [1:10,]
age_cholest_nsort_test <- age_cholest_np [11:20,]
agecholest_pred <- knn(train = age_cholest_nsort_train, test = age_cholest_nsort_test,
                       cl = age_cholest_nsort_train_target, k=5)
options(digits = 4)
table(age_cholest_nsort_test_target,agecholest_pred)

install.packages(models)
library(gmodels)
CrossTable(x=age_cholest_nsort_test_target,y=agecholest_pred,prop.chisq=FALSE,all=TRUE)





head(agecholest_1,20)
data_train<-agecholest_1[1:20,]#Slect training data
dim(data_train)
data_test<-agecholest_1[1:20,]#Select test data
dim(data_test)
data_train_target<-agecholest_1$Species[1:130]#Trainig data labels
length(data_train_target)
data_test_target<-data$Species[131:150]#Test data labels
length(data_test_target)



#Question 2
data<-iris#Assign iris dataset as data
summary(data)
str(data)
data$Species<-as.factor(data$Species)#kNN "target" Species must be a
factor
head(data,5)#Data grouped by Species
data[51:56,]
tail(data,5)
set.seed(54321)#Need to ungroup data
rand<-runif(nrow(data))# Generate 150 random numbers (0,1)
rand[1:20]
data<-data[order(rand),]#Reorder data randomly
head(data,25)
#Normalize data (0,1)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
normalize(c(1,2,3,4,5))#Check normalize function
normalize(c(10,20,30,40,50))#Should be same as above
data_n<-as.data.frame(lapply(data[,c(1,2,3,4)],normalize))#Normalize
data
head(data_n,20)
data_train<-data_n[1:130,]#Slect training data
dim(data_train)
data_test<-data_n[131:150,]#Select test data
dim(data_test)
data_train_target<-data$Species[1:130]#Trainig data labels
length(data_train_target)
data_test_target<-data$Species[131:150]#Test data labels
length(data_test_target)
#kNN algorithm
library(class)
mod<-knn(train=data_train,test=data_test,cl=data_train_target, k=15)
table(data_test_target,mod)#Compare test data predictions/reality
#Show CrossTable display
library(gmodels)
CrossTable(x=data_test_target,y=mod,prop.chisq=FALSE,all=TRUE)
#Show confusionMatrix display
library(caret)
confusionMatrix(mod,data_test_target)

