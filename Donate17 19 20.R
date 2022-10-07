mtcarshe5
mtcarshe5$Car
mtcarshe5 <- strsplit((mtcarshe5$Car), " ")
mtcarshe5
Model <- sapply(mtcarshe5, "["2, )
Model
Make <- sapply(mtcarshe5, "["1, )
Make
roster <- cbind(Model,Make, [,-1]) 
mtcarshe5$Make
mtcarshe5$Model
mtcarshe5
carname<-strsplit((mtcarshe5$Car)," ")
make<-sapply(carname,"[",1)
model<-sapply(carname,"[",2)
make_model<-cbind(make,model)
head(mtcarsmakemodel<-cbind(make_model,mtcarshe5[,-1]))




bbdat<-bikebuyers
ageq<-quantile(bikebuyers$Age,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0))
plot(ageq,type="b",pch=16,col="blue",main="Bike Buyer Age 
     Quantiles",xlab="Quantiles",ylab="Age (Years)",cex.main=.8, 
     font.lab=2,cex.lab=.7,cex.axis=.7,font.axis=2)
ageq
incq<-quantile(bbdat$Income,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0))
plot(incq,type="b",pch=16,col="blue",main="Bike Buyer Income 
     Quantiles",xlab="Quantiles",ylab="Income",cex.main=.8, 
     font.lab=2,cex.lab=.7,cex.axis=.7,font.axis=2)
incq


library(dplyr)
filtered_data <- filter(he6_f, Donate17 == "Yes")
View(Donate <-he6_f %>% filter(Donate17=="Yes") 
     %>% group_by(Donate17))
View(filtered_data)

library(dplyr)
Donate <- filter(he6_f, Donate17 == "Yes", Donate20 == "Yes")
View(Donate <-he6_f %>% filter(Donate17=="Yes", Donate20=="Yes") 
     %>% group_by(Donate17, Donate20)%>% summarise(count=n()))
View(Donate)
Donate<-he6_f %>% filter(Donate17=="Yes", Donate20=="Yes")%>% group_by(Donate17, Donate20) %>% summarise("mean age"=mean(Age))
View(Donate)
View(Donate <-he6_f %>% filter(Donate17=="Yes", Gender=="F") 
     %>% group_by(Donate17, Gender)%>% summarise(count=n()))
View(Donate)
Donate<-he6_f %>% filter(Donate20=="Yes", Gender=="F")  %>% group_by(Donate20, Gender) %>% summarise("mean age"=mean(Age))
View(Donate)
Donate <- filter(he6_f, Donate20 == "Yes", Gender=="F")
View(Donate <-he6_f %>% filter(Donate20=="Yes", Gender=="F") 
     %>% group_by(Donate20, Gender)%>% summarise(count=n()))
View(Donate)

library(dplyr)


