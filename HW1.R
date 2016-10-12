library(data.table)
setwd("S:/IEMS313Lab_jhj930")
library(sqldf)

df<-fread("S:/IEMS313Lab_jhj930/Medicare_Provider_Util_Payment_PUF_CY2014/Medicare_Provider_Util_Payment_PUF_CY2014.txt",sep="\t")
df1<-fread("S:/IEMS313Lab_jhj930/Medicare_Provider_Util_Payment_PUF_CY2014/Medicare_Provider_Util_Payment_PUF_CY2014.txt",sep="\t",select=c(20,21,22,23,24,25,26))
HW1.data<-na.omit(df)
set.seed(7)
HW1.data<-HW1.data[sample(nrow(HW1.data),1000000),]

##HW1.mat<-df1[sample(nrow(df1),1000000),]
HW1.mat<-df[sample(nrow(df),1000000),]

HW1.mat<-as.matrix(HW1.mat)

#Payment_Rate<-HW1.mat$average_Medicare_allowed_amt/HW1.mat$average_submitted_chrg_amt

Payment_Rate<-HW1.data$average_Medicare_allowed_amt/HW1.data$average_submitted_chrg_amt


##HW1.matr<-data.frame(HW1.mat,Payment_Rate)

HW1.matr<-data.frame(HW1.data,Payment_Rate)

attach(HW1.data)
summary(HW1.data)
summary(HW1.matr)


par(mfrow=c(2,2))
plot(line_srvc_cnt)
qqnorm(line_srvc_cnt)
qqline(line_srvc_cnt) #mostly normal
plot(bene_unique_cnt)
qqnorm(bene_unique_cnt)
qqline(bene_unique_cnt)

plot(bene_day_srvc_cnt)
qqnorm(bene_day_srvc_cnt)
qqline(bene_day_srvc_cnt)#some outliers
plot(average_Medicare_allowed_amt)
qqnorm(average_Medicare_allowed_amt)#couple outliers
qqline(average_Medicare_allowed_amt)#mostly normal

plot(average_submitted_chrg_amt)
qqnorm(average_submitted_chrg_amt)
qqline(average_submitted_chrg_amt)
plot(average_Medicare_payment_amt)
qqnorm(average_Medicare_payment_amt)
qqline(average_Medicare_payment_amt)

plot(average_Medicare_standard_amt)#some outliers 
qqnorm(average_Medicare_standard_amt)#mostly normal
qqline(average_Medicarae_standard_amt)


cor(HW1.matr)

Standard<-scale(HW1.matr)


#clustering

#data<-HW1.matr[,c("average_submitted_chrg_amt","Payment_Rate")]
##data<-HW1.matr[,c("bene_day_srvc_cnt","Payment_Rate")]
data<-HW1.matr[,c("nppes_provider_state","Payment_Rate")]

data<-scale(data)

wssplot <- function(data, nc=20, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(data,20)

set.seed(6)

library(cluster)
km1<-kmeans(data,6)
##km1<-kmeans(data,7)
km1$centers
km1$cluster
km1$size

library(cluster)
clusplot(data,km1$cluster,main="2D Representation of the Cluster Solution",color=TRUE,shade=TRUE, labels=2,lines=0)

plot(HW1.matr[c("average_submitted_chrg_amt","Payment_Rate")],col=km1$cluster)
points(km1$centers[,c("average_submitted_chrg_amt","Payment_Rate")],col=1:8,pch=8,cex=2)

##plot(HW1.matr[c("bene_day_srvc_cnt","Payment_Rate")],col=km1$cluster)
##points(km1$centers[,c("bene_day_srvc_cnt","Payment_Rate")],col=1:8,pch=8,cex=2)
###plot(HW1.matr[c("bene_day_srvc_cnt","Payment_Rate")],col=km1$cluster)
###points(km1$centers[,c("bene_day_srvc_cnt","Payment_Rate")],col=1:8,pch=8,cex=2)
