#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("rmote")
#install.packages("ClusterR")
#install.packages("cluster")
#install.packages("reshape2")
#install.packages("corrplot")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("ggfortify")
#install.packages("datarium")
#install.packages("ggpubr")
#install.packages("rstatix")
#install.packages("broom")
#install.packages("olsrr")
#install.packages("randomForest")
library(ROCR)
library(randomForest)
library(olsrr)
library(broom)
library(ggpubr)
library(rstatix)
library(datarium)
library(corrplot)
library(rmote)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(sp)
library(FactoMineR)
library(factoextra)
library(ggfortify)


exam=MKT_data_exam
any(is.na(exam))
sum(is.na(exam))
complete.cases(exam)
exam[is.na(exam)]=0
any(is.na(exam))
exam=as.tibble(exam)
exam$NumDealsPurchases <- as.character(exam$NumDealsPurchases)
sapply(exam,class)
table(exam$NumDealsPurchases)

inc=ggplot(exam,aes(y=Income,x=NumDealsPurchases,fill=NumDealsPurchases))+
  geom_boxplot(alpha=0.5)+
  geom_jitter(alpha=0.25)

p1=ggplot(exam,aes(x=Income))+geom_bar()
p2=ggplot(exam,aes(x=MntWines))+geom_bar()
p3=ggplot(exam,aes(x=MntFruits))+geom_bar()
p4=ggplot(exam,aes(x=MntMeatProducts))+geom_bar()

#p5=ggplot(exam,aes(x=MntFishProducts))+geom_bar()
#p6=ggplot(exam,aes(x=MntSweetProducts))+geom_bar()
#p7=ggplot(exam,aes(x=NumDealsPurchases))+geom_bar()
#p8=ggplot(exam,aes(x=NumWebPurchases))+geom_bar()
#p9=ggplot(exam,aes(x=NumCatalogPurchases))+geom_bar()
#p10=ggplot(exam,aes(x=NumStorePurchases))+geom_bar()

#grid.arrange(p1, p2, p3,p4,ncol=4)
exam$NumDealsPurchases[exam$NumDealsPurchases == "5 ou plus"]  <- "5"

exam2=exam[,c(8,1,2,3,4,5,6,7,9,10,11)]

exam2=exam2 %>% 
  pivot_longer(
    !NumDealsPurchases,
    names_to="nom",
    values_to="valeur")

#ggplot(exam2,aes(x=NumDealsPurchases,fill=nom))+geom_bar(position = "dodge")
exam2$valeur <- as.integer(exam2$valeur)


exam3 = exam2 %>% group_by(NumDealsPurchases, nom) %>%
  summarise(valeur=sum(valeur)
  )

exam4= exam[,c("NumDealsPurchases",
               "MntFishProducts",
               "MntFruits",
               "MntGoldProds",
               "MntMeatProducts",
               "MntSweetProducts",
               "MntWines"
)]

exam4=exam4 %>% 
  pivot_longer(
    !NumDealsPurchases,
    names_to="nom",
    values_to="valeur")
exam4 = exam4 %>% group_by(NumDealsPurchases, nom) %>%
  summarise(valeur=sum(valeur)
  )


#ggplot(exam4,aes(x=nom,y=valeur,fill=NumDealsPurchases))+
geom_bar(stat = "identity")+
  geom_text(aes(label=valeur),position = position_stack(vjust = 0.5), 
            color="white", size=3)+
  scale_fill_brewer(palette="Paired")+
  theme(legend.position = "right")



exam5=exam[,c("NumDealsPurchases",
              "NumCatalogPurchases",
              "NumStorePurchases",
              "NumWebPurchases"
)]
exam5=exam5 %>% 
  pivot_longer(
    !NumDealsPurchases,
    names_to="nom",
    values_to="valeur")
exam5 = exam5 %>% group_by(NumDealsPurchases, nom) %>%
  summarise(valeur=sum(valeur)
  )

#ggplot(exam5,aes(x=nom,y=valeur,fill=NumDealsPurchases))+
geom_bar(stat = "identity")+
  geom_text(aes(label=valeur),position = position_stack(vjust = 0.5), 
            color="white", size=3)+
  scale_fill_brewer(palette="Paired")+
  theme(legend.position = "right")




exam6=exam[,c("NumDealsPurchases",
              "Income"
)]
exam6=exam6 %>% 
  pivot_longer(
    !NumDealsPurchases,
    names_to="nom",
    values_to="valeur")


#ggplot(exam6,aes(x=nom,y=valeur,fill=NumDealsPurchases))+
geom_bar(stat = "identity")+
  geom_text(aes(label=valeur),position = position_stack(vjust = 0.1), 
            color="white", size=10)+
  scale_fill_brewer(palette="Paired")+
  theme(legend.position = "right")


exam$NumDealsPurchases <- as.integer(exam$NumDealsPurchases)

exam=exam[,c(8,1,2,3,4,5,6,7,9,10,11)]

exam$NumDealsPurchases <- as.factor(exam$NumDealsPurchases)
str(exam)
index=sample(2,nrow(exam),replace = TRUE)
index
training=exam[index==1,]
test=exam[index==2,]

rfm= randomForest(NumDealsPurchases~.,data=training,mtry=4,ntree=2001,importance=TRUE)
rfm
plot(rfm)
NumDealsPurchases_pred=predict(rfm,test)
test$NumDealsPurchases_pred=NumDealsPurchases_pred
cfm=table(test$NumDealsPurchases,test$NumDealsPurchases)
cfm
classif=sum(diag(cfm)/sum(cfm))
classif
resul=data.frame(test$NumDealsPurchases, predict(rfm,test[,1:11],type="response"))
resul
plot(resul)
rammdom= randomForest(NumDealsPurchases~.,ntree=20,data = exam)
varImpPlot(rfm, main="variable importance")

getTree(rfm,1,labelVar = TRUE)

plot(rammdom,main="ROC Curve for Random Forest",col=2,lwd=2)

predic=predict(rfm,test)

ggplot(resul, aes(x=test.NumDealsPurchases, y=predict.rfm..test...1.11...type....response..)) + 
  geom_point(size=4,alpha = 0.5,color="darkblue") 








