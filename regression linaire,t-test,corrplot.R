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



exam$NumDealsPurchases <- as.character(exam$NumDealsPurchases)
exam$Income<- as.integer(exam$Income)
exam6$nom=NULL
exam6$NumDealsPurchases[exam6$NumDealsPurchases == "0"]  <- "R"
exam6$NumDealsPurchases[exam6$NumDealsPurchases == "1"]  <- "P"
exam6$NumDealsPurchases[exam6$NumDealsPurchases == "2"]  <- "P"
exam6$NumDealsPurchases[exam6$NumDealsPurchases == "3"]  <- "P"
exam6$NumDealsPurchases[exam6$NumDealsPurchases == "4"]  <- "P"
exam6$NumDealsPurchases[exam6$NumDealsPurchases == "5"]  <- "P"


exam6 %>% identify_outliers(valeur)
boxplot(valeur~ NumDealsPurchases, data=exam6)
tapply(exam6$valeur, exam6$NumDealsPurchases,mean)
shapiro.test(exam6$valeur[exam6$NumDealsPurchases == "R"])
shapiro.test(exam6$valeur[exam6$NumDealsPurchases == "P"])
ttest=t.test(valeur~ NumDealsPurchases,
             data = exam6,
             alternative="two.sided", 
             paired= F)
ttest
t=ttest$statistic[[1]]
t
df=ttest$parameter[[1]]
df
r=sqrt(t^2/(t^2+df))
r
round(r,3)

######le faite d'utiliser un coupon de reduc n'est pas corrélé aux revenue 
##donc si on utulise pas de bon  ca veut pas dire qu'on a  plus de moyen et inversement

stat.test=exam6 %>% t_test(valeur~ NumDealsPurchases) %>% add_significance()
stat.test
bxp=ggboxplot(exam6, x="NumDealsPurchases",y="valeur",ylab = "valeur",xlab = "NumDealsPurchases",add="jitter"
)
bxp
stat.test=stat.test%>% add_xy_position(x="NumDealsPurchases")

ggqqplot(exam6, x="valeur")
ggqqplot(exam6, x="valeur",facet.by = "NumDealsPurchases")

############corrrélation
exam$NumDealsPurchases <- as.integer(exam$NumDealsPurchases)
exam$Income<- as.integer(exam$Income)

corr=cor(exam)
cercle=corrplot(corr,method="pie",type = "lower")
cercle
done=corrplot(corr,method="number",type = "lower")
done


###### régrésion lineaire


plot(MntWines ~ MntFruits , data = exam, xlab="MntWines",ylab= "MntFruits")
exam.lm=lm(exam$MntWines~exam$MntFruits)
abline(exam.lm,col="green3")

predict(exam.lm)
plot(na.omit(exam$Income),predict(exam.lm))

results.lm=augment(exam.lm)
results.lm



######régresion multiple

final.fit=lm(Income~ MntWines +MntFruits+ MntGoldProds,data = exam)
summary(final.fit)
ols_vif_tol(final.fit)
plot(final.fit)

####ANOVA
exam$NumDealsPurchases <- as.character(exam$NumDealsPurchases)
exam2$NumDealsPurchases[exam2$NumDealsPurchases == "0"]  <- "R"
exam2$NumDealsPurchases[exam2$NumDealsPurchases == "1"]  <- "P"
exam2$NumDealsPurchases[exam2$NumDealsPurchases == "2"]  <- "P"
exam2$NumDealsPurchases[exam2$NumDealsPurchases == "3"]  <- "P"
exam2$NumDealsPurchases[exam2$NumDealsPurchases == "4"]  <- "P"
exam2$NumDealsPurchases[exam2$NumDealsPurchases == "5"]  <- "P"

exam2$NumDealsPurchases <- as_factor(exam2$NumDealsPurchases)
exam2$nom<- as_factor(exam2$nom)


set.seed(123)

bxp=ggboxplot(exam2,x="nom",y="valeur", color = "NumDealsPurchases",palette = "jco")
bxp

exam2 %>% group_by(NumDealsPurchases,nom) %>% shapiro_test(valeur)
ggqqplot(exam2,"valeur", ggtheme = theme_bw()) +
  facet_grid(nom~NumDealsPurchases)

res.aov=aov(valeur~ NumDealsPurchases+nom,data = exam2)
summary(res.aov)
model.tables(res.aov,type = "means",se=TRUE)
TukeyHSD(res.aov,which = "nom")
plot(res.aov,2)
aov_residuals=residuals(object = res.aov)
aov_residuals



