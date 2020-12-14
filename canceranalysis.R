#Exploitary Data Analysis
#import
breast_cancer<-read.delim("breast-cancer.data.txt",sep ="," )
colnames(breast_cancer)<-c("Class","age","menopause","tumor-size","inv-nodes","node-caps","deg-malig","breast","breast-quads","irradiate")
View(breast_cancer)

#No of records
dim(breast_cancer)

#structure
str(breast_cancer)

#Levels
levels(breast_cancer$Class)
levels(breast_cancer$age)

#2 way contigency table
table(breast_cancer$Class,breast_cancer$age)

#filter records in which breast is equal to left
library(dplyr)
k<-breast_cancer%>%filter(breast=='left')
View(k)

#missing records
summary(breast_cancer)

#group by age
k2<-breast_cancer%>%group_by(age)
k2

#arrange by deg-malig
View(breast_cancer%>%arrange(`deg-malig`))

#count
a<-breast_cancer%>%count(age)
norec<-nrow(breast_cancer)
b=vector()
for(i in a$n){
  b=c(b,(i/norec)*100)
}
a=data.frame(a,percentage=b)
View(a)
a$percentage<-as.integer(a$percentage)
ggplot(breast_cancer,aes(x=breast_cancer$age,y=breast_cancer$`tumor-size`,col=breast_cancer$`tumor-size`,size=age))+geom_point(shape=36)
#basic plot
plot(a$percentage,type='b',main="Age vs Percentage",xlab="Age levels",ylab="percentage",col="red")
#Bar plot
barplot(a$percentage,names.arg=a$age,col='steelblue',main='Percentage of people affected',xlab = 'Age',ylab = 'Percentage')

#Box plot
ggplot(breast_cancer,aes(x=menopause,y=`deg-malig`))+geom_boxplot()
#side by side barchart
library(ggplot2)
ggplot(breast_cancer, aes(x = age, fill = irradiate)) + 
  geom_bar(position = "dodge")+ggtitle("Irradiate Rate")+xlab("Age")+ylab("Count")

#stack barchart
ggplot(breast_cancer, aes(x = age, fill = irradiate)) + 
  geom_bar(position = "fill")

#Bar plot
ggplot(breast_cancer,aes(x=breast_cancer$`tumor-size`))+geom_bar()+facet_wrap(~irradiate)+theme(axis.text.x = element_text(angle = 90))+ggtitle("Irradiation Based on Tumor Size")+xlab("Tumor Size")+ylab("Count")

#histogram
#ggplot(breast_cancer,aes(x=breast_cancer$breast))+geom_histogram()
hist(breast_cancer$`deg-malig`,col="steelblue")
#Density plot
ggplot(breast_cancer,aes(x=breast_cancer$`deg-malig`,fill=`tumor-size`))+geom_density(alpha=0.4)+ggtitle("Deg Malignant vs Tumor size")+xlab("Deg Malignant")+ylab("Density")

#violin plot
ggplot(breast_cancer,aes(x=`breast-quads`,y=`deg-malig`,fill=`breast-quads`))+geom_violin()

#pie chart
a<-a[-2]
View(a)
as.factor(a$age)
a<-tapply(INDEX=a$age,X=a$percentage,FUN=sum)
pie(a,labels=names(a),col=c("violet","steelblue","pink","yellow","orange","maroon"),main="Rate of people affected")

#bubble chart
ggplot(breast_cancer,aes(x=`tumor-size`,y=`breast-quads`, size=`deg-malig`,frame=age,col=age))+geom_point()

install.packages("treemap")
library(treemap)
tree<-treemap(breast_cancer,index = c("tumor-size"),vSize ="deg-malig")
#Decision Tree

#shuffle the index
shuffle_index<-sample(1:nrow(breast_cancer))
breast_cancer<-breast_cancer[shuffle_index,]
View(breast_cancer)
library(dplyr)

#fetch the required colums
clean_breast_cancer<-breast_cancer%>%select(-c(`breast-quads`,`node-caps`,`tumor-size`,`inv-nodes`))%>%
  mutate(age=factor(breast_cancer$age,levels=c("20-29","30-39","40-49","50-59","60-69","70-79"),labels=c("Between 20 &29","Between 30 &39","Between 40 &49","Between 50 &59","Between 60 &69","Between 70 &79")))
View(clean_breast_cancer)
#create training and test set
library(caTools)
set.seed(13)
split<-sample.split(clean_breast_cancer,SplitRatio=0.8)
training_set<-subset(clean_breast_cancer,split==T)
View(training_set)
test_set<-subset(clean_breast_cancer,split==F)
View(test_set)

#build a model
library(rpart)
library(rpart.plot)
fit<-rpart(formula=irradiate~.,data=training_set,method='class')

#plot decesion tree
rpart.plot(fit)
#make a prediction
pred_unseen<-predict(object=fit,newdata=test_set,type='class')
pred_unseen
#confusion matrix
table_mat<-table(test_set$irradiate,pred_unseen)
table_mat
accuracy<-sum(diag(table_mat))/sum(table_mat)
accuracy
error<-1-accuracy
error









