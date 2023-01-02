library(tidyverse)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(caret)
library(corrplot)
heartdisease <- read_csv("/Users/ashishpandey009/Documents/AIT 580/Project/Final Project/heart.csv")
View(heartdisease)
str(heartdisease)
summary(heartdisease)

heart <- heartdisease

ggplot(heartdisease, aes(y=BMI, x=AgeCategory)) + 
  geom_bar(stat="summary", fun="mean") + 
  facet_wrap(~Diabetic)+
  labs(title = "Heart Disease distribution based on SleepTime, Age, Sex")

ggplot(heartdisease, aes(x=AgeCategory, y=SleepTime)) + 
  geom_bar(aes(fill = Sex), position = "dodge", stat="summary", fun="mean")+
  labs(title = "Heart Disease distribution based on SleepTime, Age, Sex")+
  facet_wrap(~HeartDisease)

ggplot(heartdisease, aes(x=Race, y=Sex)) + 
  geom_bar(aes(fill = HeartDisease), position = "dodge", stat="summary", fun="mean")+
  labs(title = "Heart Disease distribution based on Race, Age, Sex")+
  facet_wrap(~AgeCategory) + coord_flip()

p1<- ggplot(heartdisease, aes(x=AgeCategory, y=PhysicalHealth)) + 
  geom_bar(aes(fill = Sex), position = "dodge", stat="summary", fun="mean")+
  labs(title = "Heart Disease distribution based on Physical Health, Age, Sex")+
  facet_wrap(~HeartDisease) + coord_flip()
p2<- ggplot(heartdisease, aes(x=AgeCategory, y=MentalHealth)) + 
  geom_bar(aes(fill = Sex), position = "dodge", stat="summary", fun="mean")+
  labs(title = "Heart Disease distribution based on Mental Health, Age, Sex")+
  facet_wrap(~HeartDisease) + coord_flip()

ggplot(heartdisease, aes(x=AgeCategory, fill=Sex)) + 
  geom_bar(position = "fill") +
  facet_wrap(~HeartDisease) + coord_flip()


gridExtra::grid.arrange(p1,p2, nrow=1, widths = c(1.5,1.5))

##Question 1
ggplot(heartdisease, aes(x = BMI)) +
  geom_histogram(heartdisease, mapping = aes(y = ..density..), binwidth = 5, fill = "grey") +
  geom_density()+
  labs(title = "Diabetes distribution based on BMI")

ggplot(heartdisease, aes(x=Diabetic, y=BMI)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) + coord_flip()

ggplot() +
  geom_violin(heartdisease,mapping =  aes(x=BMI,fill=Diabetic, y=HeartDisease),alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() + 
  labs(title = "BMI distribution based on Diabetes", xlab = "BMI", ylab = "Density") + coord_flip() +
  facet_wrap(~AgeCategory)

heartdisease
##Question 2 - race, sex and age
str(heartdisease)
heartdisease
plot1 <- ggplot(heartdisease, aes(fill=HeartDisease, y=HeartDisease, x=AgeCategory)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Studying 4 species..") +
  theme_ipsum() +
  xlab("")

plot2 <- ggplot(heartdisease, aes(fill=HeartDisease, y=HeartDisease, x=Race)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("GGWp") +
  theme_ipsum() +
  xlab("")
gridExtra::grid.arrange(plot1,plot2, nrow=1, widths = c(2,1))


table(heartdisease$Diabetic)
prop.table(table(heartdisease$Diabetic))
summary(heartdisease)
table(heartdisease$Diabetic)

nrow(heartdisease[is.na(heartdisease$Diabetic) | is.na(heartdisease$BMI),])
heartdisease$Diabetic <- as.factor(heartdisease$Diabetic)
set.seed(123)
training.samples <- heartdisease$Diabetic %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- heartdisease[training.samples, ]
test.data <- heartdisease[-training.samples, ]
str(test.data)
logistic <- glm(Diabetic ~ BMI, data = heartdisease, family = "binomial")
summary(logistic)$coef
probabilities <- logistic %>% predict(test.data, type = "response")
predicted.classes <- logistic %>% predict(test.data)
predicted.classes <- list(rep("No",nrow(test.data)))
predicted.classes
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
c_matrix <- confusionMatrix(data=as.factor(predicted.classes), reference = as.factor(test.data$Diabetic))
c_matrix
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Yes', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'No', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Yes', cex=1.2, srt=90)
  text(140, 335, 'No', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}
#Display results 
draw_confusion_matrix(c_matrix)


### -------------------------------
heart$HeartDisease<-as.factor(heart$HeartDisease)
heart$Smoking<-as.factor(heart$Smoking)
heart$AlcoholDrinking<-as.factor(heart$AlcoholDrinking)
heart$Stroke<-as.factor(heart$Stroke)
heart$DiffWalking<-as.factor(heart$DiffWalking)
heart$Sex<-as.factor(heart$Sex)
heart$AgeCategory<-as.factor(heart$AgeCategory)
heart$Race<-as.factor(heart$Race)
heart$Diabetic<-factor(heart$Diabetic,labels=c("No","Border","Yes","Yes, Pregnent")) # Renaming levels
heart$PhysicalActivity<-as.factor(heart$PhysicalActivity)
heart$GenHealth<-factor(heart$GenHealth,labels=c(5,2,3,1,4)) # Recoding to Likert scale (1-5)
heart$Asthma<-as.factor(heart$Asthma)
heart$KidneyDisease<-as.factor(heart$KidneyDisease)
heart$SkinCancer<-as.factor(heart$SkinCancer)
table(heart$HeartDisease)
summary(heart)

cor(heart[,c("BMI","PhysicalHealth","MentalHealth","SleepTime")])
cov(heart[,c("BMI","PhysicalHealth","MentalHealth","SleepTime")])
set.seed(2904)
s<-sort(sample(nrow(heart),nrow(heart)*.8))
train<-heart[s,]
test<-heart[-s,]
library(randomForest)
set.seed(2904)
rf<-randomForest(HeartDisease~.,ntree=100,data=train)
rf
plot(rf$err.rate[,1],type="l",main="Random Forest Error Rate",xlab="Number of Trees")
varImpPlot(rf,main="Variable Importance Plot for Random Forest")
rfpred<-predict(rf,test,type="class")
rftable<-table(test$HeartDisease,rfpred)
rftable
sum(diag(rftable))/nrow(test)
rftable["No","Yes"]/sum(rftable["No",])
rftable["Yes","No"]/sum(rftable["Yes",])
test$HeartDiseaseRF<-rfpred