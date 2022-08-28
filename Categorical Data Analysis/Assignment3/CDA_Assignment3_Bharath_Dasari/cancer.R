
#Load the data set
PROSTATE_CANCER_DATA <- read_excel("Desktop/PROSTATE CANCER DATA.xlsx")
View(PROSTATE_CANCER_DATA) 

#Data cleansing
data<-PROSTATE_CANCER_DATA
data[ , c(1)] <- list(NULL)
View(data)
data$Y<- ifelse(data$GS > 7, 1, 0)
data<- as.data.frame(data) 
str(data)
data$SVI<-as.factor(data$SVI)
data$GS<-as.numeric(data$GS)


#training and testing splitting
dat = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dat,]
test<-data[-dat,] 

#Checking correlation among predictors
library(corrplot)
vars <- names(train) %in% c('Y','PSA','Cancer_volume','Weight','Age','BPH','CP')
selected_train <- train[vars]
corr.matrix <- cor(selected_train)
corrplot(corr.matrix, main="\n\nCorrelation Plot of numerical variables", method="number")

#Initial Model with all predictors
full.mod.tr<- glm(Y~ .-GS, family = binomial(link = "logit"),data=train)
summary(full.mod.tr)
library(performance)
check_collinearity(full.mod.tr)

#Variable Selection
m1.tr<- glm(Y~ PSA, family = binomial(link = "logit"))
summary(m1.tr)
m2.tr<-glm(Y~ PSA+Age, family = binomial(link = "logit"))
summary(m2.tr)
m3.tr<-glm(Y~ PSA+Cancer_volume, family = binomial(link = "logit"),data = train)
summary(m3.tr)
anova(full.mod.tr,m1.tr,test= "LRT")
anova(full.mod.tr,m2.tr,test= "LRT")
anova(full.mod.tr,m3.tr,test= "LRT")


#stepwise model selection
step(full.mod.tr,direction='backward')
final.model.tr<-glm(Y ~ PSA + Cancer_volume,family = binomial(link = "logit"),data = train )
summary(final.model.tr)
AIC(full.mod.tr,m1.tr,m2.tr,final.model.tr)

#Predictive Ability: Testing data set
test.model<- predict(final.model.tr,test,type = "response")
table(ActualValue=test$Y, PredictedValue=test.model>0.5)
library(InformationValue)
sensitivity(test$Y, test.model)
specificity(test$Y, test.model)
optimal <- optimalCutoff(test$Y, test.model)
misClassError(test$Y, test.model, threshold=optimal)


#Predictive Ability: Training Data set
train.model<- predict(final.model.tr,train,type = "response")
table(ActualValue=train$Y, PredictedValue=train.model>0.5)
library(InformationValue)
sensitivity(test$Y, test.model)
specificity(test$Y, test.model)
optimal <- optimalCutoff(test$Y, test.model)
misClassError(test$Y, test.model, threshold=optimal)

