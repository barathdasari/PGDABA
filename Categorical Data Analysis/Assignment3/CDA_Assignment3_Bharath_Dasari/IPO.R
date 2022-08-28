
#loading the data set
IPO_DATA <- read_excel("Desktop/IPO DATA.xlsx")
View(IPO_DATA)                                                                                   

#data cleansing
str(IPO_DATA)
IPO_DATA <- as.data.frame(IPO_DATA)
IPO_DATA$VCF<- as.factor(IPO_DATA$VCF)
IPO_DATA$LB<- as.factor(IPO_DATA$LB)
str(IPO_DATA)
View(IPO_DATA)

#splitting data into training and tesing sets
set.seed(2)
dt = sort(sample(nrow(IPO_DATA), nrow(IPO_DATA)*.7))
train<-IPO_DATA[dt,]
test<-IPO_DATA[-dt,]

#Full model
attach(train)
m1.tr<- glm(VCF ~ FVC + NSO + LB, family = binomial(link = "logit"))
summary(m1.tr)


#Variable selection
m2.tr<- glm(VCF ~  NSO + LB, family = binomial(link = "logit"))
summary(m2.tr)
anova(m1.tr,m2.tr,test = "LRT")

m3.tr<- glm(VCF ~ FVC + LB, family = binomial(link = "logit"))
summary(m3.tr)
anova(m1.tr,m3.tr,test = "LRT")

m4.tr<- glm(VCF ~ FVC + NSO , family = binomial(link = "logit"))
summary(m4.tr)
anova(m1.tr,m4.tr,test = "LRT")


#Multicollinearity check
summary(m1.tr)
summary(m4.tr)
library(performance)
check_collinearity(m4.tr)

#Best subsets
AIC(m1.tr,m2.tr,m3.tr,m4.tr)
BIC(m1.tr,m2.tr,m3.tr,m4.tr)

#stepwise model selection
step(m1.tr,direction = "backward")


#use model to predict effect of VCF on testing data set
predicted <- predict(m4.tr, test, type="response")
table(ActualValue=test$VCF, PredictedValue=predicted>0.5)
library(InformationValue)
sensitivity(test$VCF, predicted)
specificity(test$VCF, predicted)
optimal <- optimalCutoff(test$VCF, predicted)
misClassError(test$VCF, predicted, threshold=optimal)


#use model to predict effect of VCF on training data set
predicted.tr <- predict(m4.tr, train, type="response")
table(ActualValue=train$VCF, PredictedValue=predicted.tr>0.5)
library(InformationValue)
sensitivity(train$VCF, predicted.tr)
specificity(train$VCF, predicted.tr)
optimal <- optimalCutoff(train$VCF, predicted.tr)
misClassError(train$VCF, predicted.tr, threshold=optimal)






