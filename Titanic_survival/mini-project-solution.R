#################preprocess the data############################################
library(car)
library(pROC)
#import data 
data <- read.csv("D:/personal/study/555/555TERMPROJECT/world_population.csv")
#check and cleaning
data <- na.omit(data)
is.integer(data$Population..2020.)
is.integer(data$Land.Area..Km².)
is.integer(data$Urban.Pop..)
data$Urban.Pop..<-data$Urban.Pop..[!is.na(data$Urban.Pop..)]
as.integer(data$Urban.Pop..)
is.integer(data$Med..Age)
attach(data)

#Correlation 
# Calculate Correlation Coefficient
cor(Population..2020.,Land.Area..Km².)
cor(Med..Age,Urban.Pop..)
#show Sample means 
mean(Urban.Pop..)
mean(Population..2020.)
Meandifference <- mean(Population..2020.)-mean(Urban.Pop..)

#RQ1 POPULATION~LANDAREA::Simple Linear Regression
myModel <- lm(Population..2020.~ Land.Area..Km².)
anova(myModel)
summary(myModel)
plot(Population..2020.~ Land.Area..Km².,xlab="Land Area in KM Square",ylab="Population in 2020")
# A line on the past plot
abline(myModel,col="red")

#ANOVA
anova_table<- anova(lm(Population..2020.~Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share))
#create Analysis of Variance Table
SSE <-anova_table$`Sum Sq`[3]
SSE
SST <-anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]+anova_table$`Sum Sq`[3]
SST
R2 <- (anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]) / SST
R2
#ANCOVA
anova(glm(Population..2020.~ Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share), type=3)

#Variables are independent within each other


#Multiple Linear Regression 
m2 <- lm(Population..2020.~ Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share)
# Summar function can calculate almost everything that you need.
summary(m2)
#change model
m3 <- lm(Population..2020.~World.Share+Net.Change)
summary(m3)
# Calculate R squared Manually and the P-Value
# Total Sum of Squared.
totalss <- sum((Population..2020. - mean(Population..2020.))^2)
totalss
# Regression and Residual Sum of the Squared.
regss <- sum((fitted(m3) - mean(Population..2020.))^2)
regss
resiss <- sum((Population..2020.-fitted(m3))^2)
resiss
# Calculate the F Value.
fstatistic <- (regss/2)/(resiss/191)
fstatistic
# The P-Value for F-Statistic.
pvalue <- 1-pf(fstatistic, df1=9, df2=184)
pvalue

# Calculate R squared.
R2 <- regss/totalss
R2
# Regression Diagnostics
# Residual Plots
resid(m3)
par(mfrow=c(1,3))
plot(Population..2020., resid(m2), axes=TRUE, frame.plot=TRUE, xlab='pop', ylab='residue')
plot(fitted(m3), resid(m3), axes=TRUE, frame.plot=TRUE, xlab='fitted values', ylab='residue')
hist(resid(m3))
confint(m3, level=0.99)

#Access the model
cooks.dist <- cooks.distance(m3)
which(cooks.dist > (4/(nrow(data)-2-1)))

#backward selection
b1 = step(lm(Population..2020.~Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share),direction="backward")

#forward selection
FwModel = step(lm(Population..2020.~1), direction="forward", scope=(~Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share))
#backward and forward get the same result. 
summary(lm(Population..2020. ~ World.Share + Net.Change + Yearly.Change + Fert..Rate))


#Prediction of population if every country's urban population equals to 100
#Creating a data frame
variable_pop<-data.frame(Urban.Pop..=rep(100, times=length(Population..2020.)))

#fiting the linear model
liner_model<-lm(Population..2020.~Urban.Pop..+World.Share,data = data)

#predicts the future values
predict(liner_model,newdata = variable_pop)


#Cross Validation

library(tidyverse)
library(caret)

set.seed(125)
trainControl

# K-fold cross validation
# defining training control as repeated cross-validation and value of K is 10 and repetition is 3 times
train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)

# training the model by assigning sales column as target variable and rest other column as independent variable
model <- train(Population..2020. ~., data = data,
               method = "lm",
               trControl = train_control)

# printing model performance metrics along with other details
print(model)



#Logistics Regression

head(data)
data$levelofmedage <- ifelse(Med..Age>30,1,0)
cor(data$levelofmedage,Land.Area..Km².)
cor(data$levelofmedage,data$Population..2020.)
cor(data$levelofmedage,data$Yearly.Change)
cor(data$levelofmedage,data$Net.Change)
cor(data$levelofmedage,data$Density..P.Km².)
cor(data$levelofmedage,data$Migrants..net.)
cor(data$levelofmedage,data$Fert..Rate)
cor(data$levelofmedage,data$Urban.Pop..)
cor(data$levelofmedage,data$World.Share)
m <- glm(data$levelofmedage~Fert..Rate, family = "binomial")
anova(m)
summary(m)

# Odd ratio or ORs per 1 unit increase 
# same as calculation by hand (OR): exp(0.02119)

exp(m$coefficients[2])

exp(cbind(OR = coef(m), confint.default(m)))

# ROC plot and result
par(pty="s")

roc(data$levelofmedage~Fert..Rate, plot=TRUE)



