library(caret)
library(MASS)



red <- read.csv("C:/Users/Kevin/Desktop/機器學習/hw2/winequality-red.csv",sep=";")
white <- read.csv("C:/Users/Kevin/Desktop/機器學習/hw2/winequality-white.csv",sep=";")
str(red)
summary(red)
str(white)
summary(white)

data <- rbind(red,white) #combind red and white wine data
class(data)
head(data)

#check for missing value
for ( i in 1:length(data[1,]) ){
  na <- sum(is.na(data[, i]) )
  print( na )
} 
#no missing value

#add wine category label(dummy coding)
data["wine_category"] = c(rep(1,dim(red)[1]),rep(0,dim(white)[1])) #red=1,white=0

#Randomly shuffle the data
data <- data[sample(nrow(data)),]

#correlation plot
library(corrplot)
wine_corr <- cor(data)
corrplot(wine_corr, method = "circle")


#boxplot
library(ggplot2)

ggplot(data,aes(x=wine_category,y=fixed.acidity))+
  geom_boxplot(aes(fill=wine_category))+ #box color
  geom_jitter(aes(x=wine_category,y=fixed.acidity),
  position=position_jitter(width=0.1,height=0),
  alpha=0.1, #size of dot
  size=1)+ #trasparency of dot
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for fixed.acidity vs wine_category")
  
ggplot(data,aes(x=wine_category,y=volatile.acidity))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=volatile.acidity),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for volatile.acidity vs wine_category")

ggplot(data,aes(x=wine_category,y=citric.acid))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=citric.acid),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for citric.acid vs wine_category")

ggplot(data,aes(x=wine_category,y=residual.sugar))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=residual.sugar),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for residual.sugar vs wine_category")

ggplot(data,aes(x=wine_category,y=chlorides))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=chlorides),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for chlorides vs wine_category")

ggplot(data,aes(x=wine_category,y=free.sulfur.dioxide))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=free.sulfur.dioxide),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for free.sulfur.dioxide vs wine_category")

ggplot(data,aes(x=wine_category,y=total.sulfur.dioxide))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=total.sulfur.dioxide),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for total.sulfur.dioxide vs wine_category")

ggplot(data,aes(x=wine_category,y=density))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=density),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for density vs wine_category")

ggplot(data,aes(x=wine_category,y=pH))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=pH),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for pH vs wine_category")

ggplot(data,aes(x=wine_category,y=sulphates))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=sulphates),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for sulphates vs wine_category")

ggplot(data,aes(x=wine_category,y=alcohol))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=alcohol),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for alcohol vs wine_category")

ggplot(data,aes(x=wine_category,y=quality))+
  geom_boxplot(aes(fill=wine_category))+
  geom_jitter(aes(x=wine_category,y=quality),
    position=position_jitter(width=0.1,height=0),
    alpha=0.1,
    size=1)+
  scale_fill_manual(values=c("white","red"))+
  ggtitle("boxplot for quality vs wine_category")

#split for train and test
set.seed(2)
test_index = sample(nrow(data),0.2*nrow(data))
train_data = data[-test_index,]
test_data = data[test_index,]


#logistic regression
logis <- glm(wine_category~.-quality,family = binomial(link = logit),data = train_data)
summary(logis)
logis.pred <- predict(logis,test_data,type="response") #機率
logis.pred <- ifelse(logis.pred>=0.5,1,0)


#confusion matrix, column label="qdac.pred$class", row label="test_data$wine_category"
table(logis.pred,test_data$wine_category,dnn=c("預測","實際")) #confusion matrix

mean(logis.pred==test_data$wine_category)


#LDA
ldac <- lda(wine_category~.-quality,data=train_data)
ldac
ldac.pred <- predict(ldac,test_data,type="response")

#confusion matrix, column label="qdac.pred$class", row label="test_data$wine_category"
table(ldac.pred$class,test_data$wine_category,dnn=c("預測","實際")) #confusion matrix

mean(ldac.pred$class==test_data$wine_category)


#QDA
qdac <- qda(wine_category~.-quality,data=train_data)
qdac
qdac.pred <- predict(qdac,test_data)

#confusion matrix, column label="qdac.pred$class", row label="test_data$wine_category"
table(qdac.pred$class,test_data$wine_category,dnn=c("預測","實際")) 

mean(qdac.pred$class==test_data$wine_category)

#cross validation
#the caret package only accept factor type response for classification 
data$wine_category <- as.factor(data$wine_category)
train_control <- trainControl(method="cv", number=10)
modellogis <- train(wine_category~.-quality, data=data, trControl=train_control, method="glm")
modellda <- train(wine_category~.-quality, data=data, trControl=train_control, method="lda")
modelqda <- train(wine_category~.-quality, data=data, trControl=train_control, method="qda")


