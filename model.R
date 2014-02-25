require("nnet")
require("plyr")
train <- read.csv("newTrainWithLastQuoteAndCost.csv")
test <- read.csv("newTestWithLastQuoteAndCost.csv")
attach(train)
train$A <- as.factor(A)
train$B <- as.factor(B)
train$C <- as.factor(C)
train$D <- as.factor(D)
train$E <- as.factor(E)
train$F <- as.factor(F)
train$G <- as.factor(G)
detach(train)

train$ABCDEFG<-paste0(train$A,train$B,train$C,train$D,train$E,train$F,train$G)
train.omit <- na.omit(train)

fitModel.2 <- function (mydatatrain, mydatatest,formulastring,outcome){
  mystring <- paste0(outcome, formulastring)
  myformula <- as.formula(mystring)
  A.fit <- glm(myformula,data = mydatatrain,family=binomial)
  A.probs <- predict(A.fit, type = "response",newdata = mydatatest)
  A.pred <- ifelse(A.probs>0.5, 1,0)
  A.pred <- as.numeric(A.pred)
  list(model = A.fit, pred = A.pred, correctness = length(which(A.pred==mydatatest[[outcome]]))/length(A.pred))
}


fitModel <- function (mydatatrain, mydatatest, formulastring, outcome){
  mystring <- paste0(outcome, formulastring)
  myformula <- as.formula(mystring)
  A.fit <- multinom(myformula,data = mydatatrain, maxit = 200)
  A.probs <- predict(A.fit, type = "probs",newdata = mydatatest)
  max.col.name= colnames(A.probs)[apply(A.probs,1,which.max)]
  A.pred <- as.numeric(max.col.name)
  list(model = A.fit, pred = A.pred, correctness = length(which(A.pred==mydatatest[[outcome]]))/length(A.pred))
}

formulastring<-" ~ state +  homeowner + car_age + car_value + I(age_oldest/2 + age_youngest/2) + married_couple +  Alast * numQuote "
A.model <- fitModel(train.omit,train.omit,formulastring,"A")
A.model$correctness
A <- A.model$pred

formulastring<-" ~ state +  homeowner + car_age + car_value + I(age_oldest/2 + age_youngest/2) + married_couple +  Blast + numQuote "
B.model <- fitModel.2(train.omit,train.omit,formulastring,"B")
B.model$correctness
B <- B.model$pred

formulastring<-" ~ state +  homeowner + car_age + car_value + I(age_oldest/2 + age_youngest/2) + married_couple +  Clast * numQuote "
C.model <- fitModel(train.omit,train.omit,formulastring,"C")
C.model$correctness
C <- C.model$pred

formulastring<-" ~ state +  homeowner + car_age + car_value + I(age_oldest/2 + age_youngest/2) + married_couple +  Dlast * numQuote"
D.model <- fitModel(train.omit,train.omit,formulastring,"D")
D.model$correctness
D <- D.model$pred


formulastring<-" ~ state +  homeowner + car_age + car_value + I(age_oldest/2 + age_youngest/2) + married_couple +  Elast * numQuote "
E.model <- fitModel.2(train.omit,train.omit,formulastring,"E")
E.model$correctness
E<- E.model$pred

formulastring<-" ~ state +  homeowner + car_age + car_value + I(age_oldest/2 + age_youngest/2) + married_couple +  Flast * numQuote "
F.model <- fitModel(train.omit,train.omit,formulastring,"F")
F.model$correctness
F <- F.model$pred

formulastring<-" ~ state +  homeowner + car_age + car_value + I(age_oldest/2 + age_youngest/2) + married_couple +  Glast * numQuote "
G.model <- fitModel(train.omit,train.omit,formulastring,"G")
G.model$correctness
G <- G.model$pred

pred <- paste0(A,B,C,D,E,F,G)
length(which(pred==train.omit$ABCDEFG))/dim(train.omit)[1]

test.A <- test
test.A$car_value[which(!test.A$car_value %in% train$car_value)]<-'a'
A.probs <- predict(A.model$model,type = "probs",newdata = test.A)
max.col.name= colnames(A.probs)[apply(A.probs,1,which.max)]
A.pred <- as.numeric(max.col.name)

B.probs <- predict(B.model$model, type = "response",newdata = test.A)
B.pred <- ifelse(B.probs>0.5, 1,0)
B.pred <- as.numeric(B.pred)

C.probs <- predict(C.model$model,type = "probs",newdata = test.A)
max.col.name= colnames(C.probs)[apply(C.probs,1,which.max)]
C.pred <- as.numeric(max.col.name)

D.probs <- predict(D.model$model,type = "probs",newdata = test.A)
max.col.name= colnames(D.probs)[apply(D.probs,1,which.max)]
D.pred <- as.numeric(max.col.name)

E.probs <- predict(E.model$model, type = "response",newdata = test.A)
E.pred <- ifelse(E.probs>0.5, 1,0)
E.pred <- as.numeric(E.pred)

F.probs <- predict(F.model$model,type = "probs",newdata = test.A)
max.col.name= colnames(F.probs)[apply(F.probs,1,which.max)]
F.pred <- as.numeric(max.col.name)

G.probs <- predict(G.model$model,type = "probs",newdata = test.A)
max.col.name= colnames(G.probs)[apply(G.probs,1,which.max)]
G.pred <- as.numeric(max.col.name)

pred <- paste0(A.pred, B.pred, C.pred, D.pred, E.pred, F.pred, G.pred)
test.A$ABCDEFG<-with(test.A,paste0(Alast, Blast, Clast, Dlast, Elast, Flast, Glast))
length(which(pred==test.A$ABCDEFG))/dim(test.A)[1]

submit<-data.frame(customer_ID = test.A$customer_ID, plan = pred)
write.csv(submit,"submit.csv",quote=FALSE, row.names=FALSE)
