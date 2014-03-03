require("nnet")
require("plyr")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
mean_bygroup <- function(dataset) { 
  return(data.frame(location = dataset$location[which.max(dataset$shopping_pt)], state =dataset$state[which.max(dataset$shopping_pt)], group_size =dataset$group_size[which.max(dataset$shopping_pt)],
                    homeowner = dataset$homeowner[which.max(dataset$shopping_pt)], car_age = dataset$car_age[which.max(dataset$shopping_pt)],
                    car_value = dataset$car_value[which.max(dataset$shopping_pt)], risk_factor=dataset$risk_factor[which.max(dataset$shopping_pt)],
                    age_oldest = dataset$age_oldest[which.max(dataset$shopping_pt)], age_youngest=dataset$age_youngest[which.max(dataset$shopping_pt)],
                    married_couple = dataset$married_couple[which.max(dataset$shopping_pt)], C_previous = dataset$C_previous[which.max(dataset$shopping_pt)],
                    duration_previous = dataset$duration_previous[which.max(dataset$shopping_pt)],
                    Alast = dataset$A[which.max(dataset$shopping_pt)],Blast = dataset$B[which.max(dataset$shopping_pt)], Clast = dataset$C[which.max(dataset$shopping_pt)], 
                    Dlast = dataset$D[which.max(dataset$shopping_pt)],Elast = dataset$E[which.max(dataset$shopping_pt)], Flast = dataset$F[which.max(dataset$shopping_pt)], 
                    Glast =dataset$G[which.max(dataset$shopping_pt)],numQuote = length(dataset$shopping_pt),
                    costFirst = dataset$cost[which.min(dataset$shopping_pt)], costLast = dataset$cost[which.max(dataset$shopping_pt)] )) 
}
newTrain <- ddply(train,.(customer_ID),mean_bygroup)
train2 <- subset(train, record_type==1)
newTrain$A <- with(train2, A)
newTrain$B <- with(train2, B)
newTrain$C <- with(train2, C)
newTrain$D <- with(train2, D)
newTrain$E <- with(train2, E)
newTrain$F <- with(train2, F)
newTrain$G <- with(train2, G)

newTest <- ddply(test,.(customer_ID),mean_bygroup)
newTrain$numQuote <- newTrain$numQuote - 1

index<<-1
getAToG <- function(dataset) {
  result <- data.frame(shopping_pt = dataset$shopping_pt[index], day = dataset$day[index], time = dataset$time[index], A = dataset$A[index], B = dataset$B[index], C = dataset$C[index], D = dataset$D[index], E = dataset$E[index],
                       F = dataset$F[index], G = dataset$G[index])
  names(result) <- paste0(names(result), index)
  return(result) 
}
for(i in 1:12){
  index <<-i
  temp <- ddply(train,.(customer_ID),getAToG)
  newTrain <- join(newTrain, temp, by="customer_ID")
  print(i) 
}

write.csv(newTrain,"newTrainComprehensive.csv",quote=T,row.names=F,na="")


index<<-1
for(i in 1:10){
  index <<-i
  temp <- ddply(test,.(customer_ID),getAToG)
  newTest <- join(newTest, temp, by="customer_ID")  
}

write.csv(newTest,"newTestComprehensive.csv",quote=T,row.names=F,na="")
