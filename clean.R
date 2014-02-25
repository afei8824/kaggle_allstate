require("nnet")
require("plyr")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
mean_bygroup <- function(dataset) { 
  return(data.frame(state =dataset$state[which.max(dataset$shopping_pt)], group_size =dataset$group_size[which.max(dataset$shopping_pt)],
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
newtrain <- ddply(train,.(customer_ID),mean_bygroup)
newtest <- ddply(test,.(customer_ID),mean_bygroup)
newtrain$numQuote <- newtrain$numQuote - 1