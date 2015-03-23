featureEngineer <- function(df){
        
    names <- c("season", "holiday", "workingday", "weather")
    df[,names] <- lapply(df[,names],ordered)
    
    nrow = length(df$weather)
    
    df$datetime <- as.character(df$datetime)
    df$datetime <- strptime(df$datetime, format = "%Y-%m-%d %T", tz="EST")
    
    df$hour <- as.integer(substr(df$datetime, 12, 13))
    df$hour <- as.factor(df$hour)
    
    df$weekday <- as.factor(weekdays(df$datetime))
    df$weekday <- factor(df$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                "Friday", "Saturday", "Sunday"))
    
    df$month <- as.integer(substr(df$datetime, 6,7))
    df$year <- as.integer(substr(df$datetime,1,4))
    
    
    hrs <- as.numeric(df$hour)
    #create daypart column, default to 4 to make things easier for ourselves
    df$hour2block <- round(hrs/2)
    df$hour3block <- round(hrs/3)+1
    df$hour5block <- round(hrs+2/5)    
    df$hour7block <- round(hrs+4/7)+1
    #     df$hour4block <- round(hrs/4)
    #     df$hour6block <- round(hrs/3)+1
    #     df$hour8block <- round(hrs+2/5)    
    
    
    num_weather <- as.numeric(df$weather) #convert from unordered factor into numeric
    num_hour <- as.numeric(df$hour) #convert from unordered factor into numeric
    
    
    df$coldnwetness <- pmax(num_weather - 2, 0) * pmax(20 - df$temp, 1)
    df$windynwetness <- pmax(num_weather - 2, 0) * df$windspeed
    df$coldnwetness2 <- num_weather^2 * pmax(20 - df$temp, 1)
    df$windynwetness2 <- num_weather^2 * df$windspeed
    
    df$coldwindyness <-  pmax(20 - df$temp, 1) * df$windspeed
    df$coldwindywetness <- (pmax(20 - df$temp, 1) * df$windspeed)^((num_weather+1)/2)
    df$darkwetness <- abs(14 - num_hour)^(pmax(num_weather-1,1))
    df$darkwindyness <- max(abs(14 - num_hour)-4, 1) * df$windspeed
    df$quarter <- ceiling(df$month/4)
    df$sixth <- ceiling(df$month/6)

    
    return(df)
}


# START
library(randomForest)

# if(getwd() != "/Users/lionelward/Documents/MSc_Term2/Kaggle4"){
#     setwd("Documents/MSc_Term2/Kaggle4")
# }

train <- read.csv("train.csv")
test <- read.csv("test.csv")
# pairs(train)

trainNew <- featureEngineer(train)
testNew <- featureEngineer(test)

is.factor(trainNew$hour)
is.factor(trainNew$weekday)


myNTree = 2500
myMTry = 5
myImportance = TRUE
set.seed(415)


# #the following code does importance analysis 
# testTrain <- subset(trainNew, select = -c(datetime, count, registered))
# testFit <- randomForest(casual ~ ., data = testTrain, ntree = myNTree, mtry = myMTry, importance = myImportance)
# 
# testTrain <- subset(trainNew, select = -c(datetime, count, casual))
# testFit <- randomForest(registered ~ ., data = testTrain, nTree = myNTree, mtry = myMTry, importance = myImportance)

for (munf in 1:24) {
    
    print(munf)
    
    if (munf<=12){  thisTrain <- trainNew[trainNew$month <= munf & trainNew$year == 2011, ]
    }else{ thisTrain <- trainNew[trainNew$month <= (munf-12) | trainNew$year == 2011, ]
    }
    
    casualFit <- randomForest(casual ~ hour + year + humidity +temp + atemp + workingday + hour2block + hour3block
                              + hour5block +  hour7block + weekday + month + coldnwetness +  windynwetness
                              + darkwetness + darkwindyness + quarter + sixth, data = thisTrain, ntree = myNTree,
                              mtry = myMTry, importance = myImportance)
    registeredFit <- randomForest(registered ~ hour + year + month + weather + hour2block + hour3block + hour5block + 
                                      hour7block + workingday + humidity + weekday + atemp+ coldnwetness +  windynwetness
                                  + darkwetness + darkwindyness + quarter + sixth, data = thisTrain, 
                                  ntree = myNTree, mtry = myMTry, importance = myImportance)
    
    #rs: ROW SELECTOR. the boolean vector for selecting the rows from the test set
    rs <- testNew$month == (((munf-1) %% 12)+1) & testNew$year == 2011 + floor((munf-1)/12)
    
    test$casual[rs] <- predict(casualFit, testNew[rs, ])
    test$registered[rs] <- predict(registeredFit, testNew[rs, ])
    
    test$count[rs] <- round(test$casual[rs] + test$registered[rs], 0)
}

plot(test$count)
submit <- data.frame(datetime = test$datetime, count = test$count)
write.csv(submit, file = paste(format(Sys.time(), "%Y-%m-%d %I-%p"), "csv", sep = "."), row.names = FALSE)
