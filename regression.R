df <- read.csv("bikeshare.csv")

#randomly split data 70/30

split <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))
df_train <- df[split,]
df_test <- df[!split,]

model <- lm(count ~ factor(season) + factor(holiday) + factor(workingday) + factor(weather) + temp + atemp + humidity + windspeed, data=df_train)
summary(model)

df_train$pred <- predict(model, df_train)
df_test$pred <- predict(model, df_test)

1 - sum((df_train$count - df_train$pred)^2)/sum((df_train$count - mean(df_train$count))^2)
1 - sum((df_test$count - df_test$pred)^2)/sum((df_test$count - mean(df_test$count))^2)

#R² is almost identical



#split data chronologically, predict future

split <- vector(length = nrow(df))
split[1:round(10886*0.7)] <- T
df_train <- df[split,]
df_test <- df[!split,]

model <- lm(count ~ factor(season) + factor(holiday) + factor(workingday) + factor(weather) + temp + atemp + humidity + windspeed, data=df_train)
summary(model)

df_train$pred <- predict(model, df_train)
df_test$pred <- predict(model, df_test)

1 - sum((df_train$count - df_train$pred)^2)/sum((df_train$count - mean(df_train$count))^2)
1 - sum((df_test$count - df_test$pred)^2)/sum((df_test$count - mean(df_test$count))^2)

#R² is much worse when trying to predict the future