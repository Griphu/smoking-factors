library(caret)
library(randomForestSRC)
install.packages("plotly")
library(plotly)
drzewo = train(smokes~ .,
      data = train_df,
      preProcess= c("center", "scale"),
      method="rpart",
      control = rpart::rpart.control(
       maxdepth = 30,
       cp=0.0005),
      na.action = na.omit
)





pred_train <- prediction(predict(drzewo,type="prob")[,2],train_df$pali)

auc_train <- performance(pred_train, measure = "auc")
auc_train <- auc_train@y.values[[1]]


pred_test <- prediction(predict(drzewo,newdata = test_df,type="prob")[,2],test_df$pali)

auc_test <- performance(pred_test, measure = "auc")
auc_test <- auc_test@y.values[[1]]

#####
random_forest=rfsrc(smokes~.,
                    data = as.data.frame(train_df),
                    importance = T,
                    ntree = 500,
                    nodedepth = 5,
                    na.action = "na.omit")



pred_train <- prediction(predict(random_forest,type="prob")$predicted[,2],train_df$smokes)

auc_train <- performance(pred_train, measure = "auc")
auc_train <- auc_train@y.values[[1]] #0.722


pred_test <- prediction(
   predict(
      random_forest,
      newdata = as.data.frame(test_df),
      type="prob")$predicted[,2],
   test_df$smokes)

auc_test <- performance(pred_test, measure = "auc")
auc_test <- auc_test@y.values[[1]] #0.741

rf_importance = vimp(random_forest)

plot_ly(y = sort(as.data.frame(random_forest$importance[,1])[,1],decreasing = T),
        x= rownames(as.data.frame(random_forest$importance[,1]))[order(as.data.frame(random_forest$importance[,1])[,1],decreasing = T)],
        type = 'bar')

head(as.data.frame(random_forest$importance)[order(as.data.frame(random_forest$importance[,1])[,1],decreasing = T),])



install.packages("xgboost")
library(xgboost)
#Training with xgboost - gives better scores than 'rf'
trctrl <- trainControl(method = "cv", number = 10)

tune_grid <- expand.grid(nrounds = 500,
                         max_depth = 4,
                         eta = 0.005,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0.01,
                         subsample = 0.8)

xgb_fit <- train(smokes ~.,
                data = train_df,
                method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)



pred_train <- prediction(predict(xgb_fit,type="prob")[,2],train_df$smokes)

auc_train <- performance(pred_train, measure = "auc")
auc_train <- auc_train@y.values[[1]] # 0.717


pred_test <- prediction(predict(xgb_fit,newdata = test_df,type="prob")[,2],test_df$smokes)

auc_test <- performance(pred_test, measure = "auc")
auc_test <- auc_test@y.values[[1]] # 0.743

xgb_fit$results

ggplot(varImp(xgb_fit))


