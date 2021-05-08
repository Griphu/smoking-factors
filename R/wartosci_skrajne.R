#install.packages("isotree")
library(isotree)
#install.packages("dbscan")
#library(dbscan)
iforest = isolation.forest(df,
                           ntrees = 100,
                           sample_size = 256,
                           random_seed = 1)
iforest_score = predict(iforest,df)
plot(iforest_score)

#lof_scores = lof(df, k=50) # tutaj trzeba by by³o zamieniæ wszystko na one hot encoding

df_no_outliers = df[-which(iforest_score>0.55),]
