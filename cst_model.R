library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(forcats)
library(caret)
library(DMwR)
library(UBL)
library(C50)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(Matrix)
library(Epi)
library(reshape2)


# category ('region','sex','occp','allownc','marri_1','marri_2','DI1_dg',
# 'DI2_dg','DM1_dg','DM2_dg','DE1_dg','DC7_dg','DM8_dg','LQ_1EQL',
# 'educ','BO1','BD1_11','BP1','BS3_1','L_BR_FQ','L_LN_FQ','L_DN_FQ')


# seq ('age','HE_sbp1','HE_dbp1','HE_glu','HE_HbA1c',
# 'HE_insulin','HE_chol','HE_Uacid','N_WATER') 
library(readr)
HN20_ALL7 <- read_csv("HN20_ALL7.csv")

df <- data.frame(HN20_ALL7)
head(df,5)

df[!complete.cases(df),] 
df<-na.omit(df)
str(df)

df$target <- ifelse(df$HE_BMI>=25,'4',
                    ifelse(df$HE_BMI>=23,'3',
                           ifelse(df$HE_BMI>18.5,'2','1')))


head(df,2)
df <- df[,-21]


#as.character
columns <- c('region','sex','occp','allownc','marri_1','marri_2','DI1_dg',
             'DI2_dg','DM1_dg','DM2_dg','DE1_dg','DC7_dg','DM8_dg','LQ_1EQL',
             'educ','BO1','BD1_11','BP1','BS3_1','L_BR_FQ','L_LN_FQ','L_DN_FQ')

for(col in columns){
  df[,col] = as.character(df[,col])
}

columns <- c('age','HE_sbp1','HE_dbp1','HE_glu','HE_HbA1c',
             'HE_insulin','HE_chol','HE_Uacid','N_WATER')


variable_counts <- function(columns,stage){
  if(stage=='pre'){
    print('Pre Conversion to Integer')
  }
  else{
    print('Post Conversion to Integer')
  }
  
  for(col in columns){
    print(paste("Variable:", col, "| Count Unique:",length(unique(df[,col])),"| Min: ", min(df[,col]), "| Max: ",max(df[,col])))
  }
}
variable_counts(columns, 'pre')


for(col in columns){
  df[,col] <- as.integer(round(df[,col]))
}
variable_counts(columns,'post')


for (i in seq(24,31)) {
  a <- boxplot(df[,i])$stats[5,]
  b <- boxplot(df[,i])$stats[4,]
  c <- boxplot(df[,i])$stats[1,]
  d <- boxplot(df[,i])$stats[2,]
  for (j in seq(1,4202)) {
    if(df[j,i]>a){
      df[j,i] = NA
    }else if(df[j,i]<c){
      df[j,i] = NA
    }
  }
}

df <- na.omit(df)
table(df$target) 

str(df)


df$target = as.factor(df$target)
columns <- c('region','sex','occp','allownc','marri_1','marri_2','DI1_dg',
             'DI2_dg','DM1_dg','DM2_dg','DE1_dg','DC7_dg','DM8_dg','LQ_1EQL',
             'educ','BO1','BD1_11','BP1','BS3_1','L_BR_FQ','L_LN_FQ','L_DN_FQ')

for(col in columns){
  df[,col] = as.factor(df[,col])
} 


library(DMwR)
up_df <- SMOTE(target~., df, perc.over = 200, k = 5, perc.under = 500)
table(up_df$target) #441 / 624 / 354 / 492 


up_df2 <- up_df[,-32] 
df_prep <- dummyVars(~.,data=up_df2)
df_new <- data.frame(predict(df_prep,newdata = up_df))


S <- sample(1:nrow(df_new),1337)
x <- df_new
y <- factor(up_df[,32])

x_train <- x[S,]
x_train <- subset(x_train, select=-c(marri_2.88,marri_2.99,DI1_dg.8,DI2_dg.8,DM1_dg.8,DM2_dg.8,DE1_dg.8,
                                     DC7_dg.8,DM8_dg.8,LQ_1EQL.8,BD1_11.8,BS3_1.8))
y_train <- y[S]

x_test <- x[-S,] 
x_test <- subset(x_test, select=-c(marri_2.88,marri_2.99,DI1_dg.8,DI2_dg.8,DM1_dg.8,DM2_dg.8,DE1_dg.8,
                                   DC7_dg.8,DM8_dg.8,LQ_1EQL.8,BD1_11.8,BS3_1.8))
y_test <- y[-S]


S2 <- sample(1:nrow(up_df),1337)
x2 <- up_df
y2 <- factor(up_df[,32])

x_train2 <- x2[S2,]
y_train2 <- y2[S2]

x_test2 <- x2[-S2,] 
y_test2 <- y2[-S2]


str(x_train)
model_minmax = preProcess(x=x_train[,c(20,91,92,93,94,95,96,97,98)],method = "range")
x_train_mm_scaled <-predict(model_minmax,x_train) # minmaxcaled

model_minmax = preProcess(x=x_test[,c(20,91,92,93,94,95,96,97,98)],method = "range")
x_test_mm_scaled = predict(model_minmax,x_test)

model_sd = preProcess(x=x_train[,c(20,91,92,93,94,95,96,97,98)],method = c('center','scale'))
x_train_scaled <- predict(model_sd,x_train)

model_sd = preProcess(x=x_test[,c(20,91,92,93,94,95,96,97,98)],method = c('center','scale'))
x_test_scaled = predict(model_sd,x_test)

x_train2 <-x_train[,c(20,91,92,93,98)]
x_train3 <- data.frame(age=c(), sbp=c(),
                       dbp=c(), glu=c(),
                       water=c())

for(i in 1:5){
  for(j in 1){
    x_train3[j,i] <- (mean(x_train2[,i])-min(x_train2[,i])) / (max(x_train2[,i])-min(x_train2[,i]))
  }
}
colnames(x_train3) <- c('age','sbp','dbp','glu','water')


xx<- cbind(x_train_mm_scaled,y_train)
tree2 <- rpart(y_train ~ .,
               data = xx,
               control = rpart.control(
                 minsplit = 1,  # min number of obs for a split 
                 minbucket = 1, # min number of obs in terminal nodes
                 cp=0.01)
)
rpart.plot(tree2)


x_train3

train_control2 = trainControl(method = "repeatedcv",
                              number = 5,
                              repeats=4,
                              search = "grid",
                              allowParallel=T, 
                              savePredictions="final")

ranger_tune_grid <- expand.grid(mtry=c(22),
                                splitrule=c("extratrees", "gini"), 
                                min.node.size=1)
x_train_scaled$target <- y_train

model2 = train(target~., 
               data = x_train_scaled, 
               method = "ranger", 
               trControl = train_control2,
               tuneGrid = ranger_tune_grid)
model2 #75~77%

pred_y <- predict(model2,x_test_scaled) 
sum(as.factor(pred_y)==y_test)/nrow(x_test_scaled)*100 #81%

