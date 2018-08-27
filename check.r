require(caret)
require(dplyr)
library(readr)

################################### read in data ###########################################
setwd("~/county health rank 2018")
#setwd("C:/Users/Owner/Desktop/county")
dat <- read_csv("cleaned.csv")
dat$X1=NULL

out_name=names(dat)[1:12]
feature_name=names(dat)[13:71]

feature_dat=dat[13:71]
out_dat=dat[1:12]

feature_dat1=data.frame(lapply(feature_dat, function(x) as.numeric(as.numeric(x))))
out_dat1=data.frame(lapply(out_dat, function(x) as.numeric(as.numeric(x))))
################################ checking assumption ##################################################

#check variables with near zero variance for feature data
for (i in 1:dim(feature_dat1)[2]){
  print(nearZeroVar((feature_dat1[i]), saveMetrics= TRUE) ) 
}
#no near zero variance for features

#find vars that have high correlations
#checked in another file. #can't take complete cases, because it only has 980 cases left. 

#normality plot
for (i in 1: dim(dat)[2]){
  qqnorm(unlist(dat[i]));qqline(unlist(dat[i]), col = 2)
}
fitdistr(na.omit(dat$premature_age_adjusted_mortality), "t")$estimate

#normality check
#https://machinelearningmastery.com/a-gentle-introduction-to-normality-tests-in-python/

norm_table=function(dat){
  estimate=rep(NA, dim(dat)[2])
  p=rep(NA, dim(dat)[2])
  for (i in 1:dim(dat)[2]){
    estimate[i]=shapiro.test(as.numeric(unlist(dat[i])))$statistic
    p[i]=shapiro.test(as.numeric(unlist(dat[i])))$p.value
  }
  norm_feature=data.frame(cbind(names(dat),estimate, p))
  return(norm_feature)
}

norm_table(feature_dat1)

norm_table(out_dat1)
#very skewed, very not normal 

#centering and scaling features (no imputing for now)

feature_pre<- preProcess(feature_dat1, method =c("center", "scale"))
feature_dat2<- predict(feature_pre, feature_dat1)

out_pre<- preProcess(out_dat1, method =c("center", "scale"))
out_dat2<- predict(out_pre, out_dat1)

#check normality about centering and scaling
norm_table(feature_dat2)

norm_table(out_dat2)

#normality test show they are not normal, but the graphs show they are quite normal 

#imputation for feature data
#imputing complication: http://rismyhammer.com/ml/ImputeMissingData1.html
#imputing features wiht mice
#decide just impute with mean.
for(i in 1:ncol(feature_dat2)){
  feature_dat2[is.na(feature_dat2[,i]), i] <- mean(feature_dat2[,i], na.rm = TRUE)
}

#final features: 
head(feature_dat2)
#final outcomes:
head(out_dat2)


# define training control (repeated k fold CV)
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Species~., data=, trControl=train_control, method="nb")
# summarize results
print(model)


