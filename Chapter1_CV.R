bodyfat = read.csv("bodyfat.csv")

dim(bodyfat)
n = dim(bodyfat)[1]

attach(bodyfat)
names(bodyfat) 
#open data
bodyfat = read.csv("bodyfat.csv")
dim(bodyfat)
n = dim(bodyfat)[1]
names(bodyfat) 
#fit response on all predictors
Fullmodel = (BodyFatSiri ~ Age + Weight + Height + BMI + Neck + Chest + Abs + Hip
             + Thigh + Knee + Ankle + Biceps + Forearm + Wrist)
Fullfit = lm(Fullmodel,data=bodyfat)
summary(Fullfit)  
#note the reasonably high R-squared value, 0.7365 after adjustment for multiple predictors

k = 10 #using 10-fold cross-validation
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))  #produces list of group labels

set.seed(4)
cv_sample= sample(groups,n)  #orders randomly, with seed (2) to determine starting point

#prediction via cross-validation
allpredictedCV = rep(NA,n)#vector of NA
groupi = which(cv_sample==5)
for (items  in 1:k)  {# we iterate through differnet
  groupi = which(cv_sample== items)
  model_group = lm(formula = Fullmodel,data=bodyfat[groupi,])
  allpredictedCV[groupi] = predict.lm(model_group,bodyfat[groupi,])
}

plot(Fullfit$fitted.values,BodyFatSiri)
points(allpredictedCV,BodyFatSiri,pch=20,col="red"