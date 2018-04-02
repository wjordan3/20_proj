# INCOME

fit = lm(Income ~ Borough + poly(TotalPop, 1)+poly(White, 4)+poly(Black,2)+poly(Asian,2)+poly(Citizen,4)+poly(Professional,4)+poly(Service,5)+poly(Construction,5)+poly(Production,5)+poly(Drive,6)+Transit+poly(Walk,3)+poly(OtherTransp,8)+poly(WorkAtHome,4)+poly(MeanCommute,3)+poly(Employed, 3)+poly(PrivateWork, 3)+SelfEmployed+poly(PercentMale,1), data=census)

summary(fit)
hist(residuals(fit))

k=10
set.seed(1)
folds=sample(1:k,nrow(census),replace=TRUE)
cv.errors=matrix(NA,k, dimnames=list(NULL))
for(j in 1:k){
  cur_census = census[folds != j,]
  fit = lm(Income ~ Borough + poly(TotalPop, 1)+poly(White, 4)+poly(Black,2)+poly(Asian,2)+poly(Citizen,4)+poly(Professional,4)+poly(Service,5)+poly(Construction,5)+poly(Production,5)+poly(Drive,6)+Transit+poly(Walk,3)+poly(OtherTransp,8)+poly(WorkAtHome,4)+poly(MeanCommute,3)+poly(Employed, 3)+poly(PrivateWork, 3)+SelfEmployed+poly(PercentMale,1), data=cur_census)
  pred = predict(fit, census[folds == j,])
  cv.errors[j] = mean((census[folds ==j,]$Income-pred)^2)
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors


# INCOME PER CAPITA

fit = lm(IncomePerCap~Borough+ns(TotalPop, df=6)+ns(Hispanic, df=6)+ns(White, df=9)+ns(Black, df=2)+ns(Asian, df=6)+ns(Citizen, df=1)+ns(Professional, df=4)+ns(Service, df=7)+ns(Construction, df=6)+ns(Production, df=7)+ns(Drive, df=13)+ns(OtherTransp, df=5)+ns(MeanCommute,df=3)+ns(Employed,df=11),data=census2)
summary(fit)
plot(fit)

k=10
set.seed(1)
folds=sample(1:k,nrow(census2),replace=TRUE)
cv.errors=matrix(NA,k, dimnames=list(NULL))
for(j in 1:k){
  cur_census = census2[folds != j,]
  fit = lm(IncomePerCap~Borough+ns(TotalPop, df=6)+ns(Hispanic, df=6)+ns(White, df=9)+ns(Black, df=2)+ns(Asian, df=6)+ns(Citizen, df=1)+ns(Professional, df=4)+ns(Service, df=7)+ns(Construction, df=6)+ns(Production, df=7)+ns(Drive, df=13)+ns(OtherTransp, df=5)+ns(MeanCommute,df=3)+ns(Employed,df=11),data=cur_census)
  pred = predict(fit, census2[folds == j,])
  cv.errors[j] = mean((census2[folds ==j,]$IncomePerCap-pred)^2)
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
