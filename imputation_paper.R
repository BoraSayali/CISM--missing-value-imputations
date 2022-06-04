library(mice)


head(bmd)
health=data.frame(Age=bmd[,2],weight=bmd[,5],height=bmd[,6],bmd=bmd[,9])
head(health)
set.seed(11)
#20 missing values
health[sample(1:nrow(health), 20), "bmd"] = NA


#pattern
library(mice)
md.pattern(health , plot = TRUE, rotate.names =F)

#mean imputation
mean_imputation=mice(health, method = "mean", m = 1,maxit = 1,seed=100)
x=complete(mean_imputation)
par(mfrow=c(1,2))
hist(x$bmd,main="Mean Imputation",col=2, xlab="values")
hist(bmd$bmd, main="Original data",col=2,xlab="values")

#multiple imputation
mul_imp = mice(health, print = F,m=5)
fit = with(mul_imp,lm(bmd~Age+weight+height))
Summary=summary(pool(fit))
Summary


#m=11
mul_imp1 = mice(health , print =F,m=11)
fit_17 = with(mul_imp1,lm(bmd~Age+weight+height))
Summary1=summary(pool(fit_17))
Summary1

#complete case analysis
fit_comp =lm(bmd~Age+weight+height, data =health,na.action = na.omit)
summary(fit_comp)


#Original
fit1 =lm(bmd~age+weight_kg+height_cm, data =bmd)
summary(fit1)


