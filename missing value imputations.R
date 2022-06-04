library(mice)
library(missForest)

data = iris
iris1 = subset(iris, select = -c(Species))
#mean imputation, multiple,reression, missforest
nrmse_mean={}
nrmse_mulim={}
nrmse_reg={}
nrmse_forest={}

for(i in 1:500)
{
  set.seed(i+6)
  iris2 = prodNA(iris1, noNA = 0.2)
  imputed_Data=complete(mice(iris2,method = "mean",m=1,maxit = 1,printFlag = F))
  nrmse_mean[i]=nrmse(imputed_Data,iris2,iris1)
  imputed_Dmul=complete(mice(iris2,method = "pmm",m=5,maxit = 1,printFlag = F))
  nrmse_mulim[i]=nrmse(imputed_Dmul,iris2,iris1)
  imputed_reg=complete(mice(iris2,method = "norm.predict",m=1,maxit = 1,printFlag = F))
  nrmse_reg[i]=nrmse(imputed_reg,iris2,iris1)
  imp_forest=missForest(iris2)
  nrmse_forest[i]=imp_forest$OOBerror
  
}
#nrmse_mean
avg_meanimp=mean(nrmse_mean)
avg_meanimp
#nrmse_mul
avg_mulimp=mean(nrmse_mulim)
avg_mulimp
#nrmse_reg
avg_reg=mean(nrmse_reg)
avg_reg
#nrmse_for
avg_forest=mean(nrmse_forest)
avg_forest


#KNN
library(bnstruct)
library(VIM)
nrmse_knn={}

iris2 <- prodNA(iris1, noNA = 0.2)
nrmse_knn={}
for (i in 1:500)
  {
  set.seed(i+6)
  iris2 <- prodNA(iris1, noNA = 0.2)
  imp_knn=kNN(iris2,k=5)
  L1=which(is.na(iris2$Sepal.Length))
  L2=which(is.na(iris2$Sepal.Width))
  L3=which(is.na(iris2$Petal.Length)) 
  L4=which(is.na(iris2$Petal.Width)) 
  nrmse1=sqrt(mean((imp_knn[L1,1]-iris1[L1,1])^2))/(max(iris1[,1])-min(iris1[,1]))
  nrmse2=sqrt(mean((imp_knn[L2,2]-iris1[L2,2])^2))/(max(iris1[,2])-min(iris1[,2]))
  nrmse3=sqrt(mean((imp_knn[L3,3]-iris1[L3,3])^2))/(max(iris1[,3])-min(iris1[,3]))
  nrmse4=sqrt(mean((imp_knn[L4,4]-iris1[L4,4])^2))/(max(iris1[,4])-min(iris1[,4]))
  nrmse_knn[i]=(nrmse1+nrmse2+nrmse3+nrmse4)/4
}
nrmse_knn
avg_nrmse=mean(nrmse_knn)

##part B
for(i in 1:500)
{
  set.seed(i+6)
  iris3 = prodNA(iris1, noNA = 0.1)
  imputed_Data=complete(mice(iris3,method = "mean",m=1,maxit = 1,printFlag = F))
  nrmse_mean[i]=nrmse(imputed_Data,iris3,iris1)
  imputed_Dmul=complete(mice(iris3,method = "pmm",m=5,maxit = 1,printFlag = F))
  nrmse_mulim[i]=nrmse(imputed_Dmul,iris3,iris1)
  imputed_reg=complete(mice(iris3,method = "norm.predict",m=1,maxit = 1,printFlag = F))
  nrmse_reg[i]=nrmse(imputed_reg,iris3,iris1)
  imp_forest=missForest(iris3)
  nrmse_forest[i]=imp_forest$OOBerror
  
}
#nrmse_mean
avg_meanimp=mean(nrmse_mean)
avg_meanimp
#nrmse_mul
avg_mulimp=mean(nrmse_mulim)
avg_mulimp
#nrmse_reg
avg_reg=mean(nrmse_reg)
avg_reg
#nrmse_for
avg_forest=mean(nrmse_forest)
avg_forest


#KNN

library("VIM")
nrmse_knn={}

install.packages("DMwR")
library(DMwR)

nrmse_knn={}
for (i in 1:500)
{
  set.seed(i+6)
  iris3 = prodNA(iris1, noNA = 0.1)
  imp_knn=kNN(iris3,k=5)
  L1=which(is.na(iris3$Sepal.Length))
  L2=which(is.na(iris3$Sepal.Width))
  L3=which(is.na(iris3$Petal.Length)) 
  L4=which(is.na(iris3$Petal.Width)) 
  nrmse1=sqrt(mean((imp_knn[L1,1]-iris1[L1,1])^2))/(max(iris1[,1])-min(iris1[,1]))
  nrmse2=sqrt(mean((imp_knn[L2,2]-iris1[L2,2])^2))/(max(iris1[,2])-min(iris1[,2]))
  nrmse3=sqrt(mean((imp_knn[L3,3]-iris1[L3,3])^2))/(max(iris1[,3])-min(iris1[,3]))
  nrmse4=sqrt(mean((imp_knn[L4,4]-iris1[L4,4])^2))/(max(iris1[,4])-min(iris1[,4]))
  nrmse_knn[i]=(nrmse1+nrmse2+nrmse3+nrmse4)/4
}
nrmse_knn
avg_nrmse=mean(nrmse_knn)



####Q2###
library(MASS)
library(mclust)
data2= read.csv(file.choose())
data3 = subset(data2, select = -c(Gender))

fit = Mclust(data3,2)
summary(fit,parameter=2)
#comparison
  fit1=Mclust(data2,2)
summary(fit1,parameter=2)






data(iris)
iris
y=iris$Petal.Length

#initial values
n=150

alpha1=0.25
alpha2=0.50
alpha3=1-alpha1-alpha2

mu1=4
mu2=5
mu3=1

v1=2.8
v2=3.4
v3=3.2

##this is for one iteration ###
temp_s1=alpha1*dnorm(y,mu1,v1)
temp_s2=alpha2*dnorm(y,mu2,v2)
temp_s3=alpha3*dnorm(y,mu3,v3)

sum_s=temp_s1 + temp_s2 + temp_s3

x_s1= temp_s1/sum_s
x_s2= temp_s2/sum_s                     
x_s3= temp_s3/sum_s                         

alpha1=sum(x_s1)/n
alpha2=sum(x_s2)/n
alpha3=1-alpha1-alpha2

mu_new1=sum(x_s1*y)/sum(x_s1)
mu_new2=sum(x_s2*y)/sum(x_s2)
mu_new3=sum(x_s3*y)/sum(x_s3)

var_new1=sum(x_s1*((y-mu_new1)^2))/sum(x_s1)
var_new2=sum(x_s2*((y-mu_new2)^2))/sum(x_s2)
var_new3=sum(x_s3*((y-mu_new3)^2))/sum(x_s3)

##for many iterations
e=0.001
dif=e+1
r=0#to know in how many iterations we get the info
while(dif>e){
  
  temp_s1=alpha1*dnorm(y,mu1,v1)
  temp_s2=alpha2*dnorm(y,mu2,v2)
  temp_s3=alpha3*dnorm(y,mu3,v3)
  
  sum_s=temp_s1 + temp_s2 + temp_s3
  
  x_s1= temp_s1/sum_s
  x_s2= temp_s2/sum_s                     
  x_s3= temp_s3/sum_s 
  
  a_s_new1=sum(x_s1)/n
  a_s_new2=sum(x_s2)/n
  a_s_new3=1-a_s_new1-a_s_new2
  
  
  mu_new1=sum(x_s1*y)/sum(x_s1)
  mu_new2=sum(x_s2*y)/sum(x_s2)
  mu_new3=sum(x_s3*y)/sum(x_s3)
  
  var_new1=sum(x_s1*((y-mu_new1)^2))/sum(x_s1)
  var_new2=sum(x_s2*((y-mu_new2)^2))/sum(x_s2)
  var_new3=sum(x_s3*((y-mu_new3)^2))/sum(x_s3)
  
  dif=max(abs(alpha1-a_s_new1),abs(mu1-mu_new1),abs(v1-var_new1),
          abs(alpha2-a_s_new2),abs(mu2-mu_new2),abs(v2-var_new2),
          abs(alpha3-a_s_new3),abs(mu3-mu_new3),abs(v3-var_new3))
  
  mu1=mu_new1
  mu2=mu_new2
  mu3=mu_new3
  
  alpha1=a_s_new1
  alpha2=a_s_new2
  alpha3=1-alpha1-alpha2
  
  v1=var_new1
  v2=var_new2
  v3=var_new3
  
  r=r+1
}

L_mu=c(mu1,mu2,mu3)
L_var=c(v1,v2,v3)
L_alph=c(alpha1,alpha2,alpha3)
L_mu
L_alph
L_var

library(mclust)
library(MASS)
d1=subset(iris, select=c(Petal.Length,Species))
fit = Mclust(d1,3)
summary(fit,parameter=T)

