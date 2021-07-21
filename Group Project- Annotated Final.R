###### Group Project #######
# Dorothy Walls, 10175187 ##
#Greg Brown, 30080752     ##
#Abraham Hussein, 30016352 #
############################

## Libraries used
library(car)
library(knitr)
## Adding our data set
cancer<-read.csv("breast-cancer-wisconsin.csv", header = T)
## Attaching our data
attach(cancer)

## Briefly exploring the set
cancer[1:5,]
list(cancer)
names(cancer)
n<-nrow(cancer)
## Identifying how many rows we have

n

## Changing 'class' to 'malignant' to avoid errors
malignant<-class 

dim(cancer)

## Ensuring there are no empty values
sum(is.na(cancer))

## Set number of charts per page
par(mfrow=c(2,2))

## The following builds a boxplot for each variable
boxplot(radius~malignant, ylab="Tumor Radius",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(perimeter~malignant, ylab="Tumor Perimeter",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(texture~malignant, ylab="Texture ",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(area~malignant, ylab="Tumor Area",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(smoothness~malignant, ylab="Tumor Smoothness ",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(compactness~malignant, ylab="Tumor Compactness ",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(concavity~malignant, ylab="Tumor Concavity ",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(concave.points~malignant, ylab="# Concave Points on the Tumor",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(symmetry~malignant, ylab="Tumor Symmetry",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")

## Our first model, the general case and diagnostic tool
m1<-glm(malignant~radius+texture+perimeter+area+smoothness+
    compactness+concavity+concave.points+symmetry,family=binomial)
summary(m1)

## Change the number of charts per page
par(mfrow=c(2,2))
## Scatterplot for our first model
plot(m1)

## Marginal model plot for first model
mmps(m1)
par(mfrow=c(1,1))
mmp(m1, fitted.values(m1))

## Identify any transformations to help create a better fit model
summary(powerTransform(cbind(radius,texture,perimeter,area,smoothness,compactness,concavity,concave.points,symmetry)))

## Change the number of charts per page
par(mfrow=c(1,2))

## The following builds a boxplot for each variable, transformed and normal
boxplot(radius~malignant, ylab="Tumor Radius",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot(sqrt(radius)~malignant, ylab="sqrt(Radius)",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")

boxplot(area~malignant, ylab="Tumor Area",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot((area)^(-1)~malignant, ylab="1/Area",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")

boxplot(concave.points~malignant, ylab="# Convave Points",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot((concave.points)^(-1)~malignant, ylab="1/concave points",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")

boxplot(symmetry~malignant, ylab="Tumor Symmetry",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")
boxplot((symmetry)^(-4)~malignant, ylab="1/symmetry^4",
        xlab="Is Tumor Malignant? (0=No, 1=Yes)")


## The following are the suggested transformations
nradius<-sqrt(radius)
narea<-(area)^(-1)
nconcave.points<-(concave.points)^(-1)
nsymmetry<-(symmetry)^(-4)
par(mfrow=c(1,1))

## This is our second model, transformed Radius
m2<-glm(malignant~radius+texture+perimeter+area+smoothness+
  compactness+concavity+concave.points+symmetry+nradius,family=binomial)

## Summary and marginal model plots

summary(m2)

mmps(m2)
mmp(m2, fitted.values(m2))

## This is our third model, transformed Symmetry
m3<-glm(malignant~radius+texture+perimeter+area+smoothness+compactness+
    concavity+concave.points+symmetry+nsymmetry,family=binomial)

## Summary and marginal model plots

summary(m3)

mmps(m3)
mmp(m3, fitted.values(m3))

## This is our fourth model, transformed Area
m4<-glm(malignant~radius+texture+perimeter+area+narea+smoothness+compactness+
    concavity+concave.points+symmetry,family=binomial)

## Summary and marginal model plots

summary(m4)

mmps(m4)
mmp(m4, fitted.values(m4))

## This is our 5th model, transformed Concave Points
m5<-glm(malignant~radius+texture+perimeter+area+smoothness+compactness+
    concavity+concave.points+nconcave.points+symmetry,family=binomial)

## Summary and marginal model plots

summary(m5)

mmps(m5)
mmp(m5, fitted.values(m5))

## This is our kth model, all transformed predictors
mk<-glm(malignant~radius+texture+perimeter+area+smoothness+compactness+
    concavity+concave.points+symmetry+nsymmetry+narea+nconcave.points+nsymmetry,family=binomial)

## Summary and marginal model plots

summary(mk)

mmps(mk)
mmp(mk, fitted.values(mk))

## Tests comparing each model
anova(m1,m2,test="Chisq")
anova(m1,m3,test="Chisq")
anova(m1,m4,test="Chisq")
anova(m1,m5,test="Chisq")
anova(m1,mk,test="Chisq")

## Check for multicolinearity
vif(m1)

## Determine range for each variable
range(radius)
range(nradius)
range(texture)
range(perimeter)
range(area)
range(narea)
range(smoothness)
range(compactness)
range(concavity)
range(concave.points)
range(nconcave.points)
range(symmetry)
range(nsymmetry)

## Change charts per page
par(mfrow=c(2,2))

## Change variable name
y<-malignant


## Comparing the line of best fit for each possible y value
plot(texture[y==0],perimeter[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Texture",ylab="Perimeter")
points(texture[y==1],perimeter[y==1],pch=3,col=6)
abline(lsfit(texture[y==0],perimeter[y==0]),pch=2,col=5)
abline(lsfit(texture[y==1],perimeter[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(texture[y==0],smoothness[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Texture",ylab="Smoothness")
points(texture[y==1],smoothness[y==1],pch=3,col=6)
abline(lsfit(texture[y==0],smoothness[y==0]),pch=2,col=5)
abline(lsfit(texture[y==1],smoothness[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(texture[y==0],compactness[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Texture",ylab="Compactness")
points(texture[y==1],compactness[y==1],pch=3,col=6)
abline(lsfit(texture[y==0],compactness[y==0]),pch=2,col=5)
abline(lsfit(texture[y==1],compactness[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(texture[y==0],concavity[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Texture",ylab="Concavity")
points(texture[y==1],concavity[y==1],pch=3,col=6)
abline(lsfit(texture[y==0],concavity[y==0]),pch=2,col=5)
abline(lsfit(texture[y==1],concavity[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(perimeter[y==0],smoothness[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Perimeter",ylab="Smoothness")
points(perimeter[y==1],smoothness[y==1],pch=3,col=6)
abline(lsfit(perimeter[y==0],smoothness[y==0]),pch=2,col=5)
abline(lsfit(perimeter[y==1],smoothness[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(perimeter[y==0],compactness[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Perimeter",ylab="Compactness")
points(perimeter[y==1],compactness[y==1],pch=3,col=6)
abline(lsfit(perimeter[y==0],compactness[y==0]),pch=2,col=5)
abline(lsfit(perimeter[y==1],compactness[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(perimeter[y==0],concavity[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Perimeter",ylab="Concavity")
points(perimeter[y==1],concavity[y==1],pch=3,col=6)
abline(lsfit(perimeter[y==0],concavity[y==0]),pch=2,col=5)
abline(lsfit(perimeter[y==1],concavity[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(smoothness[y==0],compactness[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Smoothness",ylab="Compactness")
points(smoothness[y==1],compactness[y==1],pch=3,col=6)
abline(lsfit(smoothness[y==0],compactness[y==0]),pch=2,col=5)
abline(lsfit(smoothness[y==1],compactness[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(smoothness[y==0],concavity[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Smoothness",ylab="Concavity")
points(smoothness[y==1],concavity[y==1],pch=3,col=6)
abline(lsfit(smoothness[y==0],concavity[y==0]),pch=2,col=5)
abline(lsfit(smoothness[y==1],concavity[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

plot(compactness[y==0],concavity[y==0],pch=2,col=5,xlim=c(1,10),ylim=c(1,20),
     xlab="Compactness",ylab="Concavity")
points(compactness[y==1],concavity[y==1],pch=3,col=6)
abline(lsfit(compactness[y==0],concavity[y==0]),pch=2,col=5)
abline(lsfit(compactness[y==1],concavity[y==1]),pch=3,col=6)
legend(1,20,legend=c("Benign","Malignant"),pch=2:3,col=5:6)

par(mfrow=c(1,1))
## Our Sixth model, Compactness x Texture
m6<-glm(malignant~radius+texture+perimeter+area+smoothness+compactness
    +concavity+concave.points+symmetry+compactness:texture,family=binomial)


## Summary and marginal model plots

summary(m6)

mmps(m6)
mmp(m6, fitted.values(m6))

## Our Seventh model, Compactness x Perimeter
m7<-glm(malignant~radius+texture+perimeter+area+smoothness+compactness+
    concavity+concave.points+symmetry+compactness:perimeter,family=binomial)

## Summary and marginal model plots

summary(m7)

mmps(m7)
mmp(m7, fitted.values(m7))

## Our Eighth model, Compactness x Smoothness
m8<-glm(malignant~radius+texture+perimeter+area+smoothness+compactness+
    concavity+concave.points+symmetry+compactness:smoothness,family=binomial)

## Summary and marginal model plots

summary(m8)

mmps(m8)
mmp(m8, fitted.values(m8))

## Our Eighth model, Compactness x Smoothness
mI<-glm(malignant~radius+texture+perimeter+area+smoothness+compactness+
  concavity+concave.points+symmetry+compactness:texture+compactness:perimeter
  +compactness:smoothness, family=binomial)

## Summary and marginal model plots

summary(mI)

mmps(mI)
mmp(mI, fitted.values(mI))

#######################################################
## Selection Processes ################################

## Backward elimination targeting AIC, using m6 1

backAIC<-step(m6,direction = "backward")

## Our Ninth model, Backwards AIC
m9<-glm(malignant ~ radius + texture + perimeter + area + compactness 
    + concavity + symmetry + texture:compactness, family = binomial)

## Summary and marginal model plots

summary(m9)

mmps(m9)
mmp(m9, fitted.values(m9))


## Backward elimination targeting BIC, using m6 2
backBIC<-step(m6,direction = "backward",k=log(n))

## Our Tenth model, Backwards BIC
m10<-glm(malignant ~ radius + texture + compactness + concavity 
    + texture:compactness, family = binomial)

## Summary and marginal model plots

summary(m10)

mmps(m10)
mmp(m10, fitted.values(m10))

backAIC<-step(m6,direction = "backward")

## Testing the differences between the models
anova(m10,m9,test="Chisq")


## Backward elimination targeting AIC, using mI

backAIC2<-step(mI,direction = "backward")

## Our 11th model, Backwards AIC
m11<-glm(malignant ~ radius + texture + perimeter + area + smoothness + 
    compactness + concavity + symmetry + texture:compactness + smoothness:compactness, 
    family = binomial)

## Summary and marginal model plots

summary(m11)
mmps(m11)
mmp(m11, fitted.values(m11))


## Backward elimination targeting BIC, using mI 
backBIC2<-step(mI,direction = "backward",k=log(n))

##Our Twelfth  model, Backwards BIC
m12<-glm(malignant ~ radius + texture + compactness + concavity + texture:compactness
    , family = binomial)

##Summary and marginal model plots

summary(m12)

mmps(m12)
mmp(m12, fitted.values(m12))

anova(m11,m12, test = "Chisq")

## Compiling the Deviance, AIC, and BIC for each model.
Deviance<-c(m1$deviance,m2$deviance,m3$deviance,m4$deviance,m5$deviance,mk$deviance,
    m6$deviance,m7$deviance,m8$deviance,mI$deviance,m9$deviance,m10$deviance,m11$deviance,m12$deviance)
A.I.C.<-c(m1$aic,m2$aic,m3$aic,m4$aic,m5$aic,m6$aic,
    m6$aic,m7$aic,m8$aic,mI$aic,m9$aic,m10$aic,m11$aic,m12$aic)
BIC<- Deviance+c(
  (m1$rank-1)*log(n),(m2$rank-1)*log(n),(m3$rank-1)*log(n),
  (m4$rank-1)*log(n),(m5$rank-1)*log(n),(mk$rank-1)*log(n),(m6$rank-1)*log(n),(m7$rank)*log(n),
  (m8$rank)*log(n),(mI$rank-1)*log(n),(m9$rank)*log(n),(m10$rank)*log(n),(m11$rank-1)*log(n),(m12$rank-1)*log(n)
)

Model<-c("m1","m2","m3","m4","m5","mk",
         "m6","m7","m8","mI","m9","m10","m11","m12")
Scores<-data.frame(Model,Deviance,A.I.C.)
kable(Scores)

#The final model chosen was m11.

## Pull the value of each coefficient 
coef(m11)

## The following lines determine the mean of the remaining predictors
mean(radius)
mean(texture)
mean(perimeter)
mean(area)
mean(smoothness)
mean(compactness)
mean(concavity)
mean(symmetry)
mean(texture)*mean(compactness)
mean(smoothness)*mean(compactness)

## Storing the means as a vector
x<-c(1,
  mean(radius),
  mean(texture),
  mean(perimeter),
  mean(area),
  mean(smoothness),
  mean(compactness),
  mean(concavity),
  mean(symmetry),
  mean(texture)*mean(compactness),
  mean(smoothness)*mean(compactness) 
)

## Calculating the probability for a maliginant tumor given an average in each score.
probability<-exp(sum(coef(m11)*x))/(1+exp(sum(coef(m11)*x)))

## Calculating the log odds.
logodds<-sum(coef(m11)*x)

cat("The probability of having a malignant tumour, given an average 
    in each score, is ",probability,".\n")
logodds
cat("The log odds of having a malignant tumour, given an average 
    in each score, is ",logodds,".\n")
cat("The odds of having a malignant tumour, given an average 
    in each score, is ",exp(logodds),". ")
