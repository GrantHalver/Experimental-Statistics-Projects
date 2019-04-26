library(foreign)
library(car)
library(binomTools)
library(pROC)

#Importing Data and Creating Table
pneumonia <- read.dta("pneumonia.dta")
head(pneumonia)
drunk = ifelse(pneumonia$Alcohol>=1, 1, 0)#if Alcohol value is greater than 1, rewrite as 1. Else, mark as 0.
cough = ifelse(pneumonia$Cigarette>=1, 1, 0) #if Cigarette value is greater than 1, rewrite as 1. Else, mark as 0.
pneumonia$Alcohol = drunk #set Alcohol column to binomial value
pneumonia$Cigarette = cough #set Cigarette column to binomial value
A <- glm(pneumonia ~  Agemom + Urban + Alcohol + Cigarette + Region + poverty + birthweight + Race + Education + Number_of_siblings + Month_weaned + Month_solid_food, family=binomial, data=pneumonia)
summary(A)

#Forward Selection
null = glm(pneumonia~1, data=pneumonia, family = binomial)
full = glm(pneumonia ~  Agemom + Urban + Alcohol + Cigarette + Region + poverty + birthweight + Race + Education + Number_of_siblings + Month_weaned + Month_solid_food, family=binomial, data=pneumonia)
step(null, scope = list (lower = null, upper = full), direction = "forward")


#Forward Selection Analaysis
B=glm(formula = pneumonia ~ Month_weaned + Cigarette + Education + Region + Number_of_siblings + Agemom, family = binomial, data = pneumonia)
summary(B)
vif(B)
Rsq(B)
Rsq.B = Rsq(B)
HLtest(Rsq.B)
1-pchisq(B$null.deviance-B$deviance,B$df.null-B$df.residual)
1-(B$deviance/B$null.deviance)

#Predict Plot
pi<-predict(B, type="response")
Month_weaned.sub <- pneumonia$Month_weaned[pneumonia$Cigarette == 0 & pneumonia$Region == 1]
pi.sub <- pi[pneumonia$Cigarette == 0 & pneumonia$Region == 1]
my.order <- order(Month_weaned.sub)
plot(Month_weaned.sub[my.order], pi.sub[my.order], xlim=c(0, 28), ylim=c(0, 0.2), xlab="Months Weaned", ylab="Pneumonia", type='b', col=1, pch=16)

for(i in 0:1){
  for(j in 1:4){
    Month_weaned.sub <- pneumonia$Month_weaned[pneumonia$Cigarette == i & pneumonia$Region == j]
    pi.sub <- pi[pneumonia$Cigarette == i & pneumonia$Region == j]
    my.order <- order(Month_weaned.sub)
    lines(Month_weaned.sub[my.order], pi.sub[my.order], type='b', col=j+i, pch=16)
  }
}

#Backward Selection
step(full, data = pneumonia, direction = "backward")

#Backward Selection Analysis
C=glm(formula = pneumonia ~ Agemom + Cigarette + Region + Number_of_siblings + Month_weaned, family = binomial, data = pneumonia)
summary(C)
vif(C)
Rsq(C)
Rsq.C = Rsq(C)
HLtest(Rsq.C)
1-pchisq(C$null.deviance-C$deviance,C$df.null-C$df.residual)
1-(C$deviance/C$null.deviance)

#Stepwise Selection
step(null, scope = list(upper = full), direction = "both")
D=glm(formula = pneumonia ~ Month_weaned + Cigarette + Region + Number_of_siblings + Agemom, family = binomial, data = pneumonia)
summary(D)
vif(D)
Rsq(D)
Rsq.D = Rsq(D)
HLtest(Rsq.D)
1-pchisq(D$null.deviance-D$deviance,D$df.null-D$df.residual)
R2.D=1-(D$deviance/D$null.deviance)

#ROC Curve of Logistic Regression
pred = predict(A, type="response")
g = roc(pneumonia$pneumonia ~ pred)
plot(g)

#Interaction Model
I = glm(formula = pneumonia~1+Agemom+Number_of_siblings+Cigarette:Agemom+Number_of_siblings*Agemom+Number_of_siblings*Region+Month_weaned*Cigarette, family = binomial, data = pneumonia)
summary(I)
Rsq(I)
Rsq.I = Rsq(I)
HLtest(Rsq.I)
1-pchisq(I$null.deviance-I$deviance,I$df.null-I$df.residual)
1-(I$deviance/I$null.deviance)
