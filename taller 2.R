library(readr)
grasa <- read_csv("grasa.csv")
View(grasa)
colnames(grasa) = c("siri", "age", "bmi", "abdomen", "neck", "thigh", "hip")
#
library(xtable)

tabla.1 = summary(grasa)
print(xtable(t(tabla.1)), include.rownames = FALSE)

which(grasa$siri == 0)

library(psych)
png("plotcor.png")
corPlot(grasa, main = "Matriz de Correlaciones", 
        scale = FALSE, xlas = 2, cex = 0.8, upper = FALSE)
dev.off()

png("plotpairs1.png")
pairs(grasa[,c(1,2,3,4)])
dev.off()

png("plotpairs2.png")
pairs(grasa[,c(1,5,6,7)])
dev.off()

#

mod = lm(siri~age+bmi+abdomen+neck+thigh+hip, data = grasa)
tabla.2 = summary(mod)
print(xtable(tabla.2))
tabla.3 = summary.aov(mod)
print(xtable(tabla.3))

sqrt(sum(mod$residuals^2)/245)

library(car)
tabla.4 = as.table(vif(mod))
print(xtable(t(tabla.4)))

#

mod.r1 = lm(siri~abdomen+neck+hip, data = grasa)
tabla.5 = summary(mod.r1)
print(xtable(tabla.5))
tabla.6 = summary.aov(mod.r1)
print(xtable(tabla.6))
tabla.7 = anova(mod.r1,mod)
print(xtable(tabla.7))

#

x0pred = data.frame(age=55,bmi=25,abdomen=90,neck=35,thigh=60,hip=100)
x0 = predict(mod,x0pred,interval='prediction')
x1pred = data.frame(age=30,bmi=23,abdomen=110,neck=40,thigh=55,hip=80)
x1 = predict(mod,x1pred,interval='prediction')
x2pred = data.frame(age=40,bmi=30,abdomen=99,neck=35,thigh=60,hip=107)
x2 = predict(mod,x2pred,interval='prediction')
x3pred = data.frame(age=70,bmi=21,abdomen=82,neck=35,thigh=49,hip=88)
x3 = predict(mod,x3pred,interval='prediction')

class(x0)
tabla.8 = rbind(x0,x1,x2,x3)
print(xtable(tabla.8))

y0pred = data.frame(abdomen=90,neck=35,hip=100)
y0 = predict(mod.r1,y0pred,interval='confidence')
y1pred = data.frame(abdomen=110,neck=40,hip=80)
y1 = predict(mod.r1,y1pred,interval='prediction')
y2pred = data.frame(abdomen=99,neck=35,hip=107)
y2 = predict(mod.r1,y2pred,interval='prediction')
y3pred = data.frame(abdomen=82,neck=35,hip=88)
y3 = predict(mod.r1,y3pred,interval='prediction')

class(x0)
tabla.9 = rbind(y0,y1,y2,y3)
print(xtable(tabla.9))

newPoints = cbind(x0=rep(1,4),x1=c(55,30,40,70),x2=c(25,23,30,21)
                  ,x3=c(90,110,99,82),x4=c(35,40,35,35),X5=c(60,55,60,49),x6=c(100,88,107,88))
X = model.matrix(mod)
XtX.inv = solve(t(X)%*%X)
h.values = hatvalues(mod)
hmax = max(h.values)
h0 = apply(newPoints,MARGIN = 1,function(X){t(X)%*%XtX.inv%*%X})
h0 >hmax

getOption("max.print")

# 
y = grasa$siri
Z = apply(X[,-1],2,function(x){(x-mean(x))/sqrt(sum((x-mean(x))^2))})
ys = (y-mean(y))/sqrt(sum((y-mean(y))^2))

mod.std = lm(ys~Z-1)
summary(mod.std)
