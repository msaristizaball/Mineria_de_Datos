#install.packages("numDeriv")
#install.packages("Rfast")
#install.packages("Formula")
#install.packages("ISLR")
#install.packages("aplore3")
#install.packages("ssym")
#install.packages("CASdatasets")
#install.packages("GLMsData")
#install.packages(file.choose(), repos=NULL)
library(glmtoolbox)
library(ssym)
library(numDeriv)
library(Rfast)
library(Formula)
library(ISLR)
library(aplore3)
library(CASdatasets)
library(GLMsData)
library(olsrr)
################################################################################
#punto 3 #######################################################################
################################################################################
#Este conjunto de datos, disponible en el objeto deposit del paquete GLMsData de R,
#corresponde a un experimento en el que lotes de insectos se expusieron durante seis
#d ??ias a varias dosis (en miligramos) de tres insecticidas. El objetivo del an ??alisis consiste
#en explicar las diferencias entre los lotes con respecto a la proporci ??on de insectos que
#terminaron muertos (Killed/Number) usando las diferencias entre los lotes con respecto
#al tipo (Insecticide) y la dosis (Deposit) del insecticida a la que se expusieron.

data("deposit")

summary(deposit)
attach(deposit)
sd(Killed)
sd(Number)
sd(Insecticide)
sd(Deposit)
#(A) Haga un analisis descriptivo a los datos. Comente.

deposit$insect_dead=deposit$Killed/deposit$Number

boxplot(deposit$insect_dead ~ deposit$Insecticide, xlab ="tipo",ylab = "Killed/Number", )

boxplot(deposit$insect_dead ~ deposit$Deposit, xlab ="Nivel de dosis",ylab = "proporcion", )

#El conjunto de datos deposit tiene 3 variables numericas Killed, Number, Deposit; y una variable
#categorica. Para Killed tenemos que su media es de 28.11, su realizacion minima es de 2, 
#su realizacion maxima 50 y su desviacion estandar es de 15.941. De igual forma para 
#la variable Number tenemos que su media es de 47.89, su realizacion minima es de 29, 
#su realizacion maxima 50 y su desviacion estandar es de 5.508. Para 
#la variable Deposit tenemos que su media es de 4.462, su realizacion minima es de 2, 
#su realizacion maxima 8 y su desviacion estandar es de 2.118. Por ultimo para la variable
#Insecticide tenemos tres niveles A,B,C con una frecuencia de 6 para cada nivel.



#(B) Ajuste a los datos modelos de respuesta binomial con varias funciones de enlace
#(logit, probit, complemento log-log, y Cauchy) y el predictor lineal dado por 1 +
#  Insecticide + I(1/Deposit). Use los criterios AIC, BIC y R2 ajustado para
#comparar y seleccionar el "mejor" modelo.

attach(deposit)
fit3.1= glm(Killed/Number~Insecticide+I(1/Deposit), weights=Number, family=binomial("logit"))
fit3.2= glm(Killed/Number~Insecticide+I(1/Deposit), weights=Number, family=binomial("probit"))
fit3.3= glm(Killed/Number~Insecticide+I(1/Deposit), weights=Number, family=binomial("cloglog"))
fit3.4= glm(Killed/Number~Insecticide+I(1/Deposit), weights=Number, family=binomial("cauchit"))

summary(fit3.2)
AIC(fit3.1, fit3.2, fit3.3, fit3.4)
BIC(fit3.1, fit3.2, fit3.3, fit3.4)

with(fit3.1,1-deviance*df.null/(null.deviance*df.residual))
with(fit3.2,1-deviance*df.null/(null.deviance*df.residual))
with(fit3.3,1-deviance*df.null/(null.deviance*df.residual))
with(fit3.4,1-deviance*df.null/(null.deviance*df.residual))

summary(fit3.1)
str(deposit)





#El mejor modelo segun el AIC, BIC y R^2 es el fit3.4 usando funcion de enlace Cauchy

#(C) Haga un gr ??afico de los datos junto con el modelo seleccionado. Comente.

glmtoolbox::envelope(fit3.4,rep=50, ylim=c(-5,5))
envelope(fit3.2)
graphs_glm(fit3.4)

??graphs_glm
#(D) Use las estad ??isticas de Wald y raz ??on de verosimilitudes para evaluar, al nivel de
#significancia aproximado de 5 %, si la proporci ??on esperada de insectos muertos
#depende de la interaci ??on entre Insecticide y I(1/Deposit).

fit3.2.1= glm(Killed/Number~Insecticide+I(1/Deposit)+Insecticide*I(1/Deposit), weights=Number, family=binomial("probit"))
anova2(fit3.2, fit3.2.1, test = "lr")
anova2(fit3.2, fit3.2.1, test = "wald")

#Las dos estadisticas concluyen que no existe evidencia significativa para rechazar
# $H_0$, por lo tanto segun las estadisticas de wald y de razon de verosimilitud la proporcion esperada
#de insectos muertos no depende de la interacion entre Insecticide y I(1/Deposit).

#(E) Haga el an ??alisis de diagn ??ostico al modelo (an ??alisis residual y an ??alisis de sensibili-
#dad). Comente.
par(mfrow = c(1, 2))
residuals2(fit3.2)
glmtoolbox::envelope(fit3.2)

which(cooks.distance(fit3.2)>1)
which(cooks.distance(fit3.2)>qf(0.5, 4, 17))

summary(fit3.2)

plot(cooks.distance(fit3.2))


#(F) Estime la proporci ??on esperada de insectos muertos en un lote que se expone
#a una dosis de 5 miligramos del insecticida del tipo A. Haga esta estimaci ??on
#"manualmente" y usando la funci ??on predict().

#exp(2.5240-10.9310*I(1/5))/(1+exp(2.5240-10.9310*I(1/5)))
#pred=data.frame(Deposit=5, Insecticide="A")        ############para enlace logit
#predict(fit3.1,pred, type = "response")

pred=data.frame(Deposit=5, Insecticide="A")
predict(fit3.2,pred, type = "response")

0.5+si1.4675-6.3386*I(1/5)


summary(fit3.2)
?glm


################################################################################
#punto 5 #######################################################################
################################################################################


#5. Estos datos, disponibles en el objeto danishlc del paquete GLMsData de R, se refieren
#al n ??umero de casos de c ??ancer de pulm ??on (Cases) y a la poblaci ??on en riesgo (Pop) por
#grupos de edad (Age) en cuatro ciudades danesas (City) entre 1968 y 1971.

data("danishlc")

#(A) Haga un an ??alisis descriptivo a los datos. Comente.

#(B) Ajuste a los datos un modelo que asume que los casos de c ??ancer de pulm ??on son
#realizaciones de variables aleatorias independientes con distribuci ??on de Poisson,
#funci ??on de enlace logaritmo natural, offset igual a log(Pop), y las variables
#explicativas City y Age.
attach(danishlc)
fit5= glm(Cases ~ City + Age , family=poisson("log") ,offset=log(Pop) , data=danishlc)
summary(fit5)

#(C) Use las estad ??isticas de Wald y raz ??on de verosimilitudes para evaluar, al nivel de
#significancia aproximado de 5 %, si el efecto del grupo de edad sobre la tasa de
#incidencia de c ??ancer de pulm ??on depende de la ciudad.

anova2(fit5, test="wald"  )
?anova2

glmtoolbox::envelope(fit5)

################################################################################
#punto 6 #######################################################################
################################################################################

#6. En estos datos, disponibles en el objeto rrates del paquete GLMsData de R, la variable
#respuesta es la velocidad de la reacci ??on del catalizador (Rate), en 109 gmole por gramo,
#y las variables explicativas son la concentraci ??on de ox ??igeno (Conc.O), en 10000 gmol
#por litro, y la temperatura (Temp), en grados Kelvin.

data("rrates")
attach(rrates)

#(A) Haga un an ??alisis descriptivo a los datos. Comente

#(B) Ajuste a los datos modelos con varias distribuciones para la variable respuesta
#(normal, lognormal, Gama, y normal inversa), varias funciones de enlace (logaritmo natural,
#inversa e identidad) y el predictor lineal dado por 1 + log(Conc.O)+ Temp. 
#Use los criterios AIC, BIC y R2 ajustado para comparar los modelos candidatos y seleccionar
#el "mejor".
attach(rrates)
fit6.1= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=gaussian("log"), data=rrates)
fit6.2= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=gaussian("inverse"), data=rrates)
fit6.3= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=gaussian("identity"), data=rrates)

fit6.4= glm(log(Rate) ~ 1 + log(Conc.O)+ Temp , family=gaussian("log"), data=rrates)
fit6.5= glm(log(Rate) ~ 1 + log(Conc.O)+ Temp , family=gaussian("inverse"), data=rrates)
fit6.6= glm(log(Rate) ~ 1 + log(Conc.O)+ Temp , family=gaussian("identity"), data=rrates)

fit6.7= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=Gamma("log"), data=rrates)
fit6.8= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=Gamma("inverse"), data=rrates)
fit6.9= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=Gamma("identity"), data=rrates)

fit6.10= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=inverse.gaussian("log"), data=rrates)
fit6.11= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=inverse.gaussian("inverse"), data=rrates)
fit6.12= glm(Rate ~ 1 + log(Conc.O)+ Temp , family=inverse.gaussian("identity"), data=rrates)

AIC(fit6.1, fit6.2, fit6.3, fit6.4, fit6.5, fit6.6, fit6.7, fit6.8, fit6.9, fit6.10,
    fit6.11, fit6.12)

BIC(fit6.1, fit6.2, fit6.3, fit6.4, fit6.5, fit6.6, fit6.7, fit6.8, fit6.9, fit6.10,
    fit6.11, fit6.12)

with(fit6.1,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.2,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.3,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.4,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.5,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.6,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.7,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.8,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.9,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.10,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.11,1-deviance*df.null/(null.deviance*df.residual))
with(fit6.12,1-deviance*df.null/(null.deviance*df.residual))

#el mejor modelo segun AIC, BIC y R^2 es fit6.4

summary(fit6.4)

#(C) Use las estad ??isticas de Wald y raz ??on de verosimilitudes para evaluar, al nivel de
#significancia aproximado de 5 %, si el efecto de la concentraci ??on de ox ??igeno sobre
#la velocidad esperada de la reacci ??on del catalizador depende de la temperatura

fit6.4.1= glm(log(Rate) ~ 1 + log(Conc.O)+ Temp+ log(Conc.O)*Temp , family=gaussian("log"), data=rrates)

anova2(fit6.4, fit6.4.1, test="wald")
anova2(fit6.4, fit6.4.1, test="lr")

#el efecto de la concentracion de oxigeno sobre la velocidad esperada de reaccion del
#catalizador NO depende de la temperatura.

#(D) Realice el an ??alisis de diagn ??ostico (an ??alisis residual y an ??alisis de sensibilidad, y
#evalue si hay evidencia estad ??isticamente significativa en contra el supuesto de
#par ??ametro de dispersi ??on constante). Comente.

residuals2(fit6.4)
vdtest(fit6.4)

#existe evidencia significativa para rechazar H_0 por lo tanto asumimos que la 
#dispercion de los residuales no es constante 

#(E) Interprete las estimaciones de los par ??ametros excepto el intercepto.
summary(fit6.4)

glmtoolbox::envelope(fit6.10,type="pearson")
residuals2(fit6.10,ylim=c(-5,5))
vdtest(fit6.10)

?envelope
################################################################################
#punto 7 completo ##############################################################
################################################################################
#7. Este conjunto de datos, analizado por James et al (2013, p ??agina 15) y disponible
#en el objeto advertising del paquete glmtoolbox de R, consta de las ventas de un
#producto (sales) en 200 mercados diferentes, junto con los presupuestos de publicidad
#del producto en cada uno de esos mercados para dos medios diferentes: televisi ??on (TV)
#y radio (radio). El objetivo del an ??alisis consiste en explicar las diferencias entre los
#mercados con respecto a las ventas usando las diferencias entre los mismos en relaci ??on
#a sus presupuestos de publicidad en TV y radio.

data("advertising")

#(A) Haga un an ??alisis descriptivo a los datos. Comente
attach(advertising)

summary(advertising)
sd(TV)
sd(radio)
sd(newspaper)
sd(sales)

#el conjunto de datos advertising tiene 4 variables numericas: TV, radio, newspaer
#y sales, donde para TV tenemos que su media es de 147.04, su realizacion minima es de 0.70,
#su realizacion maxima 296.40 y su desviacion estandar es de 85.85. 
#De igual forma para la variable radio tenemos que su media es de 23.264, su realizacion 
#minima es de 0, su realizacion maxima 49.6 y su desviacion estandar es de 14.85 .
#para la variable newspaper tenemos que su media es de 30.55, su realizacion 
#minima es de 0.30, su realizacion maxima 114 y su desviacion estandar es de 21.779 .
#para la variable sales tenemos que su media es de 14.02, su realizacion 
#minima es de 1.6, su realizacion maxima 27 y su desviacion estandar es de 5.217.
#

#(B) Ajuste a los datos modelos con varias distribuciones para la variable respuesta 
#(normal, lognormal, Gama, y normal inversa), varias funciones de enlace (logaritmo natural,
#inversa e identidad) y el predictor lineal dado por 1 + log(TV) + radio + log(TV):radio. 
#Use los criterios AIC, BIC y R2 ajustado para comparar los modelos candidatos y seleccionar 
#el "mejor".
attach(advertising)
fit7.1= glm(sales ~ log(TV) + radio + log(TV)*radio , family=gaussian("log"))
fit7.2= glm(sales ~ log(TV) + radio + log(TV)*radio , family=gaussian("inverse"))
fit7.3= glm(sales ~ log(TV) + radio + log(TV)*radio , family=gaussian("identity"))

fit7.4= glm(log(sales) ~ log(TV) + radio + log(TV)*radio , family=gaussian("log"))
fit7.5= glm(log(sales) ~ log(TV) + radio + log(TV)*radio , family=gaussian("inverse"))
fit7.6= glm(log(sales) ~ log(TV) + radio + log(TV)*radio , family=gaussian("identity"))

fit7.7= glm(sales ~ log(TV) + radio + log(TV)*radio , family=Gamma("log"))
fit7.8= glm(sales ~ log(TV) + radio + log(TV)*radio , family=Gamma("inverse"))
fit7.9= glm(sales ~ log(TV) + radio + log(TV)*radio , family=Gamma("identity"))

fit7.10= glm(sales ~ log(TV) + radio + log(TV)*radio , family=inverse.gaussian("log"))
fit7.11= glm(sales ~ log(TV) + radio + log(TV)*radio , family=inverse.gaussian("inverse"))
fit7.12= glm(sales ~ log(TV) + radio + log(TV)*radio , family=inverse.gaussian("identity"))

AIC(fit7.1, fit7.2, fit7.3, fit7.7, fit7.8, fit7.9, fit7.10, fit7.11, fit7.12)
AIC(fit7.4)+2*sum(log(sales))
AIC(fit7.5)+2*sum(log(sales))
AIC(fit7.6)+2*sum(log(sales))

BIC(fit7.1, fit7.2, fit7.3, fit7.7, fit7.8, fit7.9, fit7.10, fit7.11, fit7.12)
BIC(fit7.4)+2*sum(log(sales))
BIC(fit7.5)+2*sum(log(sales))
BIC(fit7.6)+2*sum(log(sales))


with(fit7.1,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.2,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.3,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.4,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.5,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.6,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.7,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.8,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.9,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.10,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.11,1-deviance*df.null/(null.deviance*df.residual))
with(fit7.12,1-deviance*df.null/(null.deviance*df.residual))

with(c(fit7.12,fit7.11),1-deviance*df.null/(null.deviance*df.residual))

summary(fit7.1)
#el mejor modelo segun el AIC, BIC y R^2 es fit7.6

#(C) Realice el an ??alisis de diagn ??ostico (an ??alisis residual y an ??alisis de sensibilidad).
#Comente.
glmtoolbox::envelope(fit7.1)
residuals2(fit7.1)

which(cooks.distance(fit7.1)>1)
which(cooks.distance(fit7.1)>qf(0.5, 4, 196))
which(cooks.distance(fit7.1)>0.1)

plot(cooks.distance(fit7.1))

summary(fit7.6)

#existe evidencia significartiva en contra de la dispercion constante 

#(D) Evalue si hay evidencia estad ??isticamente significativa en contra el supuesto de
#par ??ametro de dispersi ??on constante. H ??agalo tambi ??en excluyendo las observaciones
#"influyentes".

vdtest(fit7.1)
ad=advertising[-c(3,6),]
fit7.1.1= glm(ad$sales ~ log(ad$TV) + ad$radio + log(ad$TV)*ad$radio , family=gaussian("log"))
vdtest(fit7.1.1)
residuals2(fit7.1.1)

par(mfrow = c(1, 2))
glmtoolbox::envelope(fit7.1)
glmtoolbox::envelope(fit7.1.1)

#existe evidencia significartiva en contra del supuesto de dispercion constante 

#(E) Interprete las estimaciones de los par ??ametros excepto el intercepto.
summary(fit7.1)

#controlando por la inversion en publicidad en radio,si por ejemplo la inversion
#en TV aumenta 20%, la media de las ventas es
#exp(\beta_1*log(1.20)+radio*\beta_3*log(1.20)) veces la media de las ventas sin 
#el aumento de publicidad en TV, tambien vemos que la inversion en publicidad en 
#radio potencia el efecto sobre las ventas de la inversion en publicidad en TV.


################################################################################
#punto 8 completo###############################################################
################################################################################

#8. Este conjunto de datos, analizado por Rieck y Nedelman (1991) y disponible en el
#objeto Biaxial del paquete ssym de R, describe la vida (Life), en n ??umero de ciclos, de
#una pieza de metal, y la cantidad de trabajo por ciclo (Work). El objetivo del an ??alisis
#consiste en explicar las diferencias entre la vida de las piezas de metal usando las
#diferencias en el trabajo por ciclo al que fueron expuestas.

data("Biaxial")

#(A) Haga un an ??alisis descriptivo a los datos. Comente.

summary(Biaxial)
sd(Biaxial$Work)
sd(Biaxial$Life)

#el conjunto de datos Biaxial tiene dos variables numericas Work y Life, donde para Work 
#tenemos que su media es de 40.29, su realizacion minima es de 11.5, su realizacion maxima
#100.5 y su desviacion estandar es de 20.91. De igual forma para la variable Life tenemos que 
#su media es de 566, su realizacion minima es de 125, su realizacion maxima
#5046 y su desviacion estandar es de 1111.262.




#(B) Haga un gr ??afico de la vida de las piezas de metal como una funci ??on de la can-
#tidad de trabajo por ciclo al que fueron expuestas. Haga tambi ??en un gr ??afico del
#logar ??itmo natural de la vida de las piezas de metal como una funci ??on del logar ??itmo
#natural de la cantidad del trabajo por ciclo a la que fueron expuestas. Comente.
attach(Biaxial)
par(mfrow = c(1, 2))
plot(Biaxial)
plot(x=log(Work),y=log(Life),ylab="log(Life)",xlab="log(Work)")

#Notamos que al aplicar el logaritmo a los datos cambia la tendencia de la nube 
#de individuos haciendolos lineales decrecientes

#(C) Ajuste a los datos los siguientes modelos

attach(Biaxial)
fit8.1= glm(Life ~ log(Work) , family=inverse.gaussian("log") , data=Biaxial)
fit8.2= glm(Life ~ log(Work) , family=Gamma(link = "log") , data=Biaxial)
fit8.3= glm(Life ~ log(Work) , family=gaussian(link = "log"), data=Biaxial)
fit8.4= glm(log(Life) ~ log(Work) , family=gaussian(link = "log"), data=Biaxial)


par(mfrow = c(1, 2))
plot(x=log(Work),y=log(Life),ylab="log(Life)",xlab="log(Work)")

abline(fit8.1)
abline(fit8.2,col="green")
abline(fit8.3,col="red")

plot(x=log(Work),y=log(log(Life)),ylab="log(log(Life))",xlab="log(Work)")
abline(fit8.4)


par(mfrow = c(2, 2))
glmtoolbox::envelope(fit8.1)
glmtoolbox::envelope(fit8.2)
glmtoolbox::envelope(fit8.3)
glmtoolbox::envelope(fit8.4)

?glmtoolbox::envelope()

AIC(fit8.1,fit8.2,fit8.3)
AIC(fit8.4)+2*sum(log(Life))

BIC(fit8.1,fit8.2,fit8.3)
BIC(fit8.4)+2*sum(log(Life))

with(fit8.1,1-deviance*df.null/(null.deviance*df.residual))
with(fit8.2,1-deviance*df.null/(null.deviance*df.residual))
with(fit8.3,1-deviance*df.null/(null.deviance*df.residual))
with(fit8.4,1-deviance*df.null/(null.deviance*df.residual))

#el mejor modelo segun AIC, BIC y R^2 es fit8.4

#(D) Use las estad ??isticas de Wald y raz ??on de verosimilitudes para evaluar, al nivel de
#significancia aproximado de 5 %, si la vida esperada de las piezas de metal depende
#de la cantidad de trabajo al que fueron expuestas.

anova2(fit8.4, test="wald" )
anova2(fit8.4, test="lr" )

#el test de wald y de razon de verosimilitude suguiere que la vida esperada de las piezas
#de metal depende de la cantidad de trabajo al que fueron expuestas 

#(E) Haga el an ??alisis de diagn ??ostico (an ??alisis residual, an ??alisis de sensibilidad, y evalue
#si hay evidencia estad ??isticamente significativa en contra el supuesto de par ??ametro
#de dispersi ??on constante). Comente.

residuals2(fit8.4)
vdtest(fit8.4)
#cooks.distance(fit8.4)
#plot(cooks.distance(fit8.4))
which(cooks.distance(fit8.4)>1)
which(cooks.distance(fit8.4)>qf(0.5, 2, 44))

summary(fit8.4)

#no existe evidencia significativa en contra del supuesto de dispercion constante 
#No hay obseraciones influyentes 

#(F) Interprete las estimaciones de los par ??ametros excepto el intercepto.

summary(fit8.4)



#la vida esperada de las piezas de metal disminuye en la medida que es expuesta  a
#mayor trabajo

#(G) Estime la vida esperada, en n ??umero de ciclos, de una pieza de metal cuando el
#trabajo por ciclo es 40. Haga esta estimaci ??on "manualmente" y usando la funci ??on
#predict().

exp(2.76880-0.26205*log(40))

pred=data.frame(Work=40)
predict(fit8.4,pred, type = "response")


#la vida esperada para una pieza de metal es 1.80214  numero de ciclos cuando el
#trabajo por ciclo es 40 



