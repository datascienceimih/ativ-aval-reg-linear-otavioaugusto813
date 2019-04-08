#Atividade Avaliativa 01 - Machine Learning

##Aluno: OtÃ¡vio Augusto Alves Coelho
##Professor: Neylson Crepalde
##Curso: CiÃªncias de Dados


## Exercício 1 [8]

if (! "ISLR" %in% installed.packages()) install.packages("ISLR")
if (! "MASS" %in% installed.packages()) install.packages("MASS")
if (! "dplyr" %in% installed.packages()) install.packages("dplyr")
if (! "ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (! "readr" %in% installed.packages()) install.packages("readr")
if (! "texreg" %in% installed.packages()) install.packages("texreg")

#carregando a biblioteca library(ISLR), onde estÃ¡ o banco de dados Auto, e as outras bibliotecas
#estatÃ�sticas necessÃ¡rias para resoluÃ§Ã£o dos exercÃ�cios.

library(ISLR)
library(readr)
library(dplyr)
library(texreg)
library(ggplot2)

#realizando a regressÃ£o linear em que mpg Ã© o predicto e o horsepower Ã© o preditor

str(Auto)
reg1 = lm(mpg ~ horsepower, data = Auto)
summary(reg1)


## a) I - como se observa, o p-valor Ã© muito pequeno, menor que 0.05, 
# o que indica que hÃ¡ uma correlaÃ§Ã£o
#estatisticamente vÃ¡lida entre ambos.
## II - como o p-valor Ã© prÃ³ximo de 0, menor do que 0.05,
# podemos dizer que a correlaÃ§Ã£o Ã© forte. Ao mesmo tempo, pelo valor do 
# Multiple R-squared, que Ã© de 0.6059, podemos dizer que o poder explicativo do modelo Ã© de 60,59%.
## III - como se observa pelo valor negativo do coeficiente, 
# temos uma correlaÃ§Ã£o negativa entre o horsepower e o mpg, o que indica que o aumento de 
# horsepower significa uma diminuiÃ§Ã£o no nÃºmero de mpg (milhas por galÃ£o).
## IV -  


predict(reg1, data.frame(horsepower=c(98)), interval="confidence")
predict(reg1, data.frame(horsepower=c(98)), interval="prediction")

## b) 

plot(mpg ~ horsepower, data=Auto)
abline(reg1, col="red", lwd = 2)


## c)

#usamos o comando par(mfrow=c(2,2)) que irÃ¡ dividir a tela de print em quatro partes
#onde 4 grÃ¡ficos distintos serÃ£o plotados.

par(mfrow=c(2,2))
plot(reg1)

#como se observa hÃ¡ possibilidade de a correlaÃ§Ã£o entre as variÃ¡veis nÃ£o ser
#linear;


## ExerÃ�cio 2 [9]

#a) produzindo uma scatterbox plot que inclua todas as variÃ¡veis contidas no
#data set Auto

data(Auto)
pairs(Auto)

##b) 

cor(subset(Auto, select= -name))

##c) 

str(Auto)
reg2 = lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data=Auto)
summary(reg2)

## I) Sim, hÃ¡ relaÃ§Ã£o. NÃ£o hÃ¡ relaÃ§Ã£o entre o predicto e todos os preditores. Como se observa, hÃ¡ alguns valores para o 
#p-valor acima de 0.05, como para cylinders, horsepower, acceleration e origin.

## II) O modelo aponta correlaÃ§Ã£o estatÃ�stica significativa para displacement, weight, year e origin.

## III) O coeficiente positivo de 0.75 aponta que a cada ano hÃ¡ um aumento de milhas por galÃ£o,
#isto Ã©, que o carro se torna mais econÃ´mico, jÃ¡ que consegue andar mais milhas com um galÃ£o.

## d) 
par(mfrow=c(2,2))
plot(reg2)

# os resÃ�duos apontam alguns outliers, especialmente acima da curva. O leverage plot aponta observaÃ§Ãµes
## com leverage alto, como se observa nos pontos distribuÃ�dos em torno do valor 4 (sendo que 
## a maior parte dos valores estÃ¡ entre -2 e 2).



## ExercÃ�cio 3 [10]

data("Carseats")
head(Carseats)
str(Carseats)

# a)

reg3 = lm(Sales ~ Population + Urban + US, data = Carseats)
summary(reg3)

# b) a partir da operacionalizaÃ§Ã£o da correlaÃ§Ã£o linear, observa-se que nÃ£o hÃ¡ correlaÃ§Ã£o estatisticamente
#vÃ¡lida entre todos os valores. Os p-valores para Population e Urban sÃ£o maiores que 0.05, indicando sua 
#ausÃªncia de correlaÃ§Ã£o com as Vendas. Ao mesmo tempo, contudo, o p-valor apresentado para US aponta uma correlaÃ§Ã£o 
#significativa entre esta variÃ¡vel e as vendas. Nesse sentido, observa-se que hÃ¡ uma relaÃ§Ã£o entre a localizaÃ§Ã£o 
#da loja e o nÃºmero de vendas. Se a loja onde a venda foi realizada estÃ¡ nos EUA, espera-se
#um aumento de 1036 unidades vendidas.

## c) 

# Sales = 0.0007Pop - 0.1341Urbanyes + 1.036USYes + 6.72

## d) Posso rejeitar a hipÃ³tese nula apenas para a variÃ¡vel USYes, jÃ¡ que seu p-valor
# Ã© menor do que 0.05.

## e) 

reg4 = lm(Sales ~ US, data=Carseats)
summary(reg4)


## f) Ambas sÃ£o semelhantes, embora o poder explicativo da segunda seja maior, jÃ¡ que esta
## possui um Multiple R-squared (para regressÃµes lineares simples) maior do 
# que o Adjusted R-squared (para correlaÃ§Ãµes mÃºltiplas) da segunda. Ao mesmo tempo,
# por ser mais simples, possuindo menos variÃ¡veis, preferimos ela em relaÃ§Ã£o 
# Ã  primeira, embora as diferenÃ§as sejam pequenas.

## g) 

confint(reg4, level=0.95)

## h)

par(mfrow=c(2,2))
plot(reg4)

# HÃ¡ evidÃªncias de high leverage se considerarmos os valores prÃ³ximos de 3 no grÃ¡fico de leverage.

## ExercÃ�cio 4 (13)


##a)
set.seed(1)
x=rnorm(100)

##b)

eps = rnorm(100, 0, sqrt(0.25))

##c)

y = -1 + 0.5*x + eps

## o tamanho de y Ã© 100. Beta 0 Ã© -1 e Beta1 Ã© 0.5.

## d)

plot(x, y)

## observa-se uma correlaÃ§Ã£o positiva e ascendente entre as variÃ¡veis.

## e)

regz=lm(y ~ x)
summary(regz)


# observa-se uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor Ã© menor 
# do que 0.05 (pode-se rejeitar a hipÃ³tese nula).
# O acrÃ©scimo de uma unidade em x implica um aumento de 0.499 em y.
# comparando os valores, podemos perceber que ambos os modelos possuem valores para B0 e B1 similares.

## f)

abline(regz, col="red", lwd=2)
#Ã© a reta da correlaÃ§Ã£o seguindo o mÃ©todo do
# mÃ�nimos quadrados ordinÃ¡rios

# sendo a fÃ³rmula para a linha de regressÃ£o populacional Y = B0 + B1X, podemos dizer que a reta
# serÃ¡ baseada na funÃ§Ã£o criada na letra c. Dessa forma, temos a equaÃ§Ã£o para essa linha:

ypop = -1 + 0.5*x

# traÃ§ando a linha

abline(-1, 0.5, col="blue", lwd=2)

legend(1, -1, legend=c("Reta OLS", "Reta Pop."), col=c("red", "blue"), lty=1:5, cex=0.5)

## g)

regy = lm(y ~ x + I(x^2))
summary(regz)
summary(regy)

# comparando os valores de Residual standard error e Multiple R-Squared, observa-se que a funÃ§Ã£o
# aumenta levemente seu poder explicativo, jÃ¡ que o primeiro diminui (4814 > 0.479) e o segundo aumenta
# (0.4674 < 0.4779).
# 
#
# h)

#a) set.seed(1)
x1=rnorm(100)
#b) 
eps1 = rnorm(100, 0, sqrt(0.1))
# c) 
y1 = -1 + 0.5*x1 + eps1 #o tamanho de y Ã© 100. Beta 0 Ã© -1 e Beta1 Ã© 0.5
#d) 
plot(x1, y1) #observa-se uma correlaÃ§Ã£o positiva e ascendente entre as variÃ¡veis.
#e) 
regk = lm(y1 ~ x1)
summary(regk)
summary(regz)
#f) 
abline(regk, col="red", lwd=2)
ypop1 = -1 + 0.5*x
abline(-1, 0.5, col="blue", lwd=2)
legend(1, -1, legend=c("Reta OLS", "Reta Pop."), col=c("red", "blue"), lty=1:5, cex=0.5)

## se no primeiro exemplo observava-se que as retas basicamente coincidiam, com a mudanÃ§a na
#variÃ¢ncia do modelo, observa-se que a reta OLS, baseada nos mÃ�nimos quadrados ordinÃ¡rios, torna-se
#mais inclinada do que a reta populacional. 
# como se observa ao comparar ambos os modelos, o multiple R-squared Ã© muito maior no segundo modelo
#regk (0.7348 > 0.4674), o que significa que seu poder explicativo Ã© maior.


# i)

#a) set.seed(1)
x2=rnorm(100)
#b) 
eps2 = rnorm(100, 0, sqrt(0.3))
# c) 
y2 = -1 + 0.5*x2 + eps2
#d) 
plot(x2, y2) #observa-se uma correlaÃ§Ã£o positiva e ascendente entre as variÃ¡veis.
#e) 
regl = lm(y2 ~ x2)
#f) 
abline(regl, col="red", lwd=2)
ypop1 = -1 + 0.5*x
abline(-1, 0.5, col="blue", lwd=2)
legend(1, -1, legend=c("Reta OLS", "Reta Pop."), col=c("red", "blue"), lty=1:5, cex=0.5)
summary(regl)
summary(regz)


## o contrÃ¡rio ocorre em relaÃ§Ã£o ao item anterior. aumentando a variÃ¢ncia, observa-se que o modelo
# adquire um menor poder explicativo, de 52%, menor do que o valor de 73% do item anterior, 
# embora se mantenha maior do que o valor da reta populacional, 
# que possui um multiple R-squared de 0.4674, isto Ã©, um poder explicativo de 46,74%.

## ExercÃ�cio 5 [15]

##a)
library(MASS)
data(Boston)
head(Boston)
str(Boston)

reg6 = lm(crim ~ zn, data=Boston)
reg7 = lm(crim ~ indus, data=Boston)
reg8 = lm(crim ~ chas, data=Boston)
reg9 = lm(crim ~ nox, data=Boston)
reg10 = lm(crim ~ rm, data=Boston)
reg11 = lm(crim ~ age, data=Boston)
reg12 = lm(crim ~ dis, data=Boston)
reg13 = lm(crim ~ rad, data=Boston)
reg14 = lm(crim ~ tax, data=Boston)
reg15 = lm(crim ~ ptratio, data=Boston)
reg16 = lm(crim ~ black, data=Boston)
reg17 = lm(crim ~ lstat, data=Boston)
reg18 = lm(crim ~ medv , data=Boston)
??(Boston)

summary(reg6)
# HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para zn Ã© menor que 0.05. Esta correlaÃ§Ã£o, contudo, Ã© negativa.
summary(reg7)
#HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para chas Ã© menor que 0.05. Esta correlaÃ§Ã£o Ã© positiva.
summary(reg8)
##NÃ£o hÃ¡ correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis crim e chas.
summary(reg9)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para nox Ã© menor que 0.05. Esta correlaÃ§Ã£o Ã© positiva.
summary(reg10)
##HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para rm Ã© menor que 0.05. Esta correlaÃ§Ã£o Ã© negativa.
summary(reg11)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para age Ã© menor que 0.05. Esta correlaÃ§Ã£o Ã© positiva.
summary(reg12)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para dis Ã© menor que 0.05. Esta correlaÃ§Ã£o Ã© negativa.
summary(reg13)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para rad Ã© menor que 0.05.Esta correlaÃ§Ã£o Ã© positiva.
summary(reg14)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para tax Ã© menor que 0.05.Esta correlaÃ§Ã£o Ã© positiva.
summary(reg15)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para ptratio Ã© menor que 0.05.Esta correlaÃ§Ã£o Ã© positiva.
summary(reg16)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para black Ã© menor que 0.05. Esta correlaÃ§Ã£o Ã© negativa.
summary(reg17)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para lstat Ã© menor que 0.05.Esta correlaÃ§Ã£o Ã© positiva
summary(reg18)
## HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre as variÃ¡veis, jÃ¡ que o p-valor
#para medv Ã© menor que 0.05.Esta correlaÃ§Ã£o Ã© negativa.


## b)
reg_completa = lm(crim ~ zn + indus + chas + nox + rm + age + 
                    dis + rad + tax + ptratio + black + lstat + medv, data=Boston)
summary(reg_completa)

# HÃ¡ uma correlaÃ§Ã£o estatisticamente vÃ¡lida entre crim e as variÃ¡veis
# zn, dis, rad, black, medv. Para essas variÃ¡veis podemos rejeitar a hipÃ³tese nula.

# c)

# Quando comparamos os dados de a e b observamos que algumas variÃ¡veis, quando,
# associadas a outras, perdem seu efeito. Se, em um primeiro momento, apenas a 
# variÃ¡vel chas nÃ£o apresentava correlaÃ§Ã£o significativa com a taxa de crimes, quan-
# do realizamos uma correlaÃ§Ã£o mÃºltipla, outras variÃ¡veis tambÃ©m deixam de possuir
# correlaÃ§Ã£o estatÃ�stica significativa com crim. Se a variÃ¡vel chas mantÃ©m sua nÃ£o correlaÃ§Ã£o, outras 
# variÃ¡veis, como indus, nox, rm, tax, ptratio e lstat, que antes aparentavam 
# possuir correlaÃ§Ã£o significativa, agora nÃ£o a apresentam em relaÃ§Ã£o a crim, jÃ¡ que
# seus p-valores sÃ£o maiores do que 0.05. 

# Segunda parte
x = c(coefficients(reg6)[2],
      coefficients(reg7)[2],
      coefficients(reg8)[2],
      coefficients(reg9)[2],
      coefficients(reg10)[2],
      coefficients(reg11)[2],
      coefficients(reg12)[2],
      coefficients(reg13)[2],
      coefficients(reg14)[2],
      coefficients(reg15)[2],
      coefficients(reg16)[2],
      coefficients(reg17)[2],
      coefficients(reg18)[2])
y = coefficients(reg_completa)[2:14]
plot(x, y)

# coeficiente (-10,0) no modelo unilinear e (30, -10) no modelo de regressÃ£o mÃºltipla.

# d) 
# ?



## FIM