# Curso de R
# Hunter, Maria O
# Departamento de Agricultura Tropical
# Universidade Federal de Mato Grosso - Cuiaba
# hunter.maria (at) gmail.com

# Baseado no curso de:
# Gorgens, Eric Bastos
# Departamento de Engenharia Florestal
# Universidade Federal dos Vales do Jequitinhonha e Mucuri - UFVJM
# Diamantina, Brazil
# eric.gorgens (at) ufvjm.edu.br
#
#------------------------------------------------------------------------------------------
#### REGRESSAO LINEAR ####

# Neste arquivo temos o peso de serrapilheira e braquiaria (dividido em total e aéreo) e abertura de dossel.
# Deseja-se investigar se o nivel de abertura de dossel influencia no desenvolvimento da braquiaria.

#exemplo de regressao linear e polynomial
#https://davetang.org/muse/2013/05/09/on-curve-fitting/

#criar um conjunto de dados
x <- c(32,64,96,118,126,144,152.5,158)
y <- c(99.5,104.8,108.5,100,86,64,35.3,15)

#y vai ser a variavel de resposta e x de predicao
#vamos mostrar os dados (com ponto solido pch=19)
plot(x,y,pch=19)

####faz regressao linear ####
fit  <- lm(y~x)
#### regressoes polinomiais ####
# polynomial de 2 graus
# a funcao poly precisa de um vetor numerico ou matriz e o grau de polynomial
#   'raw' e opcional e significa se o residuo esta baseado na distancia no eixo y ou se
#     for a distancia ortogonal (usando x e y)
fit2 <- lm(y~poly(x,2,raw=TRUE))
#polynomial de 3 graus
fit3 <- lm(y~poly(x,3,raw=TRUE))
#polynomial de 4 graus
fit4 <- lm(y~poly(x,4,raw=TRUE))


# para mostrar os modelos cria um conjunto de dados que varia entre 30 e 160 usando seq()
# lembra que na aula previa usamos a mesma funcao, mas com opcao by (por/ a cada), aqui
# usa opcao length, que especifica o comprimento do vector resultante
xx <- seq(30,160, length=50) 

plot(x,y,pch=19,ylim=c(0,150))
# as seguintes linhas fazem duas coisas numa linha so:
# 1. calcula valores modelados usando funcao predict()
#     anote que tinha que colocar os dados xx nesta funcao como data.frame com a mesma nome 
#     para a coluna 'x' que tinha quandro formou o modelo original
# 2. mostra o valor modelado no grafico como linha usando a funcao lines()
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")


#### Resultados do ajuste da regressão linear ####
summary(fit)
#olhando os coefficientes (Coefficients) que o multiplo de Abertura esta significante (***)
#mas que o intercepto nao esta significante (p = 0.0527)
#para ver a tabela com resultados da anova podemos usar a funcao anova()
anova(fit)
#quando peco o plot da regressao tem varios resultados
plot(fit)
#primeiro: os residuais como funcao do valor estimado. os pontos com numeros estao outliers
#segundo:  o quantile-quantile com regressao
#terceiro: o raiz do medio de erro quadrado como funcao de valor estimado com linha de tendencia
#quarto:   residual como funcao de alavancagem/influencia
  #neste case tem tres pontos que tem alto importancia na criacao do modelo linear que
  #tambem tem residuais altas. a funcao esta mais ou menos bom no final porque estes tres
  #tem efeitos que se equilibram.

summary(fit2)
summary(fit3)
summary(fit4)
#pode comparar os modelos usando anova, que acerta o grau de liberdade
anova(fit,fit2,fit3,fit4)

#fit 2 e o melhor modelo, mas voltando para ver este modelo da para ver que o intercept
# nao esta significativa
summary(fit2)
fit2b <- lm(y~poly(x,2,raw=TRUE)-1)

#agora usando anova para comparar todos os modelos
anova(fit, fit2, fit2b, fit3, fit4) #fit 2 continua melhor

#agora vamos visualizar nosso resultado
plot(x,y,pch=19,ylim=c(0,150))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lty=2)
lines(xx, predict(fit2, data.frame(x=xx)), col="green",lty=1, lwd=2)
lines(xx, predict(fit2b, data.frame(x=xx)),col="green", lty=2)
lines(xx, predict(fit3, data.frame(x=xx)), col="blue", lty=2)
lines(xx, predict(fit4, data.frame(x=xx)), col="purple", lty=2)
legend('bottomleft',c('linear','poly 2 ***','poly2 - 1 *', 'poly 3 *','poly 4'),
       col=c('red','green','blue','purple'), lty=c(2,1,2,2), lwd=c(1,2,1,1))
text(60,130,expression(paste('y = 42.51 + 2.02x - 0.013',x^2)),pos=4)

#### REGRESSAO MULTI-VARIADA ####
# Example: Book Weight (Taken from Maindonald, 2007)

install.packages('DAAG')
library(DAAG)
data(allbacks)

#exploracao do conjunto de dados
names(allbacks)

#visualizar todos as variaveis no conjunto de dados
plot(allbacks, gap=0)

#um modelo muti-variada inclui mais do que um fator: vamos comecar com todos
multi.lm = lm(weight ~ volume + area + cover, data = allbacks)

#e agora tirar o tipo de capa (cover)
multi.lm = lm(weight~ volume + area, data = allbacks)

#o summario deste modelo
summary(multi.lm)

#aqui eu crio uma tabela com os dados originais, mas adiciona os valores modelados, e os
# residuais
allbacks2 = data.frame(allbacks, peso.mod = fitted(multi.lm) , residual=resid(multi.lm))
#visualizando o resultado do modelo
plot(weight~peso.mod, data = allbacks2)
abline(a=0,b=1)

#e o anova do modelo
anova(multi.lm)

#### REGRESSAO NAO-LINEAR ####
# Trabalharemos com uma base de ANATRO de diferentes especies

# Importando os dados
nl = read.csv("dados/NonLinear.csv", sep=",", header=TRUE)
head(nl)

xmod = seq(1,8)
# Criando um gráfico com todos os dados
plot(diametro~idade, data = nl, main = "Geral", xlab = "Idade (anos)", ylab = "Diâmetro (cm)")

#Vamos testar tres modelos que podem descrever estes dados
#### log - linear ####
logln = lm(diametro~log(idade), data=nl)
d_log = predict(logln,data.frame(idade=xmod))
lines(xmod,d_log)

#### SSasymp: modelo asymptotico com iniciacao automatica  ####
modA = nls(diametro ~ SSasymp(idade,Asym,R0,lrc), data=nl)
d_Asy = predict(modA, data.frame(idade=xmod))
lines(xmod,d_Asy, lty=2)

#### modelo nao-linear de Richards / Weibull ####
#este modelo nao tem um metodo para comecar sozinha (self start), 
# entao precisa passar valores estimadas para os parametros

# Define chute inicial para o modelo não linear de Richards
chute_a = max(nonlinear$diametro)

# Ajusta modelo não linear de Richards.
modR = nls(diametro~a*(1-exp(-b*idade))^c,data=nonlinear, start=list(a=chute_a, b=0.05, c=1.0))
d_Ric = predict(modR, data.frame(idade=xmod))
lines(xmod,d_Ric,lty=3)

# Análise de variância da regressoes
summary(logln)
summary(modA)
summary(modR)

AIC(logln,modA,modR)


#### FOR / WHILE LOOPS ####

#### WHILE / ENQUANTO ####
# enquanto uma condicao esta verdade, execute os comandos entre {}
val = 0
while (val < 5){
  print(val)
  val = val + 1
}

#### FOR / PARA ####
#voltando para o conjunto de dados que usou para regressao nao-linear
summary(nl)
head(nl)
#valores unicos da coluna progenie
unique(nl$progenie)

#salvar estes valores para criar modelos para cada progenie separadamente
nl_pro = unique(nl$progenie)

#criar grafico separado para cada progenie
for (i in nl_pro) {
  temp = subset(nl, progenie==i)
  plot(temp$idade, temp$diametro, main = paste("Progenie = ",i), xlab = "Idade (anos)", ylab = "Diametro (cm)")
}

#criar uma grafico com todos, diferenciando cor e tipo de ponto
# para o primeiro valor em nl_pro tem que criar grafico (i==4)
# para os seguintes valores tem que adicionar ao grafico
#  como tem dois casos, eu usei uma condicional "if... else" "se... nao"
#  anota que qualquer loop em R usa o mesmo formatacao {}
for (i in nl_pro) {
  temp = subset(nl, progenie==i)
  if (i==4) {
    plot(temp$idade, temp$diametro, xlab='Idade (anos)', ylab = 'Diametro (cm)',
         xlim=c(1,8),ylim=c(0,25), col=i)
  } else {
    points(temp$idade, temp$diametro, col=i)
  }
}

#uma maneira mais simples para fazer uma grafico parecida com aquele de cima seria...
plot(nl$idade, nl$diametro, xlab='Idade (anos)', ylab='Diametro (cm)', col=nl$progenie)

#### CRIANDO FUNCOES ####
# todos os comandos que agente usa sao funcoes. 
# mas tambem podemos criar nossas proprias funcoes por qq caso
# vamos crair uma que estima o diametro usando o funcao nao-linear Asymptotico
#   (igual a funcao predict() que usei mais cedo)

AIC(logln, modA, modR) #de linha 170, entre essas modA tinha o AIC menor

#lembrando as comandas que usamos encima
#### SSasymp: modelo asymptotico com iniciacao automatica  ####
modA = nls(diametro ~ SSasymp(idade,Asym,R0,lrc), data=nl)
d_Asy = predict(modA, data.frame(idade=xmod))

help(SSaymp) #como e a funcao asymptotica
             # tem a forma Asym + (R0 - Asym)*exp(-exp(lrc)*input)

#vamos criar uma funcao que pode ser usado na comanda curve() para mostrar variabilidade
# de modelo. o funcao curve precisa entrada que esta em funcao de x
val_A = function(x) {
  Asym = coefA[1]; R0 = coefA[2]; lrc = coefA[3]
  Asym + (R0 - Asym)*exp(-exp(lrc)*x)
}

#usando nosso loops
for (i in nl_pro) {
  temp = subset(nl, progenie==i)
  modA = nls(diametro ~ SSasymp(idade, Asym, R0,lrc), data = temp)
  coefA = coef(modA)
  if (i==nl_pro[1]) {
    plot(temp$idade, temp$diametro, xlab='Idade (anos)', ylab = 'Diametro (cm)',
         xlim=c(1,8),ylim=c(0,25), col=i)
    curve(val_A,from = 1, to = 8, col=i, add=T)
  } else {
    points(temp$idade, temp$diametro, col=i)
    curve(val_A,from = 1, to = 8, col=i, add=T)
  }
}



