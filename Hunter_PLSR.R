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

#Exemplo de PLSR - compilado dos sites: 
# http://nir-quimiometria.blogspot.com.br/2012/02/plotting-spectra-in-r.html
# 

install.packages('pls')
library('pls')
#### IMPORTANDO OS DADOS GASOLINE DO PACOTE PLS ####
data(gasoline, package="pls")
help(gasoline)
# Este conjunto de dados inclui quantidade de octane e o espectro infra-vermelho proximo de gasolina.
# agora vamos olhar melhor como estao apresentada estes dados
length(gasoline$octane)
dim(gasoline$NIR) #NIR tem 60 linhas e 401 colunas

#### VISUALIZANDO OS ESPECTROS UTILIZANDO MATPLOT ####
#podemos visualizar o espectro da gasolina usando matplot
help(matplot) 
# esta importante reparar que os dados de x e y para ser visualizado precisam ter o mesmo 
# numero de linhas

#aqui esta criado um vetor que especifica o comprimento das ondas medida na IR
wavelengths<-seq(900, 1700,by=2) 
length(wavelengths)

#para que wavelengths e gasoline$NIR tem o mesmo numero de linhas precisa transformar NIR
#usando a funcao t()
#para que nao demora demais, vamos visualizar os primeiros 10 amostras
#pode ver aqui que eu especifico o tipo de plot 'l', que seja de linhas, existem outros 
# especificacoes que podem ser vistas no help(plot)
matplot(wavelengths,t(gasoline$NIR)[,1:10],type='l', xlab="wavelengths(nm)",ylab="log(1/R)")

#### ANALISANDO OS ESPECTROS USANDO PLS ####
#Primeiro vamos investigar a funcao plsr
help(plsr)
#Componentes importantes do help:
# The formula argument should be a symbolic formula of the form response ~ terms
# Ou seja, o argumento do formula tem que usar a forma resposta ~ preditores
# Esta importante usar o formato (y ~ x, data = data) em vez de (data$y ~ data$x) para que 
# pode fazer previsao do valor modelado
# Tambem tem que especificar a maneira de validacao do modelo "CV" (Cross Validation) 
# ou "LOO" (Leave One Out / Deixa uma fora)

gas1 <- plsr(octane~NIR, ncomp = 10,data = gasoline, validation = 'LOO')

summary(gas1)

#vamos fazer grafico do RMSEP (Raiz Quadrado do Medio de Erro Padrao de Predicao) para 
#decidir o numero de componentes necessario

plot(RMSEP(gas1),legendpos='topright') #com legenda no conto superior direito

#depois de tres componentes nao esta explicada mais nada (quase) de variacao
#entao, vamos usar um modelo com tres componentes

#agora vamos olhar a validacao baseado nos tres componentes
plot(gas1, ncomp=3, asp=1, line=T)

#### OLHANDO A DIFERENCA COM COMPONENTES PRINCIPAIS ####
help(prcomp)
pca = prcomp(~NIR, data = gasoline)

#quanto varianca esta explicado pelo N componentes principais
summary(pca)
varexp = pca$sdev^2/sum(pca$sdev^2)
varexp.tot = cumsum(varexp)

plot(1:60,varexp.tot,type='l',main='Varianca Cumulativa Explicada',xlab='ncomp',ylab='Varianca Explicada Cumulativa')
#olha so os primeiros 10 componentes principais
plot(1:10,varexp.tot[1:10],type='l',main='Varianca Cumulativa Explicada',xlab='ncomp',ylab='Varianca Explicada Cumulativa')
abline(h=0.9)
varexp.tot[1:4]
#os primeiros tres componentes principais explicam mais do que 90% da varianca

#para uma visualizacao bem bonito 
#library(devtools)
#install_github("ggbiplot","vqv")
library('ggbiplot')

help(ggbiplot)
#nao esta muito bonito para estes dados. nao consegui visualizar so os primeiros tres PCs...
g = ggbiplot(pca, ellipse=T, circle=T)
print(g)

#### USANDO OS DADOS DE VALDEIR ####

library('pls')
setwd('~/Documents/RCourses/')
cal = read.table('Valdeir_Cal.csv', header=T, sep=',')
val = read.table('Valdeir_Val.csv', header=T, sep=',')

#lembrando que precisei passar variaveis separadas para a funcao PLSR vamos organizar os dados
#resposta para calibracao
C_cal = cal$Ca..mmolc.dm3. #Calcium nivel para calibracao
VN_cal = cal[,3:386]       #VisNIR para calibracao
C_val = val$Ca             #Calcium nivel para validacao
VN_val = val[,2:385]       #VisNIR para validacao

# #para fazer cross-validation, precisa estar todos os dados em uma tabela so
# VisNIR = merge(VN_cal, VN_val, all=T)
# VisNIRm = as.matrix(VisNIR)
# Calcium = c(C_cal, C_val)
# train = c(rep(T, length(C_cal)), rep(F, length(C_val)))

#o comprimento de onda do espectro esta incluido no nome das colunas
# MAS, para conseguir visualizar, esta informacao precisa ser num vetor num formato numerico
# na proxima comanda eu utiliza gsub para substituir conformando num modelo de letras, 
# numeros ou simbolos. Neste caso, eu pedi para a funcao procurar "X" e substituir nada 
# para o vetor de nomes das colunas. Com este comando, volta so o comprimento da onda medida
# mas num formato de carater. A funcao as.numeric() transforma para numero.
wavelengths = as.numeric(gsub("X","",names(VN_val)))

matplot(wavelengths, t(VN_cal[2:11,]), type='l')

#plot dos espectros de validacao (vermelho) e calibracao (preto)
matplot(wavelengths, t(VN_cal), type='l', col='red')
matplot(wavelengths, t(VN_val), type='l', col='black', add=T)

#olhando o plot dos dados de validacao, da pra ver que tem um espectro que esta errado
#vamos achar qual seja
which(VN_val[,100]> 2) #returns line 110
VN_val = VN_val[-110,] #assim redefini VN_val para incluir todas as linhas menos 110
C_val = C_val[-110]

#vamos olhar o plot dos dados de novo para confirmar que tirei a linha certa
matplot(wavelengths, t(VN_val), type='l', col='black')
matplot(wavelengths, t(VN_cal), type='l', col='red', add=T)

#vamos focar na regiao menor
matplot(wavelengths, t(VN_val), type='l', col='black',ylim=c(0,3))
matplot(wavelengths, t(VN_cal), type='l', col='red', add=T)


#agora vamos criar duas listas, uma com as informacoes para calibrar o modelo (train)
#e outra com as informacoes para validar modelo (test)
Valdeir_Train = list(VisNIR = VN_cal, Calcium = C_cal)
Valdeir_Test = list(VisNIR = VN_val, Calcium = C_val)

#PLSR dos dados de Treinamento (calibracao) com validacao estilo deixa um fora
pls_C = plsr(Calcium~VisNIR, data = Valdeir_Train, scale=T, validation="LOO")

## OPA! Deu erro -- pq o tipo de dados seja invalido para visNIR, precisa ser um matriz!
Valdeir_Train = list(VisNIR = as.matrix(VN_cal), Calcium = C_cal)
Valdeir_Test = list(VisNIR = as.matrix(VN_val), Calcium = C_val)

## Tentando rodar pls de novo da mesma forma
pls_C = plsr(Calcium~VisNIR, data = Valdeir_Train, validation="LOO")

#Primeiro, vamos olhar o sumario do modelo
summary(pls_C)
#e um grafico que mostra o error quadrado medio de predicao (lwmbrando que isso esta baseado
# somente nos dados de treinamento -- ou seja a predicao e para cada ponto baseado no modelo
# que inclui todos os outros pontos)
plot(RMSEP(pls_C), legendpos = "bottomright", ylab='RMSEP', xlab = 'ncomp', type='l')

#se for fazer igual que fizemos no exemplo anterior, pudia ver onde que minimiza RMSEP para
#estimar o numero de componentes sugeridas para usar no modelo final
#MAS -- os dados de treinamento nao apresentam a tendencia esperada
minComp = which(pls_C$validation$PRESS==min(pls_C$validation$PRESS))
minComp
#sugere usar 10 componentes

#uma outra forma de definir o numero de componentes esta por meio de variacao explicado
# plot(explvar(pls_C), legendpos = 'bottomright', ylab='Varianca explicada', xlab = 
#        'ncomp', type='l')
# #e a varianca explicada em termos cumulativos
# plot(cumsum(explvar(pls_C)), ylab = 'Varianca explicada cumulativa', xlab = 'ncomp', type='l')
# abline(v=2, col='red')
# text(2,80, '2 Componentes explicam mais do que 90% da covarianca', pos=4)
# text(2,75, 'Mais do que 4 componentes nao faz diferenca', pos=4)

#Vamos comparar com os dados de validacao 
#para ver o medio erro quadrado dos dados de validacao 
Test_Res = RMSEP(pls_C, newdata = Valdeir_Test)
#vamos repitir o grafico de antes, mas agora colocando o RMSEP de validacao junto
plot(RMSEP(pls_C), ylab='RMSEP', xlab = 'ncomp', type='l',ylim=c(8,50))
lines(Test_Res$comps, Test_Res$val, col='red', lwd=2)
legend('bottomright', c('Calibracao CV','Calibracao adjCV','Validacao'),lty=c(1,2,1),
       lwd=c(1,1,2), col=c('black','red','red'))

#Onde que o minimo de RMSEP ocorre para os dados de validacao?
which(Test_Res$val==min(Test_Res$val))
#neste caso o minimo de RMSEP_Val ocorre com 3 parametros

# com a presuposicao que 3 componentes seja melhor para nossos dados
pls_Cv =  predict(pls_C, ncomp = 3, newdata = Valdeir_Test)

#outros plots de validacao
plot(pls_C,ncomp=3, asp=1, line=T) #realmente, esta pessima
plot(pls_C, plottype = "scores", comps = 1:3) #scores de cada componente
plot(pls_C, "loadings", comps = 1:3, legendpos = "topright") #ver os loadings
abline(h = 0)

#o que que veja -- a parte do espectro mais representada no modelo de calcium esta
#a ultravioleta
