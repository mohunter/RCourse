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

#### EXPLORACAO DOS DADOS: PACOTE GRAFICO NATIVO AO R ####
# Fonte do exemplo: http://ecologia.ib.usp.br/bie5782/doku.php?id=bie5782:03_apostila:05a-graficos)

# Criando data frame com os dados
df1 = data.frame(riqueza = c(15,18,22,24,25,30,31,34,37,39,41,45),
                 area = c(2,4.5,6,10,30,34,50,56,60,77.5,80,85),
                 categoria = rep(c("pequeno", "grande"), each=6))

head(df1) # mostra o cabecalho e as primeiras linhas do arquivo

# Grafico de dispersao
?plot

plot(riqueza~area, data = df1)
plot(df1$area, df1$riqueza) # escrita alternativa

# Especificando outros parâmetros: nome do eixo x (xlab), nome do eixo y (ylab),
  # valores minimas e maximas do eixo x (xlim), titulo principal (main)

plot(riqueza~area, data = df1, xlab="Area (m²)", ylab="Riqueza (# espécies)",
     xlim=c(0,90), ylim=c(0,90), main="Riqueza = f(area)")

# Adicionando uma linha de tendência linear
abline(lm(riqueza~area, data = df1))
#aqui criei um modelo linear (lm) entre os dois variaveis e mostrei com a funcao abline()

# Criando um boxplot
# neste case 'categoria' esta um fator
boxplot(riqueza~categoria, data = df1)

# Criando um grafico de barras
barplot(df1$riqueza) #anota que tem 12 barras, pq tem 12 pontos, eles estao ordenados

# Criando um histograma
hist(df1$riqueza)

#### EXPLORACAO DOS DADOS: PACOTE GGPLOT ####

library(ggplot2)

# Para este exemplo criaremos um data frame ficticio
set.seed(22136) # fixa o gerador de numeros aleatorio para que os resultados sejam iguais para todos

df <- rbind(data.frame(group='A', tempo=rnorm(n=200, mean=10, sd=3), peso=rnorm(n=200, mean=100, sd=20)),
            data.frame(group='B', tempo=rnorm(n=200, mean=10, sd=3), peso=rnorm(n=200, mean=105, sd=20)))

#rbind combina por linhas. Neste caso combina a primeira dataframe de grupo A, com a segunda de grupo B
#rnorm cria um conjunto de dados aleatorias em volta de uma media definida, com deviacao padrao definida


head(df) # exibe primeiras linhas do data frame


#Criando um gráfico de dispersão do tempo em função do peso
ggplot(df, aes(tempo, peso)) + geom_point()
#aqui o primeiro parte do funcao defini qual seja o conjunto de dados (df) e os eixos que vao usar
# (definido por meio de aes - "aesthetic")
# depois estao adicionado os pontos ("geometria - pontos" = geom_point)

#Criando um gráfico de dispersão do peso em função do tempo
ggplot(df, aes(peso, tempo)) + geom_point()
#aqui e tudo igual menos a ordem dos eixos

# criando um boxplot
ggplot(df, aes(group, peso)) + geom_boxplot()
#aqui 'grupo' e um fator

# criando um histograma
ggplot(df, aes(peso)) + geom_histogram(binwidth=15)

# criando um histograma para cada grupo e organiza numa mesma janela
ggplot(df, aes(peso)) + geom_histogram(binwidth=10) + facet_wrap(~group, nrow=2, ncol=1)

# criando um histograma para cada grupo num mesmo gráfico, mas com cores diferentes
ggplot(df, aes(peso, color = group)) + geom_histogram(binwidth=10)

#### VOLTANDO PARA O PACOTE BASICO ####
# se quiser mostrar varias graficos numa painel so, tambem pode fazer no pacote basico
# por meio do par(mfrow=c(2,2)) onde que define o numero de colunas e linhas
par(mfrow=c(2,1))
hist(df[df$group=='A','peso'])
hist(df[df$group=='B','peso'])

hist(df[df$group=='A','peso'], main=NA, xlab = 'Group A')
hist(df[df$group=='B','peso'], main=NA, xlab= 'Group B')

#importante lembrar de voltar a janela de grafico com os parametros originais depois que termina
par(mfrow=c(1,1))

#### CORRELACAO E COVARIANCA ####

data(mtcars)
names(mtcars)
dim(mtcars)
head(mtcars)

# Correlacoes e covariancia
cor(mtcars, method="spearman")
cor(mtcars, method="pearson")
cov(mtcars)

#Correlograma

install.packages('corrgram')
library(corrgram) # ativa pacote

# install.packages("corrgram") # Comando para instalar o pacote
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pts, text.panel=panel.txt,
          main="Correlograma")

#outras opcoes incluem panel.conf (para confianca), panel.pts (dispersao), panel.density,
#panel.bar, panel.ellipse, panel.minmax, col.regions, panel.pie
corrgram(mtcars, order=T, lower.panel = panel.shade, 
         upper.panel = panel.conf, diag.panel = panel.density)

#### ESTATISTICAS DESCRITIVAS ####

data(mtcars)

# Estatisticas basicas
mean(mtcars$mpg)                            # media
var(mtcars$mpg)                             # variancia
sd(mtcars$mpg)                              # desvio padrao
quantile(mtcars$mpg, c(0.25, 0.50, 0.75))   # quantile
max(mtcars$mpg)                             # maximo
min(mtcars$mpg)                             # minimo
median(mtcars$mpg)                          # mediana
IQR(mtcars$mpg)                             # distancia intequartil


# Veja agora a função summary
summary(mtcars)

#### UTILIZANDO APPLY SIMILPLIFICADO : SAPPLY ####
sapply(mtcars, mean)
sapply(mtcars, min)
sapply(mtcars, max)
sapply(mtcars, var)
sapply(mtcars, sd)

#### TESTES DE PRESUPOSICOES ESTATISTICAS ####

# Importando base
df = read.csv("lidar.csv", header=TRUE, sep="")
names(df)

#-------------------------------------------------------------
# Normalidade

# metodo grafico
qqnorm(df$Volume) #comparando Volume com distribuicao da varianca normal
qqline(df$Volume) #addicione uma linha representando normalidade perfeita dos dados

#teste de Shapiro -- p < 0.1 apresenta normalidade
shapiro.test(df$Volume)

#teste de Lilliefors
require(nortest)
lillie.test(df$Volume) #compara os dados com distribuicao normal via K-S test


#-------------------------------------------------------------
# Homogeneidade de variancia

# Teste de Breusch-Pagan-Godfrey
require(lmtest)
bptest(Volume ~ Height, data=df) #p-val > 0.05 HETEROSKEDASTICIDADE NAO SIGNIFICATIVA

#### TESTES DOS HIPOTESES ####

#### TESTE QUI-QUADRADO ####

# comparando tempo de florescimento de especies nativas e exoticas

df <- read.csv("flowering_alien_vs_indigen.csv", sep = ";") # o separador pode mudar de computador para computador
head(df)
names(df)

# explorando a distribuição graficamente.
require(ggplot2)
ggplot(df, aes(Flowering)) + geom_histogram() + facet_wrap(~Status, nrow = 2, ncol = 1)

# e pelo teste Chi-quadrado.
m <- table(df$Status, df$Flowering)

(Xsq <- chisq.test(m)) # Resumo do teste
Xsq$observed # Valores observados (o mesmo que matriz m)
Xsq$expected # Valores esperados segundo hipótese nula
Xsq$residuals # Resíduos de Pearson
Xsq$stdres # Resíduos padronizados

#### TESTE KOLMOGOROV-SMIRNOV ####
# explorando a distribuição graficamente.
require(ggplot2)
ggplot(df, aes(Flowering)) + geom_histogram() + facet_wrap(~Status, nrow = 2, ncol = 1)


# usando o KS-test,
ks.test(df$Flowering[df$Status == "indigen"],
        df$Flowering[df$Status == "Neophyt"])


#### F TESTE ####
# Vamos utilizar dados gerados aleatorios
x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)
var.test(x, y)                  # Tem a mesma varianca?

#### TESTE T ####

data(sleep)
summary(sleep)

t.test(extra ~ group, data = sleep)

