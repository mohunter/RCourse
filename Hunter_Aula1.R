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

### ------------------------------- TIPOS DE OBJETOS -------------------------------------- 

####   Vetores  ####

a <- 2

a <-  c(1, 2, 3)

idade <- c(2, 4, 3)
experiencia <- c(1, 5, 7)


length(idade)
length(experiencia)

dados <- c(2, 4, 3, 1, 5, 7)

### Matrizes ####

matrix(dados, nrow = 2, ncol = 3, byrow = FALSE)

m1 <- matrix(dados, nrow = 2, ncol = 3, byrow = FALSE)

dim(m1)

m1[1,1]
m1[1,]
m1[,2]
m1[2, 1:3]

### Data frame ####

mtcars

# Retorna os nomes das variaveis (colunas)
names(mtcars)

# Resumo estatistico das variaveis (colunas)
summary(mtcars)

# Retorna as primeiras linhas do dataframe
head(mtcars)

# Retorna as ultimas linhas do dataframe
tail(mtcars)

# Retorna a dimensao do dataframe
dim(mtcars)

# filtra o dataframe
subset(mtcars, mtcars$cyl == 4) # exemplo 1

subset(mtcars, mtcars$cyl == 4, select = c("mpg", "hp")) # exemplo 2

### Listas ####

lista = list(mtcars, a, idade)

lista[1]

lista[2]

lista[3]


# Exibe quais sao os objetos carregados na memoria
ls()

###----------------------------- OPERADORES MATEMATICOS -------------------------------------

### Adicao ####

# Duas vetores
2 + 2

a = 2
b = 3
a+b

# Matrizes
m1

#com vetor de 1 elemento
m1 - 2

#com vetores 
m1 - c(2,3)         #by row
m1 - c(2,3,4)       #by column?
m1 - rep(c(2,3,4),each=2) #repete o vetor de varias maneiras
m1 - c(2,3,4,5)     #error

#olha o comprimento do m1
length(m1)
#criar um vetor com o mesmo comprimento do m1 mas sem dimensoes
m1 - c(2,3,4,5,6,7) #

# e agora um matriz com os mesmos dimensoes do m1
m2 = matrix(c(1,1,2,2,3,3), nrow=2, ncol=3, byrow = F)

m1 - m2

# Data Frame

head(mtcars)
colnames(mtcars)

#mpg e o economia de carro
mtcars$mpg * 2

#quantos elementos mtcars$mpg tem?
length(mtcars$mpg)          #32

mtcars$mpg * c(1,2)

#e se for com duas colunas de mtcars
mtcars[,2:3]*c(1,2)

# Lista

lista*2

lista[[1]]^2
lista[[2]]^c(1,2,3)

### ------------------------------- OPERADORES LOGIGOS ----------------------------------

m1
m1>2
m1>=2
m1<2
m1<=2

m1>2 & m1>3
m1>2 & m2>1

#especialmente util para mexer com planilhas
mtcars[mtcars$cyl==4 & mtcars$hp>100,]

### ------------------------------- IMPORTANDO PLANILHAS ---------------------------------

# Comando simplificado
df <- read.csv("flowering_alien_vs_indigen.csv") 

# comando incluindo separador de colunas
df <- read.csv("flowering_alien_vs_indigen.csv", sep = ";")

# comando incluindo separador de colunas e separador decimal
df <- read.csv("flowering_alien_vs_indigen.csv", sep = ";", dec = ".")

# comando incluindo separador de colunas, separador decimal e presença de cabeçalho
df <- read.csv("flowering_alien_vs_indigen.csv", sep = ";", dec = ".", head = TRUE)

# comando incluindo endereço completo
df <- read.csv("C:/Users/Gorgens/Documents/R/udemy R/flowering_alien_vs_indigen.csv", sep = ";")
df = read.csv('~/Documents/RCourses/flowering_alien_vs_indigen.csv', sep=';')


# Inspecionar os dados importador

dim(df)
head(df)
names(df)
summary(df)

unique(df$Status)

write.table(df[,1:4],file = '~/Documents/RCourses/flowering_plants.csv', sep=',')
df2 = read.table('~/Documents/RCourses/flowering_plants.csv', sep=',')


