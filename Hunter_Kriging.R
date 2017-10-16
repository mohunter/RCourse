# Curso de R
# Hunter, Maria O
# Departamento de Agricultura Tropical
# Universidade Federal de Mato Grosso - Cuiaba
# hunter.maria (at) gmail.com
#
#------------------------------------------------------------------------------------------
####         TUTORIAL DE KRIGING             ####
# original no site: http://rspatial.org/analysis/rst/4-interpolation.html

#### PACOTES NECESSARIOS ####
#vai precisar installar os seguintes pacotes para rodar este tutorial
install.packages('raster') #pacote para mexer com rasters
install.packages('sp') #biblioteca espacial
install.packages('rgdal') #para transformar coordenados de referencia
install.packages('gstat') #estatisticas geospaciais


#### PREPARACAO DOS DADOS ####
# abaixa o arquivo airqual.csv para interpolar niveis de ozone para
# California. Usando o variavel 'OZDLYAV' (Ozone Daily Average / Media
# Diaria de Ozonio) com unidade de particulas por billiao.

# Ler o arquivo original

x = read.csv('dados/airqual.csv')
x$OZDLYAV = x$OZDLYAV * 1000


#deixa diponivel o biblioteca espacial
library(sp)

#criar um data.frame de pontos espaciais
coordinates(x) = ~LONGITUDE + LATITUDE

#defini o projecao espacial CRS = "Coordinate Reference System"
proj4string(x) = CRS('+proj=longlat + datum=NAD83')

#definir o projecao que vai querer para este conjunto de dados "Teale Albers"
TA = CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 
         +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")

#disponibilizar o pacote rgdal
library(rgdal)

#transformar os coordenados para sistema Teale Albers
aq <- spTransform(x, TA)

#cria uma maca tipo raster para interpolar os dados de ozonio
# primeiro ler os dados de municipios de california
cageo = readRDS('dados/counties.rds')

#transformar o sistema espacial para Teale Albers para ser consistente 
#com os dados de ozonio
ca = spTransform(cageo, TA)

#visualiza a mapa de referencia e os pontos com informacao de ozonio
plot(ca)
points(aq,pch=19)

#disponibilizar pacote raster
library(raster)
#transformar numa raster
r = raster(ca)

#definir o resolucao do raster para 10 km
res(r) = 10

#mudar o raster de municipios de CA para um tipo malha espacial
g = as(r,'SpatialGrid')

#### CRIANDO VARIOGRAMA ####
library(gstat)

#definir uma formula que so tem intercept e so usa os dados de ozonio
gs = gstat(formula=OZDLYAV~1, location=aq)

#passa este conjunto para criar uma variograma empirica
v = variogram(gs, width=20)

#vamos olhar os resultados do variograma
head(v)
plot(v$dist,v$gamma, xlab='distance',ylab='semivarianca')

#agora vamos estimar a semivarianca (variograma modelado)
# a funcao vgm genera um modelo de variograma baseado no sill (85)
# tipo de modelo (Exponencial), range=75, and kappa (smoothness)=20
fve = fit.variogram(v, vgm(85, "Exp", 75, 20))
fve

#mostrando a linha do modelo de variograma com os retornos do variograma
# original
plot(variogramLine(fve,400), type='l', ylim=c(0,120))
points(v[,2:3], pch=20, col='red')

#vamos testar um outro tipo de modelo para a variograma (Spherico)
fvs = fit.variogram(v, vgm(85, "Sph", 75, 20))
fvs

#e de novo mostrando o modelo com os pontos da variograma original
plot(variogramLine(fvs, 400), type='l', ylim=c(0,120) ,col='blue', lwd=2)
points(v[,2:3], pch=20, col='red')

#os dois modelos parecem boms, mas eu prefiro o exponencial...
#entao, vamos usar o modelo fve para interpolar valores 
#agora especificando o modelo para gstat
k = gstat(formula=OZDLYAV~1, locations=aq, model=fve)

# e colocando os valores deste modelo nos lugares da malha espacial
# definido laaaa na linha 66
kp = predict(k, g)
summary(kp)
#tem dois atributos, valore modelados e a varianca

#e a visualizacao do resultado
spplot(kp)

# para ficar mais bonito, olha so onde os dados estao coincidentes com CA
# passa o resultado do kriging para um brick (raster stack menos flexivel)
ok <- brick(kp)
#usa a mapa de municipios como maquette
ok <- mask(ok, ca)
names(ok) <- c('prediction', 'variance')
plot(ok)
