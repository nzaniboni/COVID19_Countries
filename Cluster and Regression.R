
# Diretório do banco de dados 
setwd("C:\\Users\\Natalia\\Documents\\Artigos\\Coronavirus\\Encontro de finanças 2020") 

# Importar a base de dados #
library(readxl)
base <- read_excel("Base de dados.xlsx", sheet ="COVID-19") 
names(base) #ver a lista de variáveis

#ativar as bibliotecas
library(cluster)
library(dplyr)

#remove a primeira coluna
base <- base[,-1]
head(base)

summary(base)

library(Amelia)
missmap(base)


#substituir missing por zero
base$`Num_de_casos/1M`[is.na(base$`Num_de_casos/1M`)] <- 0
base$`Num_de_mortes/1M`[is.na(base$`Num_de_mortes/1M`)] <- 0
base$`Num_de_testes/1M`[is.na(base$`Num_de_testes/1M`)] <- 0
base$Export[is.na(base$Export)] <- 0
base$GDP_growth[is.na(base$GDP_growth)] <- 0
base$Import[is.na(base$Import)] <- 0

summary(base$Unemployment)
base$Unemployment[is.na(base$Unemployment)] <- 5.692

#drop Manufacturing, value added (% of GDP) - 41 missings
#drop New businesses registered (number) - 67 missings
#drop Stocks traded, total value (% of GDP) - 138 missings
round(42/191,2)

library(dplyr)
# Drop the columns of the dataframe
base2<- select(base,-c(Manufacturing,New_businesses,Stocks,GDP))

summary(base2)

#padronizar os dados 
base.padronizado <- scale(base2[,1:3]) 
head(base.padronizado) 

###################################################################
#K-Médias
###################################################################

library(cluster)    # Algoritmos de cluster
library(factoextra) #Visualização dos dados
library(gridExtra)

# método das k médias - com k = 2
set.seed(5) #Atribuir a semente para deixar o processo aleatório igual, todas as vezes que processarmos o programa

#Agora rodar de 3 a 5 centros  e visualizar qual a melhor divisão
base.k2 <- kmeans(base.padronizado, centers = 2, nstart=25, iter.max = 100) #Centers sao os k grupos e inter.max é o máximo de interações
base.k3 <- kmeans(base.padronizado, centers = 3, nstart=25, iter.max = 100)
base.k4 <- kmeans(base.padronizado, centers = 4, nstart=25, iter.max = 100)
base.k5 <- kmeans(base.padronizado, centers = 5, nstart=25, iter.max = 100)

#Gráficos
G1 <- fviz_cluster(base.k2, geom = "point", data = base.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(base.k3, geom = "point",  data = base.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(base.k4, geom = "point",  data = base.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(base.k5, geom = "point",  data = base.padronizado) + ggtitle("k = 5")

#Criar uma matriz com 4 gráficos
grid.arrange(G1, G2, G3, G4, nrow = 2)

#Foi considerado 4 grupos
base4grupos <- read_excel("Base de dados.xlsx", sheet ="COVID-19") 
baseFinal<- data.frame(base4grupos, base.k4$cluster) #Colocar o numero do grupo com a base original e salvar na tabela PokemonFinal

# numero de observações em cada grupo
table(base.k4$cluster)

grupo1<-subset(baseFinal,base.k4.cluster==1)
View(grupo1)
grupo2<-subset(baseFinal,base.k4.cluster==2)
View(grupo2)
grupo3<-subset(baseFinal,base.k4.cluster==3)
View(grupo3)
grupo4<-subset(baseFinal,base.k4.cluster==4)
View(grupo4)

par(mfrow=c(1,3)) 
boxplot(baseFinal$Num_de_casos.1M ~ as.factor(baseFinal$base.k4.cluster), col="grey",main="Casos / Milhão de hab.")
boxplot(baseFinal$Num_de_testes.1M ~ as.factor(baseFinal$base.k4.cluster), col="grey",main="Testes / Milhão de hab.")
boxplot(baseFinal$Num_de_mortes.1M ~ as.factor(baseFinal$base.k4.cluster), col="grey",main="Mortes / Milhão de hab.")



#Regressao
base_modelo<- data.frame(base2, base.k4$cluster) #Colocar o numero do grupo com a base original e salvar na tabela PokemonFinal

base_modelo$y <-ifelse(base_modelo$base.k4.cluster == 2, 1,
                ifelse(base_modelo$base.k4.cluster == 3, 1, 0))
base_modelo<- select (base_modelo,-c("base.k4.cluster"))

modelo<-glm(y ~ .  , 
            family = binomial (link='logit'), 
            data = base_modelo[,4:9])
summary(modelo)

names(base_modelo)

modelo<-glm(y ~   Unemployment+GDP_growth+Population, 
            family = binomial (link='logit'), 
            data = base_modelo[,4:9])
summary(modelo)

library(HH)
vif(modelo)

#Calculo da correlacao entre as variaveis
matriz_correl <- round(cor(base_modelo[,4:8]), 2) #round para arredondar, cor para correlação, duas casas decimais
matriz_correl
library(corrplot)
par(mfrow=c(1,1)) 
matriz_correl_I<-corrplot(matriz_correl, method = "circle") #método gráfico

#Ponto de corte
base_modelo$pred2 = predict(modelo,base_modelo, type = "response")
library(cutpointr)
ponto <- cutpointr(base_modelo, pred2, y,
                   method = minimize_metric, metric = abs_d_sens_spec)
summary(ponto) 



