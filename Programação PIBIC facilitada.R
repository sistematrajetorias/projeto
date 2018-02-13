################################
# IMAGEM DOS CLUSTERS          #
################################

library(TraMineR)
library(cluster)

#Rodar extrai-traj-mat

setwd("~/unb/PIBIC")
#trajComp <- ExtraiTrajComp("dadosComputacao.csv")
#save(trajComp,file="trajComp2.Rdata")
load("C:/Users/Gaabi/Documents/unb/PIBIC/trajComp2.Rdata")

seqComp <- seqdef(trajComp)
distComp <- seqdist(seqComp, method = "OM", indel = 1, sm = "TRATE")

clusterward <- agnes(distComp, diss = TRUE, method = "ward")

k <-  4 # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
sil <- silhouette(cl, distComp)

#seqIplot(seqComp, group = clfac, sortv = "from.start")
seqdplot(seqComp, group = gsub("Type","Tipo",clfac),border = NA)

seqdplot(seqComp,group = sil[,2], group = gsub("Type","Tipo ",clfac),withlegend="right",
         border = NA,ltext=c("Física 1","Física 2","Física 3","Circuitos Digitais","Organizaçao e 
                             Arquitetura de Computadores",
                             "Cadeia Finalizada","Formado","Não-Formado"), cex.legend=0.82)


#  ------------------------------------------------------------------------
#####################################
# VER INFORMAÇOES SOBRE OS CLUSTERS #
#####################################

load("encryptedData.rdata")
CursoComp <- "CIÊNCIA DA COMPUTAÇÃO"
tabComp <- tab[tab$Curso == CursoComp, ]
tabComp2<-tabComp[tabComp$Materia %in%  c('CIRCUITOS DIGITAIS','ORGANIZACAO E ARQUITETURA DE COMPUTADORES',
                                       'FISICA 3', 'FISICA 2', 'FISICA 1' ),]

tabnotas<-tabComp2[,c(11,12,15)]

#Carrega banco de dados com trajetórias

### Modificar a funçao da programação extrai-traj-Comp ####
#dados <- read.csv("dadosComputacao.csv", na.strings = '', encoding = 'UTF-8')
#alunos <- levels(factor(dados[[1]]))
#finais <- retiraStatus(dados, alunos)
#cadeias <- list()
#n <- length(alunos)
#pb <- txtProgressBar(style = 3)
#for (i in 1:n) {
#  setTxtProgressBar(pb, i / n)
#  cadeias[[i]] <- CadeiaAluno(dados[dados[[1]] == alunos[i], ])
#}
#cadeiasDF <- CriaCadeia(cadeias, finais)

#cadeiasDF[ncol(cadeiasDF)+1]<-alunos
#cadeiasDF2<-cadeiasDF
#save(cadeiasDF2,file="cadeiasDF3.rdata")

##
load("cadeiasDF3.rdata")
cadeiasDF<-cadeiasDF2
cadeiasDF[31]<-clfac #rodar programaçao acima   ##Verificar se o DF tem realmente 30 colunas
cadeiasDF2<-cadeiasDF[,-c(1:29)]

#Média do IRA médio de cada cluster
tabnotas2<-aggregate(tabnotas['IRA'],tabnotas['MatriculaEncrypted'],mean)
dados<-merge(cadeiasDF2,tabnotas2,by.x='V30',by.y="MatriculaEncrypted",all.x=T)   #verificar o nome do V21
aggregate(dados['IRA'],dados['V31'],mean,na.rm=T)

##### ver media reprovaçoes
funcao <- function(lista){
  z <- table(as.vector(lista)) 
  w <- z[names(z) == "II"]+z[names(z) == "MI"]+z[names(z) == "SR"]
  w <- as.numeric(w)
  w
}
tabnotas3<-aggregate(tabnotas['Conceito'],tabnotas['MatriculaEncrypted'],funcao)
tabnotas3$Conceito<-as.numeric(tabnotas3$Conceito)
dados2<-merge(dados,tabnotas3,by.x='V21',by.y="MatriculaEncrypted",all.x=T)

aggregate(dados2['Conceito'],dados2['V22'],mean,na.rm=T)


##########################################
# CALCULA MATRIZ DE PROB DE CADA CLUSTER #
##########################################
#Vai usar mais pra frente

#CLUSTERS
load("C:/Users/Gaabi/Documents/unb/PIBIC/seqComp.Rdata")
library(cluster)
library(TraMineR)
library(PST)
distComp <- seqdist(seqComp, method = "OM", indel = 1, sm = "TRATE")
clusterward <- agnes(distComp, diss = TRUE, method = "ward")
k <- 5  # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
##  ^^^ dispensável se ja rodou tudo que tem antes, mas se quiser pode rodar denovo pra garantir

seqComp[,30]<-clfac
colnames(seqComp)[30]<-"cluster"


seq1<-seqComp[seqComp$cluster == "Type1",c(1:29)]
p1<-markovchainFit(data = seq1, method = "mle", name = "MLE")$estimate

seq2<-seqComp[seqComp$cluster == "Type2",c(1:29)]
p2<-markovchainFit(data = seq2, method = "mle", name = "MLE")$estimate

seq3<-seqComp[seqComp$cluster == "Type3",c(1:29)]
p3<-markovchainFit(data = seq3, method = "mle", name = "MLE")$estimate

seq4<-seqComp[seqComp$cluster == "Type4",c(1:29)]
p4<-markovchainFit(data = seq4, method = "mle", name = "MLE")$estimate

seq5<-seqComp[seqComp$cluster == "Type5",c(1:29)]
p5<-markovchainFit(data = seq5, method = "mle", name = "MLE")$estimate



#########################
# IDENTIFICAR O CLUSTER #
#########################


ident_cluster<- function(seq,n){
  #n<-length(seq)
  prob <- table(seqComp$cluster)/ nrow(seqComp)
  for(i in 1:n){
    s.ant <- seq[i]
    s.pos <- seq[i+1]
    cont=1
    for( p in c(p1,p2,p3,p4)){
      a <- which(rownames(p@transitionMatrix) == s.ant )
      b <- which(colnames(p@transitionMatrix) == s.pos )
      prob[cont] <- ifelse( length(a) == 0 | length(b) == 0 , 0 ,prob[cont] * p@transitionMatrix[a,b]) #p(seq)=p(s1)*p(s2|s1)*...*p(sn|sn-1)
      cont <- cont + 1
    }
  }
  prob<-as.matrix(prob)
  rownames(prob) <- c("Type1","Type2","Type3","Type4")
  a<-rownames(prob)[prob == max(prob)]
  if(length(a) != 1) a="TypeNULL"
  a
}


qd<-list(NULL)
for(n in 1:8){
  seqComp[,31]<-numeric(0)
  for(i in 1:887){
    seqComp[i,31]<- ident_cluster(as.character(trajComp[i,]),n)
  }
  
  seqComp[,31]<-as.factor(seqComp[,31])
  
  
  diferentes<-all.equal(seqComp[,30],seqComp[,31])[length(all.equal(seqComp[,30],seqComp[,31]))]
  
  qd[n]<-diferentes
  
}


#Vê quais foram os diferentes, reais(linhas) -> identificados(colunas)
levels(seqComp[,23])<-c("Type1", "Type2", "Type3", "Type4", "Type5")
table(seqComp[which(!seqComp[,22]==seqComp[,23]),22],seqComp[which(!seqComp[,22]==seqComp[,23]),23])


########################################################################################################
########################################################################################################
########################################################################################################
##### MATEMÁTICA #######################################################################################
########################################################################################################
########################################################################################################
########################################################################################################

################################
# IMAGEM DOS CLUSTERS          #
################################

library(TraMineR)
library(cluster)

#Rodar extrai-traj-mat

setwd("~/unb/PIBIC")
#trajMat <- ExtraiTrajMat("dadosMatematica.csv")
#save(trajMat,file="trajMat.Rdata")
load("C:/Users/Gaabi/Documents/unb/PIBIC/trajMat.Rdata")

seqMat <- seqdef(trajMat)

distMat <- seqdist(seqMat, method = "OM", indel = 1, sm = "TRATE")

clusterward <- agnes(distMat, diss = TRUE, method = "ward")

k <- 4  # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
sil <- silhouette(cl, distMat)

#seqIplot(seqMat, group = clfac, sortv = "from.start")

seqdplot(seqMat, group = gsub("Type","Tipo",clfac),withlegend="right",
         border = NA,ltext=c("Cálculo 1","Cálculo 2","Cálculo 3","Variável Complexa 1",
        "Cadeia Finalizada","Formado","Não-Formado"), cex.legend=1)


seqdplot(seqMat, group = sil[,2],withlegend="right",
         border = NA,ltext=c("Cálculo 1","Cálculo 2","Cálculo 3","Variável Complexa 1",
                             "Cadeia Finalizada","Formado","Não-Formado"), cex.legend=1)
#  ------------------------------------------------------------------------
#####################################
# VER INFORMAÇOES SOBRE OS CLUSTERS #
#####################################

load("encryptedData.rdata")
CursoMat <- "BACHARELADO EM MATEMÁTICA"
tabMat <- tab[tab$Curso == CursoMat, ]
tabMat2<-tabMat[tabMat$Materia %in%  c('VARIAVEL COMPLEXA 1',
                                       'Calculo 3', 'Calculo 2', 'Calculo 1' ),]

tabnotas<-tabMat2[,c(11,12,15)]

#Carrega banco de dados com trajetórias

### Modificar a funçao da programação extrai-traj-Mat ####
  dados <- read.csv("dadosMatematica.csv", na.strings = '', encoding = 'UTF-8')
  alunos <- levels(factor(dados[[1]]))
  finais <- retiraStatus(dados, alunos)
  cadeias <- list()
  n <- length(alunos)
  pb <- txtProgressBar(style = 3)
  for (i in 1:n) {
    setTxtProgressBar(pb, i / n)
    cadeias[[i]] <- CadeiaAluno(dados[dados[[1]] == alunos[i], ])
  }
  cadeiasDF <- CriaCadeia(cadeias, finais)

  cadeiasDF[ncol(cadeiasDF)+1]<-alunos
  cadeiasDF2<-cadeiasDF
  save(cadeiasDF2,file="cadeiasDF2.rdata")

##
load("cadeiasDF2.rdata")
cadeiasDF<-cadeiasDF2
cadeiasDF[22]<-clfac #rodar programaçao acima   ##Verificar se o DF tem realmente 21 colunas
cadeiasDF2<-cadeiasDF[,-c(1:20)]

#Média do IRA médio de cada cluster
tabnotas2<-aggregate(tabnotas['IRA'],tabnotas['MatriculaEncrypted'],mean)
dados<-merge(cadeiasDF2,tabnotas2,by.x='V21',by.y="MatriculaEncrypted",all.x=T)   #verificar o nome do V21
aggregate(dados['IRA'],dados['V22'],mean,na.rm=T)

##### ver media reprovaçoes
funcao <- function(lista){
  z <- table(as.vector(lista)) 
  w <- z[names(z) == "II"]+z[names(z) == "MI"]+z[names(z) == "SR"]
  w <- as.numeric(w)
  w
}
tabnotas3<-aggregate(tabnotas['Conceito'],tabnotas['MatriculaEncrypted'],funcao)
tabnotas3$Conceito<-as.numeric(tabnotas3$Conceito)
dados2<-merge(dados,tabnotas3,by.x='V21',by.y="MatriculaEncrypted",all.x=T)

aggregate(dados2['Conceito'],dados2['V22'],mean,na.rm=T)


##########################################
# CALCULA MATRIZ DE PROB DE CADA CLUSTER #
##########################################
#Vai usar mais pra frente

#CLUSTERS
load("C:/Users/Gaabi/Documents/unb/PIBIC/seqMat.Rdata")
library(cluster)
library(TraMineR)
library(PST)
distMat <- seqdist(seqMat, method = "OM", indel = 1, sm = "TRATE")
clusterward <- agnes(distMat, diss = TRUE, method = "ward")
k <- 4  # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
##  ^^^ dispensável se ja rodou tudo que tem antes, mas se quiser pode rodar denovo pra garantir

seqMat[,21]<-clfac
colnames(seqMat)[21]<-"cluster"


seq1<-seqMat[seqMat$cluster == "Type1",c(1:20)]
p1<-markovchainFit(data = seq1, method = "mle", name = "MLE")$estimate

seq2<-seqMat[seqMat$cluster == "Type2",c(1:20)]
p2<-markovchainFit(data = seq2, method = "mle", name = "MLE")$estimate

seq3<-seqMat[seqMat$cluster == "Type3",c(1:20)]
p3<-markovchainFit(data = seq3, method = "mle", name = "MLE")$estimate

seq4<-seqMat[seqMat$cluster == "Type4",c(1:20)]
p4<-markovchainFit(data = seq4, method = "mle", name = "MLE")$estimate

seq5<-seqMat[seqMat$cluster == "Type5",c(1:20)]
p5<-markovchainFit(data = seq5, method = "mle", name = "MLE")$estimate



#########################
# IDENTIFICAR O CLUSTER #
#########################


ident_cluster<- function(seq,n){
  #n<-length(seq)
  prob <- table(seqMat$cluster)/ nrow(seqMat)
  for(i in 1:n){
    s.ant <- seq[i]
    s.pos <- seq[i+1]
    cont=1
    for( p in c(p1,p2,p3,p4)){
      a <- which(rownames(p@transitionMatrix) == s.ant )
      b <- which(colnames(p@transitionMatrix) == s.pos )
      prob[cont] <- ifelse( length(a) == 0 | length(b) == 0 , 0 ,prob[cont] * p@transitionMatrix[a,b]) #p(seq)=p(s1)*p(s2|s1)*...*p(sn|sn-1)
      cont <- cont + 1
    }
  }
  prob<-as.matrix(prob)
  rownames(prob) <- c("Type1","Type2","Type3","Type4")
  a<-rownames(prob)[prob == max(prob)]
  if(length(a) != 1) a="TypeNULL"
  a
}


qd<-list(NULL)
for(n in 1:8){
  seqMat[,22]<-numeric(0)
  for(i in 1:563){
    seqMat[i,22]<- ident_cluster(as.character(trajMat[i,]),n)
  }
  
  seqMat[,22]<-as.factor(seqMat[,22])
  
  
  diferentes<-all.equal(seqMat[,21],seqMat[,22])[length(all.equal(seqMat[,21],seqMat[,22]))]
  
  qd[n]<-diferentes
  
}


#Vê quais foram os diferentes, reais(linhas) -> identificados(colunas)
levels(seqMat[,23])<-c("Type1", "Type2", "Type3", "Type4", "Type5")
table(seqMat[which(!seqMat[,22]==seqMat[,23]),22],seqMat[which(!seqMat[,22]==seqMat[,23]),23])


########################################################################################################
########################################################################################################
########################################################################################################
#####ESTATÍSTICA########################################################################################
########################################################################################################
########################################################################################################
########################################################################################################


################################
# IMAGEM DOS CLUSTERS          #
################################

library(TraMineR)
library(cluster)

#Rodar extrai-traj-est

setwd("~/unb/PIBIC")
#trajEst <- ExtraiTrajEst("dadosEstatistica.csv")
#save(trajEst,file="trajEst.Rdata")
load("C:/Users/Gaabi/Documents/unb/PIBIC/trajEst.Rdata")

seqEst <- seqdef(trajEst)
distEst <- seqdist(seqEst, method = "OM", indel = 1, sm = "TRATE")

clusterward <- agnes(distEst, diss = TRUE, method = "ward")

k <- 5  # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
sil <- silhouette(cl, distEst)

#seqIplot(seqEst, group = clfac, sortv = "from.start")

seqdplot(seqEst, group = gsub("Type","Tipo",clfac),rows=3,cols=2, border = NA,ltext=c("Cálculo 1","Cálculo 2","Cálculo 3","Cálculo de Probabilidade 1",
                                                    "Inferência","Cadeia Finalizada","Formado","Não-Formado"),
         cex.legend=.85)


#  ------------------------------------------------------------------------
#####################################
# VER INFORMAÇOES SOBRE OS CLUSTERS #
#####################################

setwd("~/unb/PIBIC")
load("encryptedData.rdata")
CursoEst <- "BACHARELADO EM ESTATÍSTICA"
tabEst <- tab[tab$Curso == CursoEst, ]
tabEst2<-tabEst[tabEst$Materia %in%  c('INFERENCIA ESTATISTICA','CALCULO DE PROBABILIDADE 1',
                                       'Calculo 3', 'Calculo 2', 'Calculo 1' ),]

tabnotas<-tabEst2[,c(11,12,15)]

#Carrega banco de dados com trajetórias

### Modificar a funçao da programação extrai-traj-est ####
#ExtraiTrajEst <- function(arquivo) {
# character -> (data.frame)
#
# Programa principal
# LÃª os dados do arquivo fornecido, retira os alunos, e para cada um
# roda as funÃ§Ãµes para identificarem a respectiva cadeia.
# ApÃ³s todos rodarem, junta as cadeias em um data.frame, ajustando o tamanho
# para terem o mesmo tamanho.

# setwd('/home/pedro/Documents/UnB/PIBIC')
# dados <- read.csv(arquivo, na.strings = '', encoding = 'UTF-8')
# print(dados)
#  alunos <- levels(factor(dados[[1]]))
#  finais <- retiraStatus(dados, alunos)
# periodos <- levels(dados[[2]])
#  cadeias <- list()
#  n <- length(alunos)
# print(n)
#  pb <- txtProgressBar(style = 3)
#  for (i in 1:n) {
#    setTxtProgressBar(pb, i / n)
#    cadeias[[i]] <- CadeiaAluno(dados[dados[[1]] == alunos[i], ])
#  }
#  cadeiasDF <- CriaCadeia(cadeias, finais)
#  cadeiasDF[ncol(cadeiasDF)+1]<-alunos       <<<<<<<<<<<<<< ESSA LINHA
#  close(pb)
# cat('done\n')
#  return(cadeiasDF)
# return(cadeias)
#}

load("cadeiasDF.rdata")
cadeiasDF[23]<-clfac #rodar programaçao acima   ##Verificar se o DF tem realmente 22 colunas
cadeiasDF2<-cadeiasDF[,-c(1:21)]

#Média do IRA médio de cada cluster
tabnotas2<-aggregate(tabnotas['IRA'],tabnotas['MatriculaEncrypted'],mean)
dados<-merge(cadeiasDF2,tabnotas2,by.x='V22',by.y="MatriculaEncrypted",all.x=T)
aggregate(dados['IRA'],dados['V23'],mean,na.rm=T)

##### ver media reprovaçoes
funcao <- function(lista){
  z <- table(as.vector(lista)) 
  w <- z[names(z) == "II"]+z[names(z) == "MI"]+z[names(z) == "SR"]
  w <- as.numeric(w)
  w
}
tabnotas3<-aggregate(tabnotas['Conceito'],tabnotas['MatriculaEncrypted'],funcao)
tabnotas3$Conceito<-as.numeric(tabnotas3$Conceito)
dados2<-merge(dados,tabnotas3,by.x='V22',by.y="MatriculaEncrypted",all.x=T)

aggregate(dados2['Conceito'],dados2['V23'],mean,na.rm=T)


##########################################
# CALCULA MATRIZ DE PROB DE CADA CLUSTER #
##########################################
#Vai usar mais pra frente

#CLUSTERS
load("C:/Users/Gaabi/Documents/unb/PIBIC/seqEst.Rdata")
library(cluster)
library(TraMineR)
library(PST)
distEst <- seqdist(seqEst, method = "OM", indel = 1, sm = "TRATE")
clusterward <- agnes(distEst, diss = TRUE, method = "ward")
k <- 5  # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
##  ^^^ dispensável se ja rodou tudo que tem antes, mas se quiser pode rodar denovo pra garantir

seqEst[,22]<-clfac
colnames(seqEst)[22]<-"cluster"


seq1<-seqEst[seqEst$cluster == "Type1",c(1:21)]
p1<-markovchainFit(data = seq1, method = "mle", name = "MLE")$estimate

seq2<-seqEst[seqEst$cluster == "Type2",c(1:21)]
p2<-markovchainFit(data = seq2, method = "mle", name = "MLE")$estimate

seq3<-seqEst[seqEst$cluster == "Type3",c(1:21)]
p3<-markovchainFit(data = seq3, method = "mle", name = "MLE")$estimate

seq4<-seqEst[seqEst$cluster == "Type4",c(1:21)]
p4<-markovchainFit(data = seq4, method = "mle", name = "MLE")$estimate

seq5<-seqEst[seqEst$cluster == "Type5",c(1:21)]
p5<-markovchainFit(data = seq5, method = "mle", name = "MLE")$estimate



#########################
# IDENTIFICAR O CLUSTER #
#########################


ident_cluster<- function(seq,n){
  #n<-length(seq)
  prob <- table(seqEst$cluster)/ nrow(seqEst)
  for(i in 1:n){
    s.ant <- seq[i]
    s.pos <- seq[i+1]
    cont=1
    for( p in c(p1,p2,p3,p4,p5)){
      a <- which(rownames(p@transitionMatrix) == s.ant )
      b <- which(colnames(p@transitionMatrix) == s.pos )
      prob[cont] <- ifelse( length(a) == 0 | length(b) == 0 , 0 ,prob[cont] * p@transitionMatrix[a,b]) #p(seq)=p(s1)*p(s2|s1)*...*p(sn|sn-1)
      cont <- cont + 1
    }
  }
  prob<-as.matrix(prob)
  rownames(prob) <- c("Type1","Type2","Type3","Type4","Type5")
  rownames(prob)[prob == max(prob)]
}


qd<-list(NULL)
for(n in 1:8){
  seqEst[,23]<-numeric(0)
  for(i in 1:651){
    seqEst[i,23]<- ident_cluster(as.character(trajEst[i,]),n)
  }
  
  seqEst[,23]<-as.factor(seqEst[,23])
  
  
  diferentes<-all.equal(seqEst[,22],seqEst[,23])[length(all.equal(seqEst[,22],seqEst[,23]))]
  
  qd[n]<-diferentes
  
}


#Vê quais foram os diferentes, reais(linhas) -> identificados(colunas)
levels(seqEst[,23])<-c("Type1", "Type2", "Type3", "Type4", "Type5")
table(seqEst[which(!seqEst[,22]==seqEst[,23]),22],seqEst[which(!seqEst[,22]==seqEst[,23]),23])
