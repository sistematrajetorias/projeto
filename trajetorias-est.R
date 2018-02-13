# Arquivo trajetorias.R
# Criado por Pedro A. Rangel
# Ultima modificacao: 2016-05-03
#  ------------------------------------------------------------------------

# install.packages("TraMineR")
library(TraMineR)

load("ws-1.RData")

# Criação das trajetórias a partir dos dados dos alunos
trajEst <- ExtraiTrajEst("dadosEstatistica.csv")
save(trajEst, file="trajEst2.Rdata")


# Definição das trajetórias (sequências)
seqEst <- seqdef(trajEst)
save(seqEst2, file="seqEst2.Rdata")

#  ----------------------------------

# Calcula distancias usando Optimal matching
distEst <- seqdist(seqEst, method = "OM", indel = 1, sm = "TRATE")

# Descrição das Trajetórias -----------------------------------------------

library(TraMineR)

# Visualização

par(mfrow = c(2, 2))
seqiplot(seqEst, withlegend = FALSE, border = NA)
seqIplot(seqEst, sortv = 'from.start', withlegend = FALSE)
seqfplot(seqEst, withlegend = FALSE, border = NA)
seqlegend(seqEst)

par(mfrow = c(1, 1))
seqiplot(seqEst, withlegend = TRUE, border = NA)
seqIplot(seqEst, sortv = 'from.start', withlegend = TRUE)
seqfplot(seqEst, withlegend = TRUE, border = NA)


# Descrição

par(mfrow = c(2, 2))
seqdplot(seqEst, withlegend = FALSE, border = NA)
seqHtplot(seqEst)
seqmsplot(seqEst, withlegend = FALSE, border = NA)
seqmtplot(seqEst, withlegend = FALSE)
par(mfrow = c(1, 1))


# Construção da Tipologia -------------------------------------------------

# Criar Tipologia das trajetorias
# contrui clustering pelo método de Ward
# install.packages('cluster')
library(cluster)
clusterward <- agnes(distEst, diss = TRUE, method = "ward")
plot(clusterward, which.plot = 2)


# Uso da funcao silhouette do pacote cluster para determinar
# melhor numero de clusters

k <- 2:10
silmedia <- numeric(length(k))
silmediana <- numeric(length(k))

for (i in 1:length(k)) {
  sil <- silhouette(cutree(clusterward, k = k[i]), distEst)
  silmedia[i] <- mean(sil[, 3])
  silmediana[i] <- median(sil[, 3])
}

plot(silmediana ~ k, type = 'l', col = 'red')
plot(silmedia ~ k, type = 'l', col = 'blue')

silmedias <- numeric(10)
for (i in 2:10) {
  sil <- silhouette(cutree(clusterward, k = i), distEst)
  silmedias[[i]] <- mean(summary(sil)[[2]])
}

plot(2:10, silmedias[-1], type = 'l', col = 'blue')

k <- 5  # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
sil <- silhouette(cl, distEst)
plot(sil, nmax = 80)


# Plota todas as trajetórias por cluster
seqIplot(seqEst, group = clfac, sortv = "from.start")

# As distribuições de cada estado por cluster
seqdplot(seqEst, group = clfac, border = NA)

# A trajetória representativa de cada cluster
seqrplot(seqEst, dist.matrix = distEst, group = clfac, border = NA)

#  ------------------------------------------------------------------------
