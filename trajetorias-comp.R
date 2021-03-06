# Arquivo trajetorias.R
# Criado por Pedro A. Rangel
# Ultima modificacao: 2016-05-03
#  ------------------------------------------------------------------------

# install.packages("TraMineR")
library(TraMineR)

load("ws-1.RData")

# Criação das trajetórias a partir dos dados dos alunos
trajComp <- ExtraiTrajComp("dadosComputacao.csv")
save(trajComp, file="trajComp.Rdata")


# Definição das trajetórias (sequências)
seqComp <- seqdef(trajComp)
save(seqComp, file="seqComp.Rdata")

#  ----------------------------------

# Calcula distancias usando Optimal Compching
distComp <- seqdist(seqComp, method = "OM", indel = 1, sm = "TRATE")

# Descrição das Trajetórias -----------------------------------------------

library(TraMineR)

# Visualização

par(mfrow = c(2, 2))
seqiplot(seqComp, withlegend = FALSE, border = NA)
seqIplot(seqComp, sortv = 'from.start', withlegend = FALSE)
seqfplot(seqComp, withlegend = FALSE, border = NA)
seqlegend(seqComp)

par(mfrow = c(1, 1))
seqiplot(seqComp, withlegend = TRUE, border = NA)
seqIplot(seqComp, sortv = 'from.start', withlegend = TRUE)
seqfplot(seqComp, withlegend = TRUE, border = NA)


# Descrição

par(mfrow = c(2, 2))
seqdplot(seqComp, withlegend = FALSE, border = NA)
seqHtplot(seqComp)
seqmsplot(seqComp, withlegend = FALSE, border = NA)
seqmtplot(seqComp, withlegend = FALSE)
par(mfrow = c(1, 1))


# Construção da Tipologia -------------------------------------------------

# Criar Tipologia das trajetorias
# contrui clustering pelo método de Ward
# install.packages('cluster')
library(cluster)
clusterward <- agnes(distComp, diss = TRUE, method = "ward")
plot(clusterward, which.plot = 2)


# Uso da funcao silhouette do pacote cluster para determinar
# melhor numero de clusters

k <- 2:10
silmedia <- numeric(length(k))
silmediana <- numeric(length(k))

for (i in 1:length(k)) {
  sil <- silhouette(cutree(clusterward, k = k[i]), distComp)
  silmedia[i] <- mean(sil[, 3])
  silmediana[i] <- median(sil[, 3])
}

plot(silmediana ~ k, type = 'l', col = 'red')
plot(silmedia ~ k, type = 'l', col = 'blue')

silmedias <- numeric(10)
for (i in 2:10) {
  sil <- silhouette(cutree(clusterward, k = i), distComp)
  silmedias[[i]] <- mean(summary(sil)[[2]])
}

plot(2:10, silmedias[-1], type = 'l', col = 'blue')

k <- 5  # numero de grupos
cl <- cutree(clusterward, k = k)  # separa em k grupos
clfac <- factor(cl, labels = "Type", 1:k)
sil <- silhouette(cl, distComp)
#plot(sil, nmax = 80)

# Plota todas as trajetórias por cluster
seqIplot(seqComp, group = clfac, sortv = "from.start")

# As distribuições de cada Compado por cluster
seqdplot(seqComp, group = clfac, border = NA)

# A trajetória representativa de cada cluster
seqrplot(seqComp, dist.Comprix = distComp, group = clfac, border = NA)

#  ------------------------------------------------------------------------
