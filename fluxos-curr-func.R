# Arquivo fluxos-curr-func.R
# Criado por Pedro A. Rangel
# Ultima modificacao: 2016-05-13
#  ------------------------------------------------------------------------

fluxo <- function(arquivo, lim) {
  library('plyr')
  library('markovchain')

  dados <- read.csv(arquivo, na.strings = '', encoding = 'UTF-8')
  dados$Passou <- dados$Conceito %in% c('MM', 'MS', 'SS', 'CC')

  # Seleciona as materias que vao entrar no fluxo baseado no
  # total de alunos que passaram na materia
  mat <- levels(dados$Materia)
  dados$Materia <- as.character(dados$Materia)
  nTot <- length(mat)
  total <- numeric(nTot)
  for (i in 1:nTot) {
    total[i] <- sum(dados$Passou & dados$Materia == mat[i])
  }
  obr <- mat[total >= lim]
  nObr <- length(obr)

  # Cria matriz para armazenar as probabilidades de cursar uma materia
  # dado que foi aprovado em outra
  matProb <- matrix(0, nrow = nObr, ncol = nObr)
  # matProb <- matrix(0, nrow = nObr, ncol = nObr, dimnames = list(obr, obr))

  pb <- txtProgressBar(style = 3)
  for (j in 1:nObr) {
    matB <- obr[j]
    alunos1 <- ddply(dados, .(MatriculaEncrypted), here(transform),
                     semB = min(c(AnoMateria[Materia == matB & Passou], Inf),
                                na.rm = TRUE))
    for (i in 1:nObr) {
      setTxtProgressBar(pb, ((j - 1) * nObr + i) / nObr ^ 2)
      if (i != j) {
        matA <- obr[i]
        alunos2 <- ddply(alunos1, .(MatriculaEncrypted), here(summarise),
                         AantesB = any(Materia[AnoMateria < semB
                                               & semB != Inf] == matA))
        matProb[i, j] <- mean(alunos2$AantesB)
      }
    }
  }
  close(pb)

  matriz <- matProb
  diag(matriz) <- NA
  matriz[matriz <= .01] <- 0  # arredondamento para zero

  # Cria lista com as materias dependentes de cada materia
  # a e prerequisito de b => b depende de a
  listTodas <- list()
  for (i in 1:nObr) {
    listTodas[[i]] <- which(matriz[, i] == 0)
  }

  # Cria lista com as materias dependentes diretas
  # Se nao tiver, vai ter ela mesma (necessario para o grafo)
  listDiretas <- list()
  for (i in 1:nObr) {
    dep <- listTodas[[i]]
    deptodos <- numeric()
    for (j in dep) {
      deptodos <- union(deptodos, listTodas[[j]])
    }
    diretos <- setdiff(dep, deptodos)
    if (length(diretos) == 0) {
      diretos <- i
    }
    listDiretas[[i]] <- diretos
  }

  # Criacao da matriz de transicao
  mtrans <- matrix(0, nrow = nObr, ncol = nObr, dimnames = list(obr, obr))
  for (i in 1:nObr) {
    dep <- listDiretas[[i]]
    mtrans[i, dep] <- 1 / length(dep)
  }

  plot(new('markovchain', transitionMatrix = mtrans))

  return(mtrans)
}

fluxo("dadosEstatistica.csv", 300)

fluxo("dadosMatematica.csv", 300)
