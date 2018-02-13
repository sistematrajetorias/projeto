EstadoCadeias <- function(mat, estado, menc) {
  # (vet de character, vet de numeric) -> (vet de numeric)
  #
  # Retorna o novo estado.
  # Para cada matéria aprovada, verificamos se ela pertence a alguma
  # subcadeia do curr�???culo, e se aluno estiver num estado anterior, mudamos
  # o estado para o correspondente da matéria.
  # No caso do aluno pegar uma matéria fora de ordem, o a função só muda
  # o estado da cadeia se o aluno estiver avançando.
  
  # Cadeia A: C1 >> C2 >> C3 >> CP1 >> IE
  if (any(mat == 'INFERENCIA ESTATISTICA')) {
    estado <- 50
  } else if (any(mat == 'CALCULO DE PROBABILIDADE 1') & estado < 40) {
    estado <- 40
  } else if (any(mat == 'Calculo 3') & estado < 30) {
    estado <- 30
  } else if (any(mat == 'Calculo 2') & estado < 20) {
    estado <- 20
  } else if (any(mat == 'Calculo 1') & estado < 10 ) {
    estado <- 10
  }
  
  if( estado == 10 & any(menc == "Calculo 1SS" )){
    estado <- 11
  }
  if( estado == 20 & any(menc == "Calculo 2SS" )){
    estado <- 21
  }
  return(estado)
}

####################################

# Arquivo extrai-traj-est.R
# Criado por Pedro A. Rangel
# Ultima modificacao: 2016-04-28
#  ------------------------------------------------------------------------

PassouMateria <- function(mat) {
  # vetor de character -> (vetor de logical)
  #
  # Retorna um vetor de valores lógicos, com o mesmo número de elementos de mat,
  # dizendo para cada elemento se a menção é de aprovação.

  return(mat == 'MM' | mat == 'MS' | mat == 'SS' | mat == 'CC')
}


EstadoCadeias <- function(mat, estado, menc) {
  # (vet de character, vet de numeric) -> (vet de numeric)
  #
  # Retorna o novo estado.
  # Para cada matéria aprovada, verificamos se ela pertence a alguma
  # subcadeia do curr�???culo, e se aluno estiver num estado anterior, mudamos
  # o estado para o correspondente da matéria.
  # No caso do aluno pegar uma matéria fora de ordem, o a função só muda
  # o estado da cadeia se o aluno estiver avançando.

  # Cadeia A: C1 >> C2 >> C3 >> CP1 >> IE
  if (any(mat == 'INFERENCIA ESTATISTICA')) {
    estado <- 5
  } else if (any(mat == 'CALCULO DE PROBABILIDADE 1') & estado < 4) {
    estado <- 4
  } else if (any(mat == 'Calculo 3') & estado < 3) {
    estado <- 3
  } else if (any(mat == 'Calculo 2') & estado < 2) {
    estado <- 2
  } else if (any(mat == 'Calculo 1') & estado < 1 ) {
    estado <- 1
  }
  return(estado)
}


AtualizaMats <- function(dadosAluPer, estadoAnterior) {
  # (data.frame, vec de numeric) -> (vec de numeric)
  #
  # Retorna o novo estado.
  # Função intermediária, muda as menções para caractere, verifica quais
  # foram de aprovação, e chama a função para atualizar o estado

  mat <- as.character(dadosAluPer[[4]])
  matPass <- PassouMateria(mat)
  return(EstadoCadeias(dadosAluPer[matPass, 3], estadoAnterior,paste(dadosAluPer[matPass, 3],as.character(dadosAluPer[[4]]),sep='')))
}


CriaEstado <- function(vetor) {
  # vec de numeric -> (character)
  #
  # Cria um estado para ser somente um item de charactere em vez de um
  # vetor de numeros.

  estado <- character()
  for (i in 1:length(vetor)) {
    estado <- paste(estado, vetor[i], sep = '')
  }
  return(estado)
}


CadeiaAluno <- function(dadosAluno) {
  # data.frame -> (vec de numeric)
  #
  # Retorna a cadeia individual do aluno a partir dos dados
  # O argumento de entrada é o pedaço da base de dados referente a somente
  # um aluno.
  # A função pega o primeiro e último semstre presente nesses dados, completa
  # se faltar algum per�???odo no meio, com base nos n�???veis originais da variável.
  # Com cada per�???odo, chama outra função para verificar a mudança de estado,
  # baseado no anterior.
  # O padrão é criar um estado inicial igual para todos os alunos, contendo
  # somente
  #
  # Nessa função é definida na variável 'anterior' o tamanho do vetor
  # do estado. Uma para o per�???odo e 11 para as cadeias.

  # periodosTot <- levels(dadosAluno[[2]])
  # periodosAlu1 <- levels(factor(dadosAluno[[2]]))
  # periodosAluno <- periodosTot[which(periodosTot == min(periodosAlu1)):
  #                                which(periodosTot == max(periodosAlu1))]

  periodosTot <- levels(factor(dadosAluno[[2]]))
  periodosAluno <- periodosTot[which(periodosTot == min(periodosTot)):
                                 which(periodosTot == max(periodosTot))]

  # periodosAluno <- levels(dadosAluno[[2]])
  anterior <- 0
  cadeia <- anterior
  for (j in 1:(length(periodosAluno))) {
    proximo <- AtualizaMats(dadosAluno[dadosAluno[[2]] == periodosAluno[j], ],
                            anterior)
    cadeia[j + 1] <- proximo
    anterior <- proximo
  }
  return(cadeia)
}


CompletaLinha <- function(linha, final, n) {
  # (vec de numeric, numeric, numeric) -> (vec de numeric)
  #
  # Completa a linha repetindo o argumento 'final'
  # para juntar no data.frame com todos os alunos

  l <- length(linha)
  if (l < n) {
    linha <- c(linha, rep(final, n - l))
  }
  return(linha)
}


CriaCadeia <- function(listaCad, finais) {
  # (list, vec numeric) -> (data.frame)
  #
  # Junta a lista fornecida com cada item sendo a cadeia individual do aluno
  # e cria um data.frame juntando todos eles.
  # A função verifica qual das cadeias tem o comprimento maior, e ajusta todas
  # as outras repetindo o último item para caber no mesmo número de colunas
  # do data.frame.

  #   nPer <- numeric(length(listaCad))
  #   for (i in 1:length(listaCad)) {
  #     nPer[i] <- length(listaCad[[i]])
  #   }
  nPer <- sapply(listaCad, length)
  numPeriodos <- max(nPer)

  cadeia <- data.frame()
  for (i in 1:length(listaCad)) {
    aluno <- as.numeric(listaCad[[i]])
    cadeia <- rbind(cadeia, CompletaLinha(aluno, finais[i], numPeriodos))
  }
  names(cadeia) <- 1:numPeriodos
  return(cadeia)
}

retiraStatus <- function(dados, alunos) {
  # (data.frame, vec de charactere) -> (vec numeric)
  #
  # Retorna um vetor com os StatusFinal para a lista de alunos correspondente
  # O StatusFinal é codificado em um número

  n <- length(alunos)
  status <- numeric(n)
  for (i in 1:n) {
    aluno <- dados[dados[[1]] == alunos[i], 6][1]
    if (aluno == 'Formatura') {
      status[i] <- 11
    } else if (aluno == 'Ativo') {
      status[i] <- 12
    } else {
      status[i] <- 13
    }
  }

  return(status)
}


ExtraiTrajEst <- function(arquivo) {
  # character -> (data.frame)
  #
  # Programa principal
  # Lê os dados do arquivo fornecido, retira os alunos, e para cada um
  # roda as funções para identificarem a respectiva cadeia.
  # Após todos rodarem, junta as cadeias em um data.frame, ajustando o tamanho
  # para terem o mesmo tamanho.

  # setwd('/home/pedro/Documents/UnB/PIBIC')
  dados <- read.csv(arquivo, na.strings = '', encoding = 'UTF-8')
  # print(dados)
  alunos <- levels(factor(dados[[1]]))
  finais <- retiraStatus(dados, alunos)
  # periodos <- levels(dados[[2]])
  cadeias <- list()
  n <- length(alunos)
  # print(n)
  pb <- txtProgressBar(style = 3)
  for (i in 1:n) {
    setTxtProgressBar(pb, i / n)
    cadeias[[i]] <- CadeiaAluno(dados[dados[[1]] == alunos[i], ])
  }
  cadeiasDF <- CriaCadeia(cadeias, finais)
  close(pb)
  # cat('done\n')
  return(cadeiasDF)
  # return(cadeias)
}


#  ------------------------------------------------------------------------

# # Exemplo com arquivo de alunos simulados.
# ExtraiTraj('exemplo-1.csv')  # com código das disciplinas
# ExtraiTraj('exemplo-2.csv')  # com nome das disciplinas
