# Arquivo taxas-form.R
# Criado por Pedro A. Rangel
# Ultima modificacao: 2016-05-11
#  ------------------------------------------------------------------------

# Probabilidades Condicionais ---------------------------------------------

taxas_formar <- function(arquivo, lim = 0) {
  dados <- read.csv(arquivo, na.strings = '', encoding = 'UTF-8')

  dados <- dados[dados$Conceito %in% c('MM', 'MS', 'SS', 'CC'), ]

  mat <- levels(dados$Materia)
  n <- length(mat)
  formados <- numeric(n)
  total <- numeric(n)

  for (i in 1:n) {
    formados[i] <- sum(dados$Materia == mat[i] &
                       dados$StatusFinal == 'Formatura')
    total[i] <- sum(dados$Materia == mat[i])
  }

  obr <- which(total >= lim)
  taxa <- formados / total
  plot(taxa ~ total, pch = 1)
  points(total[obr], taxa[obr], pch = 8, col = 'red')

  return(list(obrigatorias =
      data.frame(mat[obr], taxa[obr])[order(taxa[obr], decreasing = TRUE), ],
    ord_taxa =
      data.frame(mat, taxa, total)[order(taxa, decreasing = TRUE), ],
    ord_total =
      data.frame(mat, taxa, total)[order(total, decreasing = TRUE), ]))
}

taxas_formar("dadosEstatistica.csv", 300)
