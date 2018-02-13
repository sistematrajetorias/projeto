# Arquivo import-data.R
# Criado por Pedro A. Rangel
# Ultima modificacao: 2016-04-20
#  ------------------------------------------------------------------------

# Esse arquive deve ler os dados do arquivo encryptedData.rdata
# Devem ser separados os alunos que fazem parte do curso e transformar
# as variáveis para serem passadas para a função no arquivo main.R
setwd("~/unb/PIBIC")
load("encryptedData.rdata")

summary(tab)
names(tab)
dimnames(table(tab$Curso))

CursoEst <- "BACHARELADO EM ESTAT�STICA"
tabEst <- tab[tab$Curso == CursoEst, ]
summary(tabEst)
dimnames(table(tabEst$Materia))
dimnames(table(tabEst$StatusFinal))
# Coluna 1: Identificador dos alunos
# Coluna 2: Semestre
# Coluna 3: Nome da Matéria
# Coluna 4: Menção Final
# Coluna 5: Curso (deve ser o mesmo)
# Coluna 6: Estado Final (Formatura, Trancamento, ...)  # adicionado pedois
dadosEst <- tab[(tab[[4]] == CursoEst & tab[[8]] != 'Ativo'),
                c(15, 10, 9, 11, 4, 8)]

# escrever os dados para arquivo csv
# write.csv(head(dadosEst), file = 'dadosEstatistica.csv', na = ' ')
write.csv(dadosEst, file = 'dadosEstatistica2.csv', na = ' ', row.names = FALSE)


#  ------------------------------------------------------------------------

# save.image("ws-1.RData")





#####################
#MAT
setwd("~/unb/PIBIC")
load("encryptedData.rdata")

summary(tab)
names(tab)
dimnames(table(tab$Curso))

CursoComp <- "CI�NCIA DA COMPUTA��O"
tabComp <- tab[tab$Curso == CursoComp, ]
summary(tabMat)
dimnames(table(tabMat$Materia))
dimnames(table(tabMat$StatusFinal))
# Coluna 1: Identificador dos alunos
# Coluna 2: Semestre
# Coluna 3: Nome da Matéria
# Coluna 4: Menção Final
# Coluna 5: Curso (deve ser o mesmo)
# Coluna 6: Estado Final (Formatura, Trancamento, ...)  # adicionado pedois
dadosComp <- tab[(tab[[4]] == CursoComp & tab[[8]] != 'Ativo'),
                c(15, 10, 9, 11, 4, 8)]

# escrever os dados para arquivo csv
# write.csv(head(dadosEst), file = 'dadosEstatistica.csv', na = ' ')
write.csv(dadosComp, file = 'dadosComputacao.csv', na = ' ', row.names = FALSE)
