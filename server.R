#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("ggplot2")
library("plotly")
load("Curriculos.RData")
dados <- dados[dados$Conceito %in% c("MM", "MS", "SS", "CC"), ]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  load("Curriculos.RData")

  output$distPlot <- renderPlotly({

    dados <- dados[dados$Curso == input$curso, ]
    mat <- intersect(dados$Materia, dados$Materia)
    n <- length(mat)
    formados <- numeric(n)
    total <- numeric(n)
    for (i in 1:n) {
      formados[i] <- sum(dados$Materia == mat[i] &
                         dados$StatusFinal == 'Formatura')
      total[i] <- sum(dados$Materia == mat[i])
    }
    selecionada <- total >= (max(total) * input$lim / 100)
    taxa <- formados / total
    dadosmat <- data.frame(mat, total, taxa = formados / total, selecionada)
    sp <- ggplot(dadosmat) +
          geom_point(aes(x = total, y = taxa, colour = selecionada, text = mat),
                     alpha = 1 / 2) +
      theme(legend.position = "none")
    ggplotly(sp)
  })

})
