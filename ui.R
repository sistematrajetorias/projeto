#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("plotly")
load("Cursos.RData")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Taxas de Formatura dada Aprovação"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("curso",
                  "Curso:",
                  Cursos,
                  selected = "BACHARELADO EM ESTATÍSTICA"),

      sliderInput("lim",
                  "Corte de Número de Alunos (%):",
                  min = 0, max = 100, value = 30,
                  round = FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
))
