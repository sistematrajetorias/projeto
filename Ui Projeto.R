require(shiny)
require(shinythemes)
Cálculo1=c("Cálculo2","y","z","")
IP=c("Métodos","Exploratória","Computação1","")
Métodos=c("Métodos2","x","y","")
Cálculo2=c("Cálculo3","Cálculo_Númerico","Análise","")
Cálculo3=c("CálculoProbab","Variável_Complexa","")
Métodos2=c("Dados","Estatítica","")
a=0:9
b=10:19
t=20:29
s=30:39
u=40:49
v=50:59


  ui =fluidPage(titlePanel("Sistema Trajetórias"),theme=shinytheme("darkly"),title="Sistema Trajetórias",

                sidebarLayout(
                  sidebarPanel(
                  helpText("Escolha as matérias que formarão o tronco a ser analisado"),
                  
    selectInput('m1', 'Primeira matéria', c("",'Cálculo1', 'IP'),selected=""),
    conditionalPanel(
      condition = "input.m1 !='' ",
    selectInput('m2', 'Segunda matéria', "")),
    conditionalPanel(
      condition ="input.m2 !=''",
    selectInput('m3','Terceira matéria',""))

    )
    ,mainPanel(
    selectInput('curso',"Nome do Curso",choices=c("Estatística","","Matemática"),selected = ""),textOutput("nomecurso"))
    ))
  
  server = function(input, output, session){
    in1 = reactive({
      if (input$m1 == "")
        return(NULL)
      in1 = get(input$m1)
      in1
    })
    observe({
      updateSelectInput(session, "m2",
                        choices = in1(),selected="")
      })
    in2 = reactive({
      if (input$m2 == "")
        return(NULL)
      in2 = get(input$m2)
      in2
    })
    observe({
      updateSelectInput(session, "m3",
                        choices = in2() ,selected=""
      )})
    
    output$nomecurso <- renderText({ 
      paste("Gráfico do tronco escolhido para", input$curso)
    })
  }

  shinyApp(ui,server)
