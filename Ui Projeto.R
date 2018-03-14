Cálculo1=c("Cálculo2","y","z","")
IP=c("Métodos","a","b","")
Métodos=c("Métodos2","t","s","")
Cálculo2=c("Cálculo3","u","v","")
Cálculo3=c()
a=0:9
b=10:19
t=20:29
s=30:39
u=40:49
v=50:59
runApp(list(
  ui = bootstrapPage(
    selectInput('m1', 'Primeira matéria', c("",'Cálculo1', 'IP'),selected=""),
    conditionalPanel(
      condition = "input.m1 == 'Cálculo1'||input.m1 == 'IP'",
    selectInput('m2', 'Segunda matéria', "")),
    conditionalPanel(
      condition ="input.m2=='Cálculo2'|| input.m2=='Métodos'",
    selectInput('m3','Terceira matéria',""))
    ),
  server = function(input, output, session){
    outVar = reactive({
      if (input$m1 == "")
        return(NULL)
      mydata = get(input$m1)
      mydata
    })
    observe({
      updateSelectInput(session, "m2",
                        choices = outVar(),selected=""
      )})
    outVar1 = reactive({
      if (input$m2 == "")
        return(NULL)
      mydata1 = get(input$m2)
      mydata1
    })
    observe({
      updateSelectInput(session, "m3",
                        choices = outVar1(),selected=""
      )})
  }
))


