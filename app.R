#shiny app

library(shiny)

#definindo interface

ui <- fluidPage(titlePanel("Painel das chances de mortalidade por câncer no Brasil."),
                sidebarLayout(sidebarPanel("Sexo"),mainPanel(h2("MOR",align="center"),
                                                             p("Aplicativo shiny realizado para monitorar a MOR da população brasileira por câncer(calculada no período entre 1985 até 2019)."))))

#definindo a logica do servidor requerida para realizar evento

#server <- function(input,output){
  
#}

#rodando app
#shinyApp(ui,server)
