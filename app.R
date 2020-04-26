#shiny app

library(shiny)

#definindo interface

ui <- fluidPage(titlePanel("Painel das chances de mortalidade por câncer no Brasil."),
                sidebarLayout(sidebarPanel(helpText(h3("Selecione o sexo da parcela da população brasileira que deseja estudar")),selectInput("sex","Sexo",c("Masculino"="masculino","Feminino"="feminino"))),mainPanel(h2("MOR",align="center"),
                                                             p("Aplicativo shiny realizado para monitorar a MOR da população brasileira por câncer(calculada no período entre 1985 até 2019)."),textOutput("selected_var"))))

#definindo a logica do servidor requerida para realizar evento

server <- function(input,output){
  
  output$selected_var <- renderText(paste("Você esta analizando a população do sexo",input$sex))

}

#rodando app
shinyApp(ui,server)

#?selectInput()
