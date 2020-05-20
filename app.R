#shiny app

library(shiny)
library(fmsb)
library(tidyverse)

setwd('/home/lucas/Desktop/app_cancer')

#Rodando banco dos homens e arrumando

hbrasil <- read.csv2("hbrasil.csv",header = F)

hbrasil <- hbrasil[-c(1),-c(14)]
colnames(hbrasil)<-c("Regiões","Total","00 a 04","05 a 09","10 a 14","15 a 19","20 a 29","30 a 39","40 a 49","50 a 59","60 a 69","70 a 79","80 ou mais")
hbrasil <- hbrasil[-c(1),]

#cálculo da MOR por região e faixa etaria

#?oddsratio
#mora na regiao escolhida e tem uma faixa x de anos
#mora na regiao escolhida e nao tem faixa x anos
##n mora na regiao escolhida e tem faixa x de anos
##n mora na regiao escolhida e nao tem faixa x de anos

for(i in 2:ncol(hbrasil)){
  
  
  hbrasil[,i] <- as.numeric(as.character(hbrasil[,i]))
  
  
}

#Criando uma lista com um data frame para cada região(para os homens)


regioesh=list("cent_o"=data_frame(),"nordh"=data_frame(),"north"=data_frame(),"sudh"=data_frame(),"sulh"=data_frame())

#str(hbrasil)
#levels(hbrasil$Regiões)

for(i in 1:(nrow(hbrasil)-1)){
  regioesh[[i]]=hbrasil[i,]
  regioesh[[i]]=rbind(regioesh[[i]],hbrasil[6,])
}


#definindo interface

ui <- fluidPage(titlePanel("Painel das chances de mortalidade por câncer no Brasil."),
                sidebarLayout(sidebarPanel(helpText(h3("Selecione o sexo da parcela da população brasileira que deseja estudar")),selectInput("sex","Sexo",c("Masculino"="masculino","Feminino"="feminino"))),mainPanel(h2("MOR",align="center"),
                                                                                                                                                                                                                        p("Aplicativo shiny realizado para monitorar a MOR por região da população brasileira por câncer(calculada no período entre 1985 até 2018)."),textOutput("selected_var"))))

#definindo a logica do servidor requerida para realizar evento

server <- function(input,output){
  
  output$selected_var <- renderText(paste("Você esta analizando a população do sexo",input$sex))
  
}

#rodando app
shinyApp(ui,server)

#?selectInput()
