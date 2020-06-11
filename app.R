#shiny app

library(shiny)
library(fmsb)
library(tidyverse)
library(ggthemes)

setwd('/home/lucas/Desktop/app_cancer')

#Rodando banco dos homens e arrumando

hbrasil <- read.csv2("hbrasil.csv",header = F)

hbrasil <- hbrasil[-c(1),-c(14)]
colnames(hbrasil)<-c("Regiões","Total","00 a 04","05 a 09","10 a 14","15 a 19","20 a 29","30 a 39","40 a 49","50 a 59","60 a 69","70 a 79","80 ou mais")
hbrasil <- hbrasil[-c(1),]


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

#cálculo da MOR por região e faixa etaria

mor_regioesh=list()

for(i in 1:5){
  mor_regioesh[i]=NA
}

for(i in 1:5){
  names(mor_regioesh)[i]=names(regioesh)[i]
}


#?oddsratio
#a=mora na regiao escolhida e tem uma faixa x de anos
#b=mora na regiao escolhida e nao tem faixa x anos
##c=n mora na regiao escolhida e tem faixa x de anos
##d=n mora na regiao escolhida e nao tem faixa x de anos

#calculando para homens em cada regiao do brasil por faixa etaria

#View(regioesh[["cent_o"]])
faixas=c("00 a 04","05 a 09","10 a 14","15 a 19","20 a 29","30 a 39","40 a 49","50 a 59","60 a 69","70 a 79","80 ou mais")


for (j in 1:length(regioesh)) {
  
  for(i in 3:ncol(regioesh[[j]])){
    
    a=as.numeric(as.character(regioesh[[j]][1,i]))
    
    b=as.numeric(as.character(regioesh[[j]][1,2]))-a
    
    c=as.numeric(as.character(regioesh[[j]][2,i]))-a#total menos a
    
    d=as.numeric(as.character(regioesh[[j]][2,2]))-(as.numeric(as.character(regioesh[[j]][1,2]))+c)
    
    
    mor_regioesh[[j]][i-2]=list(oddsratio(a,b,c,d,p.calc.by.independence = T,conf.level = 0.95))
    
    #salvando os resultados
  }
  names(mor_regioesh[[j]])=faixas
}#calculando a MOR de todas as regioes para os homens em todas faixas etarias

#########################################################################################################

#Rodando banco das mulheres e arrumando

mbrasil <- read.csv2("mbrasil.csv",header = F)

mbrasil <- mbrasil[-c(1),-c(14)]
colnames(mbrasil)<-c("Regiões","Total","00 a 04","05 a 09","10 a 14","15 a 19","20 a 29","30 a 39","40 a 49","50 a 59","60 a 69","70 a 79","80 ou mais")
mbrasil <- mbrasil[-c(1),]


for(i in 2:ncol(mbrasil)){
  
  
  mbrasil[,i] <- as.numeric(as.character(mbrasil[,i]))
  
  
}

#Criando uma lista com um data frame para cada região(para os homens)


regioesm=list("cent_o"=data_frame(),"nordh"=data_frame(),"north"=data_frame(),"sudh"=data_frame(),"sulh"=data_frame())

#str(hbrasil)
#levels(hbrasil$Regiões)

for(i in 1:(nrow(mbrasil)-1)){
  regioesm[[i]]=mbrasil[i,]
  regioesm[[i]]=rbind(regioesm[[i]],mbrasil[6,])
}

#cálculo da MOR por região e faixa etaria

mor_regioesm=list()

for(i in 1:5){
  mor_regioesm[i]=NA
}

for(i in 1:5){
  names(mor_regioesm)[i]=names(regioesm)[i]
}


#?oddsratio
#a=mora na regiao escolhida e tem uma faixa x de anos
#b=mora na regiao escolhida e nao tem faixa x anos
##c=n mora na regiao escolhida e tem faixa x de anos
##d=n mora na regiao escolhida e nao tem faixa x de anos

#calculando para mulheres em cada regiao do brasil por faixa etaria

#View(regioesh[["cent_o"]])
faixas=c("00 a 04","05 a 09","10 a 14","15 a 19","20 a 29","30 a 39","40 a 49","50 a 59","60 a 69","70 a 79","80 ou mais")


for (j in 1:length(regioesm)) {
  
  for(i in 3:ncol(regioesm[[j]])){
    
    a=as.numeric(as.character(regioesm[[j]][1,i]))
    
    b=as.numeric(as.character(regioesm[[j]][1,2]))-a
    
    c=as.numeric(as.character(regioesm[[j]][2,i]))-a#total menos a
    
    d=as.numeric(as.character(regioesm[[j]][2,2]))-(as.numeric(as.character(regioesm[[j]][1,2]))+c)
    
    
    mor_regioesm[[j]][i-2]=list(oddsratio(a,b,c,d,p.calc.by.independence = T,conf.level = 0.95))
    
    #salvando os resultados
  }
  names(mor_regioesm[[j]])=faixas
}#calculando a MOR de todas as regioes para os homens em todas faixas etarias

#####################################################################################################################

#aplicativo

#definindo interface

data=data.frame("Sexo"=c("Masculino","Feminino"),"Total de casos de câncer"=c(hbrasil$Total[6],mbrasil$Total[6]),"Total Sul"=c(hbrasil$Total[5],mbrasil$Total[5]),"Total Sudeste"=c(hbrasil$Total[4],mbrasil$Total[4]),"Total Norte"=c(hbrasil$Total[3],mbrasil$Total[3]),"Total Nordeste"=c(hbrasil$Total[2],mbrasil$Total[2]),"Total Centro oeste"=c(hbrasil$Total[1],mbrasil$Total[1]))

ui <- fluidPage(titlePanel("Painel das chances de mortalidade por câncer no Brasil."),
                sidebarLayout(sidebarPanel(helpText(h3("Selecione o sexo da parcela da população brasileira que deseja estudar"))
                                           ,selectInput("sex","Sexo",c("Masculino"="masculino","Feminino"="feminino")),selectInput("reg","Região",c("Norte"="norte","Nordeste"="nordeste","Sudeste"="sudeste","Sul"="sul","Centro Oeste"="centro oeste")))
                              ,mainPanel(h2("MOR",align="center"),
                                         p("Aplicativo shiny realizado para monitorar a MOR por região da população brasileira por câncer(calculada no período entre 1985 até 2018)."),textOutput("selected_var"),textOutput("new_selected_var"),plotOutput("myplot"),textOutput("faixa"),
                                         htmlOutput("mor")
                                         
                              )
                )
)

#definindo a logica do servidor requerida para realizar evento

server <- function(input,output){
  
  
  output$selected_var <- renderText(paste("Você esta analizando as chances de óbito por câncer da população do sexo",input$sex))
  output$new_selected_var <- renderText(paste("\nRegião:",input$reg))
  output$faixa <- renderText("As chances de óbito por faixa etária são:")
 
  
  output$mor <- renderText({
    if (input$reg == 'centro oeste'&&input$sex=='masculino') {
      return(paste("De 00 a 04 anos:", round(mor_regioesh$cent_o$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesh$cent_o$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesh$cent_o$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesh$cent_o$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesh$cent_o$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesh$cent_o$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesh$cent_o$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesh$cent_o$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesh$cent_o$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesh$cent_o$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesh$cent_o$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'sul'&&input$sex=='masculino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesh$sulh$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesh$sulh$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesh$sulh$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesh$sulh$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesh$sulh$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesh$sulh$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesh$sulh$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesh$sulh$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesh$sulh$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesh$sulh$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesh$sulh$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'sudeste'&&input$sex=='masculino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesh$sudh$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesh$sudh$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesh$sudh$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesh$sudh$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesh$sudh$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesh$sudh$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesh$sudh$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesh$sudh$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesh$sudh$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesh$sudh$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesh$sudh$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'norte'&&input$sex=='masculino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesh$north$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesh$north$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesh$north$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesh$north$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesh$north$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesh$north$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesh$north$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesh$north$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesh$north$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesh$north$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesh$north$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'nordeste'&&input$sex=='masculino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesh$nordh$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesh$nordh$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesh$nordh$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesh$nordh$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesh$nordh$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesh$nordh$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesh$nordh$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesh$nordh$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesh$nordh$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesh$nordh$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesh$nordh$`80 ou mais`$estimate,2)))
      
      
      
    } else if (input$reg == 'centro oeste'&&input$sex=='feminino') {
      return(paste("De 00 a 04 anos:", round(mor_regioesm$cent_o$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesm$cent_o$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesm$cent_o$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesm$cent_o$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesm$cent_o$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesm$cent_o$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesm$cent_o$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesm$cent_o$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesm$cent_o$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesm$cent_o$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesm$cent_o$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'sul'&&input$sex=='feminino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesm$sulh$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesm$sulh$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesm$sulh$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesm$sulh$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesm$sulh$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesm$sulh$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesm$sulh$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesm$sulh$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesm$sulh$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesm$sulh$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesm$sulh$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'sudeste'&&input$sex=='feminino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesm$sudh$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesm$sudh$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesm$sudh$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesm$sudh$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesm$sudh$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesm$sudh$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesm$sudh$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesm$sudh$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesm$sudh$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesm$sudh$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesm$sudh$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'norte'&&input$sex=='feminino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesm$north$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesm$north$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesm$north$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesm$north$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesm$north$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesm$north$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesm$north$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesm$north$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesm$north$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesm$north$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesm$north$`80 ou mais`$estimate,2)))
      
      
      
    }else if(input$reg == 'nordeste'&&input$sex=='feminino'){
      return(paste("De 00 a 04 anos:", round(mor_regioesm$nordh$`00 a 04`$estimate,2),
                   "<br> De 05 a 09 anos:", round(mor_regioesm$nordh$`05 a 09`$estimate,2),
                   "<br> De 10 a 14 anos:", round(mor_regioesm$nordh$`10 a 14`$estimate,2),
                   "<br> De 15 a 19 anos:", round(mor_regioesm$nordh$`15 a 19`$estimate,2),
                   
                   "<br> De 20 a 29 anos:", round(mor_regioesm$nordh$`20 a 29`$estimate,2),
                   
                   "<br> De 30 a 39 anos:", round(mor_regioesm$nordh$`30 a 39`$estimate,2),
                   "<br> De 40 a 49 anos:",round(mor_regioesm$nordh$`40 a 49`$estimate,2),
                   "<br> De 50 a 59 anos:", round(mor_regioesm$nordh$`50 a 59`$estimate,2),
                   "<br> De 60 a 69 anos:", round(mor_regioesm$nordh$`60 a 69`$estimate,2),
                   "<br> De 70 a 79 anos:", round(mor_regioesm$nordh$`70 a 79`$estimate,2),
                   "<br> De 80 ou mais:", round(mor_regioesm$nordh$`80 ou mais`$estimate,2)))
      
      
      
    }
    
  })
  output$myplot <- renderPlot({
    ggplot(data,aes(data$Sexo,data$Total.de.casos.de.câncer,fill=data$Sexo)) + 
      geom_col()+labs(title="Total de óbitos registrados pelo INCA no Brasil.",x="Sexo",y="Quantidade",fill="Sexo")+scale_fill_fivethirtyeight()+theme_minimal()})
  
  
  
  # output$mor <- renderText(paste("mor= ",round(mor_regioesh$cent_o$`00 a 04`$estimate,2)))
  
}

#rodando app
shinyApp(ui,server)

#?selectInput()
