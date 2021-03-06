#shiny app

library(shiny)
library(fmsb)
library(tidyverse)
library(ggthemes)
#setwd("~/Desktop/app_cancer/cancer_app")
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

#fazendo tabelas paras as faixas etárias

#data frame das idades dos homens
faixas_table=data_frame("00 a 04 anos","05 a 09 anos","10 a 14 anos","15 a 19 anos","20 a 29 anos","30 a 39 anos","40 a 49 anos","50 a 59 anos","60 a 69 anos","70 a 79 anos","80 anos ou mais")
faixas_table=faixas_table[-1,]
length(mor_regioesh)

#lista para guardar data frames

faixah_region=NULL
for(i in 1:length(mor_regioesh)){
    faixah_region[i]=list(NULL)
}

names(faixah_region)=names(mor_regioesh)

#lista de data frames de faixa etaria


for(i in 1:length(mor_regioesh)){#acessando regiões
    
    for(j in 1:length(mor_regioesh[[i]])){#acessando faixas etarias para pegar mor
        
        faixas_table[1,j]=round(as.numeric(as.character(mor_regioesh[[i]][[j]]$estimate)),2)#a região i, estará na faixa etária j, para pegar a mor
        
    }
    
    faixah_region[[i]]=faixas_table
    
}#cada elemento de faixah region e um data frame

##################################################################################

#data frame das idades das mulheres
faixas_table=data_frame("00 a 04 anos","05 a 09 anos","10 a 14 anos","15 a 19 anos","20 a 29 anos","30 a 39 anos","40 a 49 anos","50 a 59 anos","60 a 69 anos","70 a 79 anos","80 anos ou mais")
faixas_table=faixas_table[-1,]
length(mor_regioesm)

#lista para guardar data frames

faixam_region=NULL
for(i in 1:length(mor_regioesm)){
    faixam_region[i]=list(NULL)
}

names(faixam_region)=names(mor_regioesm)

#lista de data frames de faixa etaria


for(i in 1:length(mor_regioesm)){#acessando regiões
    
    for(j in 1:length(mor_regioesm[[i]])){#acessando faixas etarias para pegar mor
        
        faixas_table[1,j]=round(as.numeric(as.character(mor_regioesm[[i]][[j]]$estimate)),2)#a região i, estará na faixa etária j, para pegar a mor
        
    }
    
    faixam_region[[i]]=faixas_table
    
}#cada elemento de faixam region e um data frame


data=data.frame("Sexo"=c("Masculino","Feminino"),"Total de casos de câncer"=c(hbrasil$Total[6],mbrasil$Total[6]),"Total Sul"=c(hbrasil$Total[5],mbrasil$Total[5]),"Total Sudeste"=c(hbrasil$Total[4],mbrasil$Total[4]),"Total Norte"=c(hbrasil$Total[3],mbrasil$Total[3]),"Total Nordeste"=c(hbrasil$Total[2],mbrasil$Total[2]),"Total Centro oeste"=c(hbrasil$Total[1],mbrasil$Total[1]))
#definindo a logica do servidor requerida para realizar evento

server <- function(input,output){
    
    
    output$selected_var <- renderText(paste("Você esta analizando as chances de óbito por câncer da população do sexo",input$sex))
    output$new_selected_var <- renderText(paste("\nRegião:",input$reg))
    output$faixa <- renderText("As chances de óbito por faixa etária são:")
    
    
    
    output$morh <- renderTable({
        if (input$reg == 'centro oeste'&&input$sex=='masculino') {
            return(faixah_region$cent_o)
            
            
        }else if(input$reg == 'sul'&&input$sex=='masculino'){
            return(faixah_region$sulh)
            
            
        }else if(input$reg == 'sudeste'&&input$sex=='masculino'){
            return(faixah_region$sudh)
            
            
        }else if(input$reg == 'norte'&&input$sex=='masculino'){
            return(faixah_region$north)
            
        }else if(input$reg == 'nordeste'&&input$sex=='masculino'){
            return(faixah_region$nordh)
            
            
        } else if (input$reg == 'centro oeste'&&input$sex=='feminino') {
            return(faixam_region$cent_o)
            
        }else if(input$reg == 'sul'&&input$sex=='feminino'){
            return(faixam_region$sulh)
            
        }else if(input$reg == 'sudeste'&&input$sex=='feminino'){
            return(faixam_region$sudh)
            
            
        }else if(input$reg == 'norte'&&input$sex=='feminino'){
            return(faixam_region$north)
            
            
        }else if(input$reg == 'nordeste'&&input$sex=='feminino'){
            return(faixam_region$nordh)
            
            
        }
        
    })
    output$myplot <- renderPlot({
        ggplot(data,aes(data$Sexo,data$Total.de.casos.de.câncer,fill=data$Sexo)) + 
            geom_col()+labs(title="Total de óbitos registrados pelo INCA no Brasil.",x="Sexo",y="Quantidade",fill="Sexo")+scale_fill_manual(values = c("#33FF83","#F5FF60"))+theme_minimal()})
    
    
    
    # output$mor <- renderText(paste("mor= ",round(mor_regioesh$cent_o$`00 a 04`$estimate,2)))
    
}
