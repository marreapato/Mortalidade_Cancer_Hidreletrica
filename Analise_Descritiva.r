library(fmsb)
library(stringr)
library(tidyverse)
library(ggthemes)
library(readODS)
library(lubridate)
library(xlsx)
library(scales)
#######################################3
#setwd("~/Documents")

novmort <- read_ods("novmort.ods")#

#######################
copi <- novmort

copi <- copi[,-38]

copi$`IDADE DO OBITO` <- as.numeric(copi$`IDADE DO OBITO`)
mean(copi$`IDADE DO OBITO`,na.rm = T)

copi %>% group_by(`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`) %>% summarise(mean(`IDADE DO OBITO`,na.rm = T))
######################

means <- aggregate(`IDADE DO OBITO` ~ `TRABALHANDO DIRETAMENTE COM ELETRICIDADE`,data = copi,mean)
#Pegando as medias de acordo com a exposicao

means$`IDADE DO OBITO` <- round(means$`IDADE DO OBITO`,0)

library(ggthemes)

attach(copi)

ggplot(subset(copi,!is.na(copi$`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`)),aes(fill=`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`,`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`,`IDADE DO OBITO`))+geom_boxplot()+labs(x="Exposição a eletricidade",y="Idade do óbito",fill="Exposição",title="Exposição a eletricidade de acordo com a idade de óbito")+
  theme_hc()+scale_fill_pander()+theme()+ geom_text(data = means, aes(label =`IDADE DO OBITO` , x=`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`,y = `IDADE DO OBITO` +2))+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                                                                                                                                                                                    shape=13, size=2,show.legend = T)

#Boxplot sem valores NA, e com a idade media de obito plotada no meio


#########################################################################################

copi<-as.tibble(copi)
view(table(copi$SEXO))
summary(copi)#visao geral do banco tirar algumas conclusões
hist(copi$`ANO NASCIMENTO`)
ggplot(copi,aes(SEXO))+geom_bar(aes(fill=SEXO))+geom_text(stat='count',aes(label=paste0(round(..count../sum(..count..)*100, 1),"%")))+labs(x="Sexo",y="Quantidade",fill="Sexo",size="horsepower")+theme_stata()
#O GRAFICO DE CIMA OK
ggplot(copi,aes(`CLASSE PROFISIONAL`,fill=SEXO,))+geom_bar(alpha=0.65,position="dodge")+coord_flip()+
  theme_economist_white()+labs(y="Quantidade",x="Classe Profissional/Sexo")+geom_text(stat='count',vjust=0.5, position = position_dodge(width = .9),aes(label=paste0(round(..count../sum(..count..)*100, 1), 
                                                                                                                                                                                                                                                                                                         "%")))
#GRAFICO MELHOR ACIMA
ggplot(copi,aes(`PROF CATEGORIZADA`,fill=SEXO,))+geom_bar(alpha=0.65,position="dodge")+coord_flip()+
  theme_economist_white()+labs(y="Quantidade",x="Categoria Profissional/Sexo")+
  geom_text(stat='count',vjust=0.5, position = position_dodge(width = .9),aes(label=paste0(round(..count../sum(..count..)*100, 1), 
                                                                                           "%")))                                                                                                                                                                                                            
#GRAFICO MELHOR ACIMA
#vjust = -0.5 E SIZE PARA TAMANHO
ggplot(copi,aes(copi$`PERIODO OBITO`,fill=SEXO))+geom_bar(alpha=0.65,position="dodge")+
  theme_economist_white()+labs(y="Quantidade",x="Período do óbito",fill="Sexo")+geom_text(stat='count',position = position_dodge(width = .9),
                                                                                          aes(label=paste0(round(..count../sum(..count..)*100, 1),"%")))
#grafico bom acima                                                                                                                                                                        
ggplot(copi,aes(copi$`PERIODO OBITO`,fill=`PROF CATEGORIZADA`,y=((..count..)/sum(..count..))))+geom_bar(alpha=0.65,position="dodge")+
  theme_stata()+labs(y="Proporção",x="Período do óbito",fill="Profissão categorizada")+scale_fill_manual(values = c("green", "red", "darkblue","purple", "gold"))+geom_text(stat='count',vjust = -0.5,position = position_dodge(width = .9),aes(label=paste0(round(..count../sum(..count..)*100, 1),"%")))
#ver total de pessoal de adm e man

View(table(copi$`PROF CATEGORIZADA`))#olhe a quantidade do pessoal de adm e man

ggplot(copi,aes(`PROF CATEGORIZADA`,fill=`PROF CATEGORIZADA`,y=((..count..)/sum(..count..))))+geom_bar(alpha=0.65)+theme_stata()+
  scale_fill_manual(values = c("green", "red", "darkblue","purple", "gold"))+labs(fill="Profissão categorizada",x="Profissão categorizada",y="Proporção")+geom_text(stat='count', vjust = -0.5,
                                                                                                                                                                    aes(label=paste0(round(..count../sum(..count..)*100, 1), 
                                                                                                                                                                                     "%")))




ggplot(copi,aes(`FAIXA ETARIA OBITO`,fill=`PROF CATEGORIZADA`,y=((..count..)/sum(..count..))))+geom_bar(position = "dodge",alpha=0.7)+labs(y="Proporção",x="Faixa etária do óbito",fill="Profissão categorizada")+theme_stata()+scale_fill_manual(values = c("green", "red", "darkblue","purple", "gold"))+geom_text(stat='count',vjust = -0.5, position = position_dodge(width = .9),
                                                                                                                                                                                                                                                                                                                     aes(label=paste0(round(..count../sum(..count..)*100, 1), 
                                                                                                                                                                                                                                                                                                                                      "%")))

#grafico acima faixa etaria do obito

############################################################################################################

#Grafico sobre os tipos de cancer

copi <- novmort

copi <- copi[,-38]

cancer<-copi %>% group_by(Nova_classificação_diagnóstico_principal,SEXO)  

cancer<-table(cancer$Nova_classificação_diagnóstico_principal,cancer$SEXO)


cancer<-as.data.frame(cancer)

cancer$Var1 <- as.character(cancer$Var1)


cancer<-cancer %>% filter(Freq>=2)

cancer <- na.omit(cancer)

for( i in 1:ncol(cancer)){
  
  if(is.numeric(cancer[,i])==FALSE){
    for(j in 1:nrow(cancer)){
      cancer[j,i] = str_to_title(cancer[j,i])
    }
  }
}#todas as celulas em title case


view(cancer)

for(i in 1:length(cancer$Var1)){
  
  if(cancer$Var1[i]=="Prostata"){
    cancer$Var1[i]="Próstata"
  }else if(cancer$Var1[i]=="Pulmao"){
    cancer$Var1[i]="Pulmão"
  }else if(cancer$Var1[i]=="Cerebro"){
    cancer$Var1[i]="Cérebro"
  }else if(cancer$Var1[i]=="Estomago"){
    cancer$Var1[i]="Estômago"
  }else if(cancer$Var1[i]=="Ovario"){
    cancer$Var1[i]="Ovário"
  }else if(cancer$Var1[i]=="Lingua"){
    cancer$Var1[i]="Língua"
  }else if(cancer$Var1[i]=="Figado"){
    cancer$Var1[i]="Fígado"
  }else if(cancer$Var1[i]=="Pancreas"){
    cancer$Var1[i]="Pâncreas"
  }
}

view(cancer)

#########################################################################
ggplot(cancer,aes(x=reorder(cancer$Var1,-Freq),y=Freq,fill=cancer$Var2))+geom_col(position = "dodge")+theme_clean()+
  labs(fill="Sexo",title = "Diagnóstico de neoplasia com mais de um óbito registrado",x="Tipo de câncer",y="Óbitos")+#scale_x_discrete(labels=c())+
  geom_text(aes(label=Freq), size = 4, 
            color = "black",
            position = position_dodge(width = 0.8),
            vjust =0.1)+coord_flip()#+ 
#scale_fill_gradient2(low = "pink", 
#high = "darkred") 




##############################################################

#Outra maneira de fazer

copi <- novmort

copi <- copi[,-38]

cancer<-copi %>% group_by(Nova_classificação_diagnóstico_principal,SEXO)  

cancer<-table(cancer$Nova_classificação_diagnóstico_principal,cancer$SEXO)


cancer<-as.data.frame(cancer)

cancer$Var1 <- as.character(cancer$Var1)


cancer<-cancer %>% filter(Freq>=2)

cancer <- na.omit(cancer)

for( i in 1:ncol(cancer)){
  
  if(is.numeric(cancer[,i])==FALSE){
    for(j in 1:nrow(cancer)){
      cancer[j,i] = str_to_title(cancer[j,i])
    }
  }
}#Title case de novo

view(cancer)

for(i in 1:length(cancer$Var1)){
  
  if(cancer$Var1[i]=="Prostata"){
    cancer$Var1[i]="Próstata"
  }else if(cancer$Var1[i]=="Pulmao"){
    cancer$Var1[i]="Pulmão"
  }else if(cancer$Var1[i]=="Cerebro"){
    cancer$Var1[i]="Cérebro"
  }else if(cancer$Var1[i]=="Estomago"){
    cancer$Var1[i]="Estômago"
  }else if(cancer$Var1[i]=="Ovario"){
    cancer$Var1[i]="Ovário"
  }else if(cancer$Var1[i]=="Lingua"){
    cancer$Var1[i]="Língua"
  }else if(cancer$Var1[i]=="Figado"){
    cancer$Var1[i]="Fígado"
  }else if(cancer$Var1[i]=="Pancreas"){
    cancer$Var1[i]="Pâncreas"
  }
}

view(cancer)

#########################################################################

#Grafico com tipos de cancer na diagonal

ggplot(cancer,aes(x=reorder(cancer$Var1,-Freq),y=Freq,fill=cancer$Var2))+geom_col(position = "dodge")+theme_clean()+
  labs(fill="Sexo",title = "Diagnóstico de neoplasia com mais de um óbito registrado",x="Tipo de câncer",y="Óbitos")+#scale_x_discrete(labels=c())+
  geom_text(aes(label=Freq), size = 4, 
            color = "black",
            position = position_dodge(width = 0.9),
            vjust =0)+ theme(axis.text.x = element_text( 
              size = 12, angle = 45)) #+ 
#scale_fill_gradient2(low = "pink", 
#high = "darkred") 

#########################################################################

#Para todos casos


copi <- novmort

copi <- copi[,-38]

cancer<-copi %>% group_by(Nova_classificação_diagnóstico_principal,SEXO)  

cancer<-table(cancer$Nova_classificação_diagnóstico_principal,cancer$SEXO)


cancer<-as.data.frame(cancer)

cancer$Var1 <- as.character(cancer$Var1)


cancer<-cancer %>% filter(Freq>=1)

cancer <- na.omit(cancer)

for( i in 1:ncol(cancer)){
  
  if(is.numeric(cancer[,i])==FALSE){
    for(j in 1:nrow(cancer)){
      cancer[j,i] = str_to_title(cancer[j,i])
    }
  }
}#title case

view(cancer)

for(i in 1:length(cancer$Var1)){
  
  if(cancer$Var1[i]=="Prostata"){
    cancer$Var1[i]="Próstata"
  }else if(cancer$Var1[i]=="Pulmao"){
    cancer$Var1[i]="Pulmão"
  }else if(cancer$Var1[i]=="Cerebro"){
    cancer$Var1[i]="Cérebro"
  }else if(cancer$Var1[i]=="Estomago"){
    cancer$Var1[i]="Estômago"
  }else if(cancer$Var1[i]=="Ovario"){
    cancer$Var1[i]="Ovário"
  }else if(cancer$Var1[i]=="Lingua"){
    cancer$Var1[i]="Língua"
  }else if(cancer$Var1[i]=="Figado"){
    cancer$Var1[i]="Fígado"
  }else if(cancer$Var1[i]=="Pancreas"){
    cancer$Var1[i]="Pâncreas"
  }else if(cancer$Var1[i]=="Esofago"){
    cancer$Var1[i]="Esôfago"
  }
}


#########################################################################
ggplot(cancer,aes(x=reorder(cancer$Var1,-Freq),y=Freq,fill=cancer$Var2))+geom_col(position = "dodge")+theme_clean()+
  labs(fill="Sexo",title = "Diagnóstico de neoplasia",x="Tipo de câncer",y="Óbitos")+#scale_x_discrete(labels=c())+
  geom_text(aes(label=Freq), size = 4, 
            color = "black",
            position = position_dodge(width = 0.9),
            vjust =0)+ theme(axis.text.x = element_text(size = 3))+coord_flip()
