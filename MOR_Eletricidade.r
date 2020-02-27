#install.packages("readODS")
library(readODS)
#setwd("C:/Users/User/Desktop/pibic")
novmort <- read.ods("novmort.ods")

#install.packages("fmsb")
library(fmsb)

#install.packages("tidyverse")
library(tidyverse)

############################################################################################################

copi <- novmort
copi <- as.data.frame(copi)
colnames(copi) <- copi[1,]
copi <- copi[-1,]
copi <- copi[,-c(1,2,3,4,5,6,9,10,13,14,15,16,19,20,21,22,23,24,25,26,27,28,29,30,32,33,34,35,36,37)]

copi <- copi[,-c(4)]
##############################################################################################################

#Mor por cancer e eletricidade com classificacao nao periculosa nos trabalhadores sem exposicao

copim <- copi %>% filter(NEOPLASIA=="Sim",copi$`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`=="Sim")
#cancer em trabalhadores expostos

copim2 <- copi %>% filter(NEOPLASIA=="Sim",copi$`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`=="Não"&copi$`TRABALHANDO EM ÁREA PERICULOSA (COM ELETRICIDADE DIRETA OU INDIRETAMENTE) `=="Não")
#cancer em trabalhadores nao expostos e em ocupacoes nao periculosas

cancerel <- table(copim$Nova_classificação_diagnóstico_principal.1)#cancer casos

cancerel <- cancerel[-1]

cancerel <- as.data.frame(cancerel)#electricidade e cancer

cancernel <- table(copim2$Nova_classificação_diagnóstico_principal.1)
#cancer casos sem eletricidade

cancernel <- cancernel[-1]

cancernel <- as.data.frame(cancernel)#sem exposicao a electricidade e com cancer

########################################################################################################

tem=FALSE

cancersemelet=NULL

for(i in 1:length(cancerel$Var1)){
  for(j in 1:length((cancernel$Var1))){
    if(cancerel$Var1[i]==cancernel$Var1[j]){
      cancersemelet[i]=cancernel$Freq[j]
      tem=TRUE
    }
  }
  
  if(tem==FALSE){
    cancersemelet[i]==NA
  }
  
  tem=FALSE
}#espelhando casos de cancer

cancercsele <- cbind(cancerel,cancersemelet)#cancer com e sem exposicao a eletricidade

cancercsele <- na.omit(cancercsele)#tirando valores NA


lista_cancer_sem_eletricidade_peri <- list(NULL)


for(i in 1:length(cancercsele$Var1)){
  lista_cancer_sem_eletricidade_peri[i]=list(NULL)
}

names(lista_cancer_sem_eletricidade_peri)=cancercsele$Var1


for(i in 1:length(cancercsele$Var1)){
  
  a=cancercsele$Freq[i]
  
  c=sum(cancercsele$Freq)-a
  
  b=cancercsele$cancersemelet[i]
  
  
  d=sum(cancernel$Freq)-b
  
  lista_cancer_sem_eletricidade_peri[[i]]= oddsratio(a,b,c,d,conf.level = 0.95,p.calc.by.independence = T)  
  
}

#########################################################################################################################

#eletricidade e sem eletricidade


copi <- novmort
copi <- as.data.frame(copi)
colnames(copi) <- copi[1,]
copi <- copi[-1,]
copi <- copi[,-c(1,2,3,4,5,6,9,10,13,14,15,16,19,20,21,22,23,24,25,26,27,28,29,30,32,33,34,35,36,37)]

copi <- copi[,-c(4)]


copim <- copi %>% filter(NEOPLASIA=="Sim",copi$`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`=="Sim")
#casos de cancer em trabalhadores expostos

copim2 <- copi %>% filter(NEOPLASIA=="Sim",copi$`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`=="Não")
#cancer casos trabalhadores nao expostos 

cancerel <- table(copim$Nova_classificação_diagnóstico_principal.1)#cancer cases

cancerel <- cancerel[-1]

cancerel <- as.data.frame(cancerel)#electricidade e cancer
#############

cancernel <- table(copim2$Nova_classificação_diagnóstico_principal.1)
#cancer casos nao eletricidade

cancernel <- cancernel[-1]

cancernel <- as.data.frame(cancernel)#nao electricidade e cancer

####################
tem=FALSE

cancersemelet=NULL

for(i in 1:length(cancerel$Var1)){
  for(j in 1:length((cancernel$Var1))){
    if(cancerel$Var1[i]==cancernel$Var1[j]){
      cancersemelet[i]=cancernel$Freq[j]
      tem=TRUE
    }
  }
  
  if(tem==FALSE){
    cancersemelet[i]==NA
  }
  
  tem=FALSE
}#espelhando os casos

cancercsele <- cbind(cancerel,cancersemelet)#cancer com e sem relacao a eletricidade

cancercsele <- na.omit(cancercsele)#tirando NA

listacancerGAsemelet <- list(NULL)

for(i in 1:length(cancercsele$Var1)){
  listacancerGAsemelet[i]=list(NULL)
}
names(listacancerGAsemelet)=cancercsele$Var1#assigning names


for(i in 1:length(cancercsele$Var1)){
  
  a=cancercsele$Freq[i]
  
  c=sum(cancercsele$Freq)-a
  
  b=cancercsele$cancersemelet[i]
  
  
  d=sum(cancernel$Freq)-b
  
  listacancerGAsemelet[[i]]= oddsratio(a,b,c,d,conf.level = 0.95,p.calc.by.independence = T)  
  
}

#########################################################################################################################

#criando tabelas
#tabela 1

cases=names(lista_cancer_sem_eletricidade_peri)

confintsup=c(NULL)

confintinf=c(NULL)

estimative=c(NULL)

pvalue=c(NULL)

#################################################################################

for(i in 1:length(lista_cancer_sem_eletricidade_peri)){
  
  confintsup[i]=round(lista_cancer_sem_eletricidade_peri[[i]]$conf.int[2],2)
  
  confintinf[i]=round(lista_cancer_sem_eletricidade_peri[[i]]$conf.int[1],2)
  
  estimative[i]=round(lista_cancer_sem_eletricidade_peri[[i]]$estimate,2)
  
  pvalue[i]=round(lista_cancer_sem_eletricidade_peri[[i]]$p.value,4)
  
}

tabela1=NULL
tabela1=cbind(cases,confintinf,confintsup,estimative,pvalue)
tabela1 <- as.data.frame(tabela1)

tabela1$confintinf <- as.character(tabela1$confintinf)
tabela1$confintsup <- as.character(tabela1$confintsup)

#formatando colunas
tabela1$confintinf<-str_replace_all(tabela1$confintinf,"1.2","1.20")
tabela1$confintinf<-str_replace_all(tabela1$confintinf,"0.1","0.10")
tabela1$confintsup<-str_replace_all(tabela1$confintsup,"5.7","5.70")

tabela1$confint <- paste(tabela1$confintinf,tabela1$confintsup,sep="-")#merging two collumns
tabela1$confint <- paste("(",tabela1$confint,")",sep = "")

tabela1 <- tabela1[,-c(2,3)]

tabela1$pvalue <- as.character(tabela1$pvalue)
tabela1$pvalue<-str_replace_all(tabela1$pvalue,"0.69","0.6900")
tabela1$pvalue<-str_replace_all(tabela1$pvalue,"0.715","0.7150")

tabela1 <- tabela1[,c(1,2,4,3)]

colnames(tabela1) <- c("Casos","Mor","IC-95%","P-valor")

#############################################################################################################

#criando tabela 2


cases=names(listacancerGAsemelet)

confintsup=c(NULL)

confintinf=c(NULL)

estimative=c(NULL)

pvalue=c(NULL)

for(i in 1:length(listacancerGAsemelet)){
  
  confintsup[i]=round(listacancerGAsemelet[[i]]$conf.int[2],2)
  
  confintinf[i]=round(listacancerGAsemelet[[i]]$conf.int[1],2)
  
  estimative[i]=round(listacancerGAsemelet[[i]]$estimate,2)
  
  pvalue[i]=round(listacancerGAsemelet[[i]]$p.value,4)
  
}

tabela2=NULL

Amplitude=confintsup-confintinf

Quantidade=cancercsele$Freq+cancercsele$cancersemelet

tabela2=cbind(cases,Quantidade,confintinf,confintsup,Amplitude,estimative,pvalue)

tabela2 <- as.data.frame(tabela2)



tabela2$confintinf <- as.character(tabela2$confintinf)

tabela2$confintsup <- as.character(tabela2$confintsup)

tabela2$estimative <- as.character(tabela2$estimative)

#formatando colunas
tabela2$confintinf<-str_replace_all(tabela2$confintinf,"0.8","0.80")
tabela2$confintsup<-str_replace_all(tabela2$confintsup,"6.1","6.10")


tabela2$estimative<-str_replace_all(tabela2$estimative,"5.5","5.50")
tabela2$estimative<-str_replace_all(tabela2$estimative,"1.3","1.30")
tabela2$estimative<-str_replace_all(tabela2$estimative,"1.301","1.31")

tabela2$confint <- paste(tabela2$confintinf,tabela2$confintsup,sep="-")#merging two collumns
tabela2$confint <- paste("(",tabela2$confint,")",sep = "")

tabela2 <- tabela2[,-c(4,3)]


tabela2 <- tabela2[,c(1,2,4,6,3,5)]

colnames(tabela2) <- c("Tipo","Casos","Mor","IC-95%","Amplitude-IC","P-valor")

###########################################################################################################################################################

tabela1$Casos <- c("Bexiga","Colorretal","Esôfago","Estômago","Fígado","Laringe","Linfoma não Hodgkin","Mama","Osso","Pâncreas","Pele","Próstata","Pulmão","Rim")

tabela2$Tipo <- c("Bexiga","Colorretal","Esôfago","Estômago","Fígado","Laringe","Linfoma não Hodgkin","Mama","Osso","Pâncreas","Pele","Próstata","Pulmão","Rim")

mean(as.numeric(as.character(tabela2$`Amplitude-IC`)))

median(as.numeric(as.character(tabela2$`Amplitude-IC`)))
