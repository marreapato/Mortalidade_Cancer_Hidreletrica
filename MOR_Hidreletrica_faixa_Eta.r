#install.packages("readODS")
library(readODS)
library(fmsb)#

library(tidyverse)
novmort <- read.ods("novmort.ods")#

copi <- novmort
copi <- as.data.frame(copi)
colnames(copi) <- copi[1,]
copi <- copi[-1,]
copi <- copi[,-c(1,2,3,4,5,6,7,8,9,10,13,14,15,16,19,20,21,22,23,24,25,26,27,28,29,30,32,33,34,35,36,37)]
copi <- copi[,-c(2,4,5)]
copi <- copi[,-c(6,8,4)]

#############################################################
#MOR por cancer sexo e faixa etaria de obito na hidreletrica


copim <- copi %>% filter(SEXO=="M"&NEOPLASIA=="Sim")
#Homens

#Vetor de idade dos homens
agemen <- colSums(table(copim$Nova_classificação_diagnóstico_principal.1,copim$`FAIXA ETARIA OBITO`))
agemen

cancermen<-table(copim$Nova_classificação_diagnóstico_principal.1)#n of cancer cases

canceridademen<-table(copim$Nova_classificação_diagnóstico_principal.1,copim$`FAIXA ETARIA OBITO`)
#n de casos por idade

canceridademen <- canceridademen[-1,]#primeira linha invalida

canceridademen <- as.data.frame(canceridademen)

canceridademen <- spread(canceridademen,key = Var2,value = Freq)

mencancernames <- canceridademen[,1]

canceridademen <- canceridademen[,-1]

canceridademen

#table(copim$Nova_classificação_diagnóstico_principal.1)

#######################################################################################

length(mencancernames)

listacancermen <- list(NULL)

for(i in 1:length(mencancernames)){
  listacancermen[i]=list(NULL)
}
names(listacancermen)=mencancernames


for(i in 1:length(mencancernames)){
  for(j in 1:length(agemen)){
    listacancermen[[i]][j]=canceridademen[i,j]
    
  }
}#lista de cancer homens


#lista da mor
listaMORMEN=list(NULL)
for(i in 1:length(mencancernames)){
  listaMORMEN[[i]] = list("30 to 39"=list(NULL),"40 to 49"=list(NULL),"50 to 59"=list(NULL),"60 to 69"=list(NULL),"70 to 79"=list(NULL),"80 or more"=list(NULL))
}

names(listaMORMEN)=mencancernames


for(i in 1:length(mencancernames)){
  for(j in 1:length(agemen)){
    a=listacancermen[[i]][j]
    c=agemen[j]-a
    b=sum(listacancermen[[i]])-a
    expon=sum(listacancermen[[i]])+c
    d=length(copim$NEOPLASIA)-expon
    
    listaMORMEN[[i]][[j]]= oddsratio(a,b,c,d,conf.level = 0.95,p.calc.by.independence = T)  
  }
}
#lista da MOR em homens


#########################################################################################################################

#calculating for women

#tidying data
copi <- novmort
copi <- as.data.frame(copi)
colnames(copi) <- copi[1,]
copi <- copi[-1,]
copi <- copi[,-c(1,2,3,4,5,6,7,8,9,10,13,14,15,16,19,20,21,22,23,24,25,26,27,28,29,30,32,33,34,35,36,37)]
copi <- copi[,-c(2,4,5)]
copi <- copi[,-c(6,8,4)]

#Mor per cancer and sex hydroeletric
novmort<-as.data.frame(novmort)
copim <- copi %>% filter(SEXO=="F"&NEOPLASIA=="Sim")
#women

#women age vector
agewomen <- colSums(table(copim$Nova_classificação_diagnóstico_principal.1,copim$`FAIXA ETARIA OBITO`))
agewomen

cancerwomen<-table(copim$Nova_classificação_diagnóstico_principal.1)#women cancer vector

canceridadewomen<-table(copim$Nova_classificação_diagnóstico_principal.1,copim$`FAIXA ETARIA OBITO`)
canceridadewomen#n of cancer cases per age

canceridadewomen <- as.data.frame(canceridadewomen)

canceridadewomen <- spread(canceridadewomen,key = Var2,value = Freq)#spreading data

womencancernames <- canceridadewomen[,1]#cancer names vector


canceridadewomen <- canceridadewomen[,-c(1)]
womencancernames



canceridadewomen#mirrowed with womencancer names vector

#table(copim$Nova_classificação_diagnóstico_principal.1)

#algorithm


length(cancerwomen)
#in the following vectors every pos is a age;
#a=vetor[i]#30 a 39 com doenca	#exposure non healthy#cancer
#c=ageman[i]-a	#30 a 39 sem deoenca #exposure but healthy#agenam
#b=vetordecancer[i]-a#que n tem 30 a 39 e tem doenca #non exposure non healthy
#expon=vetordecancer[i]+c #todas as exposicoes com e sem doenca e nao exposicoes com a doenca all the exposures and non healthy non exposure
#d=1130-expon#que n tem 30 a 39 e n tem a doenca #non exposure and healthy general population-exposure and non healthy non exposure

listacancerwomen <- list(NULL)
#filling the list
for(i in 1:length(womencancernames)){
  listacancerwomen[i]=list(NULL)
}
names(listacancerwomen)=womencancernames#assigning names


for(i in 1:length(womencancernames)){
  for(j in 1:length(agewomen)){
    listacancerwomen[[i]][j]=canceridadewomen[i,j]
    
  }
}#full women cancer list


#lists down bellow


#lists down bellow
listaMORWOMEN=list(NULL)
for(i in 1:length(womencancernames)){
  listaMORWOMEN[[i]] = list("30 to 39"=list(NULL),"40 to 49"=list(NULL),"50 to 59"=list(NULL),"60 to 69"=list(NULL),"70 to 79"=list(NULL),"80 or more"=list(NULL))
}

names(listaMORWOMEN)=womencancernames#assigning names



#for women
for(i in 1:length(listacancerwomen)){
  for(j in 1:length(agewomen)){
    a=listacancerwomen[[i]][[j]]
    c=agewomen[j]-a
    b=sum(listacancerwomen[[i]])-a
    expon=sum(listacancerwomen[[i]])+c
    d=length(copim$NEOPLASIA)-expon
    
    listaMORWOMEN[[i]][[j]]= oddsratio(a,b,c,d,conf.level = 0.95,p.calc.by.independence = T)  
  }
}
