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

#calculando para as mulheres


copi <- novmort
copi <- as.data.frame(copi)
colnames(copi) <- copi[1,]
copi <- copi[-1,]
copi <- copi[,-c(1,2,3,4,5,6,7,8,9,10,13,14,15,16,19,20,21,22,23,24,25,26,27,28,29,30,32,33,34,35,36,37)]
copi <- copi[,-c(2,4,5)]
copi <- copi[,-c(6,8,4)]
###########################


novmort<-as.data.frame(novmort)
copim <- copi %>% filter(SEXO=="F"&NEOPLASIA=="Sim")

#vetor de idades das mulheres
agewomen <- colSums(table(copim$Nova_classificação_diagnóstico_principal.1,copim$`FAIXA ETARIA OBITO`))
agewomen

cancerwomen<-table(copim$Nova_classificação_diagnóstico_principal.1)#women cancer vector

canceridadewomen<-table(copim$Nova_classificação_diagnóstico_principal.1,copim$`FAIXA ETARIA OBITO`)
canceridadewomen#n de casos

canceridadewomen <- as.data.frame(canceridadewomen)

canceridadewomen <- spread(canceridadewomen,key = Var2,value = Freq)

womencancernames <- canceridadewomen[,1]


canceridadewomen <- canceridadewomen[,-c(1)]
womencancernames



canceridadewomen

#table(copim$Nova_classificação_diagnóstico_principal.1)



length(cancerwomen)

listacancerwomen <- list(NULL)

for(i in 1:length(womencancernames)){
  listacancerwomen[i]=list(NULL)
}

names(listacancerwomen)=womencancernames

for(i in 1:length(womencancernames)){
  for(j in 1:length(agewomen)){
    listacancerwomen[[i]][j]=canceridadewomen[i,j]
    
  }
}#lista cancer mulheres


#lista mor abaixo

listaMORWOMEN=list(NULL)
for(i in 1:length(womencancernames)){
  listaMORWOMEN[[i]] = list("30 to 39"=list(NULL),"40 to 49"=list(NULL),"50 to 59"=list(NULL),"60 to 69"=list(NULL),"70 to 79"=list(NULL),"80 or more"=list(NULL))
}

names(listaMORWOMEN)=womencancernames


for(i in 1:length(listacancerwomen)){
  for(j in 1:length(agewomen)){
    a=listacancerwomen[[i]][[j]]
    c=agewomen[j]-a
    b=sum(listacancerwomen[[i]])-a
    expon=sum(listacancerwomen[[i]])+c
    d=length(copim$NEOPLASIA)-expon
    
    listaMORWOMEN[[i]][[j]]= oddsratio(a,b,c,d,conf.level = 0.95,p.calc.by.independence = T)  
  }
}#Lista MOR mulheres
