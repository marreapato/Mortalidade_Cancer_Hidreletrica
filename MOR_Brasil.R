library(tidyverse)

library(fmsb)

#https://www.inca.gov.br/MortalidadeWeb/pages/Modelo10/consultar.xhtml#panelResultado

##############################################################

#calculando para as mulheres

Wobr <- read.csv2("MulheresBrasil.csv",stringsAsFactors = FALSE)

Wobr <- Wobr[,-c(9)]

colnames(Wobr) <- c("Cancer","Total","30 a 39","40 a 49","50 a 59","60 a 69","70 a 79","80 ou mais")




#utilizando apenas as neoplasias necessarias

Wobr[23,1]="figado"
Wobr[45,1]="mama"
Wobr[51,1]="ovario"
Wobr[49,1]="endometrio"
Wobr[c(24,25),1]="vesicula biliar"
Wobr[c(26),1]="pancreas"
Wobr[c(61),1]="cerebro"
Wobr[c(32),1]="pulmao"
Wobr[c(75:79),1]="leucemia"

####################################################################################################################

Womordf=NULL


Womordf <- as.data.frame(Womordf)

for(i in 1:nrow(Wobr)){
  for(j in 1:ncol(Wobr)){
    Womordf[i,j]=NA
  }
}


for(i in 1:length(Wobr$Cancer)){
  if(Wobr[i,1]==tolower(Wobr[i,1])){
    Womordf[i,]=Wobr[i,]
  }
}

Womordf=na.omit(Womordf) 


Womordf=aggregate(Womordf[,c(2:8)], by=list(Category=Womordf$V1), FUN=sum)
#soma de todos os casos similares

colnames(Womordf) <-colnames(Wobr) 

Total <- Wobr[83,]

Totalmor <- Total[,c(3:8)]

cancerwomenbr <- NULL

womenmordf <- Womordf[,c(3:8)]

for(i in 1:nrow(womenmordf)){
  cancerwomenbr[i]=list(NULL)
}

names(cancerwomenbr) <-Womordf[,1] 

for(i in 1:nrow(womenmordf)){
  for(j in 1:ncol(womenmordf)){
    cancerwomenbr[[i]][j]=womenmordf[i,j]
  }
}


Morbrwomen=NULL

for(i in 1:nrow(Womordf)){
  Morbrwomen[[i]]=list("30 to 39"=list(NULL),"40 to 49"=list(NULL),"50 to 59"=list(NULL),"60 to 69"=list(NULL),"70 to 79"=list(NULL),"80 or more"=list(NULL))
}

names(Morbrwomen) <- Womordf[,1]

morwom <- as.vector(Totalmor[,c(1:6)])

for(i in 1:nrow(womenmordf)){
  for(j in 1:ncol(womenmordf)){
    a=as.numeric(cancerwomenbr[[i]][[j]])
    c=as.numeric(morwom[[j]])-a
    b=as.numeric(sum(cancerwomenbr[[i]]))-a
    expon=as.numeric(sum(cancerwomenbr[[i]]))+c
    d=as.numeric(Total[,2])-expon
    
    Morbrwomen[[i]][[j]]= oddsratio(a,b,c,d,conf.level = 0.95,p.calc.by.independence = T)  
  }
}
#a lista final e chamada Morbrwomen

#############################################################################################################

#calculando para os homens

library(tidyverse)
library(fmsb)
######################################

Menbr <- read.csv2("HomensBrasil.csv")

Menbr <- Menbr[,-c(9)]

colnames(Menbr) <- c("Cancer","Total","30 a 39","40 a 49","50 a 59","60 a 69","70 a 79","80 ou mais")


#apenas as neoplasias necessarias

Menbr$Cancer <- str_replace(Menbr$Cancer,"C81 - DOENCA DE HODGKIN","linfoma")
Menbr[c(71:75),1]="leucemia"
Menbr[c(65,66,68),1]="linfoma n hodgkin"
Menbr[c(19,21),1]="colorretal"
Menbr[c(2,3),1]="lingua"
Menbr[c(23),1]="figado"
Menbr[c(53),1]="bexiga"
Menbr[c(50),1]="rim"
Menbr[c(57),1]="cerebro"
Menbr[c(24,25),1]="vesicula biliar"
Menbr[c(26),1]="pancreas"
Menbr[c(32),1]="pulmao"
Menbr[c(59),1]="tireoide"
Menbr[c(30),1]="laringe"
Menbr[c(36,37),1]="osso"
Menbr[c(70),1]="mieloma"
Menbr[c(38),1]="pele"
Menbr[c(47),1]="prostata"
Menbr[c(17),1]="estomago"
Menbr[c(16),1]="esofago"

Menmordf=NULL

###############################################

Menmordf <- as.data.frame(Menmordf)

for(i in 1:nrow(Menbr)){
  for(j in 1:ncol(Menbr)){
    Menmordf[i,j]=NA
  }
}


for(i in 1:length(Menbr$Cancer)){
  if(Menbr[i,1]==tolower(Menbr[i,1])){
    Menmordf[i,]=Menbr[i,]
  }
}

Menmordf=na.omit(Menmordf) 


Menmordf=aggregate(Menmordf[,c(2:8)], by=list(Category=Menmordf$V1), FUN=sum)
#soma dos casos similares

colnames(Menmordf) <-colnames(Menbr) 

################################################

Total <- Menbr[79,]

Totalmor <- Total[,c(3:8)]

cancermenbr <- NULL

menmordf <- Menmordf[,c(3:8)]

for(i in 1:nrow(Menmordf)){
  cancermenbr[i]=list(NULL)
}

names(cancermenbr) <-Menmordf[,1] 

for(i in 1:nrow(menmordf)){
  for(j in 1:ncol(menmordf)){
    cancermenbr[[i]][j]=menmordf[i,j]
  }
}

################################################

Morbrmen=NULL

for(i in 1:nrow(Menmordf)){
  Morbrmen[[i]]=list("30 to 39"=list(NULL),"40 to 49"=list(NULL),"50 to 59"=list(NULL),"60 to 69"=list(NULL),"70 to 79"=list(NULL),"80 or more"=list(NULL))
}
names(Morbrmen) <- Menmordf[,1]


mor <- as.vector(Totalmor[,c(1:6)])

for(i in 1:nrow(menmordf)){
  for(j in 1:ncol(menmordf)){
    a=as.numeric(cancermenbr[[i]][[j]])
    c=as.numeric(mor[[j]])-a
    b=as.numeric(sum(cancermenbr[[i]]))-a
    expon=as.numeric(sum(cancermenbr[[i]]))+c
    d=as.numeric(Total[,2])-expon
    
    Morbrmen[[i]][[j]]= oddsratio(a,b,c,d,conf.level = 0.95,p.calc.by.independence = T)  
  }
}
#a lista final e chamada Morbrmen
