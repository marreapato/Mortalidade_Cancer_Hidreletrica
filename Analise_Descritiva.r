library(fmsb)
library(stringr)
library(tidyverse)
library(ggthemes)
library(readODS)

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
