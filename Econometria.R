# AULA 01 #


#apagar os objetos do ambiente
rm(list=ls()) 

#liberar/organizar memória
gc() 

#evitar notação científica
options(scipen=9999) 

#permite imprimir vários itens
options(max.print=100000)

#colocar um gráfico por vez
par(mfrow=c(1,1))

#Define a pasta de trabalho
setwd("C:/Users/Nicolly Soares/Desktop/ECONOMETRIA")

library(tidyverse)

datainic <- "2003-01-01" ; datafim <- "2026-07-01"

{
  library(readxl)
  
  #################### Carregamento dos arquivos ###############
  PIB_Ac_12_meses<- read_excel("PIB Ac. 12 meses - R$ mi.xlsx")
  pibShiny  <- ts(PIB_Ac_12_meses[, 2],   frequency = 12, start = c(2003, 1), end = c(2026, 7));pibShiny;plot.ts(pibShiny)
  
  ### Criação de Janelas (window)
  pibShiny2026<-window(pibShiny, frequency=12, start=c(2003,1), end=c(2026,07))
  pibShiny2026
  plot.ts(pibShiny2026)
  write.table(pibShiny2026,file = "PIBAC12B.txt")

}

pibac12=read.table("PIBAC12B.txt",head=T); pibac12

#disposição da Base de Dados em Séries Temporais
pib<-ts(pibac12, frequency=12, start=c(2003,1),end=c(2026,07));pib
plot(pib)

#Apresentação Gráfica da Base de Dados - Séries Temporais
## Referência Bibliográfica: Utilizando a Linguagem R - Cap.7
options(scipen=9999)
plot(pib, xlab="Janeiro 2003 até Julho 2026", ylab="R$ Milhões", 
     main="Gráfico 01: Produto Interno Bruto, 2003-2026 (Acum. 12 Meses)")

require(fBasics)

hist(pib,breaks="Sturges")
hist(pib, nclass=50, col="darkgreen")

pib
plot(pib, xlab="Janeiro 2003 a Julho 2026", ylab="R$ Milhões", bty="l", lwd="2", col="blue", 
     main="Gráf.1: Produto Interno Bruto, 2003-2026 (Acum. 12 Meses)")
min(pib)
mean(pib)
median(pib)
max(pib)

summary(pib)

plot(pib, xlab="Janeiro 2003 a Julho 2026", ylab="R$ Milhões", bty="l", lwd="2", col="blue", 
     main="Gráf.1: Produto Interno Bruto, 2003-2025 (Acum. 12 Meses)")
abline(h=mean(pib), col="red", lwd="3")

sd(pib)

Coef.Var.PIB=sd(pib)/mean(pib)*100
Coef.Var.PIB

library(moments)
skewness(pib)
kurtosis(pib)

library(fBasics)
jarqueberaTest(pib)

par(mfrow=c(1,1))
qqnorm(pib, col="blue")
qqline(pib, col="red")
shapiro.test(pib)

pib
n=length(pib)
n
head(pib, n=2)
tail(pib, n=2)
txpib<-((13267298/1502126)-1)*100
txpib; plot(pib)

txpib1<-((pib[n]/pib[1])-1)*100
txpib1

#  Taxa Média Mensal - Aritmética 

txpib2<-((13267298-1502126)/(1502126*283))*100
txpib2

txpib3<-(pib[n]-pib[1])/(pib[1]*n)*100
txpib3

#  Taxa Média Mensal - Geométrica

txpib4<-(((13267298/1502126)^(1/283))-1)*100
txpib4

txpib5<-(((pib[n]/pib[1])^(1/n))-1)*100
txpib5

######  USO DO PACOTE DE DEFLATOR - "deflateBR"  pibR2.txt##########
{
  library(deflateBR)
  #Converter os dados nominais em dados reais:
  #Deflacionando o PIB nominal para gerar o PIB real:
  
  times=seq(as.Date("2003/1/1"), by = "month", length.out = 283); times
  pibR2=deflate(pibac12, nominal_dates = times, real_date = '07/2026', index = 'ipca')
  pibR2
  plot.ts(pibR2)
}

{
  tail(pib, n=2)
  
  VFpibL=pib[n]*(1+(txpib3/100))
  VFpibL
  pib[n]
  
  VFPibG=pib[n]*(1+(txpib5/100))
  VFPibG
}

#==================================================================================
##Taxa de Crescimento - Modelo Semilogaritmicos /Log-Lin##
#Gujarati, Cap 6, Seção 6.6 - Taxa de Crescimento (pg.179)
#==================================================================================
{
  #Gerando a Série de Tendencia (t)##
  t=seq(1,n)
  t
  ##Calculando a Taxa de Crescimento##
  Taxa5=lm(log(pib)~t)
  Taxa5
  #Taxa Crescimento Instantânea - em um ponto no tempo
  Cresc=(Taxa5$coeff[2])
  Cresc
  Cresc.Med=Cresc*100
  Cresc.Med
  
  numeroindice=read.table("numeroindice.txt",head=T)
  
  pibR=(pibac12/numeroindice$numeroindice)*100
  pibR 
  pibR<-ts(pibR,frequency=12,start=c(2003,1))
  pibR
  
  head(pibR, n=2)
  tail(pibR, n=2)
  