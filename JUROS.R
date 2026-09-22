#UNIVERSIDADE FEDERAL DA PARAÍBA
#DEPARTAMENTO DE ECONOMIA
#CURSO DE ECONOMETRIA
#PROF.DR.SINÉZIO FERNANDES MAIA
#AULA01 - ESTATÍSTICA DESCRITIVA -

## Video de Apresentação do R: https://youtu.be/mrCaGBuXPRA

# AULA 01 #
rm(list=ls())
gc()

install.packages("tidyverse")
install.packages("readxl")
install.packages("fBasics")
install.packages("moments")
install.packages("deflateBR")
install.packages("mFilter")

options(scipen=9999)
options(max.print=100000)
par(mfrow=c(1,1))
setwd("C:/Users/Nicolly Soares/Desktop/ECONOMETRIA")
library(tidyverse)

datainic <- "2003-01-01" ; datafim <- "2026-07-01"
{
  library(readxl)
  
  #################### Carregamento dos arquivos ###############
  SELIC_Ac_12_meses<- read_excel("Selic - Ac. 12 meses (%a.a).xlsx")
  selicShiny  <- ts(SELIC_Ac_12_meses[, 2],   frequency = 12, start = c(2003, 1), end = c(2026, 7));  selicShiny
  plot.ts(selicShiny,bty="l", lwd=5, col="blue")
}

selic12=read.table("selic12.txt",head=T); selic12

#disposição da Base de Dados em Séries Temporais
selic<-ts(selic12, frequency=12, start=c(2003,1),end=c(2026,07));selic
plot(selic,bty="l", lwd=5, col="darkgreen")

require(fBasics)
#Distribuição de Frequência - Histograma
hist(selic,breaks="Sturges")
hist(selic, nclass=40, col="darkgreen")

selic
plot(selic, xlab="Janeiro 2003 a Julho 2026", ylab="% a.a.", bty="l", lwd="2", col="blue", 
     main="Gráf.1: Taxa Selic Nominal a.a., 2003-2026 (Acum. 12 Meses)")
min(selic)
mean(selic)
median(selic)
max(selic)

sd(selic)

Coef.Var.SELIC=sd(selic)/mean(selic)*100
Coef.Var.SELIC

selic
n=length(selic)
n
head(selic, n=2)
tail(selic, n=2)
txselic<-((14.15/25.06)-1)*100
txselic; plot(selic)

txselic1<-((selic[n]/selic[1])-1)*100
txselic1

#  Taxa Média Mensal - Aritmética 

txselic2<-((14.15-25.06)/(25.06*283))*100
txselic2

txselic3<-(selic[n]-selic[1])/(selic[1]*n)*100
txselic3

#  Taxa Média Mensal - Geométrica

txselic4<-(((14.15/25.06)^(1/283))-1)*100
txselic4

txselic5<-(((selic[n]/selic[1])^(1/n))-1)*100
txselic5

#==================================================================================
##Taxa de Crescimento - Modelo Semilogaritmicos /Log-Lin##
#Gujarati, Cap 6, Seção 6.6 - Taxa de Crescimento (pg.179)
#==================================================================================
{
  #Gerando a Série de Tendencia (t)##
  t=seq(1,n)
  t
  ##Calculando a Taxa de Crescimento##
  Taxa5=lm(log(selic)~t)
  Taxa5
  #Taxa Crescimento Instantânea - em um ponto no tempo
  Cresc=(Taxa5$coeff[2])
  Cresc
  Cresc.Med=Cresc*100
  Cresc.Med

  #=====================================================================================
  #Previsão com Matemática Financeira
  #==================================================================================
  {
    tail(selic, n=2)
    
    VFselicL=selic[n]*(1+(txselic3/100))
    VFselicL
    selic[n]
    
    VFselicG=selic[n]*(1+(txselic5/100))
    VFselicG
  }
  
  ## Previsão usando LOG-LIN
  VFselicZ=selic[n]*(1+(Cresc.Med/100))
  VFselicZ
  
  
  library(readxl)
  
  #################### Carregamento dos arquivos ###############
  SELIC_Ac_12_meses<- read_excel("Selic - Ac. 12 meses (%a.a).xlsx")
  selicShiny  <- ts(SELIC_Ac_12_meses[, 2],   frequency = 12, start = c(2003, 1), end = c(2026, 7));  selicShiny
  plot.ts(selicShiny,bty="l", lwd=5, col="blue")
  
  
  #################### Carregamento dos arquivos ###############
  IPCA_AC12_meses<- read_excel("IPCA - 12 meses (%a.m.).xlsx")
  IPCAShiny  <- ts(IPCA_AC12_meses[, 2],   frequency = 12, start = c(2003, 1), end = c(2026, 7));  IPCAShiny
  plot.ts(IPCAShiny,bty="l", lwd=5, col="blue")
  
  SelicReal <- ((1+(selicShiny )/100)/(1+( IPCAShiny)/100) -1)*100 #selic real ex-post 
  SelicReal;  print(round(SelicReal, 2))
  plot(SelicReal,bty="l", lwd=5, col="darkgreen")
  SelicR <- SelicReal
  
  min(SelicR)
  mean(SelicR)
  median(SelicR)
  max(SelicR)
  summary(SelicR)
  
  sd(SelicR)
  Coef.Var.SelicR=sd(SelicR)/mean(SelicR)*100
  Coef.Var.SelicR
  
  SelicR
  n=length(SelicR)
  n
  head(SelicR, n=2)
  tail(SelicR, n=2)
  
  
  txSelicR<-((9.297204/9.251332)-1)*100
  txSelicR; plot(SelicR)
  
  txSelicR1<-((SelicR[n]/SelicR[1])-1)*100
  txSelicR1
  
  txSelicR2<-((9.297204-9.251332)/(9.251332*283))*100
  txSelicR2
  
  txSelicR3<-(SelicR[n]-SelicR[1])/(SelicR[1]*n)*100
  txSelicR3
  
  #  Taxa Média Mensal - Geométrica
  
  txSelicR4<-(((9.297204/9.251332)^(1/283))-1)*100
  txSelicR4
  
  txSelicR5<-(((SelicR[n]/SelicR[1])^(1/n))-1)*100
  txSelicR5
  
  tail(SelicR, n=2)
  
  VFSelicRL=SelicR[n]*(1+(txSelicR3/100))
  VFSelicRL
  SelicR[n]
  
  VFSelicRG=SelicR[n]*(1+(txSelicR5/100))
  VFSelicRG
  
  #Gerando a Série de Tendencia (t)##
  t=seq(1,n)
  t
  
  SelicR
  ##Calculando a Taxa de Crescimento##
  Taxa5=lm(log(SelicR)~t)
  Taxa5
  #Taxa Crescimento Instantânea - em um ponto no tempo
  Cresc=(Taxa5$coeff[2])
  Cresc
  Cresc.Med=Cresc*100
  Cresc.Med
  
  VFSelicRG=SelicR[n]*(1+(Cresc.Med/100))
  VFSelicRG
  
  coef(Taxa5)
  PrevEconm <- Taxa5$coef[1]+283*Taxa5$coef[2]
  PrevEconm
  
  install.packages("mFilter")
  
  library(mFilter)
  SelicR.hp<-hpfilter(na.omit(SelicR, type='lambda', freq=14400))
  JuroNeutro<-SelicR.hp$trend;JuroNeutro
  plot(JuroNeutro, col="blue", lwd=4, main="Gráfico 3 - Selic Neutra 2003-2026")
  
  SelicNeutra3<-SelicR.hp$cycle; SelicNeutra3
  plot(SelicNeutra3, col="blue", lw=3)
  abline(h=0, col="red", lwd=3)
  
  