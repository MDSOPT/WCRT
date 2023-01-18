#RunSurveyCorr.R
#This is the R code that created the correlation and wave analyses for the 
#"Worst-Case Resistance Testing: A Nonresponse Bias Solution for Today’s Behavioral Research Realities"
#paper.  It includes exploratory data analysis, reliability
#analysis, correlation analysis, and wave analysis.
#
# Author         Date           Notes
# Stephen France 01/16/2023     Initial version for paper

#Set workspace: NOTE TO USERS. PLEASE REPLACE WITH YOUR OWN WORKING DIRECTORY
setwd("~/R/FileDraw")

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if (!require("psych")) install.packages("psych")
library(psych)

if (!require("GGally")) install.packages("GGally")
library(GGally)

if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

FDSurveyIn<-read.csv("NRBDataExportApril2021.csv")
FDSurvey<-na.omit(FDSurveyIn)

#Reverse Intent 2 and enjoyment 1
FDSurvey$INTENT.2<-8-FDSurvey$INTENT.2
FDSurvey$ENJOY.1<-8-FDSurvey$ENJOY.1

FDSingle<-FDSurvey[,11:29]


cor_FDSingle<-cor(FDSingle, use = "pairwise.complete.obs")
#cor_FDSingle<-cor_FDSingle>0
#cor_FDSingle<-apply(cor_FDSingle,2,as.numeric)

corrplot(cor_FDSingle, method="circle")
corrplot(cor_FDSingle, method="color")
corrplot(cor_FDSingle, method="square")


#R also has a nice range of simple correlation plots
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
dev.off()  #First, we'll reset the graphics


#Test Chronbach's alpha
sink('ChronbachAlpa.txt')
psych::alpha(FDSingle[1:3])
psych::alpha(FDSingle[4:8])
psych::alpha(FDSingle[9:11])
psych::alpha(FDSingle[12:15])
psych::alpha(FDSingle[16:19])
sink()

#Create summated ratings scales
EXP.SUM<-rowSums(FDSingle[1:3])
SAT.SUM<-rowSums(FDSingle[4:8])
PWOM.SUM<-rowSums(FDSingle[9:11])
INTENT.SUM<-rowSums(FDSingle[12:15])
ENJOY.SUM<-rowSums(FDSingle[16:19])

#Create a data frame with the summated rating scales
FDSums<-data.frame(EXP.SUM,SAT.SUM,PWOM.SUM,INTENT.SUM,ENJOY.SUM)

#Create a data frame with the summated rating scales
pairs.panels(FDSums,lm=TRUE,stars=TRUE)
cor.test(EXP.SUM, ENJOY.SUM)

source("WaveAnalysis.R")

#Plot correlations over time between the summated rating scales
#Each correlation is for the default window of 30 respondents
plot(CorrWindows(EXP.SUM,SAT.SUM),type="b",cex=1,pch=4)
plot(CorrWindows(EXP.SUM,PWOM.SUM),type="b",cex=1,pch=4)
plot(CorrWindows(EXP.SUM,INTENT.SUM),type="b",cex=1,pch=4)
plot(CorrWindows(EXP.SUM,ENJOY.SUM),type="b",cex=1,pch=4)

plot(CorrWindows(SAT.SUM,PWOM.SUM),type="b",cex=1,pch=4)
plot(CorrWindows(SAT.SUM,INTENT.SUM),type="b",cex=1,pch=4)
plot(CorrWindows(SAT.SUM,ENJOY.SUM),type="b",cex=1,pch=4)

plot(CorrWindows(PWOM.SUM,INTENT.SUM),type="b",cex=1,pch=4)
plot(CorrWindows(PWOM.SUM,ENJOY.SUM),type="b",cex=1,pch=4)


#Plot cumulative correlations over time, starting at defaul
#value of 20.
plot(CorrCum(EXP.SUM,SAT.SUM),type="b",cex=1,pch=4)
plot(CorrCum(EXP.SUM,PWOM.SUM),type="b",cex=1,pch=4)
plot(CorrCum(EXP.SUM,INTENT.SUM),type="b",cex=1,pch=4)
plot(CorrCum(EXP.SUM,ENJOY.SUM),type="b",cex=1,pch=4)

plot(CorrCum(SAT.SUM,PWOM.SUM),type="b",cex=1,pch=4)
plot(CorrCum(SAT.SUM,INTENT.SUM),type="b",cex=1,pch=4)
plot(CorrCum(SAT.SUM,ENJOY.SUM),type="b",cex=1,pch=4)

plot(CorrCum(PWOM.SUM,INTENT.SUM),type="b",cex=1,pch=4)
plot(CorrCum(PWOM.SUM,ENJOY.SUM),type="b",cex=1,pch=4)

#Source the function of wave analysis functions.
source("WaveAnalysis.R")

#Let's try the more traditional wave analysis
Results<-matrix(rep(0,60),10,6)

#Split respondents into waves (use default even splits)
#Then calculate correlations for the different waves
Results[1,1:2]<-CorrWave(EXP.SUM,SAT.SUM)
Results[2,1:2]<-CorrWave(EXP.SUM,PWOM.SUM)
Results[3,1:2]<-CorrWave(EXP.SUM,INTENT.SUM)
Results[4,1:2]<-CorrWave(EXP.SUM,ENJOY.SUM)

Results[5,1:2]<-CorrWave(SAT.SUM,PWOM.SUM)
Results[6,1:2]<-CorrWave(SAT.SUM,INTENT.SUM)
Results[7,1:2]<-CorrWave(SAT.SUM,ENJOY.SUM)

Results[8,1:2]<-CorrWave(PWOM.SUM,INTENT.SUM)
Results[9,1:2]<-CorrWave(PWOM.SUM,ENJOY.SUM)

Results[10,1:2]<-CorrWave(INTENT.SUM,ENJOY.SUM)

#Create wave analysis calculations for the paper
#With the different values of n for the 3rd wave
#of n3=415,1245,3735
TempWaveAnalysis<-function(XBar1,XBar2,n)
{
  Results<-rep(0,4)
  Results[1]<-WaveAnalysis(XBar1,XBar2,"2",n)
  Results[2]<-WaveAnalysis(XBar1,XBar2,"3",n,415)
  Results[3]<-WaveAnalysis(XBar1,XBar2,"3",n,1245)
  Results[4]<-WaveAnalysis(XBar1,XBar2,"3",n,3735)
  return(Results)
}

#Format and output the results
n<-nrow(FDSingle)
for (i in 1:10)
{Results[i,3:6]<-TempWaveAnalysis(Results[i,1],Results[i,2],n)}
Results[Results<(-1)]<--1
Results[Results>1]<-1
write.csv(Results,"Results.csv")