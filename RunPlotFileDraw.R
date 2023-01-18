#RunSurveyCorr.R
#This is the R code that created the n-curve WCRT plots for the paper
#"Worst-Case Resistance Testing: A Nonresponse Bias Solution for Today’s Behavioral Research Realities"
#paper.  Data files are included separately.
#
# Author         Date           Notes
# Stephen France 01/16/2023     Initial version for paper

setwd("~/R/FileDraw")
source("PlotFileDraw.R")

#To become significant
FDCorr1<-read.csv("FileDrawCorr.csv")
PlotFileDraw(FDCorr1[,1:2],ESName="r",ESHighlight=c(-0.1,-0.3,-0.5),yTrans="log10",yText=8000,PlotTitle="Effect size and n required for sig. at alpha=0.10: PES-1 and ENJOY-1(r=-0.099,z=-0.896)")
PlotFileDraw(FDCorr1[,c(1,3)],ESName="r",ESHighlight=c(-0.1,-0.3,-0.5),yTrans="log10",yText=9000,PlotTitle="Effect size and n required for sig. at alpha=0.05: PES-1 and ENJOY-1 (r=-0.099,z=-0.896)")
PlotFileDraw(FDCorr1[,c(1,4)],ESName="r",ESHighlight=c(-0.1,-0.3,-0.5),yTrans="log10",yText=18000,PlotTitle="Effect size and n required for sig. at alpha=0.01: PES-1 and ENJOY-1 (r=-0.099,z=-0.896)")

#To become non-significant
FDCorr2<-read.csv("FileDrawCorr2.csv")
PlotFileDraw(FDCorr2[,1:2],ESName="r",ESHighlight=c(-0.1,-0.3,-0.5),yTrans="log10",yText=20000,PlotTitle="Effect size and n required for non. sig. at alpha=0.10: PES-1 and INTENT-1 (r=0.571,z=5.853)")
PlotFileDraw(FDCorr2[,c(1,3)],ESName="r",ESHighlight=c(-0.1,-0.3,-0.5),yTrans="log10",yText=25000,PlotTitle="Effect size and n required for non. sig. at alpha=0.05: PES-1 and INTENT-1 (r=0.571,z=5.853)")
PlotFileDraw(FDCorr2[,c(1,4)],ESName="r",ESHighlight=c(-0.1,-0.3,-0.5),yTrans="log10",yText=35000,PlotTitle="Effect size and n required for non. sig. at alpha=0.01: PES-1 and INTENT-1 (r=0.571,z=5.853)")

#For highest possible correlation
AllCorr<-read.csv("WCRTOutput.csv")
Data1<-AllCorr[AllCorr$Corr=="EXP:SAT"&AllCorr$Alpha==0.1,c("r2","N2")]
PlotFileDraw(Data1,ESName="r",ESHighlight=c(-0.1,-0.3,-0.5,-0.7,-0.9),yTrans="log10",yText=8000,PlotTitle="Effect size and n required for non-sig. at alpha=0.10: EXP and SAT (r=0.94)")
Data1<-AllCorr[AllCorr$Corr=="EXP:SAT"&AllCorr$Alpha==0.05,c("r2","N2")]
PlotFileDraw(Data1,ESName="r",ESHighlight=c(-0.1,-0.3,-0.5,-0.7,-0.9),yTrans="log10",yText=8000,PlotTitle="Effect size and n required for non-sig. at alpha=0.05: EXP and SAT (r=0.94)")
Data1<-AllCorr[AllCorr$Corr=="EXP:SAT"&AllCorr$Alpha==0.01,c("r2","N2")]
PlotFileDraw(Data1,ESName="r",ESHighlight=c(-0.1,-0.3,-0.5,-0.7,-0.9),yTrans="log10",yText=8000,PlotTitle="Effect size and n required for non-sig. at alpha=0.01: EXP and SAT (r=0.94)")

#For lowest possible correlation
Data1<-AllCorr[AllCorr$Corr=="INTENT:ENJOY"&AllCorr$Alpha==0.1,c("r2","N2")]
PlotFileDraw(Data1,ESName="r",ESHighlight=c(-0.1,-0.3,-0.5,-0.7,-0.9),yTrans="log10",yText=200,PlotTitle="Effect size and n required for non-sig. at alpha=0.10: INTENT and ENJOY (r=0.24)")
Data1<-AllCorr[AllCorr$Corr=="INTENT:ENJOY"&AllCorr$Alpha==0.05,c("r2","N2")]
PlotFileDraw(Data1,ESName="r",ESHighlight=c(-0.1,-0.3,-0.5,-0.7,-0.9),yTrans="log10",yText=400,PlotTitle="Effect size and n required for non-sig. at alpha=0.05: INTENT and ENJOY (r=0.24)")
Data1<-AllCorr[AllCorr$Corr=="INTENT:ENJOY"&AllCorr$Alpha==0.01,c("r2","N2")]
PlotFileDraw(Data1,ESName="r",ESHighlight=c(-0.1,-0.3,-0.5,-0.7,-0.9),yTrans="log10",yText=300,PlotTitle="Effect size and n required for non-sig. at alpha=0.01: INTENT and ENJOY (r=0.24)")
