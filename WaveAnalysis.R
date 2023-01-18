#KPHelper.R
#A series of helper functions for implementing wave analysis
#as per Armstrong and Overton (1977).

#CorrWindows
#Creates correlations for windows of fixed size for 
#ordered data, iterating from the first item to the 
#last item that allows a full window (i+WinSize-1).
#INPUTS
# x - A vector of the first variable to be correlated
# y - A vector of the second variable to be correlated
# WinSize (default 30) - The number of items used to calculate each correlation
#OUTPUTS A vector of n-WinSize+1 correlations
# Author         Date           Notes
# Stephen France 01/16/2023     Initial version for paper
CorrWindows<-function(x,y,WinSize=30)
{
  #Get length.  At sum stage, we'll check x is same length as y
  n<-length(x)
  Results<-rep(0,n-WinSize+1)
  for (i in 1:(n-WinSize+1))
  {
    Results[i]<-cor(x[i:(i+WinSize-1)],y[i:(i+WinSize-1)])
  }
  return(Results)
}

#CorrCum
#Creates cumulative correlations for  
#ordered data, iterating from the 1:Start to 1:n. 
#INPUTS
# x - A vector of the first variable to be correlated
# y - A vector of the second variable to be correlated
# Start (default 20) - The start item (after burn-in) to start calculations
#OUTPUTS A vector of n-Start+1 correlations
# Author         Date           Notes
# Stephen France 01/16/2023     Initial version for paper
#Now run correlations with windows, taking cumulative correlations
CorrCum<-function(x,y,Start=20)
{
  #Get length.  At sum stage, we'll check x is same length as y
  n<-length(x)
  Results<-rep(0,n-Start+1)
  for (i in Start:n)
  {
    Results[i-Start+1]<-cor(x[1:i],y[1:i])
  }
  return(Results)
}

#CorrWave
#Creates correlations for different waves in wave analysis
#Note: This is quite a simple representation, where waves
#are assumed to be even
#INPUTS
# x - A vector of the first variable to be correlated
# y - A vector of the second variable to be correlated
# Nowaves (default 20) - The start item (after burn-in) to start calculations
#OUTPUTS A vector of NoWaves correlations
# Author         Date           Notes
# Stephen France 01/16/2023     Initial version for paper
CorrWave<-function(x,y,NoWaves=2)
{
  #Get length.  At sum stage, we'll check x is same length as y
  n<-length(x)
  Results<-rep(0,NoWaves)
  EndVal<-0
  for (i in 1:NoWaves)
  {
    StartVal<-EndVal+1
    EndVal<-round(n*i/NoWaves)
    Results[i]<-cor(x[StartVal:EndVal],y[StartVal:EndVal])
  }
  return(Results)
}

#WaveAnalysis
#Creates wave analysis estimates different waves in wave analysis
#Assumes two waves and a predicted third wave, as per Armstrong & Overton (1977)
#Code is general for any effect of interest (e.g, correlation, difference between 2 means etc.)
#INPUTS
# xbar1 - Mean value for the first wave
# xbar2 - Mean value for the second wave
# n - The number of items in the first two waves (assumed equal n1,n2)
# Mode - Which wave is being calculated? 1, 2, or 3 (nonresponse wave)
# n3 - The number of items in the nonresponse wave
#OUTPUTS A vector of NoWaves correlations
# Author         Date           Notes
# Stephen France 01/16/2023     Initial version for paper
WaveAnalysis<-function(xbar1,xbar2,Mode,n,n3)
{
  #Create n1 and n2 to be consistent with calculations
  n1<-round(n/2)
  n2<-n-n1
  switch(Mode,
         "1" = {xbar2},
         "2" = {xbar2+(xbar2-xbar1)*n2/(n1+n2)},
         "3" = {xbar2+(xbar2-xbar1)*(n2+n3)/(n1+n2)})
}


