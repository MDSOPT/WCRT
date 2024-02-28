#Correlationz.R
#This is the R version of the code for the correlation z test with Fisher transform for the File Draw Problem
#This version inputs the sample data
#INPUTS 
#IsRHS - True if the test is RHS. False otherwise
#SampleData - The sample data in a two column data frame, one for each variable
#Alpha - The alpha (Type I Error) for the test
#r2 - The WCRT opposing effect size
#Delta - The convergence criteria for the optimization algorithm (lower is more accurate and takes longer to converge)
#OUTPUT
#A vector consisting of four values
#nOpt - The optimized WRCT n robustness value
#nOptRound - The value above rounded to the lowest integer value needed to change a test result.
#IsSignificant - True if the WRCT n is to turn a significant result non-significant. False, the opposite.
#Iter - The number of iterations required for convergence 
#
# Author         Date           Notes
# Stephen France 01/2/2024     Convert from Excel VBA macro to R

Correlationz<-function(IsRHS,SampleData,Alpha,r2,Delta=0.0001)
{
  #Sample data should be a vector or a single column dataframe (convert to vector)

  r1<-cor(SampleData[,1],SampleData[,2])
  n1<-nrow(SampleData)
  
  #Check if significant
  zTrans1<-0.5*log((1+r1)/(1-r1))
  zVal1<-zTrans1/sqrt(1/(n1-3))
  zCrit<-ifelse(IsRHS==TRUE,qnorm(1-Alpha),qnorm(Alpha))
  IsSignificant<-((zVal1>zCrit)&(IsRHS==TRUE)|(zVal1<zCrit)&(IsRHS==FALSE))
  
  #Utilize an initial starting value of n2 = n1 and call this nOpt. 

  zTrans2<-0.5*log((1+r2)/(1-r2))
  if ((IsSignificant==TRUE & IsRHS==TRUE)|(IsSignificant==FALSE & IsRHS==FALSE))
  {
    LB<-zTrans2;UB<-zTrans1
  }
  else
  {
    LB<-zTrans1;UB<-zTrans2
  }
  #For the freak occurance where the second effect size is identical to the data effect
  #size.  This shouldn't happen in reality, but could happen in experiment.
  if (abs(LB-UB)<=Delta)
  {
    LB = LB - Delta
    UB = UB + Delta
  }
  
  Iter<-0
  while (abs(LB-UB)>Delta)
  {
    Iter<-Iter+1
    Midpoint<-(LB+UB)/2
    #Calculate the value of n2
    n2 = ((n1-3)*zTrans1-3*zTrans2-Midpoint*(n1-6))/(Midpoint-zTrans2)
    if (n2<0)
    {
      {n2<-NaN}
      break
    }
    
    #Now create the actual sample Z
    zVal=Midpoint*((n1+n2-6)^0.5)
      
    if (zVal < zCrit) 
    {
      LB = Midpoint
    }
    else
    {
      UB = Midpoint
    }
    
  }
  if (abs(zVal - zCrit) > 0.1)
  {n2<-NaN}
  
  if ((IsSignificant==TRUE & IsRHS==TRUE)|(IsSignificant==FALSE & IsRHS==FALSE))
  {n2Round<-floor(n2)}
  else
  {n2Round<-ceiling(n2)}
  return (c(n2,n2Round,IsSignificant,Iter))
}

#Correlationbz.R
#This is the R version of the code for the correlation z test with Fisher transform for the File Draw Problem
#This version inputs the sample correlation and sample size rather than the actual data
#INPUTS 
#IsRHS - True if the test is RHS. False otherwise
#r1 - The correlation for the  sample data
#n1 - The size of the sample data
#Alpha - The alpha (Type I Error) for the test
#r2 - The WCRT opposing effect size
#Delta - The convergence criteria for the optimization algorithm (lower is more accurate and takes longer to converge)
#OUTPUT
#A vector consisting of four values
#nOpt - The optimized WRCT n robustness value
#nOptRound - The value above rounded to the lowest integer value needed to change a test result.
#IsSignificant - True if the WRCT n is to turn a significant result non-significant. False, the opposite.
#Iter - The number of iterations required for convergence 
#
# Author         Date           Notes
# Stephen France 01/2/2024     Convert from Excel VBA macro to R
Correlationbz<-function(r1,n1,Alpha,r2,Delta=0.0001)
{
  #Here, we are passing in the correlation
  IsRHS<-(r1>0)
  
  #Check if significant
  zTrans1<-0.5*log((1+r1)/(1-r1))
  zVal1<-zTrans1/sqrt(1/(n1-3))
  zCrit<-ifelse(IsRHS==TRUE,qnorm(1-Alpha),qnorm(Alpha))
  IsSignificant<-((zVal1>zCrit)&(IsRHS==TRUE)|(zVal1<zCrit)&(IsRHS==FALSE))
  
  #Utilize an initial starting value of n2 = n1 and call this nOpt. 
  
  zTrans2<-0.5*log((1+r2)/(1-r2))
  if ((IsSignificant==TRUE & IsRHS==TRUE)|(IsSignificant==FALSE & IsRHS==FALSE))
  {
    LB<-zTrans2;UB<-zTrans1
  }
  else
  {
    LB<-zTrans1;UB<-zTrans2
  }
  #For the freak occurance where the second effect size is identical to the data effect
  #size.  This shouldn't happen in reality, but could happen in experiment.
  if (abs(LB-UB)<=Delta)
  {
    LB = LB - Delta
    UB = UB + Delta
  }
  
  Iter<-0
  while (abs(LB-UB)>Delta)
  {
    Iter<-Iter+1
    Midpoint<-(LB+UB)/2
    #Calculate the value of n2
    n2 = ((n1-3)*zTrans1-3*zTrans2-Midpoint*(n1-6))/(Midpoint-zTrans2)
    if (n2<0)
    {
      {n2<-NaN}
      break
    }
    
    #Now create the actual sample Z
    zVal=Midpoint*((n1+n2-6)^0.5)
    
    if (zVal < zCrit) 
    {
      LB = Midpoint
    }
    else
    {
      UB = Midpoint
    }
    
  }
  if (abs(zVal - zCrit) > 0.1)
  {n2<-NaN}
  
  if ((IsSignificant==TRUE & IsRHS==TRUE)|(IsSignificant==FALSE & IsRHS==FALSE))
  {n2Round<-floor(n2)}
  else
  {n2Round<-ceiling(n2)}
  return (c(n2,n2Round,IsSignificant,Iter))
}

#Correlationbz.R
#Generates data for two groups using four potential data distributions. Alters the second
#group to produce data that gives a specified correlation.
#INPUTS 
#DistType - The type of data distribution. 
#   Norm - Normal, Unif - Uniform, Poisson, NExponential - Negative Exponential
#Corr - The value of r (correlation) required for the output data
#nSample - The sample size for the output data
#OUTPUT
#A two column dataframe, with each column containing the data for one of the variables.
#
# Author         Date           Notes
# Stephen France 01/2/2024     Convert from Excel VBA macro to R
GenerateCorr<-function(DistType,Corr,nSample) {
  
  switch(DistType, 
    Norm={
      InData1<-rnorm(nSample,0,1)
      InData2<-rnorm(nSample,0,1)
      },
    Unif={
      InData1<-runif(nSample, min = -1.732051, max = 1.732051) 
      InData2<-runif(nSample, min = -1.732051, max = 1.732051)
      },
    Poisson={
      InData1<-rpois(nSample, 1)-1
      InData2<-rpois(nSample, 1)-1
      },
    NExponential={
      InData1<-rexp(nSample, rate = 1)-1 
      InData2<-rexp(nSample, rate = 1)-1 
      },
  )
#Now alter so that the values are correlated
  Multiplier = Corr/sqrt(1-Corr^2)
  InData3<-Multiplier*InData1+InData2
  InData<-data.frame(InData1,InData3)
  #print(cor(InData))
  return(InData)
}
