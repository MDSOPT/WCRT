#SingleSamplet.R
#This is the R version of the code for the single sample t and z tests for the File Draw Problem
#INPUTS 
#IsRHS - True if the test is RHS. False otherwise
#SampleData - The sample data in a vector or single column data frame
#Mu0 - The hypothesized mean for the test
#Alpha - The alpha (Type I Error) for the test
#d2 - The WCRT opposing effect size
#sMult - The variance multiplier for the WRCT file draw data
#TestType - "t" for a t test and "z" for a z test
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

SingleSamplet<-function(IsRHS,SampleData,Mu0,Alpha,d2,sMult,TestType="t",Delta=0.0001)
{
  #Sample data should be a vector or a single column dataframe (convert to vector)
  SampleData<-as.numeric(unlist(SampleData))
  XBar1<-mean(SampleData)
  s1<-sd(SampleData)
  s2<-s1*sMult
  n1<-length(SampleData)
  
  #Check if significant
  tzVal<-(XBar1-Mu0)/(s1/sqrt(n1))
  if (TestType=="t")
  {
    tzCrit<-round(ifelse(IsRHS==TRUE,qt(1-Alpha,n1-1),qt(Alpha,n1-1)),4)
  }
  else
  {
    tzCrit<-ifelse(IsRHS==TRUE,qnorm(1-Alpha),qnorm(Alpha))
  }
  IsSignificant<-((tzVal>tzCrit)&(IsRHS==TRUE)|(tzVal<tzCrit)&(IsRHS==FALSE))

  #Utilize an initial starting value of n2 = n1 and call this nOpt. 
  nOpt<-n1;nCalc<-0;Iter<-0
  while (abs(nOpt-nCalc)>Delta)
  {
    Iter<-Iter+1
    #Calculate XBar2,XBarC, and sc using nOpt. 
    XBar2<-d2*s2+Mu0
    XBarC<-(XBar1*n1+XBar2*nOpt)/(n1+nOpt)
    sC<-((n1-1)*s1^2+(nOpt-1)*s2^2+((n1*nOpt)/(n1+nOpt))*(XBar1^2+XBar2^2-2*XBar1*XBar2))/(n1+nOpt-1)
    sC<-sqrt(sC)
    #Calculate n2 from equation (9) and store this in variable nCalc.
    df<-n1+nOpt-1
    #Update critical value for revised degrees of freedom. Note, we don't need to do this for z test.
    if (TestType=="t")
    {
      tzCrit<-ifelse(IsRHS==TRUE,qt(1-Alpha,df),qt(Alpha,df))
    }
    #nCalc<-(sC^2)*(tCrit^2)/((XBarC-Mu0)^2)-n1
    nCalc<-(tzCrit*sC*sqrt(n1+nOpt)-n1*(XBar1-Mu0))/(XBar2-Mu0)
    #print(nCalc)
    #Recalculate nOpt as (nOpt+nCalc)/2.
    if (is.finite(nCalc))
    {
      nOpt<-(nOpt+nCalc)/2
    }
    else
    {
      nOpt<-NaN
      break
    }
  }
  
  if (is.finite(nOpt)&(nOpt<0))
  {
    nOpt=NaN
  }
  
  if ((IsSignificant==TRUE & IsRHS==TRUE)|(IsSignificant==FALSE & IsRHS==FALSE))
   {nOptRound<-floor(nOpt)}
  else
   {nOptRound<-ceiling(nOpt)}
  return (c(nOpt,nOptRound,IsSignificant,Iter))
}
