#TwoSamplet.R
#This is the R version of the code for the two sample t and z tests for the File Draw Problem
#INPUTS
#IsRHS - True if the test is RHS. False otherwise
#SampleData - The sample data in a two column dataframe for groups A (col 1) and B (col 2), testing XBar(A)-XBar(B)
#D0 - The hypothesized mean difference for the test
#Alpha - The alpha (Type I Error) for the test
#d2 - The WCRT opposing effect size
#sMult - The variance multiplier for the WRCT file draw data
#TestType - "Studentt" for a Student t test, "Welcht" for a Welch t test and "z" for a z test
#Delta - The convergence criteria for the optimization algorithm (lower is more accurate and takes longer to converge)
#OUTPUT
#A vector consisting of four values
#nA2, nB2 - The optimized WRCT n robustness values for groups A and B
#nA2Round, nB2Round - The values above rounded to the lowest integer value needed to change a test result.
#IsSignificant - True if the WRCT n is to turn a significant result non-significant. False, the opposite.
#Iter - The number of iterations required for convergence
#
# Author         Date           Notes
# Stephen France 01/2/2024     Convert from Excel VBA macro to R

TwoSamplet<-function(IsRHS,SampleData,D0,Alpha,d2,sMult,TestType="Studentt",Delta=0.0001)
{
  #Sample data should be two separate vectors
  GroupA<-SampleData[!is.na(SampleData[,1]),1]
  nA1<-length(GroupA)
  XBarA1<-mean(GroupA)
  sA1<-sd(GroupA)
  GroupB<-SampleData[!is.na(SampleData[,2]),2]
  nB1<-length(GroupB)
  XBarB1<-mean(GroupB)
  sB1<-sd(GroupB)
  
  #Calculate the pooled standard deviation
  sPooled<-sqrt(((nA1-1)*(sA1^2)+(nB1-1)*(sB1^2))/(nA1+nB1-2))
  #Calculate Alpha for LHS/RHS tests
  AlphaRun<-ifelse(IsRHS==TRUE,1-Alpha,Alpha)
  
  if (TestType=="Studentt")
  {
    #Check if significant
    tzVal<-(XBarA1-XBarB1-D0)/(sPooled*sqrt(1/nA1+1/nB1))
    dftVal<-nA1+nB1-2
    tzCrit<-qt(AlphaRun,dftVal)
  }
  else if ((TestType=="Welcht"))
  {
    tzVal<-(XBarA1-XBarB1-D0)/(sqrt((sA1^2)/nA1+(sB1^2)/nB1))
    dftNum<-((sA1^2)/nA1+(sB1^2)/nB1)^2
    dftDenom<-((sA1^2)/nA1)^2/(nA1-1)+((sB1^2)/nB1)^2/(nB1-1)
    dftVal<-dftNum/dftDenom
    tzCrit<-qt(AlphaRun,dftVal)
  }
  else #Run a z test
  {
    tzVal<-(XBarA1-XBarB1-D0)/(sqrt((sA1^2)/nA1+(sB1^2)/nB1))
    dftVal<-NA
    tzCrit<-qnorm(AlphaRun)
  }

  IsSignificant<-((tzVal>tzCrit)&(IsRHS==TRUE)|(tzVal<tzCrit)&(IsRHS==FALSE))
  
  #Calculate the standard deviations for group 2 with the multiplier
  sA2<-sA1*sMult
  sB2<-sB1*sMult
  sPooled2<-sPooled*sMult
  
  #Setup the initial values of the k multipliers
  kOpt<-1;kCalc<-0;Iter<-1
  while (abs(kOpt-kCalc)>Delta)
  {
    #Calculate the 
    XDiff2 = d2 * sPooled2 + D0
    
    #Multiply original NB1 by ratio k
    nA2<-nA1*kOpt
    nB2<-nB1*kOpt
    
    #Additional degrees of freedom from the single sample problem.  We are going to assume
    #the same overall average as the first
    XBarA2<-(XBarA1+XBarB1)/2+XDiff2/2
    XBarB2<-(XBarA1+XBarB1)/2-XDiff2/2
    
    #The combined difference is a combination across the two samples
    XDiffC=(XBarA1-XBarB1+(kOpt*XDiff2))/(1+kOpt)
    
    #Calculate the combined standard deviations for A and B, as per Cochrane
    sAC=sqrt(((nA1-1)*sA1^2+(nA2-1)*sA2^2+(nA1*nA2)*(XBarA1^2+XBarA2^2-2*XBarA1*XBarA2)/(nA1+nA2))/(nA1+nA2-1))
    sBC=sqrt(((nB1-1)*sB1^2+(nB2-1)*sB2^2+(nB1*nB2)*(XBarB1^2+XBarB2^2-2*XBarB1*XBarB2)/(nB1+nB2))/(nB1+nB2-1))
    
    #Calculate the pooled standard deviation for the new data
    sPooledC=sqrt(((nA1+nA2-1)*sAC^2+(nB1+nB2-1)*sBC^2)/(nA1+nA2+nB1+nB2-2))
    
    kCalcPrev<-kCalc
    if (TestType=="Studentt")
    {
      dfCombined<-nA1+nB1+nA2+nB2-2
      SECalc<-sqrt(sPooledC^2*(1/(nA1+nA2)+1/(nB1+nB2)))
      kCalc<-((XBarA1-XBarB1)-qt(AlphaRun,dfCombined)*SECalc-D0)/(qt(AlphaRun,dfCombined)*SECalc+D0-XDiff2)
    }
    else if ((TestType=="Welcht"))
    {
      dfCombined<-(sAC^2/(nA1+nA2)+sBC^2/(nB1+nB2))^2/((sAC^2/(nA1+nA2))^2/(nA1+nA2-1)+(sBC^2/(nB1+nB2))^2/(nB1+nB2-1))
      SECalc<-sqrt(sAC^2/(nA1+nA2)+sBC^2/(nB1+nB2))
      kCalc<-((XBarA1-XBarB1)-qt(AlphaRun,dfCombined)*SECalc-D0)/(qt(AlphaRun,dfCombined)*SECalc+D0-XDiff2)
    }
    else #Run a z test
    {
      SECalc=sqrt(sAC^2/(nA1+nA2)+sBC^2/(nB1+nB2))
      kCalc=((XBarA1-XBarB1)-qnorm(AlphaRun)*SECalc-D0)/(qnorm(AlphaRun)*SECalc+D0-XDiff2)
    }
    #print(kCalc)
    #Check for the convergence criteria
    if (Iter>10000)
    {
      kCalc<-(kCalc+kCalcPrev)/2
    }
    
    if (is.finite(kCalc))
    {
      if (kCalc<0)
      {
        kOpt<-NaN
        break
      }
      else
      {
        #Update the value of kOpt
        kOpt<-(kOpt+kCalc)/2
      }
    } else
    {
      kOpt<-NaN
      break
    }
    Iter<-Iter+1;

  }
  
  nA2<-nA1*kOpt
  nB2<-nB1*kOpt
  if ((IsSignificant==TRUE & IsRHS==TRUE)|(IsSignificant==FALSE & IsRHS==FALSE))
  {
    return(c(nA2,nB2,floor(nA2),floor(nB2),IsSignificant,Iter))
  }
  else
  {
    return(c(nA2,nB2,ceiling(nA2),ceiling(nB2),IsSignificant,Iter))
  }
}
