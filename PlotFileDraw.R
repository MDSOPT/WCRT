PlotFileDraw <- function(InData,EsMin=NULL,EsMax=NULL,ESHighlight=NULL,xTrans=NULL,yTrans=NULL,yText,ESName="Effect Size",PlotTitle=NULL){
  #PlotFileDraw
  #Plots an analysis from the file draw problem using ggplot.  Plots effect size
  #on the x axis and the number of n required to make a value or test non-significant
  #Inputs
  #  Data - A two column data structure where the first column is the effect size and
  #       the second column is the number of items
  #  ESRange - The start and end values of the effect size to plot
  #  ESHighlight - Allows multiple effect sizes to be highlighted
  #  yTrans - Apply a transformation to the y axis
  #  ESName - The name of the effect size measure, e.g., r for correlation, d for Cohen's d
  #Outputs
  #  ggOut - A ggplot object
  # Author         Date           Notes
  # Stephen France 01/16/2023     Initial version for paper
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  if (!require("scales")) install.packages("scales")
  library(scales)
  
  #Set the values to be plotted
  if (is.null(EsMin)==FALSE)
  {
    InData<-InData[InData[,1]>=EsMin]
  }
  if (is.null(EsMax)==FALSE)
  {
    InData<-InData[InData[,1]<=EsMax]
  }
  
  xName<-colnames(InData)[1]
  yName<-colnames(InData)[2]
  OutPlot<-ggplot(data=InData, aes_string(x=xName, y=yName)) + 
    geom_line(colour="red", linetype="solid", size=1) + 
    geom_point(colour="red", size=2, shape=21, fill="white") +
    xlab(ESName) + ylab("n")+
    theme_bw()+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

  if (is.null(PlotTitle)==FALSE)
  {
    OutPlot<-OutPlot+ggtitle(PlotTitle)
  }
  
  #Add the scaled values
  if (is.null(xTrans)==FALSE)
  {
    OutPlot<-OutPlot+scale_x_continuous(trans=xTrans)
  }

  if (is.null(yTrans)==FALSE)
  {
    OutPlot<-OutPlot+scale_y_continuous(trans=yTrans,labels = scales::number_format())
  }
  else
  {
    OutPlot<-OutPlot+scale_y_continuous(labels = scales::number_format())
  }
  
  #Add the intercept lines for the main plot sizes
  if(is.null(ESHighlight)==FALSE)
  {
    for (i in 1:length(ESHighlight))
    {
      OutPlot<-OutPlot+geom_vline(xintercept = ESHighlight[i], colour="blue",size=1, linetype = "longdash")
      #We need to find the y value
      MatchIndex<-which(InData[,1]== ESHighlight[i])
      if (length(MatchIndex)>0)
      {
        yval<-InData[MatchIndex,2]
        InLabel=paste("\n",ESName,"=",ESHighlight[i],", ","n=",format(yval,digits=6),sep="")
      }
      else
      {
        InLabel=paste("\n",ESName,"=",ESHighlight[i],sep="")
      }
      OutPlot<-OutPlot+geom_text(aes_string(x=ESHighlight[i],y=yText), label=InLabel,colour="blue", angle=90,size=5)
    }
  }
  
  return(OutPlot)
  
}