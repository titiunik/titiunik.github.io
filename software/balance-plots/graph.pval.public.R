
# Function to create figure with both summary statistics and plots of p-values for different variables across different groups
# Author: Rocio Titiunik
# Date: September 5th, 2008
# Version: 1.0

# Version to distribute publicly

# NOTE: This function is *very far* from being a real function. As you'll see, there are many parameters set inside the function
#       that should be arguments to the function instead. I'm currently working on a version that is fully flexible and does not
#       set any parameters inside the function. But for now, this is it.

# About the arguments of the function:

# 'results':     a matrix whose rows are different variables; whose first two columns contain the means for treated and control;
#                and whose remaining columns have the pvalues to be plotted for every variable

# 'title':       title of the overall graph

# at1, at2,at3:  scalars which indicates where to locate the three differents groups (mean treatment, mean controls, graph area) in the figure area

# xlim1 :         the left limit of the x-axis; right limit is always set to 1

# textsize:      scalar indicating the size of text in the figure

# legend:        logical indicating whether the legend should be included

# legendx:       scalar indicating the x-coordinate of the legend's location

# legendy:       scalar indicating the y-coordinate of the legend's location

# parcex:        scalar setting cex parameter



plot.pval <- function(results, title=NULL, legend,legendx=0.15,legendy=2.2, textsize=0.9, parcex=0.8, at1=-0.35, at2=-0.15, at3=-0.9,xlim1=-0.85) {


  # set values of different parameters
  xlim = c(xlim1,1); pchset = c(21,24,22,23); pchcolset = c("blue","yellow","red","darkgreen")

  # set margins and letter size
  par(cex=parcex, mai = c(0.5, 0.35, 1.1, 0.35))

  # set number of rows 
  ny = nrow(results)

  # create the empty figure
  if(!is.null(title))  plot(x=NULL,axes=F, xlim=xlim, ylim=c(1,ny),xlab="",ylab="", main=title)
  if(is.null(title))   plot(x=NULL,axes=F, xlim=xlim, ylim=c(1,ny),xlab="",ylab="")
  
  # add the 0, 0.05 and 0.1 vertical lines
  abline(v=c(0,0.05,0.1),lty=c(1,4,4), lwd=c(1,2,2))
  axis(side=1,at=c(0,0.05,0.1,1),tick=TRUE, las=2, cex.axis=0.7)

  # add labels on top of the three areas of the graph
  axis(side=3,at=at1,labels="Mean\nTreated",tick=FALSE, padj=0.5,cex.axis=textsize)
  axis(side=3,at=at2,labels="Mean\nControl",tick=FALSE, padj=0.5,cex.axis=textsize)
  axis(side=3,at=0.5,labels="P-values",tick=FALSE, padj=0.5,cex.axis=textsize)

  # Fill the figure with the information which is inside the 'results' matrix
  # First, add the p-values as points
  for(i in 4:ncol(results)) points(results[,i],ny:1, pch = pchset[i-4+1], col = pchcolset[i-4+1], bg = pchcolset[i-4+1])

  # Second, add each variable name and the means for treated and control
  for(i in 1:ny) {
    text(at3,ny-i+1,results[i,1],adj = 0,cex=textsize) # variable name
    text(at1,ny-i+1,results[i,2], cex=textsize)        # treatment mean
    text(at2,ny-i+1,results[i,3], cex=textsize)        # control mean
  }

  # Add dotted horizontal lines every two variables to make it prettier
  for(i in seq(2,by=2,length.out=floor((ny-1)/2))) abline(h = i+0.5, lty = 3)

  # Add legend
  if(legend) legend(x=legendx, y=legendy, c("Diff-means", "Diff-medians", "D-stat", "Rank-sum"), pch=pchset, pt.bg = pchcolset, cex=0.8)
}



###############################################
#
#
# EXAMPLE
# 
#
###############################################

set.seed(10012)
foo1 <- c("Variable 1","Variable 2","Variable 3", "Variable 4", "Variable 5")
foo2 <- matrix(data=round(rnorm(10,mean=20),digits=2),nrow=5,ncol=2)
foo3 <- matrix(data=round(runif(15),digits=2),nrow=5,ncol=3)


results <- cbind(foo1,foo2,foo3)


plot.pval(results=results, title="Example", legend=TRUE,legendx=0.15,legendy=2.2, textsize=0.9, parcex=0.8, at1=-0.35, at2=-0.15, at3=-0.9,xlim1=-0.85)
