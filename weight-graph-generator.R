######################################################
### FUNCTIONS FOR GENERATING WEIGHT GRAPH FOR RATS ###
######################################################

### This is a collection of functions that will generate 
### graphs for keeping track of the rats' weight during an experiment.



#Function for rounding to the closest multiple of 5 or 10 or whatever (to use later in the y axis)
mround <- function(z,base=5){
  base*round(z/base)
}

#Function for identifying odd vs. even numbers (to be used for the color of the rectangles demarcating the days)
is.even <- function(i){
  if((i/2 == round(i/2, 0))==TRUE) {return(TRUE)} else {return(FALSE)}
}

#Actual function for plotting individual charts for each rat. These are 
#the parameters I need to define:
# rats: vector with the NAMES assigned to each subject
# BLweight: vector with the weight of each rat on the day I removed the food (make sure the order is the same as in "rats")
# BLday: day I removed the food. It needs to be entered like this: as.Date("YYYY-MM-DD")
# days: number of days to plot after 1st day of food deprivation
# perPage: number of charts per page
# folder: folder to save the graph this function generates
# The graph will indicate the 90% and 85% of baseline weight with an orange and red horizontal lines respectively


weight_graph <- function(rats, BLweight, BLday, 
                         days=20, perPage=2, folder){
  
  
  filename <- paste(folder, "weight-graph.pdf", sep="")
  
  pdf(file = filename, onefile=TRUE, paper="USr")
  
  #Define margins and indicate how many plots per page
  par(mar=c(4, 0.5, 4, 0.5))
  par(mfrow=c(perPage,1))
  
 
  #Create a weight plot per rat
  sapply(seq(1, length(rats)), function(x){
      
      max_weight <- BLweight[x]+5
      min_weight <- round(BLweight[x]*0.84, 0)
        
      yaxis_5 <- seq(mround(min_weight, base=5), mround(max_weight, base=5), 5)
      yaxis_10 <- seq(mround(min_weight, base=5), mround(max_weight, base=5), 10)
      
      plot.new()
      plot.window(xlim = c(1, days), 
                  ylim = c(min(yaxis_5), max(yaxis_5)))
      
      #Title: name of the rat
      title(main=paste("Rat #: ", rats[x], sep=""), line=0.5)
      
      #Days (first line to write labels in between tickmarks, second line to create tickmarks)
      
      allDays <- seq.Date(from = BLday, to = BLday+days, by = 1)
      
      axis(side=1, at=seq(0, days)+0.5, tick=FALSE,
           labels=format(allDays, format = "%m-%d"), 
           las=2)
      axis(side=1, at=seq(0, days+1), tick=TRUE, labels=FALSE)
      
      sapply(seq(0, days+1), function(y){
        
        if(is.even(y)){colpick <- "gray80"} else {colpick <- "white"}
        
        rect(
          xleft = y,
          xright = y+1,
          ybottom = min(yaxis_5),
          ytop = max(yaxis_5),
          col = colpick,
          border = NA
        )
      })
      
      mtext(side=1, text="Date", line=4, font=2)
      
      #Horizontal lines
      axis(side=2, at=seq(min(yaxis_10), max(yaxis_10), by=10), las=2, pos=0)
      mtext(side=2, text="Body weight", line=3, font=2)
      
      abline(h=yaxis_5, lty=2)
      abline(h=yaxis_10)
      
      #Min weights (90% and 85% of the weight)
      abline(h=BLweight[x]*0.9, col="orange", lwd=2)
      abline(h=BLweight[x]*0.85, col="red", lwd=2)
      
    })
  
  dev.off()

  }