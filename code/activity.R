#### DAILY ACTIVITY ANALYSIS

require(lubridate)
require(ggplot2)

## daily activity plot function !!!
dailyActivity <- function(x,startTime){
    time <- hour(x[,3]) + minute(x[,3])/60
    ## values for correct plotting
    timeP <- (time - startTime)%%24
    
    df <- data.frame(name = x[,1],timeP)
        
    togetherPlot <-
        (
            ggplot(df,aes(x=timeP))
            + geom_density(fill="green",alpha=0.3)
            +scale_x_continuous(limits = c(0,24),breaks = 0:24,
                                labels=(startTime:30)%%24)
        )
    
    separatePlot <- 
        (
            ggplot(df,aes(x=timeP,colour=name))
            +geom_density(size=1)
            +scale_x_continuous(limits = c(0,24),breaks = 0:24,
                                labels=(startTime:30)%%24)
        )  
    
    ## maybe separate plots for each participant
    
    list(togetherPlot,separatePlot)
}