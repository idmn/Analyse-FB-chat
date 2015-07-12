#### DAILY ACTIVITY ANALYSIS

require(lubridate)
require(ggplot2)

## daily activity plot function !!!
dailyActivity <- function(data,startTime = 6){
    time <- hour(data$time) + minute(data$time)/60
    ## values for correct plotting
    time <- (time - startTime)%%24
    
    df <- data.frame(name = data$name,time)
        
    togetherPlot <-
        (
            ggplot(df,aes(x=time))
            + geom_density(fill="green",alpha=0.3)
            +scale_x_continuous(limits = c(0,24),breaks = 0:24,
                                labels=(startTime:30)%%24)
        )
    
    separatePlot <- 
        (
            ggplot(df,aes(x=time,colour=name))
            +geom_density(size=1)
            +scale_x_continuous(limits = c(0,24),breaks = 0:24,
                                labels=(startTime:30)%%24)
        )  
    
    ## maybe separate plots for each participant
    
    list(togetherPlot,separatePlot)
}