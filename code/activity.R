#### DAILY ACTIVITY ANALYSIS

library(lubridate)
library(ggplot2)

## daily activity plot function
## startTime - the leftmost point on the plot
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
                                labels=(startTime + 0:24)%%24)
        )
    separatePlot <- 
        (
            ggplot(df,aes(x=time,colour=name))
            +geom_density(size=1)
            +scale_x_continuous(limits = c(0,24),breaks = 0:24,
                                labels=(startTime + 0:24)%%24)
        )  
    list(togetherPlot,separatePlot)
}

globalActivity <- function(data,start_date = NULL, end_date = NULL){
    ## if not specified, use all the data
    ## if start and end dates are specified as character strings,
    ## convert them to Date
    if(is.null(start_date)) start_date <- as.Date(tail(data,1)$time)
    else if(is.character(start_date)) {start_date <- as.Date(start_date)}
    if(is.null(end_date)) end_date <- as.Date(head(data,1)$time) else
        if(is.character(end_date)) end_date <- as.Date(end_date)
    
    date <- as.Date(data$time)
    interval <- ((start_date <= date) && (date <= end_date)) 
    df <- data.frame(name = data$name[interval],date = date[interval])

    togetherPlot <-
        (
            ggplot(df,aes(x=date))
            + geom_density(fill="green",alpha=0.3)
        )
    
    separatePlot <- 
        (
            ggplot(df,aes(x=date,colour=name))
            +geom_density(size=1)
        )  
    
    list(togetherPlot,separatePlot)
}