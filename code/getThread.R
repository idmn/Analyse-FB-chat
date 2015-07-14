## function to process datetime
fbTime <- function(x, locale = 'en_GB'){
    strptime(x, format = 
        switch(locale,
            'en_GB' = '%A, %d %B %Y at %H:%M',   
            'en_US' = '%A, %B %d, %Y at %I:%M%p'           
        )                     
    )
}


## process one thread; apply this to elements of nodeList
getThread <- function(x){
    nodeSize <- xmlSize(x)
    nodeName <- xmlValue(x[[1]])
    messageInfo <- t(
        sapply(
            x[seq(2,nodeSize-1,2)], 
            function(y) xmlSApply(y[[1]],xmlValue)        
        )
    )
    rownames(messageInfo) <- 1:nrow(messageInfo)
    
    message <- sapply(
        x[seq(3,nodeSize,2)],
        xmlValue
    )
    names(message) <- 1:length(message) 
    ## stick together to make dataframe
    data <- data.frame(messageInfo,message,stringsAsFactors = F)
    
    ## fix names and col classes
    names(data) <- c("name","time","message")
    for(i in 1:ncol(data)) names(data[,i]) <- NULL
    for(i in 2:3) data[,i] <- as.character(data[,i])
    ## read datetime
    ## locale is a global variable, read from the file settiings.hml
    data[[2]] <- fbTime(data[[2]],locale)
    ## names as factors
    data[,1] <- as.factor(data[,1])
    
    data
}

getPerson <- function(nodeList,person,use.chatnames = F){
    ## list of data.frames with only one person's messages selected
    lst <- lapply(nodeList,function(x){
      data <- getThread(x)
      data[data$name == person,]
    })
    if(!use.chatnames) names(lst) <- NULL
    ## bind all data.frames
    do.call(rbind,lst)
}