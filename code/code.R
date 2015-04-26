setwd("D:/Iaroslav/Projects/Facebook")
Sys.setlocale("LC_CTYPE", "russian")
file <- "data/html/messages.htm"
page <- readLines(file)
Encoding(page) <- "UTF-8"

require(XML)
parsed <- htmlParse(page)

##GET NAMES 
names <- xpathSApply(parsed,"//div[@class = 'thread']/text()")
##GET LIST OF THREAD NODES
nodeList <- getNodeSet(parsed,"//div[@class = 'thread']")


## function to process date
fbDate <- function(x){
    ## 7th part of strsplit is timezone. 
    ## pay attention in future
    strptime(
        sapply(strsplit(x," "),
               function(y) paste(y[c(1,2,3,6)],collapse=" ")),
        format = "%d %B %Y %H:%M"
    )   
}


##process one thread, apply to elements of nodeList
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
    names(data) <- c("name","date","message")
    for(i in 1:ncol(data)) names(data[,i]) <- NULL
    for(i in 2:3) data[,i] <- as.character(data[,i])
    ## read datetime
    date <- fbDate(data[,2])
    data <- data.frame(data[,c(1,3)],date)
    ## names as factors
    data[,1] <- as.factor(data[,1])
    
    data
}

