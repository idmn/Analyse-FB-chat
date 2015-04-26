require(stringr)
spl <- split(data[,2],data[,1])

## LENGTH
len <-(sapply(spl,function(x) mean(nchar(x))))

## total number of messages
numb <- sapply(spl,length)


## COUNT WORDS
wordMean <- function(data,regex){
    spl <- split(data[,2],data[,1])
    sapply(spl,function(y) mean(str_count(y,regex)))
}

wordCount <- function(data,regex){
    spl <- split(data[,2],data[,1])
    sapply(spl,function(y) sum(str_count(y,regex)))
}

## list of regexes
regexList <- list(huy = "[Õõ][Óó][ÉéÅå¨¸ßÿÈè]",
                  nu  = "(^| )[Íí][Óó]([ ]|$|.)",
                  ya  = "(^| )[ßÿ]([ ]|$|.)")

## make a plot ??? wait till internet
myPlot <- function(x,title,fileName = NULL){
    if(!is.null(fileName)) png(fileName)
    else dev.off()
    barplot(x,main=title)
    dev.off()
}

## count letters
totalLetters <- wordCount(data,"[À-ßà-ÿ]")
