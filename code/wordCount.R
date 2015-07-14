### TRY TO USE DATA.TABLE
library(data.table)
library(stringr)

## first, make this:
## data <- getThread(nodeList[[%some_number%]])
## dataT <- data.table(data)

## mean length of a message
# len <- dataT[,.(mes_length = mean(nchar(message))), by = name]

## total number of messages
# numb <- dataT[,.(mes_numb = length(message)), by = name]


## COUNT WORDS
regexCount <- function(dataT, regex, ignore_case = T, type = 'ttl'){
    ## type is 'ttl' or 'avrg'
    switch(type,
        'ttl' = {title <-  paste('total n. of',regex)
                   f <- sum
                 },
        'avrg' = {title <- paste('avrg n. of',regex,'per msg')
                   f <- mean
                 }          
    )    
    if(ignore_case) 
         ans <- dataT[,.(f(str_count(message, ignore.case(regex)))),by = name]
    else ans <- dataT[,.(f(str_count(message, regex))),by = name]
    setnames(ans, 'V1', title)
    ans
}


wordSearch <- function(data,regex){
    data[grep(regex,data[,2]),]
}