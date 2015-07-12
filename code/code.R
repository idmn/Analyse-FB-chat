setwd("D:/Iaroslav/Projects/Facebook")
Sys.setlocale("LC_CTYPE", "russian")
Sys.setlocale('LC_TIME','english')
# messages <- "data/html/messages.htm"
# page <- readLines(file)
# Encoding(page) <- "UTF-8"

require(XML)
messages.parsed <- htmlParse(readLines("data/html/messages.htm"))
settings.parsed <- htmlParse(readLines("data/html/settings.htm"))


## get locale
locale <- xpathSApply(settings.parsed, '//table[1]/tr[1]/td',xmlValue)


## GET LIST OF THREAD NODES
nodeList <- getNodeSet(messages.parsed,"//div[@class = 'thread']")
## ASSIGN NAMES 
names(nodeList) <- xpathSApply(messages.parsed,"//div[@class = 'thread']/text()",
                               xmlValue)

## load other .R files
source("Analyse-FB-chat/code/getThread.R")
source("Analyse-FB-chat/code//activity.R")
source("Analyse-FB-chat/code//wordCount.R")