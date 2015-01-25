# "Bad" Code
# Code I couldn't get to work
# I had trouble with htmltab package
    # Couldn't unlist the data frames from the html tables
    # It was fine one website at a time, but running a function
    # pushed the tables all together

library("htmltab")
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")

url <- "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings"

df <- htmltab(doc = url, header = c(1:2))
df$season <- rep("14-15", times = length(df))
rename(df, c("Conference Only" = "Conf_Rec", "Conf_Win" ="Conference Only.1" = , 
       "Conference Only.2" = "Conf_PF", "Conference Only.3" = "Conf_PA",
       "Overall" = "All_Rec", "Overall.1" = "All_Win", "Overall.2" = "All_PF", 
       "Overall.3" = "All_PA", "Overall.4" = "All_L10", "Overall.5" = "All_Streak"))

url2 <- c("http://www.d3hoops.com/conf/NCAC/men/2006-07/standings", 
          "http://www.d3hoops.com/conf/NCAC/men/2007-08/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2008-09/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2010-11/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2011-12/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2012-13/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")


# Does NCAC have differing team numbers in each season
######## WORKS! ########
for (i in url2)  {
    standings <- htmltab(doc = i, header = c(1:2))
    sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",i)
    sub2 <- gsub('/standings',"",sub1)
    standings$season <- rep(sub2, times = length(i))
    merge_all(standings)
}
#########


# Need to split each df apart

standings <- list()
record <- function() {
    for (i in url2)  {
    standings <- htmltab(doc = i, header = c(1:2))
    sub1[i] <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",i)
    sub2[i] <- gsub('/standings',"",sub1[i])
    standings$season <- rep(sub2[i], times = length(i))
    print(standings)
    }
}

for (i in url2) {
    sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",i)
    sub2 <- gsub('/standings',"",sub1)
    x <- rep(sub2, times = 11)
    print(x)
}

function(){
    library("htmltab")
    url2 <- c("http://www.d3hoops.com/conf/NCAC/men/2006-07/standings", 
              "http://www.d3hoops.com/conf/NCAC/men/2007-08/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2008-09/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2010-11/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2011-12/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2012-13/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")
    standings <- data.frame()
    df <- data.frame()
    for (i in url2)  {
        standings <- htmltab(doc = i, header = c(1:2))
    }
    for (i in url2) {
        sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",i)
        sub2 <- gsub('/standings',"",sub1)
        x <- rep(sub2, times = 11)
    }
    df <- cbind(df, x)
}

htmltab(doc = "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings", header = c(1:2))

for i in list {
    url2 <- c("http://www.d3hoops.com/conf/NCAC/men/2006-07/standings", 
              "http://www.d3hoops.com/conf/NCAC/men/2007-08/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2008-09/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2010-11/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2011-12/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2012-13/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")
    list <- lapply(url2, htmltab)
    rep("14-15", 2) 
}

for (i in url2)  {
    df <- htmltab(doc = i, header = c(1:2))
    sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",i)
    sub2 <- gsub('/standings',"",sub1)
    x <- rep(sub2, times = 11)
    bind <- cbind(df,x)
    class(bind)
}


#### Second Try ####
library(XML)
library(RCurl)
library(htmltab)

url2 <- "http://www.d3hoops.com/conf/NCAC/men/2006-07/standings"
html <- getURL(url2)
tbl <- readHTMLTable(html, as.data.frame = T, which = 1)

# Third Attempt
url2 <- lapply(url,unlist)
url3 <- getURL(url2)
tbl <- readHTMLTable(url3, as.data.frame = T)
# need to merge the data frames within the list (tbl)

for (i in url) {
    sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",i)
    sub2 <- gsub('/standings',"",sub1)
    season <- rep(sub2, times = length(i))
    print(season)
}