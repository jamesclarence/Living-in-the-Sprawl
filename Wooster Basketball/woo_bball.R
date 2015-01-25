# College of Wooster Basketball

library(XML)
library(RCurl)
library(plyr)
library(magrittr)

url <- c("http://www.d3hoops.com/conf/NCAC/men/2006-07/standings", 
         "http://www.d3hoops.com/conf/NCAC/men/2007-08/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2008-09/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2010-11/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2011-12/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2012-13/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")

url2 <- lapply(url,unlist)
url3 <- getURL(url2)
tbl <- readHTMLTable(url3, as.data.frame = T, stringsAsFactors = F)

# need to merge the data frames within the list (tbl)
merge.all <- function(x, y)
    merge(x, y, all=TRUE)
out <- Reduce(merge.all, tbl)

# df maintenance
out <- out[-c(1:2),] # remove first two rows
out <- out[, -6] # remove empty column

## Seasons
sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",url)
sub2 <- gsub('/standings',"",sub1)

# DePauw (four seasons - 2011-12:2014-15), Earlham (3 seasons - 2006-07:08-09)
s1 <- data.frame(season = rep(sub2, times = 2))
s2 <- data.frame(season = sub2[c(5:8, 1:3)]) # Depauw and Earlham
s3 <- data.frame(season = rep(sub2, times = 7))
season <- rbind(s1, s2, s3)

## Bind out and season
woo <- cbind(out, season)

# Rename Columns
colnames(woo)
# [1] "V1"     "V2"     "V3"     "V4"     "V5"     "V7"     "V8"     "V9"    
# [9] "V10"    "V11"    "V12"    "season"

woo <- rename(woo, c("V1" = "school", "V2" = "conf_rec", "V3" = "conf_Win", "V4" = "conf_pf",
"V5" = "conf_pa", "V7" = "all_rec", "V8" = "all_win", "V9" = "all_pf",
"V10" = "all_pa", "V11" = "all_l10", "V12" = "all_streak"))

# pf = Points For
# pa = Points Allowed
# l10 = Last 10 Games record