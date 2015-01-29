# College of Wooster Basketball

library(XML)
library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)

# NCAC Standings, 2006-2015 seasons
url <- c("http://www.d3hoops.com/conf/NCAC/men/2006-07/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2007-08/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2008-09/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2010-11/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2011-12/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2012-13/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings",
         "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")

# Get standing tables from above URLs
url2 <- lapply(url,unlist)
url3 <- getURL(url2)
tbl <- readHTMLTable(url3, as.data.frame = T, stringsAsFactors = F)

# Merge the data frames within the list (tbl)
merge.all <- function(x, y)
    merge(x, y, all=TRUE)
out <- Reduce(merge.all, tbl)

# Data frame maintenance
out <- out[-c(1:2),] # remove first two rows
out <- out[, -6] # remove empty column

# Get seasons for out data frame
sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",url)
sub2 <- gsub('/standings',"",sub1)

# DePauw (four seasons - 2011-12:2014-15), Earlham (3 seasons - 2006-07:08-09)
s1 <- data.frame(season = rep(sub2, times = 2))
s2 <- data.frame(season = sub2[c(5:8, 1:3)]) # Depauw and Earlham
s3 <- data.frame(season = rep(sub2, times = 7))
season <- rbind(s1, s2, s3)

# Bind out and season
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

woo <- woo %>% # add Wooster column for plotting
    mutate(wooster = ifelse(school == "Wooster", "Y","N"))

woo$conf_Win <- as.numeric(woo$conf_Win)
woo$conf_pf <- as.numeric(woo$conf_pf)

### Plots

# Plot 1: Wooster & Other NCAC School Winning %, 2006-2015
ggplot(woo, aes(x = season, y = conf_Win, group = school, color = wooster, size = wooster)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0.5, 1, by = 0.1)) +
    scale_color_manual(name = "School",
                       breaks = c("Y", "N"),
                       labels = c("Wooster", "Other"),
                       values = c("black", "gold")) +
    scale_size_manual(values = c(1,2)) +
    guides(size = F) +
    labs(x = "Season", y = "",
         title = "NCAC Conference Winning Percentage, 2006-2015") +
    theme_minimal() +
    theme(axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(face = "bold"))

# Plot 2: Conference Points Per Game
woo %>% # don't include 14-15 season
    select(school, conf_pf, season, wooster) %>%
    filter(season != "14-15") %>%
    group_by(season) %>%
    mutate(conf_pfsd = sd(conf_pf), conf_pfmn = mean(conf_pf),
           conf_pfvar = ((conf_pf - conf_pfmn)/conf_pfsd)) %>%
    ggplot(aes(x = conf_pf, y = conf_pfvar, color = wooster)) +
        geom_point() +
        labs(x = "Points Scored By Season", y = "Distance From Mean",
             title = "NCAC Conference Season Point Total")
        theme_minimal() +
        scale_color_manual(name = "School",
                           breaks = c("Y", "N"),
                           labels = c("Wooster", "Other"),
                           values = c("black", "gold"))

ggplot(woo, aes(x = as.numeric(conf_pf), y = as.numeric(conf_pa), color = wooster)) +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 1500, by = 100)) +
    scale_y_continuous(breaks = seq(0, 1500, by = 100))

