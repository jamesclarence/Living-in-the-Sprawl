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

woo <- setnames(woo, c("V1" = "school", "V2" = "conf_rec", "V3" = "conf_Win", "V4" = "conf_pf",
"V5" = "conf_pa", "V7" = "all_rec", "V8" = "all_win", "V9" = "all_pf",
"V10" = "all_pa", "V11" = "all_l10", "V12" = "all_streak", "season" = "season"))

# pf = Points For
# pa = Points Allowed
# l10 = Last 10 Games record

woo <- woo %>% # add Wooster column for plotting
    mutate(wooster = ifelse(school == "Wooster", "Y","N"))

woo$conf_Win <- as.numeric(woo$conf_Win)
woo$conf_pf <- as.numeric(woo$conf_pf)
woo$conf_pa <- as.numeric(woo$conf_pa)

### Plots

# Plot 1: Wooster & Other NCAC School Winning %, 2006-2015
png("plot1.png", height = 325, width = 689)
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
dev.off()

# Plot 2: Conference Points Scored Per Game
woo_pf <- woo %>% # don't include 14-15 season
    select(school, conf_pf, season, wooster) %>%
    filter(season != "14-15") %>%
    group_by(season) %>%
    mutate(conf_pfsd = sd(conf_pf), conf_pfmn = mean(conf_pf),
           conf_pfvar = ((conf_pf - conf_pfmn)/conf_pfsd))

png("plot2.png", height = 325, width = 689)
ggplot(woo_pf, aes(x = conf_pf, y = conf_pfvar, color = wooster)) +
    geom_point(size = 8, alpha = 0.8) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    guides(size = F) +
    labs(x = "Points Scored By Season", y = "Standard Deviation",
         title = "NCAC Conference Season Points Scored, 2006-2014") +
    theme_minimal() +
    theme(axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(face = "bold")) +
    scale_color_manual(name = "School",
                       breaks = c("Y", "N"),
                       labels = c("Wooster", "Other"),
                       values = c("black", "gold"))
dev.off()

# Plot 3: Conference Points Allowed Per Season
woo_pa <- woo %>% # don't include 14-15 season
    select(school, conf_pa, season, wooster) %>%
    filter(season != "14-15") %>%
    group_by(season) %>%
    mutate(conf_pasd = sd(conf_pa), conf_pamn = mean(conf_pa),
           conf_pavar = ((conf_pa - conf_pamn)/conf_pasd))

png("plot3.png", height = 325, width = 689)
ggplot(woo_pa, aes(x = conf_pa, y = conf_pavar, color = wooster)) +
    geom_point(size = 8, alpha = 0.8) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    guides(size = F) +
    labs(x = "Points Allowed By Season", y = "Standard Deviation",
         title = "NCAC Conference Season Points Allowed, 2006-2014") +
    theme_minimal() +
    theme(axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(face = "bold")) +
    scale_color_manual(name = "School",
                       breaks = c("Y", "N"),
                       labels = c("Wooster", "Other"),
                       values = c("black", "gold"))
dev.off()