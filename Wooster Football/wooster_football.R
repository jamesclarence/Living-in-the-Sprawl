# Living in the Sprawl
# College of Wooster Football, 1999-2013

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

setwd("~/Documents/Blog/Living-in-the-Sprawl/Wooster Football")

woo <- read.csv("woo_foot.csv")
woossn <- read.csv("woo_foot_ssn.csv")


### What I want to look up:
# 1. Winning Percentage - each season
png("plot1.png", height = 350, width = 550)
ggplot(woossn, aes(x=Season, y=WinPct)) +
    geom_bar(fill="gold",color="black",stat="identity") +
    labs(x="Season", y="Winning Percentage", 
         title="College of Wooster Football, 1999-2013") +
    theme_minimal()
dev.off()

# 2. Point Differential
png("plot2.png", height = 350, width = 550)
ggplot(woossn, aes(x=Wins, y=Differential)) +
    geom_point(color="black", size=6, alpha=8/10) +
    geom_point(color="gold", size=5) +
    labs(x="Wins", y="Point Differential",
         title="College of Wooster Football Point Differential, 1999-2013") +
    geom_hline(aes(yintercept=0), color="grey",linetype="dashed") +
    guides(size=F) +
    theme_minimal()
dev.off()