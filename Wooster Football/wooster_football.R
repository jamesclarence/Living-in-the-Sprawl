# Living in the Sprawl
# College of Wooster Football, 1999-2013

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

setwd("~/Documents/Blog/Living-in-the-Sprawl/Wooster Football")

woo <- read.csv("woo_foot.csv")
woossn <- read.csv("woo_foot_ssn.csv")


### What I want to look up:
# 1. Winning Percentage - each season
ggplot(woossn, aes(x=Season, y=WinPct)) +
    geom_bar(fill="gold",color="black",stat="identity") +
    labs(x="Season", y="Winning Percentage", 
         title="College of Wooster Football, 1999-2013") +
    theme_minimal()

# 2. Point Differential
ggplot(woossn, aes(x=Wins, y=Differential)) +
    geom_point(color="gold", aes(size = 4)) +
    labs(x="Wins", y="Point Differential",
         title="College of Wooster Football Point Differential, 1999-2013") +
    theme_minimal()