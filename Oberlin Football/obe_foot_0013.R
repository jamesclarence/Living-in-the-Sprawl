# Oberlin College Football Attendance

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

setwd("~/Documents/Blog/Living-in-the-Sprawl/Oberlin Football")

obe <- read.csv("obe_foot_0013.csv")
head(obe)
colnames(obe)
str(obe)

obe$Attendance <- as.numeric(as.character(obe$Attendance))
obe$Season <- as.factor(obe$Season)

# Attendance Plot
ggplot(obe, aes(x=Season, y=Attendance, fill=Location)) +
    geom_boxplot(alpha=8/10) +
    scale_fill_manual(values=c("Darkred","Gold"),breaks=c("Home","Away")) +
    scale_x_discrete(breaks=seq(2000,2013,2)) +
    labs(x="", y="Attendance",title="Oberlin College Football Attendance,\nHome & Away (2000-2013)") +
    theme_classic()

filter(obe, Opponent=="wooster") %.%
    ggplot(aes(x=ObeScore, y=OppScore, color=Result, shape=Result, size=10)) +
        geom_point() +
        scale_color_manual(values=c("Black","Red"),breaks=c("L","W")) +
        scale_x_continuous(limits=c(0,60)) +
        labs(x="Oberlin (Points Scored)", y = "Wooster (Points Scored)",
             title="Oberlin versus Wooster, 2000-2013") +
        guides(size=F) +
        theme(legend.justification=c(1,0), legend.position=c(1,0)) +
        theme_classic()




obeW <- filter(obe, Result == "W")
ggplot(obeW, aes(x=Season, y=Result)) +
    geom_bar(stat="identity") +
    scale_y_discrete(breaks=seq(0,10,2))

group_by(obe, Season, Result) %.%
    summarise(Result)

ggplot(obe, aes(x=Season, y=Result)) +
    geom_bar(stat="identity") +
    scale_y_discrete(breaks=seq(0,10,2))



