# Oberlin College Football Attendance

# Load dplyr and ggplot2 packages
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

# Set working directory
setwd("~/Documents/Blog/Living-in-the-Sprawl/Oberlin Football")

obe <- read.csv("obe_foot_0013.csv")
head(obe)
colnames(obe)
str(obe)

obe$Attendance <- as.numeric(as.character(obe$Attendance))
obe$Season <- as.factor(obe$Season)

obessn <- read.csv("obe_foot_ssn.csv")
str(obessn)

obessn$Season <- as.factor(obessn$Season)

# Attendance Plot by Season
png("plot1.png", height = 350, width = 550)
ggplot(obessn, aes(x=Season, y=AttendanceAvg, group=Location, size=2)) +
	geom_point(aes(color=Location)) +
	geom_smooth(aes(color=Location, size = 1), method=lm,   # Add linear regression lines
                se=FALSE) +
	scale_color_manual(values=c("Darkred","Gold"),breaks=c("Home","Away")) +
    scale_x_discrete(breaks=seq(2000,2013,2)) +
    labs(x="", y="Average Attendance",title="Oberlin College Football Average Attendance, 2000-2013") +
	guides(size=F) +
    theme_minimal()
dev.off()

# Point Differential Plot - Each Game from 2000-2013	
png("plot2.png", height = 350, width = 550)
ggplot(obe, aes(x=Season, y=PtDiff)) +
        geom_point(color="gray0", size=5.5, alpha=8/10)+
		geom_point(aes(color=Wooster, size=5)) +
		scale_color_manual(values=c("Darkred","Gold"),
											name="Opponent",
											breaks=c("Y","N"),
											labels=c("Wooster","Other Team")) +
		geom_hline(aes(yintercept=0), color="grey",linetype="dashed") +
		guides(size=F) +
		labs(x="", y = "",
             title="Oberlin College Football Point Differential,\nEach Game From 2000-2013")  +
		scale_x_discrete(breaks=seq(2000,2013,2))  +
		theme_minimal()
dev.off()