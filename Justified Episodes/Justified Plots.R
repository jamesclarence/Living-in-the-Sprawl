#### Justified IMDb Rankings: Plots

##### Set WD, READ CSV

justified <- read.csv(file="justified imdb rankings.csv")

library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

# Clean up data for graphs
justified$Episode <- as.numeric(as.character(justified$Episode))
justified$Rating <- as.numeric(as.character(justified$Rating))
justified$Votes <- as.numeric(as.character(justified$Votes))
justified$TVrating <- as.numeric(as.character(justified$TVrating))

# Weighted Average
ssn1 <- justified[justified$Season=="1",]
ssn2 <- justified[justified$Season=="2",]
ssn3 <- justified[justified$Season=="3",]
ssn4 <- justified[justified$Season=="4",]
ssn5 <- justified[justified$Season=="5",]

# Get weighted mean of Ratings by Votes for Each Season
weight <- sapply(split(justified,justified$Season), function(x) weighted.mean(x[,3], w=x$Votes))
weightssn <- 1:5
weight <- as.data.frame(cbind(weightssn,weight))
weight$Season <- weight$weightssn


#### Plot 1: IMDb Rating Per Season
png("plot1.png", height = 380, width = 650)
ggplot(weight, aes(x=Season, y=weight, color="red", size=2)) +
    geom_line() +
    ylim(8,9) +
    labs(x="Season", y="", 
         title = "Justified: IMDb Rating Per Season, Weighted by Votes") +
    theme(axis.title.x = element_text(face="bold"),
          axis.text.y = element_text(size=12),
          plot.title = element_text(face="bold")) +
    guides(size=FALSE,color=FALSE) +
    theme_bw()
dev.off()

#### Plot 2: Justified: IMDb Rating by Episode of Season
png("plot2.png", height = 380, width = 650)
ggplot(justified, aes(x=Episode, y=Rating)) +
    geom_point(aes(size=TVrating)) +
    geom_smooth() +
    scale_x_discrete(limits=1:13) +
    ylim(7,10) +
    scale_size_continuous(name="Millions of\nViewers") +
    labs(x="Episode of Season", y= "IMDb Episode Rating", 
         title = "Justified: IMDb Rating by Episode of Season") +
    theme_bw()
dev.off()


#### Plot 3: IMDb ratings and TV viewership ratings
png("plot3.png", height = 380, width = 650)
ggplot(justified, aes(x=TVrating, y=Rating)) +
    geom_point(alpha=0.75,aes(color=Season, size=Rating)) +
    geom_smooth(method=lm, se=F) +
    ylim(7,10) +
    scale_color_gradient(low="black",high="red") +
    labs(x = "TV Ratings (millions of viewers)", y="IMDb User Rating", 
         title = "Justified: IMDb Rating versus TV Rating") +
    guides(size=F) +
    theme_bw()
dev.off()

#### Plot 4: TV Ratings per Episode of Season
png("plot4.png", height = 380, width = 650)
ggplot(justified, aes(x=Episode, y=TVrating)) +
    geom_point() +
    geom_smooth(se=F) +
    scale_x_discrete(limits=1:13) +
    labs(x="Episode of Season", y= "TV Ratings (millions of viewers)", 
         title = "Justified: TV Ratings per Episode of Season") +
    theme_bw()
dev.off()


#### Other Plots

ggplot(justified, aes(x=Episode, y=Votes)) +
    geom_point() +
    geom_smooth(se=F) +
    scale_x_discrete(limits=1:13) +
    ylim(0,1000) +
    labs(x="Episode of Season", y= "Number of IMDb Votes", 
         title = "Justified: Number of IMDb Rating Votes by Episode of Season") +
    theme_bw()

ggplot(justified, aes(x=Episode, y=Votes,color=Season,group=Season)) +
    geom_line() +
    scale_x_discrete(limits=1:13) +
    ylim(0,1000) +
    labs(x="Episode of Season", y= "Number of IMDb Votes", 
         title = "Justified: Number of IMDb Rating Votes by Episode of Season") +
    theme_bw()

### Ranking vs. Votes
ggplot(justified, aes(x=Rating, y=Votes)) +
    geom_point() +
    geom_smooth() +
    xlim(7.5,9) +
    ylim(0,1000)
    
ggplot(justified, aes(x=Rating, y=TVrating)) +
    geom_point() +
    geom_smooth()  +
    labs(x="IMDb User Rating", y= "TV Ratings (millions of viewers)", 
         title = "Justified: IMDb Rating versus TV Rating") +
    theme_bw()


ggplot(justified, aes(x=Votes, y=TVrating, color=Season)) +
    geom_point() +
    geom_smooth(method=lm, se=F)  +
    labs(x="Number of IMDb Votes", y= "TV Ratings (millions of viewers)", 
         title = "Justified: IMDb Rating versus TV Rating") +
    theme_bw()



