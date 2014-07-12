poldi <- read.csv("Poldi_IG2.csv")

poldi$date <-strptime(poldi$date,"%m/%d/%y") # turn date column into dates
poldi$count <- poldi$ThumbsUp # rename count file
poldi <- within(poldi, rm(ThumbsUp))

tblPoldi <- as.data.frame.table(tapply(poldi$count,poldi$pose,sum))
colnames(tblPoldi)[1] <- "Pose"
colnames(tblPoldi)[2] <- "Count"

tblPoldi$Count <- as.numeric(as.character(tblPoldi$Count))

# Reorder table by tblPoldi$Count
tblPoldi$Count <- factor(tblPoldi$Count, levels = tblPoldi$Count[order(tblPoldi$Pose, decreasing=F)])



# Plot
png("PoldiPlot.png", height = 500, width = 480)
ggplot(data = tblPoldi, aes(x=Pose, y=Count, fill=Pose)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values=c("black","red","gold","black","red")) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y = element_text(face="bold")) +
    labs(title="World Cup 2014: Poldoski's Instagram Poses,\nMay 21-July 12, 2014",
         x = "") +
    guides(fill=F) +
    theme_bw()
dev.off()