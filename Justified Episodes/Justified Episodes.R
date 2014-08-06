# Justified Episode Rankings, IMDb

library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("XML", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

#### Step 1: IMDb URLs for each Justified season
season <- c("http://www.imdb.com/title/tt1489428/episodes?season=1",
            "http://www.imdb.com/title/tt1489428/episodes?season=2",
            "http://www.imdb.com/title/tt1489428/episodes?season=3",
            "http://www.imdb.com/title/tt1489428/episodes?season=4",
            "http://www.imdb.com/title/tt1489428/episodes?season=5")

#### Step 2: Find URL for each Justified episode from object season
x <- lapply(season,unlist)
url <- getURL(x) # returns html for each of the five seasons
webpage <- readLines(tc <- textConnection(url)); close(tc) #reads html lines for every season
lines <- grep('<strong><a href=\"/title/',webpage)
lines <- data.frame(lines)
print(lines)

imdb <- "http://www.imdb.com"

#### Step 3: Paste the end of each episode's URL with http://www.imdb.com
webpage <- as.data.frame(webpage)
pagedf <- webpage$webpage[lines$lines]
pagedf <- as.character(pagedf)
pagedf2 <- strsplit(pagedf,"\"")
pagedf3 <- unlist(pagedf2)
eplines <- grep('/title/',pagedf3)
eplines <- data.frame(eplines)
pagedf3 <- as.data.frame(pagedf3)
pagedf4 <- pagedf3$pagedf3[eplines$eplines]
epURLs <- paste(imdb, pagedf4,sep="")

#### Step 4: Scrape Each Episode's Site for Season, Episode Number, Rating, and Votes
y <- lapply(epURLs,unlist)
y2 <- getURL(y)
y3 <- readLines(tc <- textConnection(y2)); close(tc)

### Step 4a: Get Season and Episode Number
# Search for <span class="nobr">Season to get Season and Episode
sandep <- grep('<span class="nobr">Season',y3)
sandep <- data.frame(sandep)
y3df <- as.data.frame(y3)
y3df2 <- y3df$y3[sandep$sandep]
y3df2 <- as.character(y3df2)
y3df3 <- gsub('        <span class="nobr">',"",y3df2)

## Columns: Split Season and Episode into separate columns
SnEpSplit <- gsub('Season | Episode',"",y3df3)
SnEpSplit2 <- strsplit(SnEpSplit,", ")
dfSandEp <- ldply(SnEpSplit2)
colnames(dfSandEp) <- c("Season","Episode")

### Step 4b: Ratings - Find Ratings and Votes for each Episode
rateEp <- grep('title="Users rated this',y3)
rateEp <- data.frame(rateEp)
y3rate <- y3df$y3[rateEp$rateEp]
y3rate2 <- as.character(y3rate)
y3rate3 <- gsub('title=\"Users rated this ',"",y3rate2)
y3rate4 <- gsub(') - click stars to rate\" >',"", y3rate3)
y3rate5 <- gsub('\\(',"", y3rate4) 

## Columns: Split Rating, Votes into separate columns
rateSplit <- gsub("\\/10","",y3rate5)
rateSplit2 <- gsub(" votes","", rateSplit)
rateSplit3 <- strsplit(rateSplit2," ")
dfRate <- ldply(rateSplit3)
colnames(dfRate) <- c("Rating","Votes")

#### Step 5: TV Ratings From Wikipedia
tvratings <- read.csv(file="justified tv ratings.csv")
TVrating <- tvratings$TV.Rating

#### Step 6: Combine Season/Episode Numbers with Ratings/Votes
justified <- cbind(dfSandEp,dfRate)
justified <- justified[-53,] # remove the "Sneak Peak" for Season 5
justified <- cbind(justified, TVrating)

#### Step 7: Create CSV
write.csv(justified, file = "justified imdb rankings.csv", row.names=F)