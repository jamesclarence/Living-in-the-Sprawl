### Calculate Atlanta Braves Salary per Win, 1988-2014

## Load XML, reshape2, dplyr, and ggplot2 packages
library("XML", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

## Season & Salary from USA Today
fileUrl <- "http://www.usatoday.com/sports/mlb/braves/salaries/2014/team/all/"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)

# Seasons
season1 <- xpathSApply(doc, '//td [@class="season_key"]', xmlValue)
season2 <- gsub("\n", "",season1)
season3 <- gsub("[[:space:]]","", season2)

# End of Season Salary
salary1 <- xpathSApply(doc, '//td [@class="salary"]', xmlValue)
salary2 <- gsub("\n", "",salary1)
salary3 <- gsub("[[:space:]]|\\$|,", "",salary2)

## Wins Per Season, 1988-2014 (27 seasons) from Baseball Reference
fileUrl2 <- "http://www.baseball-reference.com/teams/ATL/"
doc2 <- htmlTreeParse(fileUrl2,useInternal=TRUE)

wins1 <- xpathSApply(doc2,'//tr  [@class=""]',xmlValue)
wins2 <- wins1[2:28]

wins1[1]
bravesBR <- colsplit(wins2,"\\n", names=c("Rk","Year", "Team", "Lg","G","W","L","Ties","W-L%", "pythW-L%",
                              "Finish","GB","Playoffs","R","RA","BatAge", "PAge", "#Bat",
                              "#P","Top Player", "Managers"))

## Combine the columns and format it
braves <- cbind(Year = bravesBR$Year, Team = bravesBR$Team, Wins = bravesBR$W, Salary = salary3)
braves <- as.data.frame(braves)
braves$Wins <- as.numeric(as.character(braves$Wins))
braves$Salary <- as.numeric(as.character(braves$Salary))
str(braves)

# Add the Salary/Wins column
mutate(braves, ratio = Salary/Wins) %.%
    ggplot(aes(x=Year, y=ratio, group=Team)) +
        geom_line()

# Include the other NL playoff teams since Frank Wren took over?
