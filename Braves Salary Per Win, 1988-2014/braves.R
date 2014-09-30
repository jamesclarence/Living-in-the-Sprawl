### Calculate Atlanta Braves Salary per Win, 1988-2014

## Load RCurl, XML, reshape2, dplyr, and ggplot2 packages
library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("XML", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

# 2014 NL Playoff Teams

# Washington - USA Today: nationals; BR: WSN
# St. Louis -  USA: cardinals ; BR: STL
# Pittsburgh - USA: pirates; BR: PIT
# Los Angeles - USA: dodgers; BR: LAD
# San Francisco - USA: giants; BR: SFG

atl_usa <- "http://www.usatoday.com/sports/mlb/braves/salaries/2014/team/all/"
wsn_usa <- "http://www.usatoday.com/sports/mlb/nationals/salaries/2014/team/all/"
stl_usa <- "http://www.usatoday.com/sports/mlb/cardinals/salaries/2014/team/all/"
pit_usa <- "http://www.usatoday.com/sports/mlb/pirates/salaries/2014/team/all/"
lad_usa <- "http://www.usatoday.com/sports/mlb/dodgers/salaries/2014/team/all/"
sfg_usa <- "http://www.usatoday.com/sports/mlb/giants/salaries/2014/team/all/"

atl_br <- "http://www.baseball-reference.com/teams/ATL/"
wsn_br <- "http://www.baseball-reference.com/teams/WSN/"
stl_br <- "http://www.baseball-reference.com/teams/STL/"
pit_br <- "http://www.baseball-reference.com/teams/PIT/"
lad_br <- "http://www.baseball-reference.com/teams/LAD/"
sfg_br <- "http://www.baseball-reference.com/teams/SFG/"

# 1: BR (Get Team Name)
# 2: USA Today (Get Salary)

br <- c(atl_br, wsn_br, stl_br,pit_br, lad_br, sfg_br)
usa <- c(atl_usa, wsn_usa, stl_usa,pit_usa, lad_usa, sfg_usa)

### Step 1: Team Names, Season, Games Played, and Wins from Baseball-Reference
x <- lapply(br,unlist)
url_x <- getURL(x)
html_br <- htmlTreeParse(url_x,useInternal=TRUE)

wins_br1 <- xpathSApply(html_br,'//tr  [@class=""]',xmlValue)
team_br <- colsplit(wins_br1,"\\n", names=c("Rk","Year", "Team", "Lg","G","W","L","Ties","W-L%", "pythW-L%",
                                          "Finish","GB","Playoffs","R","RA","BatAge", "PAge", "#Bat",
                                          "#P","Top Player", "Managers"))

# Select Year, Team, G, & W columns
# Remove spaces from each column
team_br <- select(team_br, Year, Team, G, W)
team_br$Year <- gsub("[[:space:]]", "",team_br$Year)
team_br$Team <- gsub("   ", "",team_br$Team)
team_br$G <- gsub("[[:space:]]", "",team_br$G)
team_br$W <- gsub("[[:space:]]", "",team_br$W)

# Remove Rows with "  Tm" as Team () 
team_br <- filter(team_br, !Team=="  Tm")

# Format data frame
team_br$Year <- as.numeric(as.character(team_br$Year))
team_br$Team <- as.factor(as.character(team_br$Team))
team_br$G <- as.numeric(as.character(team_br$G))
team_br$W <- as.numeric(as.character(team_br$W))

# Only keep years 1998:2014
team_br <- filter(team_br, Year>=1988)


### SALARIES FOR THESE TEAMS (from USA Today)
y <- lapply(usa,unlist)
url_y <- getURL(y)
html_usa <- htmlTreeParse(url_y,useInternal=TRUE)

# Seasons
season1 <- xpathSApply(html_usa, '//td [@class="season_key"]', xmlValue)
season2 <- gsub("\n", "",season1)
season3 <- gsub("[[:space:]]","", season2)

# End of Season Salary
salary1 <- xpathSApply(html_usa, '//td [@class="salary"]', xmlValue)
salary2 <- gsub("\n", "",salary1)
Salary <- gsub("[[:space:]]|\\$|,", "",salary2)

Salary <- data.frame(Salary)
Salary$Salary <- as.numeric(as.character(Salary$Salary))

### Bind team_br and Salary together
salarywins <- cbind(team_br, Salary)

# Add Salary/Wins ratio
salarywins <- mutate(salarywins, ratio = Salary/W)

# Format Year column into Factor
salarywins$Team <- as.factor(as.character(salarywins$Team))

### Plots
# Plot 1: 1988-2014
ggplot(salarywins, aes(x=Year, y=ratio, group=Team, color=Team)) +
    geom_line(size=2, alpha = 4/5) +
    scale_x_continuous(breaks=seq(1988,2014,5)) +
    scale_y_continuous(labels = c(0, 0.5, 1, 1.5, 2, 2.5)) +
    scale_color_manual(values=c("Red","#1E90FF","darkred","Black","Orange","#C41E3A", "darkred"),
                       name="Teams",
                       labels=c("ATL","LAD","MON","PIT","SFG","STL","WSN")) +
    labs(x="", y="Salary per Win (millions $)", 
         title="Team Salary Per Win, 1988-2014:\nAtlanta Braves vs. 2014 National League Playoff Teams") +
    theme_classic()

# Plot 2: Frank Wren Years (2008-2014)
ggplot(salarywins, aes(x=Year, y=ratio, group=Team, color=Team)) +
    geom_line(size=2, alpha = 3/4) +
    scale_x_continuous(limits = c(2005, 2014), breaks=seq(2005,2014,1)) +
    geom_vline(xintercept=2008, linetype="dashed") + 
    scale_y_continuous(labels = c(0, 0.5, 1, 1.5, 2, 2.5)) +
    scale_color_manual(values=c("Red","#1E90FF","darkred","Black","Orange","#C41E3A", "darkred"),
                       name="Teams",
                       labels=c("ATL","LAD","MON","PIT","SFG","STL","WSN")) +
    labs(x="", y="Salary per Win (millions $)", 
         title="Team Salary Per Win, 2008-2014: The Frank Wren Years") +
    theme_classic()
