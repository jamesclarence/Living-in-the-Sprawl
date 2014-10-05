### Calculate Atlanta Braves Salary per Win, 1988-2014

## Load RCurl, XML, reshape2, dplyr, scales, and ggplot2 packages
library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("XML", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("scales", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
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

salarywins <- salarywins %>%
    mutate(ratio_81 = Salary/81) %>%
    mutate(ratio_diff = ratio-ratio_81) %>%
    mutate(W_81diff = W-81) %>%
    mutate(ratio_diffpct = ((ratio_81-ratio)/ratio))

# Team Names Have Strange Characters in Them
salarywins$Team <- gsub("[^[:alnum:]]","",salarywins$Team)

# Format Year column into Factor
salarywins$Team <- as.factor(as.character(salarywins$Team))

# Create CSV
setwd("~/Documents/Blog/Living-in-the-Sprawl/Braves Salary Per Win, 1988-2014")
write.csv(salarywins, file="SalaryPerWins_NL.csv")

# If ratio_diff is positive, ratio > ratio_81
# If ratio_diff is negative, ratio < ratio_81

# If W_81diff is positive, team is x games over .500

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

# Plot 2
ggplot(salarywins, aes(x=W_81diff, y=ratio_diffpct, color=Team, size=2)) +
    geom_point(alpha=3/5) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_vline(xintercept=0, linetype="dashed") +
    scale_y_continuous(labels = percent) +
    scale_color_manual(values=c("Red","#1E90FF","darkred","Black","Orange","#C41E3A", "darkred"),
                       name="Teams",
                       labels=c("ATL","LAD","MON","PIT","SFG","STL","WSN")) +
    labs(x="Wins Above and Below 81 Games", y="Salary/Win Ratio Comparison") +
    guides(size=F) +
    theme_classic()

# Plot 3
filter(salarywins, Team=="AtlantaBraves")  %>%
    ggplot(aes(x=Year, y=ratio_diffpct, group=Team, color=Team)) +
    geom_line(size=3) +
    geom_line(size=2, color="blue") +
    scale_x_continuous(breaks=seq(1988,2014,5)) +
    scale_y_continuous(labels = percent) +
    geom_hline(yintercept=0, linetype="dashed") + 
    labs(x="", y="Salary/Win Ratio Comparison", 
         title="Atlanta Braves's Actual Salary Value versus a .500 Team") +
    guides(color=F) +
    theme_classic()