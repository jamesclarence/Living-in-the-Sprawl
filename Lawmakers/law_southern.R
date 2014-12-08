# Lawmakers Blog, Part II

library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

setwd("~/Documents/Blog/Living-in-the-Sprawl/Lawmakers")

law <- read.csv("lawmakers.csv")
census <- read.csv("census_regions.csv")

law$les <- as.numeric(levels(law$les))[law$les]

# Need to add columns from census object to law
# law object with state names: law$st_name
# census object with state names: census$State_Abbr

join <- inner_join(x = law, y = census, by = c("st_name" = "State_Abbr"))

data <- select(join, st_name, congress, year, cd, majority, les, Region, Region_Number, Division, Division_Number)

data$congress <- factor(data$congress, levels = (93:112))
data$year <- factor(data$year, levels = c(seq(1973, 2011,2)))

## Group used from data object
grp_2 <- group_by(data, congress, year, Region)
grp_3 <- group_by(data,st_name,congress,year, Region, Division)

## Code for Splitting up the South Atlantic US Census Division
# (See below code for plots for more details)

msa <- filter(grp_3, st_name == "DE" | st_name == "DC" |st_name == "MD" |st_name == "VA" |st_name == "WV") 
msa$Division <- revalue(msa$Division, c("South Atlantic" = "Middle South Atlantic"))

esa <- filter(grp_3, st_name == "FL" | st_name == "GA" |st_name == "NC" |st_name == "SC")
esa$Division <- revalue(esa$Division, c("South Atlantic" = "East South Atlantic"))

centrals <- filter(grp_3, Division == "East South Central" | Division == "West South Central")
bind <- rbind(centrals, msa, esa)

## Group used for updated Southern divisions
grp_4 <- group_by(bind, congress, year, Division)

## Legend for updated Southern divisions
breaks_color <- scale_color_discrete(name = "Census Divisions",
                                     breaks = c("East South Central", "West South Central", "Middle South Atlantic", "East South Atlantic"),
                                     labels = c("AL, KY, MI, TN",
                                                "AK, LA, OK, TX",
                                                "DC, DE, MD, VA, WV",
                                                "FL, GA,NC, SC"))

### Part 2

## Part 2, Plot 1: Number of members per Region per Congress
png("part2_plot1.png", height = 325, width = 575)
grp_2 %>%
    summarise(members=n()) %>%
    ggplot(aes(x = year, y = members, group = Region, color = Region)) +
    geom_line(size = 2, alpha = 4/5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(size = 11.5, face = "bold")) +
    labs(x = "", y = "", 
         title = "Number of House of Representatives Members Per Region, 1973-2011")
dev.off()

## Part 2, Plot 2: LES score per region per region's member
png("part2_plot2.png", height = 325, width = 575)
grp_2 %>%
    summarise(members=n(), les_region = sum(les), ratio = les_region/members) %>%
    ggplot(aes(x = year, y = ratio, group = Region, color = Region)) +
    geom_line(size = 2, alpha = 3/5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold", vjust = 1),
          plot.title = element_text(size = 11.5, face = "bold")) +
    labs(x = "", y = "LES Score",
         title = "Average Effectiveness Per Representative in Each Region")
dev.off()

## Part 2, Plot 3: South House Membership by Division
png("part2_plot3.png", height = 325, width = 575)
grp_4 %>%
    summarise(members=n()) %>%
    ggplot(aes(x = year, y = members, group = Division, color = Division)) +
    geom_line(size = 2, alpha = 4/5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(size = 11.5, face = "bold")) +
    breaks_color + 
    labs(x = "", y = "", 
         title = "Number of Southern House Members by Census Division, 1973-2011")
dev.off()

## Part 2, Part 4: LES score per Southern division per division's member
png("part2_plot4.png", height = 325, width = 575)
grp_4 %>%
    summarise(members=n(), les_region = sum(les), ratio = les_region/members) %>%
    ggplot(aes(x = year, y = ratio, group = Division, color = Division)) +
        geom_point(aes(size = 2, color = Division), alpha = 3/4) +
        geom_line(size = 2, alpha = 3/4) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
                    axis.text.y = element_text(face = "bold"),
                    axis.title.y = element_text(face = "bold", vjust = 1),
                    plot.title = element_text(size = 11, face = "bold")) +
        guides(size = F) +
        breaks_color +
        labs(x = "", y = "LES Score",
                   title = "Average Effectiveness of Southern House Members by Census Division")
dev.off()

## South Divisions (US Census)
#Division 5: South Atlantic
# Delaware        (10)
# District of Columbia (11)
# Florida         (12)
# Georgia         (13)
# Maryland        (24)
# North Carolina  (37)
# South Carolina  (45)
# Virginia        (51)
# West Virginia   (54)

# Division 6: East South Central
# Alabama         (01)
# Kentucky        (21)
# Mississippi     (28)
# Tennessee       (47)

# Division 7:  West South Central
# Arkansas        (05)
# Louisiana       (22)
# Oklahoma        (40)
# Texas           (48)

# I am splitting up the South Atlantic division:
    # Middle South Atlantic:
        # Delaware        (10)
        # District of Columbia (11)
        # Maryland        (24)
        # Virginia        (51)
        # West Virginia   (54)
    # East South Atlantic
        # Florida         (12)
        # Georgia         (13)
        # North Carolina  (37)
        # South Carolina  (45)