# Lawmakers Blog, Part II

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

# will data$st_name factors be a problem?
data$congress <- factor(data$congress, levels = (93:112))
data$year <- factor(data$year, levels = c(seq(1973, 2011,2)))

## Group used from data object
grp_2 <- group_by(data, congress, year, Region)
grp_3 <- group_by(data,st_name, congress,year, Region, Division)

### Part 2

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

breaks_color <- scale_color_discrete(name = "Census Divisions",
                                     breaks = c("East South Central", "South Atlantic", "West South Central"),
                                     labels = c("AL, KY, MI, TN",
                                                "DE, DC, FL, GA,\nMD, NC, SC, VA,\nWV",
                                                "AK, LA, OK, TX"))

# Part 2, Plot 1: Number of members per Region per Congress
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

# Part 2, Plot 2: LES score per region per region's member
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

# Part 2, Plot 3: South House Membership by Division
grp_3 %>%
    filter(Region == "SOUTH") %>%
    summarise(members=n()) %>%
    ggplot(aes(x = year, y = members, group = Division, color = Division)) +
    geom_line(size = 2, alpha = 4/5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(size = 11.5, face = "bold")) +
    breaks_color +
    labs(x = "", y = "", 
         title = "Number of House of Representatives Members in the South, 1973-2011")

# Individual Southern States
grp_3 %>%
filter(Region == "SOUTH") %>%
    summarise(members=n(), les_region = sum(les), ratio = les_region/members) %>%
    ggplot(aes(x = year, y = ratio, group = st_name, color = st_name)) +
        geom_line(alpha = 3/5) +
        geom_point(aes(size = 2), alpha = 5/6) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
                    axis.text.y = element_text(face = "bold"),
                    axis.title.y = element_text(face = "bold", vjust = 1),
                    plot.title = element_text(size = 11.5, face = "bold")) +
        guides(size = F) +
        labs(x = "", y = "LES Score",
                   title = "Average Effectiveness Per Representative in the South")
# DC Rep is the highest in 1993

state <- grp_3 %>%
    filter(Region == "SOUTH") %>%
    summarise(members=n(), les_region = sum(les), ratio = les_region/members)

ggplot(grp_3, aes(x = le))