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

# join has 82 fewer observations than law. Territorites that are missing?

data <- select(join, congress, year, cd, majority, les, Region, Region_Number, Division, Division_Number)

data$congress <- factor(data$congress, levels = (93:112))
data$year <- factor(data$year, levels = c(seq(1973, 2011,2)))
data$les <- as.numeric(levels(data$les))[data$les] # change les to numbers

## When Democrats and Republicans are in Majority

# Democrats: 93-103 (1973-95); 110-111 (2007-2011)
dem1 <- data.frame(xmin_d1 = factor(1973), xmax_d1 = factor(1995), ymin_d1 = -Inf, ymax_d1 = Inf)
dem2 <- data.frame(xmin_d2 = factor(2007), xmax_d2 = factor(2009), ymin_d2 = -Inf, ymax_d2 = Inf)

# Dem Shade
rect_d1 <- geom_rect(data = dem1, aes(xmin = xmin_d1, xmax = xmax_d1, ymin = ymin_d1, ymax = ymax_d1), 
                     fill = "blue", alpha=0.25, inherit.aes = FALSE)
rect_d2 <- geom_rect(data = dem2, aes(xmin = xmin_d2, xmax = xmax_d2, ymin = ymin_d2, ymax = ymax_d2), 
                     fill = "blue", alpha=0.25, inherit.aes = FALSE)

# Republicans: 104-109 (1995-2007); 112 (2011-2013)
rep1 <- data.frame(xmin_r1 = factor(1995), xmax_r1 = factor(2007), ymin_r1 = -Inf, ymax_r1 = Inf)
rep2 <- data.frame(xmin_r2 = factor(2009), xmax_r2 = factor(2011), ymin_r2 = -Inf, ymax_r2 = Inf)

# Rep Shade
rect_r1 <- geom_rect(data = rep1, aes(xmin = xmin_r1, xmax = xmax_r1, ymin = ymin_r1, ymax = ymax_r1), 
                     fill = "red", alpha=0.25, inherit.aes = FALSE)
rect_r2 <- geom_rect(data = rep2, aes(xmin = xmin_r2, xmax = xmax_r2, ymin = ymin_r2, ymax = ymax_r2), 
                     fill = "red", alpha=0.25, inherit.aes = FALSE)

## Group used from data object
grp <- group_by(data, congress, year, majority)
grp_2 <- group_by(data, congress, year, Region)

## Plot 1: Effectiveness of the Majority Party versus Minority Party, 1973-2011
grp %>%
    summarise(majority_n = n(), les_majority = sum(les), ratio = (les_majority/majority_n)) %>%
    ggplot(aes(x = year, y = ratio, group = majority)) +
        geom_line() +
        geom_point(aes(shape = majority, size = 2)) +
        scale_y_continuous(breaks = seq(0, 2, 0.5)) +
        scale_shape_discrete(name = "Party",
                             breaks = c("1","0"),
                             labels = c("Majority", "Minority")) +
        rect_d1 + rect_r1 + rect_d2 + rect_r2 +
        guides(size = F) +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold")) +
        labs(x = "", y = "Legislative Effectiveness Score")
# 550 x 325

## Plot 2: Number of members per Region per Congress
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

## Plot 3: LES score per region per region's member
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

# Plots Ideas:
    # 1. Democrats & Republicans
    # 2. Divisions within South region
        # 2a. South as a whole
        # 2b. Democrat & Republicans
    # 3. Boehner vs Pelosi