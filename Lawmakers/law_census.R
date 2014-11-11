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

## Effectiveness of the Majority Party versus Minority Party
group_by(data, congress, year, majority) %>%
    summarise(majority_n = n(), les_majority = sum(les), ratio = (les_majority/majority_n)) %>%
    ggplot(aes(x = year, y = ratio, group = majority)) +
        geom_line() +
        geom_point(aes(shape = majority, size = 2)) +
        scale_y_continuous(breaks = seq(0, 2, 0.5)) +
        rect_d1 + rect_r1 + rect_d2 + rect_r2 +
        guides(size = F) +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.major = element_line(colour = "grey90", size = 0.2),
              plot.title = element_text(size = 11.5)) +
        labs (x = "", y = "Legislative Effectiveness Score",
              title = "Legislative Effectiveness of House Majority & Minority Party Members, 1973-2011")
    
## Number of members per Region per Congress
group_by(data, congress, Region) %>%
    summarise(members=n()) %>%
    ggplot(aes(x = congress, y = members, group = Region, color = Region)) +
    geom_line(size = 2, alpha = 4/5) +
    theme_bw()

## LES by Region from 93-112 Congress
group_by(data, congress, Region) %>%
    summarise(les_region = sum(les)) %>%
    ggplot(aes(x = congress, y = les_region, group = Region, color = Region)) +
        geom_line(size = 2, alpha = 4/5) +
        theme_classic()   

## LES score per region per region's member
group_by(data, congress, Region) %>%
    summarise(members=n(), les_region = sum(les), ratio = les_region/members) %>%
    ggplot(aes(x = congress, y = ratio, group = Region, color = Region)) +
        geom_line(size = 2, alpha = 4/5) +
        theme_classic()

# Plots Ideas:
    # 1. Democrats & Republicans
    # 2. Divisions within South region
        # 2a. South as a whole
        # 2b. Democrat & Republicans
    # 3. Boehner vs Pelosi

# Re-ordering factors practice

df <- data.frame(f = 1:4, g = letters[1:4])
df
levels(df$g)
df$g <- factor(df$g, levels = letters[4:1])
df$g
df

# 1. Group by Congress (data$congress) and census region (data$Region)
# 2. Sum each Congress' LES score (data$les) by census region for each year ($year)