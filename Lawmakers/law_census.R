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
# data has 82 fewer observations than law
# Is it the territorites that are missing?

data <- select(join, congress, year, cd, majority, les, Region, Region_Number, Division, Division_Number)

data$congress <- factor(data$congress, levels = (93:112))
data$les <- as.numeric(levels(data$les))[data$les] # change les to numbers

# Effectiveness of the Majority Party versus Minority Part
group_by(data, congress, )


# Number of members per Region per Congress
group_by(data, congress, Region) %>%
    summarise(members=n()) %>%
    ggplot(aes(x = congress, y = members, group = Region, color = Region)) +
    geom_line(size = 2, alpha = 4/5) +
    theme_bw()

# LES by Region from 93-112 Congress
group_by(data, congress, Region) %>%
    summarise(les_region = sum(les)) %>%
    ggplot(aes(x = congress, y = les_region, group = Region, color = Region)) +
        geom_line(size = 2, alpha = 4/5) +
        theme_classic()   

# LES score per region per region's member
group_by(data, congress, Region) %>%
    summarise(members=n(), les_region = sum(les), ratio = les_region/members) %>%
    ggplot(aes(x = congress, y = ratio, group = Region, color = Region)) +
        geom_line(size = 2, alpha = 4/5) +
        theme_classic()


# Re-ordering factors practice

df <- data.frame(f = 1:4, g = letters[1:4])
df
levels(df$g)
df$g <- factor(df$g, levels = letters[4:1])
df$g
df

# 1. Group by Congress (data$congress) and census region (data$Region)
# 2. Sum each Congress' LES score (data$les) by census region for each year ($year)