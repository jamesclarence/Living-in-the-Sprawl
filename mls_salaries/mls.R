# Major League Soccer Salaries, 2007-2014
# Source: MLS Players Association

library(dplyr)
library(ggplot2)
library(scales)

mls <- read.csv("mlssalary_0714.csv", stringsAsFactor = F)

# Salary Distribution By Year, adjusted by standard deviation
mls_var <- select(mls, year, club, lastname, guaranteedcompensation) %>%
    arrange(year) %>%
    group_by(year) %>%
        mutate(sal_sd = sd(guaranteedcompensation, na.rm = T), sal_mn = mean(guaranteedcompensation, na.rm = T),
               sal_var = ((guaranteedcompensation - sal_mn)/sal_sd))

### Plots
# Plot: Distribution of MLS Salaries from 2007-2014 (Standard Deviation)
ggplot(mls_var, aes(x = sal_var, y = as.factor(year))) +
    geom_point(color = "dark green", size = 3, alpha = 0.5) +
    scale_y_discrete(limits = rev(levels(as.factor(mls_var$year)))) +
    labs(x = "", y = "",
         title = "Distribution of MLS Salaries from 2007-2014 (Standard Deviation)") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))

ggplot(mls_var, aes(x = sal_var, y = as.factor(year))) +
    geom_point(color = "dark green", size = 3, alpha = 0.5) +
    xlim(-1, 1) +
    scale_y_discrete(limits = rev(levels(as.factor(mls_var$year)))) +
    labs(x = "", y = "",
         title = "Distribution of MLS Salaries from 2007-2014 (Standard Deviation)") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))

# Plot: Salary Distribution of MLS Players, Histogram (Count - All)
ggplot(mls_var, aes(x = sal_var)) +
    geom_histogram(binwidth = 1, color = "black", fill = "dark green") +
    labs(x = "Standard Deviation", y = "Number of Players",
         title = "Count of MLS Salaries from 2007-2014") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))

# Plot: Salary Distribution of MLS Players, Histogram (Count - 1 SD)
ggplot(mls_var, aes(x = sal_var)) +
    geom_histogram(binwidth = 0.25, color = "black", fill = "dark green") +
    xlim(-1, 1) +
    labs(x = "Standard Deviation", y = "Number of Players",
         title = "Count of MLS Salaries from 2007-2014") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))

# Plot: Distribution of MLS Salaries from 2007-2014 ($ Values)
ggplot(mls_var, aes(x = guaranteedcompensation, y = as.factor(year))) +
    geom_point(color = "dark green", size = 3, alpha = 0.5) +
    scale_x_log10(breaks = c(10000, 50000, 100000, 250000, 1000000,
                             5000000, 10000000), labels = dollar) +
    scale_y_discrete(limits = rev(levels(as.factor(mls_var$year)))) +
    labs(x = "", y = "",
         title = "Distribution of MLS Salaries from 2007-2014") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))

# Another Chart: One or two teams (Seattle, Los Angeles, Toronto)
filter(mls, club == "LA") %>%
    ggplot(aes(x = guaranteedcompensation, y = as.factor(year))) +
    geom_point(color = "dark blue", size = 3, alpha = 0.75) +
    scale_x_log10(breaks = c(10000, 50000, 100000, 250000, 1000000,
                             5000000, 10000000), labels = dollar) +
    scale_y_discrete(limits = rev(levels(as.factor(mls_var$year)))) +
    labs(x = "", y = "",
         title = "Distribution of LA Galaxy Salaries from 2007-2014") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))

# Median and Average Salary from 2007-2014
summarise(mls_var, median = median(guaranteedcompensation, na.rm = T),
          mean = mean(guaranteedcompensation, na.rm = T))

# Players making over a million, 2007-2014
filter(mls_var, guaranteedcompensation >= 1000000) %>% summarise(count = n())
