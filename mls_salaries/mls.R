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
# Plot 1: Distribution of MLS Salaries from 2007-2014 ($ Values)
png("plot1.png", height = 300, width = 636)
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
dev.off()

# Plot 2: Distribution of MLS Salaries from 2007-2014 (Standard Deviation)
png("plot2.png", height = 300, width = 636)
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
dev.off()

# Plot 3: Salary Distribution of MLS Players, Histogram (Count - All)
png("plot3.png", height = 300, width = 636)
ggplot(mls_var, aes(x = sal_var)) +
    geom_histogram(binwidth = 1, color = "black", fill = "dark green") +
    labs(x = "Standard Deviation", y = "Number of Players",
         title = "Count of MLS Salaries from 2007-2014") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold", vjust = 1))
dev.off()

# Plot 4: Salary Distribution of MLS Players, Histogram (Count - 1 SD)
png("plot4.png", height = 300, width = 636)
ggplot(mls_var, aes(x = sal_var)) +
    geom_histogram(binwidth = 0.25, color = "black", fill = "dark green") +
    xlim(-1, 1) +
    labs(x = "Standard Deviation", y = "Number of Players",
         title = "Count of MLS Salaries from 2007-2014, One Standard Deviation") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold", vjust = 1))
dev.off()

# Plot 5: Salary Distribution of LA Galaxy, 2007-2014
png("plot5.png", height = 300, width = 636)
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
dev.off()

### Stats
# Median and Average Salary from 2007-2014
summarise(mls_var, median = median(guaranteedcompensation, na.rm = T),
          mean = mean(guaranteedcompensation, na.rm = T))

# Players making over a million, 2007-2014
filter(mls_var, guaranteedcompensation >= 1000000) %>% summarise(count = n())

### Unused Plots
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