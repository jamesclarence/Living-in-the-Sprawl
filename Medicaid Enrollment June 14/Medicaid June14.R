### Living in the Sprawl
### http://jamesclarence.wordpress.com/2014/08/17/heath-care-update-medicaid-enrollment-is-up-where-they-want-medicaid/
### June 2014 Medicaid Enrollment from CMS
### Medicaid & CHIP: June 2014 Monthly Applications, Eligibility Determinations and Enrollment Report (August 8, 2014)
### CMS Report URL: http://medicaid.gov/AffordableCareAct/Medicaid-Moving-Forward-2014/Downloads/June-2014-Enrollment-Report.pdf

setwd("~/Documents/Blog/Living-in-the-Sprawl/Medicaid Enrollment June 14")

# Load packages
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("scales", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

# Read in CSV file
med <- read.csv("Medicaid_Enrollment_June14.csv")

### Tidy Up Data
# Change column names
colnames(med)[2] <- "Marketplace"
colnames(med)[4] <- "May14"
colnames(med)[5] <- "June14"
colnames(med)[9] <- "Change"

# Change columns to numbers
med$Change <- as.numeric(as.character(med$Change))
med$MayJuneChange <- as.numeric(as.character(med$MayJuneChange))

# Remove State Subtotal lines
med2 <- med[med$State != "State Subtotal",]

### Plots
# Plot 1: Graph of May 14-June 14 Change by Expansion
png("plot1.png", height = 400, width = 550)
ggplot(med2, aes(x = Expanding, y = MayJuneChange)) +
    geom_point(colour="darkgrey", size = 6) +
    geom_point(alpha = 1/2, aes(size=5.5, color = Expanding)) +
    scale_y_continuous(labels=percent) +
    labs(x = "Expanding Medicaid", y = "",
         title="Medicaid Enrollment Change: May to June 2014") +
    guides(size=F, color=F)
dev.off()

# Plot 2: Graph of June-Sep 2013 to June 14 Change by Expansion
png("plot2.png", height = 400, width = 550)
ggplot(med2, aes(x = Expanding, y = Change)) +
    geom_point(colour="darkgrey", size = 6) +
    geom_point(alpha = 1/2, aes(size=5.5, color = Expanding)) +
    scale_y_continuous(labels=percent) +
    labs(x = "Expanding Medicaid", y = "",
         title="Medicaid Enrollment Change: Pre-Open Enrollment to June 2014") +
    guides(size=F, color=F)
dev.off()

# Plot 3: Graph of June-Sep 2013 to June 14 Change by Expansion by Marketplace Type
png("plot3.png", height = 400, width = 550)
ggplot(med2, aes(x = Expanding, y = Change)) +
    geom_point(colour="darkgrey", size = 6) +
    geom_point(alpha = 1/2, aes(size=5.5, color = Marketplace)) +
    scale_y_continuous(labels=percent) +
    labs(x = "Expanding Medicaid", y = "",
         title="Medicaid Enrollment Change: Pre-Open Enrollment\nto June 2014, by Marketplace") +
    guides(size=F)
dev.off()