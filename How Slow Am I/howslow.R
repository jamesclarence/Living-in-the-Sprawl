### How Slow Am I

### Races
# Athens Half Marathon (2011)
    # http://results.active.com/events/athens-ga-half-marathon-athhalf2011/males-overall
# Iron Horse 15K (2011) - can't find results
# Save the Light Half Marathon (Folly Beach, SC) (2/4/2012)
    # http://actioncarolina.com/sl12hres.html
# Olde Rope Mill (4/7/2012)
    # http://www.athlinks.com/race/event?raceid=200974
# Red Top Roaster (8/4/2012)
    # http://www.athlinks.com/Race/Event?raceID=222383&courseID=308130
# Atlanta Marathon (10/28/2012)
    # http://atlantatrackclub.org/documents/download/2013amresults_final
# Tom King Classic (East Nashville Half Marathon) (3/9/2013)
    # http://www.nashvillestriders.com/storage/results/results-2013/TK%20HALF%20OVERALL%202013.txt
# RunWILD Peeler Park (4/7/2013) - can't find results
# 4 Bridges Half Marathon (10/20/2013) - http://jamesclarence.wordpress.com/2013/10/28/4-bridges-half-marathon-in-box-plots/
    # https://dl.dropboxusercontent.com/u/52479449/Results/4B_ResultsOverall.html
# Gobble Jog (11/28/2014)
    # http://results.active.com/events/mdj-must-gobble-jog-5k-10k/10k-award-winners

library(XML)
library(RCurl)
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)

## 4 Bridges Half Marathon (2013)
url_4b <- "https://dl.dropboxusercontent.com/u/52479449/Results/4B_ResultsOverall.html"
html_4b <- getURL(url_4b)
tbl_4b <- readHTMLTable(html_4b, as.data.frame = T, which = 1)

# Rename columns; Extra space at end of column names
# use data.table to rename
colnames(tbl_4b)
# [1] "Position "    "Bib # "       "Name "        "Finish Time "
# [5] "Pace "        "Chip Time "   "Division "    "Gender " 

setnames(tbl_4b, old=c("Position ","Bib # ", "Name ", "Finish Time ", "Pace ","Chip Time ", "Division ", "Gender "), 
         new=c("Position", "Bib", "Name", "Time_Finish", "Pace", "Time_Chip", "Division", "Gender"))

write.csv(tbl_4b, "4briges2013.csv", row.names=F)
# Time format is off in Excel, where I read CSV file, but it is fine when
# custom formatted in the Time category

# Read all races (except Iron Horse 15K & RunWild15K) from CSV
setwd("~/Documents/Blog/Living-in-the-Sprawl/How Slow Am I")
times <- read.csv("times.csv")

times$hms <- as.POSIXct(as.character(times$TIME), format = "%H:%M:%S")

# All Races Distribution
ggplot(times, aes(x=hms, fill = as.factor(Distance))) +
    geom_histogram(binwidth = 30, alpha = 0.5, position = "identity") +
    theme_classic()
    

ggplot(times, aes(x=hms, fill = as.factor(Distance))) +
    geom_density(alpha = .5) +
    theme_classic()
        

# Half Marathon Distribution
filter(times, Distance == 13.1) %>%
    ggplot(aes(x=hms)) + 
        geom_histogram()

# Marathon Distribution
filter(times, Distance == 26.2) %>%
    ggplot(aes(x=hms)) + 
    geom_histogram()


filter(times, Distance == 6.2) %>%
    ggplot(aes(x=hms)) + 
    geom_histogram() +
    theme_classic()

### Plot 1: All Races, by Time and Result

breaks_all <- scale_color_discrete(name = "Events",
                                   breaks = c("4 Bridges", "Athens Half Marathon", 
                                              "Atlanta Marathon", "Gobble Jog",
                                              "Olde Rope", "RedTopRoaster", 
                                              "Save the Light", "TomKingClassic"),
                                   labels = c("4 Bridges", "Athens Half", "Atl. Marathon",
                                              "Gobble Jog", "Olde Rope", "Red Top",
                                              "Save The Light", "Tom King"))

# All Races
png("plot1.png", height = 325, width = 575)
times %>% # Like this one
    group_by(PLACE, JAY, hms) %>%
    ggplot(aes(x = hms, y = PLACE)) +
    geom_point(aes(color=Event, size = JAY)) +
        theme_classic() +
        guides(size=F) +
        labs(x = "Race Time", y = "Place") +
        breaks_all +
        theme(axis.text.x = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.text.y = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"),
              legend.justification=c(1,0), legend.position=c(1,0))
dev.off()

# Half Marathon Results
half <- filter(times, Distance == 13.1)

breaks_half <- scale_color_discrete(name = "Half Marathons",
                                    breaks = c("4 Bridges", "Athens Half Marathon", "Save the Light", "TomKingClassic"),
                                    labels = c("4 Bridges",
                                               "Athens",
                                               "Save The Light",
                                               "Tom King"))

half %>%
    group_by(PLACE, JAY, hms) %>%
    ggplot(aes(x = hms, y = PLACE)) +
        geom_point(aes(color=Event, size = JAY)) +
        guides(size = F) +
        theme_classic() +
        theme(axis.text.x = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.text.y = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold")) +
        labs(x = "Finish Time", y = "Place") +
        breaks_half

# Median per Event
group_by(times, Event) %>%
    summarise(median = median(hms, na.rm=T)) 

### Plot 2: Percentile Finish

jay_place <- select(times, Event, Year, Distance, JAY, PLACE) %>%
    arrange(Year) %>%
    filter(JAY == "Y") %>%
    group_by(Event)

event_place <- group_by(times, Event) %>%
    summarise(OutOf = n())

jay <- inner_join(x = jay_place, y = event_place, by = "Event")

jay_pct <- jay %>%
    mutate(percentile = round(100*(1 - (PLACE/OutOf)),1))
    
ggplot(jay_pct, aes(x = Year, y = percentile)) +
    geom_point(aes(color = Event, size = 10), alpha = .8) +
    theme_classic() +
    guides(size = F) +
    breaks_all +
    scale_size_continuous(range = range(c(3,10))) +
    theme(axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")) +
    labs(x = "Year", y = "Percentile Finish")

         
### Compare myself with different age groups (hilarious)
    # filter me + ages 50 and over; me + my age and younger

### Plot 3: Compare myself with different age groups

# Ages 50+ and I
times2 <- read.csv("times.csv", stringsAsFactor = F)

times2$AGE <- as.numeric(times2$AGE)
times2$hms <- as.POSIXct(as.character(times2$TIME), format = "%H:%M:%S")

# No ages in 4 Bridges

jay50_data <- filter(times2, AGE >= "50" | JAY == "Y")
jay50_data <- filter(jay50_data, Event != "4 Bridges")

# Need to make new Places based on jay50_data
jay50_data <- group_by(jay50_data, Event) %>%
    mutate(place50 = order(hms))

jay50_place <- select(jay50_data, Event, Year, Distance, AGE, JAY, place50) %>%
    arrange(Year) %>%
    filter(JAY == "Y") %>%
    group_by(Event)

event50_place <- group_by(jay50_data, Event) %>%
    summarise(OutOf = n())

jay50 <- inner_join(x = jay50_place, y = event50_place, by = "Event")

jay50_pct <- jay50 %>%
    mutate(percentile = round(100*(1 - (place50/OutOf)),1))

ggplot(jay50_pct, aes(x = Year, y = percentile)) +
    geom_point(aes(color = Event, size = 10), alpha = .8) +
    theme_classic() +
    guides(size = F) +
    breaks_all +
    scale_size_continuous(range = range(c(3,10))) +
    theme(axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")) +
    labs(x = "Year", y = "Percentile Finish")

# Combine Normal and Age 50 Comparison Percentiles
jay_all50 <- left_join(x = jay_pct, y = jay50_pct, by = "Event")

# colnames(jay_all50)
# [1] "Event"        "Year.x"       "Distance.x"   "JAY.x"       
# [5] "PLACE"        "OutOf.x"      "percentile.x" "Year.y"      
# [9] "Distance.y"   "AGE"          "JAY.y"        "place50"     
# [13] "OutOf.y"     "percentile.y"

jay_all50 <- melt(jay_all50, id.vars = c(1:6, 8:13), variable.name = "filter", 
                  value.name = "percentile")

ggplot(jay_all50, aes(x = filter, y = percentile, group = Event)) +
    geom_line(aes(color = Event), linetype = "dashed") +
    geom_point(aes(color = Event, shape = filter, size = 10), alpha = .8) +
    theme_classic() +
    guides(size = F) +
    breaks_all +
    scale_size_continuous(range = range(c(3,10))) +
    scale_x_discrete(labels=c("All Ages", "Ages 50+"))
    theme(axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")) +
    labs(x = "All Ages vs. Ages 50+", y = "Percentile Finish")
