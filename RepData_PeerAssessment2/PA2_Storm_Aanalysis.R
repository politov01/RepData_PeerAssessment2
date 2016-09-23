####################################
#
getwd()
setwd("C:\\HTML_DOC\\coursera.org\\05_Reproducible Research\\RPubs")
getwd()


library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(reshape2)

# Check if the data is downloaded and download when applicable
if(!file.exists("repdata_data_StormData.csv.bz2")) { 
    download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                  destfile="repdata_data_StormData.csv.bz2", mode = "wb") 
  } 

#Reading activity.csv:
data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"), header = TRUE)

str(data)
#'data.frame':   902297 obs. of  37 variables:
# $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
# $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
# $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
# $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
# $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
# $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
# $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
# $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ BGN_LOCATI: Factor w/ 54429 levels "","- 1 N Albion",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
# $ COUNTYENDN: logi  NA NA NA NA NA NA ...
# $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
# $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ END_LOCATI: Factor w/ 34506 levels "","- .5 NNW",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
# $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
# $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
# $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
# $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
# $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
# $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
# $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
# $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
# $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
# $ LATITUDE_E: num  3051 0 0 0 0 ...
# $ LONGITUDE_: num  8806 0 0 0 0 ...
# $ REMARKS   : Factor w/ 436781 levels "","-2 at Deer Park\n",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...

names(data)
# [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"     "COUNTYNAME"
# [7] "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"    "BGN_LOCATI" "END_DATE"  
#[13] "END_TIME"   "COUNTY_END" "COUNTYENDN" "END_RANGE"  "END_AZI"    "END_LOCATI"
#[19] "LENGTH"     "WIDTH"      "F"          "MAG"        "FATALITIES" "INJURIES"  
#[25] "PROPDMG"    "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
#[31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_" "REMARKS"   
#[37] "REFNUM" 

# The only relevant variables for this study are 
# "EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"
# and, therefore, only these were read into the dataset

data_corrected <- data[, c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
str(data_corrected)
#'data.frame':   902297 obs. of  8 variables:
# $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
# $ BGN_DATE  : POSIXct, format: "1950-04-18" "1950-04-18" ...
# $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
# $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
# $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
# $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
# $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...



# Analysis
# Before any further processing, it is advisable to check if the dataset needs 
# some data cleaning, which is considered an essential part of the statistical 
# analysis. We will check if the dataset lacks 
# headers, contains wrong data types (e.g. numbers stored as strings), 
# bad category labels, unknown or unexpected character encoding and so on.

#Begin date of the event. The events could span many days, but for our purposes
#we date them with the begin date.
data_corrected$BGN_DATE <- as.POSIXct(data_corrected$BGN_DATE,format="%m/%d/%Y %H:%M:%S")


length(unique(data_corrected$EVTYPE))
#[1] 985

#Some difference are caused by upper and lower cases, convert them to capital letters:
data_corrected$EVTYPE <- toupper(data_corrected$EVTYPE)
length(unique(data_corrected$EVTYPE))
#[1] 898

#Trim leading and trailing whitespace
data_corrected$EVTYPE <- gsub("^\\s+|\\s+$", "", data_corrected$EVTYPE)
length(unique(data_corrected$EVTYPE))
#[1] 890

#After small correction count less but stay 8 hundred. Many of them are caused by
#incorrect imputation, especially in the early years.

#Which types of severe weather events were registered most often. Top-30 over the whole observation period. 

sorted <- sort(table(data_corrected$EVTYPE), decreasing = TRUE, n = 20)[1:30]
print(as.data.frame(sorted))

unique(data_corrected$PROPDMGEXP)
# [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
#Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M

unique(data_corrected$CROPDMGEXP)
#[1]   M K m B ? 0 k 2
#Levels:  ? 0 2 B k K m M


# According to NWS Instruction 10-1605 (Murphy, 2016), 
# estimates should be in the form of US Dollar values and rounded 
# to three significant digits, followed by the magnitude of the value 
# (i.e., 1.55B for $1,550,000,000). 
# Values used to signify magnitude include: K for thousand $USD, 
# M for million $USD, and B for billion $USD. 
# Inspection shows, however, that many other values were used as 
# magnitudes: K, M, 0, 3, B, 4, 2, 6, h, 5, 1, H, 9, 7, 8. 
# It is, of course, impossible to be completely sure what the preparer had 
# in mind when introducing these values, but we can make educated guesses 
# about their meaning, such as h for hundreds and 6 for millions. 
# Now, we recalculate PropertyDamage and CropDamage variables taking these 
# magnitude values into account.

head(data_corrected)
str(data_corrected)
data_corrected$PropertyDamage <- 1
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="0"] <- 1
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="1"] <- 10
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="2"] <- 100
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="3"] <- 1000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="4"] <- 10000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="5"] <- 100000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="6"] <- 1000000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="7"] <- 10000000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="8"] <- 100000000

data_corrected$PropertyDamage[data_corrected$PROPDMGEXP ==""] <- 1
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="?"] <- 1
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="+"] <- 1
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="-"] <- 1
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="h"] <- 100
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="H"] <- 100
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="K"] <- 1000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="m"] <- 1000000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="M"] <- 1000000
data_corrected$PropertyDamage[data_corrected$PROPDMGEXP =="B"] <- 1000000000
table(data_corrected$PropertyDamage)
#     1     10    100   1000  10000  1e+05  1e+06  1e+07  1e+08  1e+09 
#466164     25     20 424669      4     28  11341      5      1     40 
head(data_corrected)
#   EVTYPE   BGN_DATE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
#1 TORNADO 1950-04-18          0       15    25.0          K       0           
#2 TORNADO 1950-04-18          0        0     2.5          K       0           
#3 TORNADO 1951-02-20          0        2    25.0          K       0           
#4 TORNADO 1951-06-08          0        2     2.5          K       0           
#5 TORNADO 1951-11-15          0        2     2.5          K       0           
#6 TORNADO 1951-11-15          0        6     2.5          K       0           
#  PropertyDamage
#1           1000
#2           1000
#3           1000
#4           1000
#5           1000
#6           1000

data_corrected$CropDamage <- 1
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="?"] <- 1
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="0"] <- 1
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="2"] <- 100
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="k"] <- 1000
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="K"] <- 1000
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="m"] <- 1000000
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="M"] <- 1000000
data_corrected$CropDamage[data_corrected$CROPDMGEXP =="B"] <- 1000000000
table(data_corrected$CropDamage)
#     1    100   1000  1e+06  1e+09 
#618439      1 281853   1995      9 

head(data_corrected)
#   EVTYPE   BGN_DATE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
#1 TORNADO 1950-04-18          0       15    25.0          K       0           
#2 TORNADO 1950-04-18          0        0     2.5          K       0           
#3 TORNADO 1951-02-20          0        2    25.0          K       0           
#4 TORNADO 1951-06-08          0        2     2.5          K       0           
#5 TORNADO 1951-11-15          0        2     2.5          K       0           
#6 TORNADO 1951-11-15          0        6     2.5          K       0           
#  PropertyDamage CropDamage
#1           1000          1
#2           1000          1
#3           1000          1
#4           1000          1
#5           1000          1
#6           1000          1

missingValues <- colSums(is.na(data_corrected))/nrow(data_corrected)
missingValues
#      EVTYPE     BGN_DATE   FATALITIES     INJURIES      PROPDMG   
#0.0000000000 0.0001861915 0.0000000000 0.0000000000 0.0000000000  
#  PROPDMGEXP      CROPDMG  CROPDMGEXP 
#0.0000000000 0.00000000000.0000000000 


#library(plyr)
StormData <- ddply(.data = data_corrected, .variables = .(EVTYPE),
                    Fatalities = sum(FATALITIES), 
                    Injuries = sum(INJURIES), 
                    PropertyDamageTotal = sum(PROPDMG * PropertyDamage), 
                    CropDamageTotal = sum(CROPDMG * CropDamage), 
                    summarize)
str(StormData)
head(StormData)

PopulationHarmFatalities <- arrange(StormData[c("EVTYPE","Fatalities")], desc(Fatalities))
PopulationHarmFatalities <- head(PopulationHarmFatalities[c("EVTYPE","Fatalities")], 10)
PopulationHarmFatalities
#           EVTYPE Fatalities
#1         TORNADO       5633
#2  EXCESSIVE HEAT       1903
#3     FLASH FLOOD        978
#4            HEAT        937
#5       LIGHTNING        816
#6       TSTM WIND        504
#7           FLOOD        470
#8     RIP CURRENT        368
#9       HIGH WIND        248
#10      AVALANCHE        224

PopulationHarmInjuries <- arrange(StormData[c("EVTYPE","Injuries")], desc(Injuries))
PopulationHarmInjuries <- head(PopulationHarmInjuries[c("EVTYPE","Injuries")], 10)
PopulationHarmInjuries
#              EVTYPE Injuries
#1            TORNADO    91346
#2          TSTM WIND     6957
#3              FLOOD     6789
#4     EXCESSIVE HEAT     6525
#5          LIGHTNING     5230
#6               HEAT     2100
#7          ICE STORM     1975
#8        FLASH FLOOD     1777
#9  THUNDERSTORM WIND     1488
#10              HAIL     1361



####################################
library(ggplot2)
plot_Fatalities <- ggplot(data = PopulationHarmFatalities, 
                          aes(x = reorder(EVTYPE, Fatalities), 
                              y = Fatalities, 
                              fill = EVTYPE)) + 
geom_bar(stat="identity") +
coord_flip() + 
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
ggtitle("Top 10 by Fatalities") + 
xlab("Weather events") + 
ylab("Number of Deaths") +
scale_fill_discrete(name = "Type of Event", breaks=PopulationHarmFatalities$EVTYPE)

plot_Injuries <- ggplot(data = PopulationHarmInjuries, 
                        aes(x = reorder(EVTYPE, Injuries), 
                            y = Injuries, 
                            fill = EVTYPE)) + 
geom_bar(stat="identity") + 
coord_flip() + 
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
ggtitle("Top 10 by Injuries") + 
xlab("Weather events") + 
ylab("Number of Injuries") +
scale_fill_discrete(name = "Type of Event", breaks=PopulationHarmInjuries$EVTYPE)

library(grid)
library(gridExtra)
grid.arrange(plot_Fatalities, plot_Injuries,  top = "Top 10 harmful weather Events for Fatalities & Injuries")

####################################

# Compute the economic loss by event type
EconomicDamage <- arrange(StormData, desc(PropertyDamageTotal + CropDamageTotal))
EconomicDamage <- head(EconomicDamage[c("EVTYPE","PropertyDamageTotal","CropDamageTotal")], 10)
EconomicDamage

library(reshape2)
EconomicDamage_melted <- melt(EconomicDamage, id="EVTYPE")
EconomicDamage_melted

ggplot(data = EconomicDamage_melted, 
       aes(x = EVTYPE, 
           y = value/1000000, 
           fill = variable)) + 
geom_bar(stat="identity") + 
scale_x_discrete(limits = EconomicDamage_melted$EVTYPE) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
ggtitle("Weather events with the highest economic consequences") + 
        xlab("Weather events") + 
        ylab("Total damage in millions $$$")

####################################
#
####################################
#
####################################
#
####################################
#
####################################
#
