# Load packages
library(tidyverse)
library(dataRetrieval) # For USGS data
library(readxl)
library(lubridate)
library(scales)
library(epitools)
library(ggpubr)
library(cowplot)
theme_set(theme_cowplot(12))

### Flow data ---------------------------------------------------------------

## CACHE LA POUDRE RIVER AT FORT COLLINS, CO (CLAFORCO) ~ Whitewater Park
# Retreive data from USGS gauge
siteNumber2 <- "06752260" 
WWPark <- readNWISsite(siteNumber2)
parameterCd <- "00060" # ID for discharge info
USGS <- readNWISdv(siteNumber2,parameterCd,
                    "1980-01-01","2019-12-31") # Collect up to 2019
USGS <- na.omit(USGS) # Omit NA
USGS <- USGS %>% 
  rename(discharge_cfs = X_00060_00003)
WWPark <- USGS[3:4] # Collect only date & discharge

## // 
# Get years in quartiles to use as reference
annual <- WWPark %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(av_dischyr = mean(discharge_cfs)) %>% 
  mutate(quartile = ntile(av_dischyr, 4))

# Get averaged annual (over 40-year period)
avg_annual <- WWPark %>% 
  mutate(day_of_year = strftime(Date, format = "%j"))
avg_annual$day_of_year <- as.numeric(avg_annual$day_of_year)
avg_annual <- avg_annual %>% 
  group_by(day_of_year) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

## \\

## // Collect years and convert date into day of year
# Wet
wet <- WWPark %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  filter(year %in% c(1980, 1983, 1984, 1997, 1999, 2011, 2014, 2015, 2016, 2017)) %>% 
  mutate(day_of_year = strftime(Date, format = "%j"))
wet$day_of_year <- as.numeric(wet$day_of_year) # To remove leading 0s

wet_annual <- wet %>% 
  group_by(day_of_year) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

wet_months <- wet %>% 
  mutate(month = month(Date)) %>% 
  group_by(month) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

# Wet typical
wetTypical <- WWPark %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  filter(year %in% c(1982, 1986, 1993, 1995, 1996, 1998, 2010, 2013, 2018, 2019)) %>% 
  mutate(day_of_year = strftime(Date, format = "%j"))
wetTypical$day_of_year <- as.numeric(wetTypical$day_of_year)  # To remove leading 0s

wetTypical_annual <- wetTypical %>% 
  group_by(day_of_year) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

wetTypical_months <- wetTypical %>% 
  mutate(month = month(Date)) %>% 
  group_by(month) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

# Dry typical
dryTypical <- WWPark %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  filter(year %in% c(1985, 1990, 1991, 2003, 2004, 2005, 2006, 2007, 2008, 2009)) %>% 
  mutate(day_of_year = strftime(Date, format = "%j"))
dryTypical$day_of_year <- as.numeric(dryTypical$day_of_year)  # To remove leading 0s

dryTypical_annual <- dryTypical %>% 
  group_by(day_of_year) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

dryTypical_months <- dryTypical %>% 
  mutate(month = month(Date)) %>% 
  group_by(month) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

# Dry
dry <- WWPark %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  filter(year %in% c(1981, 1987, 1988, 1989, 1992, 1994, 2000, 2001, 2002, 2012)) %>% 
  mutate(day_of_year = strftime(Date, format = "%j"))
dry$day_of_year <- as.numeric(dry$day_of_year)  # To remove leading 0s

dry_annual <- dry %>% 
  group_by(day_of_year) %>% 
  summarize(avg_discharge = mean(discharge_cfs))

dry_months <- dry %>% 
  mutate(month = month(Date)) %>% 
  group_by(month) %>% 
  summarize(avg_discharge = mean(discharge_cfs))
# \\

### FEIS table into R -------------------------------------------------------

FEISValues <- read_excel("Desktop/FEISValues.xlsx")
FEISValues$Month <- c(11,12,01,02,03,04,05,06,07,08,09,10) # Make months into values
FEISValues <- FEISValues[,-2] # Remove the second column (# of diversions)
FEISValues$DaysinMonth <- c(30,31,31,28,31,30,31,30,31,31,30,31) # Based on the Gregorian calendar

# AF/day to CF/s conv = (43560/86400) cfs
CFSconv <- (43560/86400)

FEISValues <- FEISValues %>% 
  mutate(FEISValues,
         Min_dayratecfs = (Min/DaysinMonth)*CFSconv,
         Max_dayratecfs = (Max/DaysinMonth)*CFSconv,
         Avg_dayratecfs = (Avg/DaysinMonth)*CFSconv)
FEISValuesUSE <- FEISValues[,-(2:5)]

# Obtain min (midpoint b/w FEIS min and avg), max (midpoint b/w FEIS max and avg), avg values
FEISValuesUSE <- FEISValuesUSE %>% 
  rowwise() %>% 
  mutate(medMin_dayratecfs =  median(c(Min_dayratecfs, Avg_dayratecfs), na.rm = TRUE),
         medMax_dayratecfs =  median(c(Max_dayratecfs, Avg_dayratecfs), na.rm = TRUE))
FEISValuesUSE <- FEISValuesUSE[,-(2:3)] # Clean up the table to show values to use


### Get post-NISP conditions ------------------------------------------------

# Extract diversion rate from FEIS table
# Use for wet
wjanrate <- FEISValuesUSE$medMax_dayratecfs[[3]]
wmarchrate <- FEISValuesUSE$medMax_dayratecfs[[5]]
waprilrate <- FEISValuesUSE$medMax_dayratecfs[[6]]
wmayrate <- FEISValuesUSE$medMax_dayratecfs[[7]]
wjunerate <- FEISValuesUSE$medMax_dayratecfs[[8]]
wjulyrate <- FEISValuesUSE$medMax_dayratecfs[[9]]
waugrate <- FEISValuesUSE$medMax_dayratecfs[[10]]
woctrate <- FEISValuesUSE$medMax_dayratecfs[[12]]
wdecrate <- FEISValuesUSE$medMax_dayratecfs[[2]]

# Use for wet typical and dry typical
taprilrate <- FEISValuesUSE$Avg_dayratecfs[[6]]
tmayrate <- FEISValuesUSE$Avg_dayratecfs[[7]]
tjunerate <- FEISValuesUSE$Avg_dayratecfs[[8]]
tjulyrate <- FEISValuesUSE$Avg_dayratecfs[[9]]
taugrate <- FEISValuesUSE$Avg_dayratecfs[[10]]

# Use for dry
daprilrate <- FEISValuesUSE$medMin_dayratecfs[[6]]
dmayrate <- FEISValuesUSE$medMin_dayratecfs[[7]]
djunerate <- FEISValuesUSE$medMin_dayratecfs[[8]]
djulyrate <- FEISValuesUSE$medMin_dayratecfs[[9]]
daugrate <- FEISValuesUSE$medMin_dayratecfs[[10]]

# Threshold found in FEIS
low <- 30 # Low flow threshold


## Assuming that diversion will occur if (historical discharge - diversion rate) is >30 cfs
# Wet year
wet_postnisp <- function(frame){
  
  mainframe <- frame %>% 
    mutate(month = month(Date)) %>% 
    group_by(month)
  mainframe <- mainframe %>% 
    mutate(avgdischarge_nisp = case_when((discharge_cfs-wjanrate) > low & month == 1 ~ (discharge_cfs-wjanrate),
                                         (discharge_cfs-wmarchrate) > low & month == 3 ~ (discharge_cfs-wmarchrate),
                                         (discharge_cfs-waprilrate) > low & month == 4 ~ (discharge_cfs-waprilrate),
                                         (discharge_cfs-wmayrate) > low & month == 5 ~ (discharge_cfs-wmayrate),
                                         (discharge_cfs-wjunerate) > low & month == 6 ~ (discharge_cfs-wjunerate),
                                         (discharge_cfs-wjulyrate) > low & month == 7 ~ (discharge_cfs-wjulyrate),
                                         (discharge_cfs-waugrate) > low & month == 8 ~ (discharge_cfs-waugrate),
                                         (discharge_cfs-woctrate) > low & month == 10 ~ (discharge_cfs-woctrate),
                                         (discharge_cfs-wdecrate) > low & month == 12 ~ (discharge_cfs-wdecrate),
                                         TRUE ~ as.numeric(discharge_cfs)))
  return(mainframe)
}

# Typical years
typ_postnisp <- function(frame){
  
  mainframe <- frame %>% 
    mutate(month = month(Date)) %>% 
    group_by(month)
  mainframe <- mainframe %>% 
    mutate(avgdischarge_nisp = case_when((discharge_cfs-taprilrate) > low & month == 4 ~ (discharge_cfs-taprilrate),
                                         (discharge_cfs-tmayrate) > low & month == 5 ~ (discharge_cfs-tmayrate),
                                         (discharge_cfs-tjunerate) > low & month == 6 ~ (discharge_cfs-tjunerate),
                                         (discharge_cfs-tjulyrate) > low & month == 7 ~ (discharge_cfs-tjulyrate),
                                         (discharge_cfs-taugrate) > low & month == 8 ~ (discharge_cfs-taugrate),
                                         TRUE ~ as.numeric(discharge_cfs)))
  return(mainframe)
}

# Dry year
dry_postnisp <- function(frame){
  
  mainframe <- frame %>% 
    mutate(month = month(Date)) %>% 
    group_by(month)
  mainframe <- mainframe %>% 
    mutate(avgdischarge_nisp = case_when((discharge_cfs-daprilrate) > low & month == 4 ~ (discharge_cfs-daprilrate),
                                         (discharge_cfs-dmayrate) > low & month == 5 ~ (discharge_cfs-dmayrate),
                                         (discharge_cfs-djunerate) > low & month == 6 ~ (discharge_cfs-djunerate),
                                         (discharge_cfs-djulyrate) > low & month == 7 ~ (discharge_cfs-djulyrate),
                                         (discharge_cfs-daugrate) > low & month == 8 ~ (discharge_cfs-daugrate),
                                         TRUE ~ as.numeric(discharge_cfs)))
  return(mainframe)
}


## Summarize into avg discharges into day of year
sumpostnisp <- function(frame){
  
  return(frame %>% 
           group_by(day_of_year) %>% 
           summarize(avg_discharge = mean(avgdischarge_nisp)))
}


## Apply functions in respective years
C_wet <- wet_postnisp(wet)
postnisp_wet <- sumpostnisp(C_wet)

C_wetTypical <- typ_postnisp(wetTypical)
postnisp_wetTypical <- sumpostnisp(C_wetTypical)

C_dryTypical <- typ_postnisp(dryTypical)
postnisp_dryTypical <- sumpostnisp(C_dryTypical)

C_dry <- dry_postnisp(dry)
postnisp_dry <- sumpostnisp(C_dry)


## Post-NISP ALL years
C_avg_annual <- full_join(C_dry, C_dryTypical, by=c("avgdischarge_nisp","Date")) %>% 
  full_join(.,C_wet, by=c("avgdischarge_nisp","Date")) %>% 
  full_join(.,C_wetTypical, by=c("avgdischarge_nisp","Date"))
C_avg_annual <- C_avg_annual[c("Date","avgdischarge_nisp")] # Filter to columns needed

# Create day of year column
C_avg_annual <- C_avg_annual %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  mutate(day_of_year = strftime(Date, format = "%j"))
C_avg_annual$day_of_year <- as.numeric(C_avg_annual$day_of_year)  # To remove leading 0s

# Take general average of post-NISP conditions
postnisp_avg_annual <- sumpostnisp(C_avg_annual)


# Boatable Days -----------------------------------------------------------

threshold <- 355 # Collected from economic survey data

# Historical
historicalbday <- function(frame){
  frame <- frame[which(frame$discharge_cfs >= threshold),] 
  frame <- frame %>%
    mutate(year = year(Date),
           logic = 1) %>%
    group_by(year) %>% 
    summarize(boatabledays = sum(logic)) 
}

hist_bdays <- historicalbday(WWPark)

wet_hist_bdays <- historicalbday(wet)
wetTypical_hist_bdays <- historicalbday(wetTypical)
dryTypical_hist_bdays <- historicalbday(dryTypical)
dry_hist_bdays <- historicalbday(dry)


# Post-NISP
postnispbday <- function(frame){
  frame <- frame[which(frame$avgdischarge_nisp >= threshold),] 
  frame <- frame %>%
    mutate(year = year(Date),
           logic = 1) %>%
    group_by(year) %>% 
    summarize(postnisp_boatabledays = sum(logic)) 
}

postnisp_bdays <- postnispbday(C_avg_annual)

wet_postnisp_bdays <- postnispbday(C_wet)
wetTypical_postnisp_bdays <- postnispbday(C_wetTypical)
dryTypical_postnisp_bdays <- postnispbday(C_dryTypical)
dry_postnisp_bdays <- postnispbday(C_dry)

# Count loss in days in column 'aloss'
count <- left_join(hist_bdays, postnisp_bdays, by = "year")
count[is.na(count)] <- 0 # Make N/A values = 0
count <- count %>% 
  mutate(aloss = abs(postnisp_boatabledays-boatabledays)) %>% 
  select(year, aloss, postnisp_boatabledays)

# Make df to have it staked bar graph compatible
dfcount <- count %>% 
  gather(impact, boatabledays, 2:3)


# Plots -------------------------------------------------------------------

plotwet <- ggplot(wet_annual, aes(x = day_of_year, y = avg_discharge)) + geom_line(color = "skyblue2") +
  geom_line(data = postnisp_wet, aes(x = day_of_year, y = avg_discharge), color = "slategray") +
  scale_y_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) +
  scale_x_continuous(breaks = c(1,61,122,183,245,306),
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) # Get every other month

plotwettyp <- ggplot(wetTypical_annual, aes(x = day_of_year, y = avg_discharge)) + geom_line(color = "skyblue2") +
  geom_line(data = postnisp_wetTypical, aes(x = day_of_year, y = avg_discharge), color = "slategray") +
  scale_y_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) +
  scale_x_continuous(breaks = c(1,61,122,183,245,306),
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) # Get every other month

plotdrytyp <- ggplot(dryTypical_annual, aes(x = day_of_year, y = avg_discharge)) + geom_line(color = "skyblue2") +
  geom_line(data = postnisp_dryTypical, aes(x = day_of_year, y = avg_discharge), color = "slategray") +
  scale_y_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) +
  scale_x_continuous(breaks = c(1,61,122,183,245,306),
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) # Get every other month

plotdry <- ggplot(dry_annual, aes(x = day_of_year, y = avg_discharge)) + geom_line(color = "skyblue2") +
  geom_line(data = postnisp_dry, aes(x = day_of_year, y = avg_discharge), color = "slategray") +
  scale_y_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) +
  scale_x_continuous(breaks = c(1,61,122,183,245,306),
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) # Get every other month

# Plot averaged annual hydrograph
plotavgannual <- ggplot(avg_annual, aes(x = day_of_year, y = avg_discharge)) + geom_line(color = "skyblue2") +
  geom_line(data = postnisp_avg_annual, aes(x = day_of_year, y = avg_discharge), color = "slategray") +
  scale_y_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) +
  scale_x_continuous(breaks = c(1,61,122,183,245,306),
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) # Get every other month


figure <- ggarrange(plotwet, plotwettyp, plotdry, plotdrytyp,plotavgannual,
                    labels = c("wet", "wet typical", "dry", "dry typical", "averaged annual"),
                    ncol = 2, nrow = 3)
print(figure)



# Plot historical boatable days
plothist_bdays <- ggplot (data = hist_bdays, aes(x = year, y = boatabledays)) + 
  geom_bar(stat = "identity", fill = "skyblue2") +
  scale_y_continuous(breaks = seq(0, 120, 20), lim = c(0, 120)) +
  labs(y = "Boatable Days", x = "Year")

print(plothist_bdays)



# Plot change in boatable days
changeinbdays <- ggplot(data = dfcount, aes(x = year, y = boatabledays, fill = impact)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values=c("tomato2", "slategray"), 
                    name="",
                    breaks=c("aloss", "postnisp_boatabledays"),
                    labels=c("Loss of days", "Post-NISP")) +
  scale_y_continuous(breaks = seq(0, 120, 20), lim = c(0, 120)) +
  theme(legend.position="bottom") +
  labs(y = "Boatable Days", x = "Year")

print(changeinbdays)

