# Setting up some libraries we'll need

options(java.parameters = "- Xmx1024m")

library(tidyverse)
library(stringr)
library(lubridate)
library(xlsx)
library(gridExtra)

# For Cape May
# These sections of code will repeat for each location, so I'm only commenting once

# First we set up the urls of the buoy data tables from the NDBC website

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44009h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1984:2016)

urls <- str_c(str1, years, str2, sep = "")

# Now that we havve urls, let's say what the filenames for each year will be

filenames <- str_c("cm", years, sep = "")

# And how many we have

N <- length(urls)

for (i in 1:N){
  
  # For each year of our data, get the table from that url and call it "file"
  
  assign(filenames[i], read_table(urls[i], col_names = TRUE))
  
  file <- get(filenames[i])
  
  # Before 1998 the year column is only 2 digits, YY, so let's get those to all match
  
  colnames(file)[1] <-"YYYY"
  
  # If the first year entry is 2 digits, put 19 in front of it
  
  if(nchar(file[1,1]) == 2 & file[1,1] > 50 ) {
    file[1] <- lapply(file[1], function(x){str_c(19, x, sep = "")})
  }
  
  # Since we turned it into a string by doing that, let's get it back to a number
  
  file$YYYY <- as.numeric(file$YYYY)
  
  # And then make sure everything else is numbers
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  # Early years didn't have a minutes column
  # For those with minutes, we want to add it in with an entry at zero
  # Otherwise, we only want 0 minutes if possible
  # But some years have entries at 50 minutes instead
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  # Pulling out just the columns we care about, then making the date column
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
  # Putting our temporary 'file' into the main dataframe for this location
  
  if(i == 1){
    CM <- file
  }
  
  else{
    CM <- rbind.data.frame(CM, file)
  }
  
}

# Now we have a few other actions to perform
# We filter out so we have just one entry for each day, the closest to noon

CM <- filter(CM, hh == 12 & mm == 00 | hh == 11 & mm == 50)

# Then make the time difference column

CM$timediff <- make_datetime(hour = CM$hh, min = CM$mm) - make_datetime(hour = 12, 
                                                                        min = 00) 
# Then we'll filter down to the columns we really want, recode the NAs, 
# and rename the columns  

CM <- select(CM, date, timediff, ATMP, WTMP)

CM$ATMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$ATMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
CM$WTMP <- apply(CM[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$WTMP <- apply(CM[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(CM)[3] <- "air_temp"
colnames(CM)[4] <- "sea_temp"
colnames(CM)[1] <- "date_time"
colnames(CM)[2] <- "time_diff"

# There are a few more columns to put in

CM$team_num <- 1
CM$reading_type <- "buoy"
CM$Lat <- 38.5
CM$Lon <- 74.7

# And then reordering the columns

CM <- CM[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
           "sea_temp", "air_temp")]

# Then we do it all again for the next location
# Molasses Reef

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1987:2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("mr", years, sep = "")

N <- length(urls)

for (i in 1:N){
  assign(filenames[i], read_table(urls[i], col_names = TRUE))
  
  file <- get(filenames[i])
  
  colnames(file)[1] <-"YYYY"
  
  if(nchar(file[1,1]) == 2 & file[1,1] > 50 ) {
    file[1] <- lapply(file[1], function(x){str_c(19, x, sep = "")})
  }
  
  file$YYYY <- as.numeric(file$YYYY)
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
  if(i == 1){
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  }
  
}

MR<-filter(MR, hh==12 & mm == 00 | hh == 11 & mm == 50)

MR$timediff <- make_datetime(hour = MR$hh, min = MR$mm) - make_datetime(hour = 12, min = 00) 

MR<-select(MR, date, timediff, ATMP, WTMP)

MR$ATMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$ATMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
MR$WTMP <- apply(MR[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$WTMP <- apply(MR[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(MR)[3] <- "air_temp"
colnames(MR)[4] <- "sea_temp"
colnames(MR)[1] <- "date_time"
colnames(MR)[2] <- "time_diff"

MR$team_num <- 1
MR$reading_type <- "buoy"
MR$Lat <- 25.0
MR$Lon <- 80.4

MR <- MR[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
           "sea_temp", "air_temp")]

# Georges Bank

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44011h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1984:2013, 2015, 2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("gb", years, sep = "")

N <- length(urls)

for (i in 1:N){
  assign(filenames[i], read_table(urls[i], col_names = TRUE))
  
  file <- get(filenames[i])
  
  colnames(file)[1] <-"YYYY"
  
  if(nchar(file[1,1]) == 2 & file[1,1] > 50 ) {
    file[1] <- lapply(file[1], function(x){str_c(19, x, sep = "")})
  }
  
  file$YYYY <- as.numeric(file$YYYY)
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
  if(i == 1){
    GB <- file
  }
  
  else{
    GB <- rbind.data.frame(GB, file)
  }
  
}

GB<-filter(GB, hh==12 & mm == 00 | hh == 11 & mm == 50)

GB$timediff <- make_datetime(hour = GB$hh, min = GB$mm) - make_datetime(hour = 12, min = 00) 

GB <- select(GB, date, timediff, ATMP, WTMP)

GB$ATMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$ATMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
GB$WTMP <- apply(GB[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$WTMP <- apply(GB[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(GB)[3] <- "air_temp"
colnames(GB)[4] <- "sea_temp"
colnames(GB)[1] <- "date_time"
colnames(GB)[2] <- "time_diff"

GB$team_num <- 1
GB$reading_type <- "buoy"
GB$Lat <- 41.1
GB$Lon <- 66.6

GB <- GB[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
           "sea_temp", "air_temp")]

# Mid-Gulf

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=42001h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1984:2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("mg", years, sep = "")

N <- length(urls)

for (i in 1:N){
  assign(filenames[i], read_table(urls[i], col_names = TRUE))
  
  file <- get(filenames[i])
  
  colnames(file)[1] <-"YYYY"
  
  if(nchar(file[1,1]) == 2 & file[1,1] > 50 ) {
    file[1] <- lapply(file[1], function(x){str_c(19, x, sep = "")})
  }
  
  file$YYYY <- as.numeric(file$YYYY)
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
  if(i == 1){
    MG <- file
  }
  
  else{
    MG <- rbind.data.frame(MG, file)
  }
  
}

MG <- filter(MG, hh == 12 & mm == 00 | hh == 11 & mm == 50)

MG$timediff <- make_datetime(hour = MG$hh, min = MG$mm) - make_datetime(hour = 12, min = 00) 

MG <- select(MG, date, timediff, ATMP, WTMP)

MG$ATMP <- apply(MG[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MG$ATMP <- apply(MG[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
MG$WTMP <- apply(MG[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MG$WTMP <- apply(MG[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(MG)[3] <- "air_temp"
colnames(MG)[4] <- "sea_temp"
colnames(MG)[1] <- "date_time"
colnames(MG)[2] <- "time_diff"

MG$team_num <- 1
MG$reading_type <- "buoy"
MG$Lat <- 25.9
MG$Lon <- 89.7

MG <- MG[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
           "sea_temp", "air_temp")]

# Finally, we need to put these dataframes into an xlsx file

write.xlsx2(CM, "group1data.xlsx", sheetName = "Cape May - Buoy # 44009")
write.xlsx2(MR, "group1data.xlsx", sheetName = "Molasses Reef - Buoy # MLRF1", append = TRUE)
write.xlsx2(GB, "group1data.xlsx", sheetName = "Georges Bank - Buoy # 44011", append = TRUE)
write.xlsx2(MG, "group1data.xlsx", sheetName = "Mid Gulf - Buoy # 42001", append = TRUE)

# Seasonal Data Visualization

# First we want to create a new dataframe that has separate month columns for each buoy

CM.seasons <- mutate(CM, month = month(CM$date_time))

MR.seasons <- mutate(MR, month = month(MR$date_time))

GB.seasons <- mutate(GB, month = month(GB$date_time))

MG.seasons <- mutate(MG, month = month(MG$date_time))

# Next we want to create separate data frames for each season
# We approximated each season to 3 months:
# Winter = January, February, and March
# Spring = April, May, and June
# Summer = July, August, and September
# Fall = October, November, and December

CM.winter <- filter(CM.seasons, month == 1 | month == 2 | month == 3)
CM.spring <- filter(CM.seasons, month == 4 | month == 5 | month == 6)
CM.summer <- filter(CM.seasons, month == 7 | month == 8 | month == 9)
CM.fall <- filter(CM.seasons, month == 10 | month == 11 | month == 12)

MR.winter <- filter(MR.seasons, month == 1 | month == 2 | month == 3)
MR.spring <- filter(MR.seasons, month == 4 | month == 5 | month == 6)
MR.summer <- filter(MR.seasons, month == 7 | month == 8 | month == 9)
MR.fall <- filter(MR.seasons, month == 10 | month == 11 | month == 12)

GB.winter <- filter(GB.seasons, month == 1 | month == 2 | month == 3)
GB.spring <- filter(GB.seasons, month == 4 | month == 5 | month == 6)
GB.summer <- filter(GB.seasons, month == 7 | month == 8 | month == 9)
GB.fall <- filter(GB.seasons, month == 10 | month == 11 | month == 12)

MG.winter <- filter(MG.seasons, month == 1 | month == 2 | month == 3)
MG.spring <- filter(MG.seasons, month == 4 | month == 5 | month == 6)
MG.summer <- filter(MG.seasons, month == 7 | month == 8 | month == 9)
MG.fall <- filter(MG.seasons, month == 10 | month == 11 | month == 12)

# Then, I want to assign plots of each season for each buoy to names

winter.CM <- ggplot() + geom_point(data = CM.winter, aes(y = air_temp, x = date_time), 
                                   color = "darkslategray4") +
  geom_smooth(data = CM.winter, aes(y = air_temp, x = date_time), color = "darkslategray", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Winter Months in Cape May")
spring.CM <- ggplot() + geom_point(data = CM.spring, aes(y = air_temp, x = date_time), 
                                   color = "darkorchid1") +
  geom_smooth(data = CM.spring, aes(y = air_temp, x = date_time), color = "darkorchid4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Spring Months in Cape May")
summer.CM <- ggplot() + geom_point(data = CM.summer, aes(y = air_temp, x = date_time), 
                                   color = "goldenrod1") + 
  geom_smooth(data = CM.summer, aes(y = air_temp, x = date_time), color = "goldenrod4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Summer Months in Cape May")
fall.CM <- ggplot() + geom_point(data = CM.fall, aes(y = air_temp, x = date_time), 
                                 color = "darkorange2") +
  geom_smooth(data = CM.fall, aes(y = air_temp, x = date_time), color = "darkorange4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Fall Months in Cape May")

winter.MR <- ggplot() + geom_point(data = MR.winter, aes(y = air_temp, x = date_time), 
                                   color = "darkslategray4") +
  geom_smooth(data = MR.winter, aes(y = air_temp, x = date_time), color = "darkslategray", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Winter Months in Molasses Reef")
spring.MR <- ggplot() + geom_point(data = MR.spring, aes(y = air_temp, x = date_time), 
                                   color = "darkorchid1") +
  geom_smooth(data = MR.spring, aes(y = air_temp, x = date_time), color = "darkorchid4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Spring Months in Molasses Reef")
summer.MR <- ggplot() + geom_point(data = MR.summer, aes(y = air_temp, x = date_time), 
                                   color = "goldenrod1") + 
  geom_smooth(data = MR.summer, aes(y = air_temp, x = date_time), color = "goldenrod4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Summer Months in Molasses Reef")
fall.MR <- ggplot() + geom_point(data = MR.fall, aes(y = air_temp, x = date_time), 
                                 color = "darkorange2") +
  geom_smooth(data = MR.fall, aes(y = air_temp, x = date_time), color = "darkorange4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Fall Months in Molasses Reef")

winter.GB <- ggplot() + geom_point(data = GB.winter, aes(y = air_temp, x = date_time), 
                                   color = "darkslategray4") +
  geom_smooth(data = GB.winter, aes(y = air_temp, x = date_time), color = "darkslategray", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Winter Months in Georges Bank")
spring.GB <- ggplot() + geom_point(data = GB.spring, aes(y = air_temp, x = date_time), 
                                   color = "darkorchid1") +
  geom_smooth(data = GB.spring, aes(y = air_temp, x = date_time), color = "darkorchid4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Spring Months in Georges Bank")
summer.GB <- ggplot() + geom_point(data = GB.summer, aes(y = air_temp, x = date_time), 
                                   color = "goldenrod1") + 
  geom_smooth(data = GB.summer, aes(y = air_temp, x = date_time), color = "goldenrod4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Summer Months in Georges Bank")
fall.GB <- ggplot() + geom_point(data = GB.fall, aes(y = air_temp, x = date_time), 
                                 color = "darkorange2") +
  geom_smooth(data = GB.fall, aes(y = air_temp, x = date_time), color = "darkorange4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Fall Months in Georges Bank")

winter.MG <- ggplot() + geom_point(data = MG.winter, aes(y = air_temp, x = date_time), 
                                   color = "darkslategray4") +
  geom_smooth(data = MG.winter, aes(y = air_temp, x = date_time), color = "darkslategray", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Winter Months in Mid Gulf")
spring.MG <- ggplot() + geom_point(data = MG.spring, aes(y = air_temp, x = date_time), 
                                   color = "darkorchid1") +
  geom_smooth(data = MG.spring, aes(y = air_temp, x = date_time), color = "darkorchid4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Spring Months in Mid Gulf")
summer.MG <- ggplot() + geom_point(data = MG.summer, aes(y = air_temp, x = date_time), 
                                   color = "goldenrod1") + 
  geom_smooth(data = MG.summer, aes(y = air_temp, x = date_time), color = "goldenrod4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Summer Months in Mid Gulf")
fall.MG <- ggplot() + geom_point(data = MG.fall, aes(y = air_temp, x = date_time), 
                                 color = "darkorange2") +
  geom_smooth(data = MG.fall, aes(y = air_temp, x = date_time), color = "darkorange4", 
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Air Temperature (°C)", title = "Year vs. Air Temperature, 
       Fall Months in Mid Gulf")

# Finally, we want to arrange each plot so that they appear side by side for each buoy,
# with appropriate titles and labels

grid.arrange(winter.CM, spring.CM, summer.CM, fall.CM, ncol=2)
grid.arrange(winter.MR, spring.MR, summer.MR, fall.MR, ncol=2)
grid.arrange(winter.GB, spring.GB, summer.GB, fall.GB, ncol=2)
grid.arrange(winter.MG, spring.MG, summer.MG, fall.MG, ncol=2)

# I also calculated the corellation coefficients for each season over time 
# against the air temperature for each buoy

# Making sure dates are numerics
CM.winter$date_time <- as.numeric(CM.winter$date_time)
CM.spring$date_time <- as.numeric(CM.spring$date_time)
CM.summer$date_time <- as.numeric(CM.summer$date_time)
CM.fall$date_time <- as.numeric(CM.fall$date_time)

# Inserting into the correlation function
cor(CM.winter$air_temp, CM.winter$date_time, use = "complete.obs")
cor(CM.spring$air_temp, CM.spring$date_time, use = "complete.obs")
cor(CM.summer$air_temp, CM.summer$date_time, use = "complete.obs")
cor(CM.fall$air_temp, CM.fall$date_time, use = "complete.obs")

MR.winter$date_time <- as.numeric(MR.winter$date_time)
MR.spring$date_time <- as.numeric(MR.spring$date_time)
MR.summer$date_time <- as.numeric(MR.summer$date_time)
MR.fall$date_time <- as.numeric(MR.fall$date_time)

cor(MR.winter$air_temp, MR.winter$date_time, use = "complete.obs")
cor(MR.spring$air_temp, MR.spring$date_time, use = "complete.obs")
cor(MR.summer$air_temp, MR.summer$date_time, use = "complete.obs")
cor(MR.fall$air_temp, MR.fall$date_time, use = "complete.obs")

GB.winter$date_time <- as.numeric(GB.winter$date_time)
GB.spring$date_time <- as.numeric(GB.spring$date_time)
GB.summer$date_time <- as.numeric(GB.summer$date_time)
GB.fall$date_time <- as.numeric(GB.fall$date_time)

cor(GB.winter$air_temp, GB.winter$date_time, use = "complete.obs")
cor(GB.spring$air_temp, GB.spring$date_time, use = "complete.obs")
cor(GB.summer$air_temp, GB.summer$date_time, use = "complete.obs")
cor(GB.fall$air_temp, GB.fall$date_time, use = "complete.obs")

MG.winter$date_time <- as.numeric(MG.winter$date_time)
MG.spring$date_time <- as.numeric(MG.spring$date_time)
MG.summer$date_time <- as.numeric(MG.summer$date_time)
MG.fall$date_time <- as.numeric(MG.fall$date_time)

cor(MG.winter$air_temp, MG.winter$date_time, use = "complete.obs")
cor(MG.spring$air_temp, MG.spring$date_time, use = "complete.obs")
cor(MG.summer$air_temp, MG.summer$date_time, use = "complete.obs")
cor(MG.fall$air_temp, MG.fall$date_time, use = "complete.obs")

# Data Visuals for Air Temp and Sea Temp Correlation

# CM Air and Sea

CM.AirSea <- ggplot(data = CM) + 
  geom_point(mapping = aes(x = sea_temp, y = air_temp), color="lightblue") + 
  labs(title="Cape May Air Temperature vs. Sea Temperature", 
       x="Sea Temperature (°C)", y="Air Temperature (°C)")

# MR Air and Sea

MR.AirSea <- ggplot(data = MR) + 
  geom_point(mapping = aes(x = sea_temp, y = air_temp), color="darkolivegreen1") + 
  labs(title="Molasses Reef Air Temperature vs. Sea Temperature", 
       x="Sea Temperature (°C)", y="Air Temperature (°C)")

# MG Air and Sea

MG.AirSea <- ggplot(data = MG) + 
  geom_point(mapping = aes(x = sea_temp, y = air_temp), color="lightpink1") + 
  labs(title="Mid Gulf Air Temperature vs. Sea Temperature", 
       x="Sea Temperature (°C)", y="Air Temperature (°C)")

# GB Air and Sea

GB.AirSea <- ggplot(data = GB) + 
  geom_point(mapping = aes(x = sea_temp, y = air_temp), color="khaki") + 
  labs(title="Georges Bank Air Temperature vs. Sea Temperature", 
       x="Sea Temperature (°C)", y="Air Temperature (°C)")

# Make the Plots Readable and Comparable with gridExtra

grid.arrange(CM.AirSea, MR.AirSea, MG.AirSea, GB.AirSea, ncol=2)

#Descriptive Statistics 
#Each descriptive statistics block contains a summary statement to retrieve the 
#minumum, maximum, mean, and median. The sd() argument produces the standard deviation
#and the cor() finds the correlation between the air_temp and sea_temp for the given block

#descriptive statistics for Cape May (all years)
summary(CM$air_temp)
sd(CM$air_temp, na.rm=TRUE)
summary(CM$sea_temp)
sd(CM$sea_temp, na.rm=TRUE)
cor(CM$air_temp, CM$sea_temp, use = "complete.obs") 

#descriptive statistics for Cape May (1984)

CM1984 <- CM %>% filter(date_time >= as.Date("1984-01-01") & 
                          date_time <= as.Date("1984-12-31"))
#this filter statement takes data only from the first complete year of available data,
#and creates a new corresponding variable so that the first year can be compared to the most
#recent year of available data

summary(CM1984$air_temp)
sd(CM1984$air_temp, na.rm=TRUE)
summary(CM1984$sea_temp)
sd(CM1984$sea_temp, na.rm=TRUE)
cor(CM1984$air_temp, CM1984$sea_temp, use = "complete.obs") 

#descriptive statistics for Cape May (2015)
CM2015 <- CM %>% filter(date_time >= as.Date("2015-01-01") & 
                          date_time <= as.Date("2015-12-31"))
#this filter statement takes data only from the most recent complete year of data
#and creates a new corresponding variable so that the most recent year can be compared to the
#first year of available data
summary(CM2015$air_temp)
sd(CM2015$air_temp, na.rm=TRUE)
summary(CM2015$sea_temp)
sd(CM2015$sea_temp, na.rm=TRUE)
cor(CM2015$air_temp, CM2015$sea_temp, use = "complete.obs") 

#Cape May T-Test
#Using the previously created variables, T-Tests are run for difference in means in both
#air temperature and sea temperature. This translates to seeing if there is a significant 
#difference in temperature between the first year and the most recent year

t.test(CM2015$air_temp, CM1984$air_temp)
t.test(CM2015$sea_temp, CM1984$sea_temp)


#The code described above is repeated for all of the buoys.
#descriptive statistics for Molasses Reef (all years)
summary(MR$air_temp)
sd(MR$air_temp, na.rm=TRUE)
summary(MR$sea_temp)
sd(MR$sea_temp, na.rm=TRUE)
cor(MR$air_temp, MR$sea_temp, use = "complete.obs")

#descriptive statistics for Molasses Reef (1987)
MR1988 <- MR %>% filter(date_time >= as.Date("1988-01-01") & 
                          date_time <= as.Date("1988-12-31"))
summary(MR1988$air_temp)
sd(MR1988$air_temp, na.rm=TRUE)
summary(MR1988$sea_temp)
sd(MR1988$sea_temp, na.rm=TRUE)
cor(MR1988$air_temp, MR1988$sea_temp, use = "complete.obs") 


#descriptive statistics for Molasses Reef (2015)
MR2015 <- MR %>% filter(date_time >= as.Date("2015-01-01") & 
                          date_time <= as.Date("2015-12-31"))
summary(MR2015$air_temp)
sd(MR2015$air_temp, na.rm=TRUE)
summary(MR2015$sea_temp)
sd(MR2015$sea_temp, na.rm=TRUE)
cor(MR2015$air_temp, MR2015$sea_temp, use = "complete.obs") 

#Molasses Reef T-Test
t.test(MR2015$air_temp, MR1988$air_temp)
t.test(MR2015$sea_temp, MR1988$sea_temp)


#descriptive statistics for Georges Bank (all years)
summary(GB$air_temp)
sd(GB$air_temp, na.rm=TRUE)
summary(GB$sea_temp)
sd(GB$sea_temp, na.rm=TRUE)
cor(GB$air_temp, GB$sea_temp, use = "complete.obs") 


#descriptive statistics for Georges Bank (1985)
GB1985 <- GB %>% filter(date_time >= as.Date("1985-01-01") & 
                          date_time <= as.Date("1985-12-31"))
summary(GB1985$air_temp)
sd(GB1985$air_temp, na.rm=TRUE)
summary(GB1985$sea_temp)
sd(GB1985$sea_temp, na.rm=TRUE)
cor(GB1985$air_temp, GB1985$sea_temp, use = "complete.obs") 

#descriptive statistics for Georges Bank (2015)
GB2015 <- GB %>% filter(date_time >= as.Date("2015-01-01") & 
                          date_time <= as.Date("2015-12-31"))
summary(GB2015$air_temp)
sd(GB2015$air_temp, na.rm=TRUE)
summary(GB2015$sea_temp)
sd(GB2015$sea_temp, na.rm=TRUE)
cor(GB2015$air_temp, GB2015$sea_temp, use = "complete.obs")

#Georges Bank T-Test
t.test(GB2015$air_temp, GB1985$air_temp)
t.test(GB2015$sea_temp, GB1985$sea_temp)


#descriptive statistics for Mid Gulf (all years)
summary(MG$air_temp)
sd(MG$air_temp, na.rm=TRUE)
summary(MG$sea_temp)
sd(MG$sea_temp, na.rm=TRUE)
cor(MG$air_temp, MG$sea_temp, use = "complete.obs") 

#descriptive statistics for Mid Gulf (1984)
MG1984 <- MG %>% filter(date_time >= as.Date("1984-01-01") & 
                          date_time <= as.Date("1984-12-31"))
summary(MG1984$air_temp)
sd(MG1984$air_temp, na.rm=TRUE)
summary(MG1984$sea_temp)
sd(MG1984$sea_temp, na.rm=TRUE)
cor(MG1984$air_temp, MG1984$sea_temp, use = "complete.obs") 

#descriptive statistics for Mid Gulf (2015)
MG2015 <- MG %>% filter(date_time >= as.Date("2015-01-01") & 
                          date_time <= as.Date("2015-12-31"))
summary(MG2015$air_temp)
sd(MG2015$air_temp, na.rm=TRUE)
summary(MG2015$sea_temp)
sd(MG2015$sea_temp, na.rm=TRUE)
cor(MG2015$air_temp, MG2015$sea_temp, use = "complete.obs") 

#Mid Gulf T-Test
t.test(MG2015$air_temp, MG1984$air_temp)
t.test(MG2015$sea_temp, MG1984$sea_temp)