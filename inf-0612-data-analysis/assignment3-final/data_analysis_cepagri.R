# Alunos
# - Kaleb Roncatti de Souza
# - Nelson Gomes Brasil Junior



# Installing some packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")

# Importing some packages
library(ggplot2)
library(dplyr)
library(reshape2)

# Creating connection with URL and reading data as CSV
conn <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
names <- c("dt", "temperature", "wind_speed", "humidity", "thermal_sensation")
cepagri <- read.csv(conn, 
                    header = FALSE,
                    fill = TRUE,
                    sep = ";",
                    col.names = names)

# Copy from original object for further analysis
cepagri_original <- cepagri 

# Showing the headers along with some entries and the dimensions
head(cepagri)
dim(cepagri)
original_len <- dim(cepagri)[1]

# Verifying some information about our data (only for numeric entries for now)
print("Summary thermal sensation")
summary(cepagri$thermal_sensation)
print("Summary humidity")
summary(cepagri$humidity)
print("Summary wind speed")
summary(cepagri$wind_speed)

# Exploring dimensions on nans by different columns
# Using same variable so we don't waste memory in case the dimensions are the same
cepagri_nans <- cepagri[is.na(cepagri$thermal_sensation), ]; dim(cepagri_nans)
cepagri_nans <- cepagri[is.na(cepagri$humidity), ]; dim(cepagri_nans)
cepagri_nans <- cepagri[is.na(cepagri$wind_speed), ]; dim(cepagri_nans)
# Dimensions are the same, it seems that we have mutual NAN's on the same columns

# It seems that the nans are on these three columns, chosing a random one to analyse the values
cepagri_nans <- cepagri[is.na(cepagri$thermal_sensation), ]
head(cepagri_nans, 10)
# Basically, we have errors for those periods on temperature, and we are not capturing any data
# Checking for unique values to see if we have something else than that
unique(cepagri_nans$temperature)

# It seems that we just have that "aberrant" case, removing all NANs and checking summary again
cepagri <- cepagri[!is.na(cepagri$wind_speed), ]; 
new_len <- dim(cepagri)[1]

# Checking if the lengths are making sense
if (new_len + dim(cepagri_nans)[1] == original_len){print("Removed all NANs sucessfully!!")}

# Checking summaries again
print("Summary thermal sensation")
summary(cepagri$thermal_sensation)
print("Summary humidity")
summary(cepagri$humidity)
print("Summary wind speed")
summary(cepagri$wind_speed)
# No more NANs!!! yey!!! Let's take a look on the data we've removed
# since >35k of entries is a very significant amount of data

# Since the only valid information on those entries we've removed is the datetime
# Let's investigate the data we've left behind. This is important to have in mind
# since it may impact our analysis

cepagri_nans$dt <- as.POSIXct(cepagri_nans$dt,
                              format = "%d/%m/%Y-%H:%M",
                              tz = "America/Sao_Paulo"); head(cepagri_nans)

# Adding year and month column to check for occurrences
cepagri_nans$dt_year <- format(cepagri_nans$dt, format="%Y")
cepagri_nans$dt_month <- format(cepagri_nans$dt, format="%m")
cepagri_nans$dt_day <- format(cepagri_nans$dt, format="%d")
cepagri_nans$dt_hour <- format(cepagri_nans$dt, format="%H")

# Checking for yearly occurrences of the aberrant case
occurrence_nans_yearly <- as.data.frame(table(cepagri_nans$dt_year)); occurrence_nans_yearly

## Checking how much data in percentage we are "losing" on each by ignoring those entries
# just for awareness. There is nothing much we can do since we have no data at all for those entries for now
# but it may engender some problems on the analysis

cepagri_original$dt <- as.POSIXct(cepagri_original$dt,
                                  format = "%d/%m/%Y-%H:%M",
                                  tz = "America/Sao_Paulo")
cepagri_original$dt_year <- format(cepagri_original$dt, format="%Y")

# Finding percentage using the original df and the nans removed df
occurrence_data_yearly <- as.data.frame(table(cepagri_original$dt_year)); 
occurrence_data_yearly$Freq_lost_data <- occurrence_nans_yearly$Freq; 
occurrence_data_yearly$percentage_lost_data <- (occurrence_data_yearly$Freq_lost_data/occurrence_data_yearly$Freq) * 100
occurrence_data_yearly
# We've lost significant data for 2020/2021 with errors (> 5%)

## Getting back to our analysis with our cleaned data
# Converting temperature to numeric and dt to datetime type
cepagri$dt <- as.POSIXct(cepagri$dt,
                         format = "%d/%m/%Y-%H:%M",
                         tz = "America/Sao_Paulo")

cepagri$temperature <- as.numeric(cepagri$temperature)


# Using only dates between 2015 and 2021
cepagri$dt_year <- format(cepagri$dt, format="%Y")
cepagri$dt_month <- format(cepagri$dt, format="%b")
cepagri$dt_hour <- format(cepagri$dt, format="%H")
cepagri$dt_day <- format(cepagri$dt, format="%d")


cepagri <- cepagri[(cepagri$dt_year >= 2015) & (cepagri$dt_year <= 2021),]

# Checking summaries/data again
print("Summary temperature")
summary(cepagri$temperature)
print("Summary thermal sensation")
summary(cepagri$thermal_sensation)
print("Summary humidity")
summary(cepagri$humidity)
print("Summary wind speed")
summary(cepagri$wind_speed)
dim(cepagri)
## Temperature
# Data seems to make sense when we take a look a real scenario
## Thermal Sensation
# We need to perform further investigation on this data, 99.9°C seems incridibly high for Campinas
## Humidity
# We also need to investigate cases where the humidity is 0.0, not normal as well (almost impossible on Earth).

p <- ggplot(cepagri, aes(x=thermal_sensation)) +
  geom_histogram() + 
  theme_grey(base_size = 18) +  
  xlab("Thermal sensation (°C)") +
  ggtitle("Histogram of thermal sensation") +
  theme(plot.title = element_text(hjust = 0.5)); p

p <- ggplot(cepagri, aes(x=humidity)) +
  geom_histogram() +
  theme_grey(base_size = 18) +  
  xlab("Humidity (%)") +
  ggtitle("Histogram of humidity") +
  theme(plot.title = element_text(hjust = 0.5)); p

# removing outliers that seem unlikable to happen

cepagri <- cepagri[(cepagri$humidity > 0) & (cepagri$thermal_sensation < 99), ]

# Visualizing how much data we have remaining per year after all the cleanup
ggplot(cepagri, aes(x=dt_year)) +
  geom_histogram(color='black', aes(fill='Retained'), stat="count",  alpha=0.5) +
  geom_histogram(data = cepagri_original, color='black', fill='black', stat="count",  alpha=0.2) +
  theme_grey(base_size = 18) +  
  xlab("Year")+
  ggtitle("Number of retained records per year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title=element_blank())


# https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
# Here we map each day of year to the season it belongs. 
# This map will help us on the following analysis

# https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
# Here we map each day of year to the season it belongs. 
# This map will help us on the following analysis

getSeason <- function(dates) {
  SS <- "12-21"  # Summer Solstice
  FE <- "03-21"  # Fall Equinox
  WS <- "06-21"  # Winter Solstice
  SE <- "09-23"  # Spring Equinox
  
  # Convert dates from any year to a fixed year
  d <- format(dates, format='%m-%d')
  
  ifelse (d >= SS | d < FE, "Summer",
          ifelse (d >= FE & d < WS, "Fall",
                  ifelse (d >= WS & d < SE, "Winter", "Spring")))
}
  
cepagri$season <- getSeason(cepagri$dt)
head(cepagri)

## Ordering the seasons
cepagri$season <- factor(cepagri$season, levels=c("Summer", "Fall", "Winter", "Spring"))

# Boxplot of Temperatures by season
ggplot(cepagri, aes(x=as.factor(season), y=temperature)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  theme_grey(base_size = 18) +  
  ggtitle("Boxplot of temperature vs season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("season") +
  ylab("temperature (°C)") 


## Ordering the seasons
cepagri$season <- factor(cepagri$season, levels=c("Summer", "Fall", "Winter", "Spring"))

ggplot(cepagri, aes(x=as.factor(season), y=humidity)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  theme_grey(base_size = 18) +  
  ggtitle("Boxplot of humidity vs season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("season") +
  ylab("humidity (%)")

cepagri %>% 
  group_by(season, dt_hour) %>% 
  summarise(avg_temp = mean(temperature)) -> temperature_by_hour

temperature_by_hour$season <- factor(temperature_by_hour$season, levels=c("Summer", "Fall", "Winter", "Spring"))

p1 <- ggplot(temperature_by_hour, aes(x=dt_hour, y=avg_temp, color = season)) + 
  geom_point(size=4, shape=19) +
  theme_grey(base_size = 14) +  
  ggtitle("Average hourly temperature on each season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("hour of the day") +
  ylab("average temperature (°C)"); p1

## a região de Campinas parece ter duas estações bem definidas quanto a temperatura
## dado que a temperatura média no decorrer do dia no outono e inverno é praticamente igual, assim como em primavera e verão
## A diferença é maior entre inverno e outono apenas nas madrugadas 

cepagri %>% 
  group_by(season, dt_hour) %>% 
  summarise(avg_wind = mean(wind_speed)) -> wind_speed_by_hour

wind_speed_by_hour$season <- factor(wind_speed_by_hour$season, levels=c("Summer", "Fall", "Winter", "Spring"))

p2 <- ggplot(wind_speed_by_hour, aes(x=dt_hour, y=avg_wind, color = season)) + 
  geom_point(size=4, shape=17) +
  theme_grey(base_size = 14) +  
  ggtitle("Average hourly wind speed on each season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("hour of the day") +
  ylab("average wind speed (km/h)"); p2

# Joining the data together to plot stuff
# This could be improved, I don't like it the way it is
joined_by_hour <- cbind(wind_speed_by_hour, avg_temp = temperature_by_hour$avg_temp); head(joined_by_hour)


cepagri_numerical = cepagri[, c('temperature', 'wind_speed', 'humidity', 'thermal_sensation')]

# creating correlation matrix
corr_mat <- round(cor(cepagri_numerical),2)
corr_mat

ggplot(cepagri[cepagri$dt_year == 2018, ], aes(x=temperature, y= thermal_sensation)) + 
  geom_point(size=4, shape=19) + 
  theme_grey(base_size = 18) +  
  ggtitle("Thermal sensation vs temperature") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Temperature (ºC)") +
  ylab("Thermal sensation (ºC)")


cepagri2018 <- cepagri[cepagri$dt_year == 2018, ]

lm2018 <- lm(temperature~thermal_sensation, data = cepagri2018) #Create the linear regression
summary(lm2018) #Review the results

cepagri %>% group_by(season, dt_hour) %>% summarise_at(.vars = c('temperature', 'humidity'), .funs = c(mean="mean")) -> temperature_humidity_relation

ggplot(temperature_humidity_relation, aes(x=temperature_mean, y= humidity_mean, color = season)) + 
  geom_point(size=4, shape=19) + 
  theme_grey(base_size = 18) +  
  ggtitle("Humidity vs temperature") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Mean Temperature (ºC)") +
  ylab("Mean humidity (%)")

## A partir da matriz de correlação acima e analisando os dados por estação (daria certo por mês tbm) podemos perceber que 
## parece haver uma relação entre a temperatura média e a umidade média
## Para uma mesma temperatura, o inverno apresenta valores mais baixos de umidade, o que faz sentido

lmSummer <- lm(temperature_mean~humidity_mean, data = temperature_humidity_relation[temperature_humidity_relation$season == 'Summer',]) #Create the linear regression
summary(lmSummer) 
## temp_summer \approx 46.36 - 0.294 humidity_summer

lmWinter <- lm(temperature_mean~humidity_mean, data = temperature_humidity_relation[temperature_humidity_relation$season == 'Winter',]) #Create the linear regression
summary(lmWinter) 

lmFall <- lm(temperature_mean~humidity_mean, data = temperature_humidity_relation[temperature_humidity_relation$season == 'Fall',]) #Create the linear regression
summary(lmFall) 

lmSpring <- lm(temperature_mean~humidity_mean, data = temperature_humidity_relation[temperature_humidity_relation$season == 'Spring',]) #Create the linear regression
summary(lmSpring)


# Histogram of temperature on Summer
summer <- cepagri[cepagri$season == 'Summer', ]

summer %>% 
  group_by(dt, dt_hour)  %>%
  summarise(avg_temp = mean(temperature)) -> summer_temperature 

ggplot(summer_temperature, aes(x=avg_temp)) +
  geom_histogram(bins = 15, color='black', fill='red') +
  geom_vline(data = summer_temperature, aes(xintercept = mean(avg_temp)), color = 'blue', size=1)+
  theme_grey(base_size = 18) +  
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Temperature")+
  ggtitle("Temperature on Summer")

# Histogram of temperature on Winter
winter <- cepagri[cepagri$season == 'Winter', ]

winter %>% 
  group_by(dt, dt_hour)  %>%
  summarise(avg_temp = mean(temperature)) -> winter_temperature 

ggplot(winter_temperature, aes(x=avg_temp)) +
  geom_histogram(bins = 15, color='black', fill='blue') +
  geom_vline(data = winter_temperature, aes(xintercept = mean(avg_temp)), color = 'red', size=1)+
  theme_grey(base_size = 18) +  
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Temperature")+
  ggtitle("Temperature on Winter")

# Histogram of temperature on Fall
Fall <- cepagri[cepagri$season == 'Fall', ]

Fall %>% 
  group_by(dt, dt_hour)  %>%
  summarise(avg_temp = mean(temperature)) -> Fall_temperature 

ggplot(Fall_temperature, aes(x=avg_temp)) +
  geom_histogram(bins = 15, color='black', fill='purple4') +
  geom_vline(data = Fall_temperature, aes(xintercept = mean(avg_temp)), color = 'orange', size=1)+
  theme_grey(base_size = 18) +  
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Temperature")+
  ggtitle("Temperature on Fall")


# Histogram of temperature on Spring
Spring <- cepagri[cepagri$season == 'Spring', ]

Spring %>% 
  group_by(dt, dt_hour)  %>%
  summarise(avg_temp = mean(temperature)) -> Spring_temperature 

ggplot(Spring_temperature, aes(x=avg_temp)) +
  geom_histogram(bins = 15, color='black', fill='orange') +
  geom_vline(data = Spring_temperature, aes(xintercept = median(avg_temp)), color = 'purple4', size=1)+
  theme_grey(base_size = 18) +  
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Temperature")+
  ggtitle("Temperature on Spring")


# Overlaying histograms by season
cepagri %>% 
  group_by(dt, dt_hour, season)  %>%
  summarise(avg_temp = mean(temperature)) -> cepagri_grouped 

ggplot(cepagri_grouped, aes(x = avg_temp, fill = season)) + 
  geom_histogram(position = "identity", alpha = 0.5) +
  theme_grey(base_size = 18) +  
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Average temperature (ºC)")+
  ggtitle("Avg temperature histogram at each season")


# Taking a look at the temperature variation along the years
ggplot(cepagri, aes(x=as.factor(dt_year), y=temperature)) + 
  geom_boxplot(fill="slateblue", alpha=0.3) + 
  theme_grey(base_size = 18) +  
  ggtitle("Boxplot of temperature vs year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("year") +
  ylab("temperature (°C)") 


# Taking a look at the thermal sensation variation along the years as boxplots
ggplot(cepagri, aes(x=as.factor(dt_year), y=thermal_sensation)) + 
  geom_boxplot(fill="slateblue", alpha=0.3) + 
  theme_grey(base_size = 18) +  
  ggtitle("Boxplot of thermal sensation vs year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("year") +
  ylab("thermal sensation (°C)") 


# Checking for the years of 2020 and 2021, in which months we've lost the majority of the data
as.data.frame(table(cepagri_nans[cepagri_nans$dt_year == 2020, ]$dt_month))
as.data.frame(table(cepagri_nans[cepagri_nans$dt_year == 2021, ]$dt_month))

# Record higher/lower temperatures by year
record_max <- cepagri%>%group_by(dt_year)%>%summarize(Max = max(temperature), Min=min(temperature))%>%.$Max
record_min <- cepagri%>%group_by(dt_year)%>%summarize(Max = max(temperature), Min=min(temperature))%>%.$Min 
year <- seq(from = 2015, to = 2021); 
record_temp <- data.frame(year, record_max, record_min); record_temp

ggplot(record_temp, aes(x=year, legend)) + 
  geom_point(aes(y=record_max, color='a'), size=4, shape=19) +
  geom_line(aes(y=record_max)) +
  geom_point(aes(y=record_min, color='b'), size=4, shape=19) +
  geom_line(aes(y=record_min)) +
  theme_grey(base_size = 14) +  
  ggtitle("Record temperatures by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Type', 
                     values =c('b'='blue','a'='red'), 
                     labels = c('Min','Max')) +
  xlab("year") +
  ylab("Temperature (°C)") +
  scale_x_continuous(breaks = year) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40))

# Record higher/lower temperatures by year
record_max <- cepagri%>%group_by(dt_year)%>%summarize(Max = max(thermal_sensation), Min=min(thermal_sensation))%>%.$Max
record_min <- cepagri%>%group_by(dt_year)%>%summarize(Max = max(thermal_sensation), Min=min(thermal_sensation))%>%.$Min 
year <- seq(from = 2015, to = 2021); 
record_thermal <- data.frame(year, record_max, record_min); record_thermal

ggplot(record_thermal, aes(x=year, legend)) + 
  geom_point(aes(y=record_max, color='a'), size=4, shape=19) +
  geom_line(aes(y=record_max)) +
  geom_point(aes(y=record_min, color='b'), size=4, shape=19) +
  geom_line(aes(y=record_min)) +
  theme_grey(base_size = 14) +  
  ggtitle("Record thermal sensation by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Type', 
                     values =c('b'='blue','a'='red'), 
                     labels = c('Min','Max')) +
  xlab("year") +
  ylab("Thermal (°C)") +
  scale_x_continuous(breaks = year) +
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40))