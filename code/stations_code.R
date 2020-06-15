##

library(data.table)
library(ggplot2)
library(dplyr)
library(leaflet)

runoff_eu_info <-  na.omit(readRDS('./data/runoff_eu_info.rds'))

###############################################
#q1
num_of_coun <- unique(runoff_eu_info$Country) ##number of countries(19) (q1)

###############################################
#q2
sta_per_coun <- data.frame(table(runoff_eu_info$Country)) ##number of stations per country

ggplot(sta_per_coun, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(label = sta_per_coun$Freq, cex=4, position = position_nudge(y = 2))+xlab('Countries')+ylab('Number of stations') ##plot of stations per country in graph

riv_per_coun <- as.data.table(unique(runoff_eu_info[,3:4])) ##all rivers and countries

riv_per_coun_N <- riv_per_coun[,count := .N,by="Country"] ##number of rivers per country

ggplot(riv_per_coun_N, aes(x = Country)) +
  geom_bar(stat = "count") +
  geom_text(aes(x = Country, y = count, label=count), cex=4, position = position_nudge(y = 2))+xlab('Countries')+ylab('Number of rivers') ##plot of rivers per country

##############################################
##q3 

sta_per_riv <- as.data.table(unique(runoff_eu_info[,2:3]))

unique(sta_per_riv$Station)
unique(sta_per_riv$River) ##there is 153 rivers and 207 stations on it

sta_per_riv_N <- sta_per_riv[,count := .N,by="River"]

mult_sta_on_riv <- unique(sta_per_riv_N[,c(2,3)])

ggplot(mult_sta_on_riv[count>1], aes(x = River, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = River, y = count, label=count), cex=4, position = position_nudge(y = 0.4)) +
  coord_flip()+xlab('Rivers with more than one station')+ylab('Number of stations') ###plot only following rivers have more than one station

#######################################################
##q4

ggplot(runoff_eu_info, aes(x = Lat, y = Lon, color = Alt)) +
  geom_point(size=1.5) +
  xlab('Latitude of stations') +
  ylab('Longitude of stations') +
  labs(color = "Altitude\nof stations") ###plot of distribution of stations

#######################################################
##q5

ggplot(runoff_eu_info, aes(x = Station, y = N.Years))+
  geom_point(size = 1.5) +
  theme(axis.text.x = element_blank()) +
  xlab('Stations') +
  ylab("Years of records") ##plot ofdistribution of record length

