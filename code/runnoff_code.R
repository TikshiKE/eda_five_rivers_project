library(data.table)
library(ggplot2)
library(dplyr)
library(leaflet)

runoff_eu_info <-  na.omit(readRDS('./data/runoff_eu_info.rds'))
runoff_eu_day <- na.omit(readRDS('./data/runoff_eu_day.rds'))
runoff_eu_year <- na.omit(readRDS('./data/runoff_eu_year.rds'))

##q1
desc_stat <- as.data.table(c('LQ','MG','HQ'))
setnames(desc_stat, c('V1'),c('Data\ngroups'))

desc_stat <- cbind(desc_stat,as.data.table(round(colMeans(runoff_eu_year[,c(3,4,5)])))) ##mean per every group
setnames(desc_stat, c('V1'),c('Mean'))

desc_stat <- cbind(desc_stat,as.data.table(round(sapply(runoff_eu_year[,c(3,4,5)], sd)))) ##standard deviation per group
setnames(desc_stat, c('V1'),c('Standard\ndeviation'))

desc_stat <- cbind(desc_stat,as.data.table(round(desc_stat$`Standard
                                                 deviation`/desc_stat$Mean, digits = 2))) ##coefficient of variation per group
setnames(desc_stat, c('V1'),c('Coefficient\nof variation'))

desc_stat <- cbind(desc_stat,as.data.table(round(sapply(runoff_eu_year[,c(3,4,5)], max))))##max for every group
setnames(desc_stat, c('V1'),c('Maximum'))

desc_stat <- cbind(desc_stat,as.data.table(round(sapply(runoff_eu_year[,c(3,4,5)], min))))##min for every group
setnames(desc_stat, c('V1'),c('Minimum'))

saveRDS(desc_stat, './source/desc_stat.rds')
desc_stat##table for q1
#############################################
##q2

to_plot <- runoff_eu_year[,.(mean_MQ = mean(MQ)), by = Year]
to_plot_2 <- runoff_eu_year[,.(mean_LQ = mean(LQ)), by = Year]
to_plot <- to_plot[to_plot_2, on =.(Year = Year)]

ggplot(to_plot, aes(x = Year, y = mean_LQ)) + 
  geom_point(aes(x = Year, y = mean_MQ, color = 'Medium')) + 
  geom_point(aes(x = Year, y = mean_LQ, color = 'Low')) + 
  labs(color = "Runoff") +
  xlab('Years')+
  ylab('Mean runoff per year') ## plot of ratios of mean/low

to_plot <- runoff_eu_year[,.(mean_MQ = mean(MQ)), by = Year]
to_plot_2 <- runoff_eu_year[,.(mean_HQ = mean(HQ)), by = Year]
to_plot <- to_plot[to_plot_2, on =.(Year = Year)]

ggplot(to_plot, aes(x = Year, y = mean_HQ)) + 
  geom_point(aes(x = Year, y = mean_MQ, color = 'Medium')) + 
  geom_point(aes(x = Year, y = mean_HQ, color = 'High')) + 
  labs(color = "Runoff") +
  xlab('Years')+
  ylab('Mean runoff per year') ##plot of ratios of mean/high


##########################################
##q3
runoff_eu_info[, alt_class := factor("Low")]
runoff_eu_info[Alt >= 100 & Alt < 300, alt_class := factor("medium")]
runoff_eu_info[Alt >= 300, alt_class := factor("High")] ##adding some altitude classes

runoff_eu_info[, time_class := factor("<80 years")]
runoff_eu_info[N.Years >= 100 & N.Years < 150, time_class := factor(">100&<150 years")]
runoff_eu_info[N.Years >= 150, time_class := factor(">150 years")] ##adding some record lenght classes


####################################
##q4

runoff_stats <- runoff_eu_day[, .(mean = round(mean(value), 0), sd = round(sd(value), 0), min = min(value), max = max(value)), by = id]
runoff_stats$cv <- runoff_stats[, sd] / runoff_stats[, mean]
runoff_snames <- runoff_eu_info[,.(id = ID, Station)]
runoff_stats$sname <- 'name'
for (i in 1:length(runoff_snames$id)) {
  for (j in 1:length(runoff_stats$id)){
    if (runoff_snames[i, 1] == runoff_stats[j, 1]){
      runoff_stats[j, 7] <- runoff_snames[i,2]
    }
  }
} 
runoff_stats_est <- runoff_stats[, .(id = id,MH = (mean/max), ML = (mean/min)), by = sname]
to_merge <- runoff_eu_info[,c(1,8,9)]
setnames(to_merge,c('ID'),c('id'))
to_merge <- as.data.table(to_merge)
to_merge[,1] <- sapply(to_merge[,1], as.character)
runoff_stats <- merge(runoff_stats, to_merge, by ='id')

runoff_stats[, alt_class := factor("Low")]
runoff_stats[Alt >= 100 & Alt < 300, alt_class := factor("medium")]
runoff_stats[Alt >= 300, alt_class := factor("High")] ##also adding classes
runoff_stats[, time_class := factor("<80 years")]
runoff_stats[N.Years >= 100 & N.Years < 150, time_class := factor(">100&<150 years")]
runoff_stats[N.Years >= 150, time_class := factor(">150 years")]
##adding before and after 1980 classes
runoff_eu_day$year <- substring(runoff_eu_day$date,1,4)
runoff_eu_day[year < 1980, year_type := factor('before 1980')]
runoff_eu_day[year >= 1980, year_type := factor('after 1980')]

runoff_before <- runoff_eu_day[year < 1980, .(Bmean = mean(value), Bmax = max(value), Bmin = min(value)), by = id]
runoff_after <- runoff_eu_day[year >= 1980, .(Amean = mean(value), Amax = max(value), Amin = min(value)), by = id]
runoff_before_est <- runoff_before[,.(MH_Before = Bmean / Bmax, ML_Before = Bmean / Bmin), by = id]
runoff_after_est <- runoff_after[,.(MH_After = Amean / Amax, ML_After = Amean / Amin), by = id]
runoff_ba_est <- merge(runoff_before_est, runoff_after_est, by = 'id')
runoff_estimated <- merge(runoff_stats_est, runoff_ba_est, by = 'id')

saveRDS(runoff_estimated, './source/runoff_estimated.rds')
runoff_estimated##finally getting some data for our problem

ggplot(runoff_estimated, aes(x = sname, y = (MH_Before - MH_After)))+
  geom_point() + 
  xlab('Stations') +
  ylab('Medium/High estimation before/after')+
  theme(axis.text.x = element_blank())

ggplot(runoff_estimated, aes(x = sname, y = (ML_Before - ML_After)))+
  geom_point()+
  xlab('Stations') +
  ylab('Medium/Low estimation before/after')+
  theme(axis.text.x = element_blank())

runoff_stats_new <- merge(runoff_stats, runoff_eu_day[,.(year_type, id)], by = 'id')
runoff_stats_newnew <- unique(runoff_stats_new)
runoff_stats_newnew <- merge(runoff_ba_est, runoff_stats_newnew, by = 'id')
saveRDS(runoff_stats_newnew, './source/runoff_stats_newnew.rds')

#Changes in mean/high by alt_class
ggplot(runoff_stats_newnew, aes(x = alt_class, y = (MH_Before - MH_After)))+
  geom_point() +
  xlab('Altitude classes') +
  ylab('Medium/High estimation before/after')
#Changes in mean/low by alt_class
ggplot(runoff_stats_newnew, aes(x = alt_class, y = (ML_Before - ML_After)))+
  geom_point() +
  xlab('Altitude classes') +
  ylab('Medium/Low estimation before/after')

#Characterizing our runoff values by seasons(winter/summer), adding months
runoff_eu_day$month <- as.numeric(substring(runoff_eu_day$date,6,7))
runoff_eu_day$Season <- factor('Winter')
runoff_eu_day[month >= 3 & month <= 5, Season := factor('Spring')]
runoff_eu_day[month >= 6 & month <= 8, Season := factor('Summer')]
runoff_eu_day[month >= 9 & month <= 11, Season := factor('Autmn')]
runoff_eu_day
saveRDS(runoff_eu_day, './source/runoff_eu_day.rds')


#Aggregating mean values by seasons & before/after 1980
runoff_summer_b <- runoff_eu_day[Season == 'Summer' & year_type == 'before 1980', .(value = mean(value)), by = .(id, year_type)]
runoff_summer_a <- runoff_eu_day[Season == 'Summer' & year_type == 'after 1980', .(value = mean(value)), by = .(id, year_type)]
runoff_winter_b <- runoff_eu_day[Season == 'Winter' & year_type == 'before 1980', .(value = mean(value)), by = .(id, year_type)]
runoff_winter_a <- runoff_eu_day[Season == 'Winter' & year_type == 'after 1980', .(value = mean(value)), by = .(id, year_type)]

runoff_change <- data.table(runoff_winter_a$id,before = (runoff_winter_b$value - runoff_summer_b$value) / 100, after = (runoff_winter_a$value - runoff_summer_a$value) / 100)

saveRDS(runoff_change, './source/runoff_change.rds')

##finally making a map
sta_map <- leaflet(data = runoff_eu_info) %>%
  addTiles() %>%
  addMarkers(~Lon, ~Lat, label = ~Station)
sta_map