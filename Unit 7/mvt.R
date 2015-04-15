mvt = read.csv("Analytics_Edge/Unit 7/mvt.csv")
str(mvt)
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)
table(mvt$Weekday)
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

library(ggplot2)
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Понедельник", "Вторник", "Среда", "Четверг", "Пятница", "Суббота", "Воскресенье"))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts") 
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts") 


table(mvt$Weekday, mvt$Hour)
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Понедельник", "Вторник", "Среда", "Четверг", "Пятница", "Суббота", "Воскресенье"))
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq))
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())


library(maps)
library(ggmap)

chicago = get_map(location="chicago", zoom=11)
ggmap(chicago)
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))

LatLonCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(LatLonCounts)
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) + scale_color_gradient(low="yellow", high="red")
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")

LatLonCounts2 = subset(LatLonCounts, LatLonCounts$Freq > 0)
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq), fill="red")

nrow(LatLonCounts) - nrow(LatLonCounts2)
