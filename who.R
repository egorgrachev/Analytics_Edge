# Intro R Lecture

WHO = read.csv("../egor/AnalyticsEdge/WHO.csv") 
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)


