WHO = read.csv("Analytics_Edge/Unit 7/WHO.csv")
install.packages('ggplot2')
library(ggplot2)

scatterplot = ggplot(WHO, aes(x=GNI, y=FertilityRate))
scatterplot + geom_point(color='darkred', size=3, shape = 15)


ggplot(WHO, aes(x=GNI, y = FertilityRate, color=Region)) + geom_point()
ggplot(WHO, aes(x=GNI, y = FertilityRate, color=LifeExpectancy)) + geom_point()

ggplot(WHO, aes(x=FertilityRate, y=Under15)) + geom_point()
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point()

model = lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)

ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", level=0.99)
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE, color="orange")

ggplot(WHO, aes(x=FertilityRate, y=Under15, color=Region)) + geom_point() + scale_color_brewer(palette="Dark2")

