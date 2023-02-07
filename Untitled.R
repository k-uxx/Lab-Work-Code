library("readxl") 
library("tibble")
library("dplyr")
library(ggplot2)
require(ggplot2)
require (lme4)
library(ggplot2)
library(sjPlot)
IUGR<-read_excel("insert_file_name")
IUGR_DF = data.frame(EO_IUGR)
IUGR_DF <- EO_IUGR_DF[, c("Baby", "Weeks", "UA")]
IUGR_DF<-na.omit(EO_IUGR_DF)


#Simple Linear Regression
lm.EO.UA<-lm(UA~ Weeks, EO_IUGR_DF)
summary(lm.EO.UA)
tab_model(lm.EO.UA)

myX<- scale_x_continuous(breaks=seq(15,40, by = 5))
myY<-scale_y_continuous(breaks=seq(from = 0, to = 6, by = 2))
g24<- ggplot(data=EO_IUGR_DF, aes(x=Weeks, y = UA, group=Baby, color=factor(Baby))) +geom_point() +ylab("UA (PI)") + xlab("Gestational age (Weeks)")
g25<- g24+theme_bw() +stat_smooth(method="lm", se= FALSE) + expand_limits(x=c(15,40), y=c(0, 6)) +guides(color=guide_legend("Baby")) + ggtitle("EO IUGR of UA vs Weeks") + facet_wrap(~Baby)
print(g25)