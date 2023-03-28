library("readxl") 
library("tibble")
library("dplyr")
library(ggplot2)
require(ggplot2)
require (lme4)
library(ggplot2)
library(sjPlot)
library(data.table)
IUGR<-read_excel("insert_file_name")
IUGR_DF = data.frame(EO_IUGR)

#Example with UA PI, however you can use any doppler indices you have 
IUGR_DF <- EO_IUGR_DF[, c("Baby", "Weeks", "UA")]
IUGR_DF<-na.omit(EO_IUGR_DF)

#Violin Plots

#This section just allows us to manipulate the groups 
dataCom$CPR <- as.numeric(IUGR_DF$UA)
CON<-dataCom[dataCom$Category == 'CON',]
LO<-dataCom[dataCom$Category == 'LO',]
EO<-dataCom[dataCom$Category == 'EO',]
dataCom$Category <- factor(dataCom$Category)

#This is how to plot the Violin Plots 
ggplot(data=dataCom, aes(x=Category, y=UA)) + 
  geom_violin()+theme_bw()+ggtitle("UA (PI) - Violin Plot")+ylab("UA (PI)")+ stat_summary(fun.y=mean, geom="point", size=2, color="red") #RED dot is the mean UA PI 
#This is how you calculate Coefficient of Variance for each group 
sd_coef_UA_CON<-sd(CON$UA)/mean(CON$UA)
sd_coef_UA_CON
EO_UA<-sd(EO$UA)/mean(EO$UA)
EO_UA
LO_UA<-sd(LO$UA)/mean(LO$UA)
LO_UA



#Simple Linear Regression
lm.EO.UA<-lm(UA~ Weeks, IUGR_DF)
summary(lm.EO.UA)
tab_model(lm.EO.UA) #This tells you the CI

#This is how you plot the pooled cross-sectional Simple Linear Regression
ggplot(LO_IUGR_DF_UA,aes(Weeks, UA)) +
  geom_point() +
  geom_smooth(method='lm') +
  ylab("UA (PI)")+ xlab("Gestational age (Weeks)") +
  ggtitle("LO-IUGR - UA vs Weeks - Simple Linear Regression") +scale_y_continuous(limits = c(0, 6),breaks = seq(0, 6, by = 2))+
  scale_x_continuous(limits = c(15, 40),breaks = seq(15, 40, by = 5))+theme_bw()


#This is how you plot the individual Simple Linear Regression 
myX<- scale_x_continuous(breaks=seq(15,40, by = 5))
myY<-scale_y_continuous(breaks=seq(from = 0, to = 6, by = 2))
g24<- ggplot(data=EO_IUGR_DF, aes(x=Weeks, y = UA, group=Baby, color=factor(Baby))) +geom_point() +ylab("UA (PI)") + xlab("Gestational age (Weeks)")
g25<- g24+theme_bw() +stat_smooth(method="lm", se= FALSE) + expand_limits(x=c(15,40), y=c(0, 6)) +guides(color=guide_legend("Baby")) + ggtitle("INSERT TITLE") + facet_wrap(~Baby)
print(g25)


#LMER.1 UA How to plot LMER.1 (Fixed slope, random intercept)
lmer.1.LO.UA<- lmer(UA~ Weeks +(1|Baby), data = IUGR_DF, REML=FALSE)
summary(lmer.1.LO.UA)
tab_model(lmer.1.LO.UA)

#This generates and works out the average slope
ab_line.LMER1.UA.LO<- coef(lmer.1.LO.UA)[["Baby"]] %>% 
  tibble::rownames_to_column("Baby") %>% 
  rename(intercept = `(Intercept)`)
ab_line.LMER1.UA.LO


#This is how you plot the individual LMER.1
ggplot(LO_IUGR_DF_UA) + 
  aes(x = Weeks, y = UA, group= Baby, color=factor(Baby)) + 
  # Add the individUAl lmer lines
  geom_abline(aes(intercept = intercept, slope = Weeks, color="red"), data = ab_line.LMER1.UA.LO) +
  labs(x="Gestational Age (Weeks)",y="UA (PI)")+ scale_x_continuous(limits=c(15,40))+ scale_y_continuous(limits=c(0,2))+
  # Map the points
  geom_point() + 
  #fixed lmer line
  geom_abline(aes(intercept=`(Intercept)`, slope=Weeks), color= "black", as.data.frame(t(fixef(lmer.1.LO.UA))))+
  # Tile
  facet_wrap("Baby")+guides(color=guide_legend("Baby"))+ ggtitle("LMER.1 LO-IUGR of UA vs Weeks") 


#LMER.2 UA - How to plot LMER.2 (Random slope and random intercept)
lmer.2.LO.UA<- lmer(UA ~Weeks+ (Weeks|Baby),LO_IUGR_DF_UA, REML=FALSE)
summary(lmer.2.LO.UA)
tab_model(lmer.2.LO.UA)

#Works out average slope
ab_line.LMER2.UA.LO<- coef(lmer.2.LO.UA)[["Baby"]] %>% 
  tibble::rownames_to_column("Baby") %>% 
  rename(intercept = `(Intercept)`)
ab_line.LMER2.UA.LO

#This plots individual LMER.2
ggplot(LO_IUGR_DF_UA) + 
  aes(x = Weeks, y = UA, group= Baby, color=factor(Baby)) + 
  # Add the individUAl lmer lines
  geom_abline(aes(intercept = intercept, slope = Weeks, color="red"), data = ab_line.LMER2.UA.LO) +
  labs(x="Gestational Age (Weeks)",y="UA (PI)")+ scale_x_continuous(limits=c(15,40))+ scale_y_continuous(limits=c(0,7))+
  geom_point() + 
  #fixed lmer line
  geom_abline(aes(intercept=`(Intercept)`, slope=Weeks), color= "black", as.data.frame(t(fixef(lmer.2.LO.UA))))+
  # Tile
  facet_wrap("Baby")+guides(color=guide_legend("Baby")) + ggtitle("LMER.2 LO-LUGR - UA vs Weeks") 

# where red is the individual lmer for each baby and the black is the average

#This compares the BEST MODEL, between the LMMs and the Simple Linear Regression
anova(lmer.2.LO.UA, lmer.1.LO.UA, lm.LO.UA) 


#Cluster Analysis

#Generates the individual trajectories 
model <- lm(UA ~ Weeks, data=IUGR_DF)
summary (model)
model$coefficients
dat <- data.table(IUGR_DF)
dat[,coef(lm(UA~Weeks)), by =Baby]

#Makes list of the trajectories 
df1<-dat[,list(intercept=coef(lm(UA~Weeks))[1], coef=coef(lm(UA~Weeks))[2]),by=Baby]
df1 <- na.omit(df1)
df1<-scale(df1)
df1<-df1[, c("coef", "intercept")]

#Makes the parameters for the cluster - might need to change and plots the cluster
km.res1 <- kmeans(df1, 2, nstart=25)
km.res1$centers
fviz_cluster(km.res1, data=df1)+
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
  )))+ xlab("Coefficient")+ylab("Intercept")+ggtitle("Cluster plot for UA intercepts and coefficient ")

#Go to excel and label which ones are the severe and mild fetuses 

#Now plot a violin plot against the different severities "rank" and the outcome marker (birth weight in this case)
ggplot(ranks1, aes(x = rank, y = birth_weight))+geom_violin()+
  ggtitle("Severity score of UA trajectory against Birth Weight")+ylab("Birth weight (g)")+xlab("Severity Score")+ 
  scale_y_continuous(limits=c(0,3000))+theme_bw()+stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  geom_label(label="Red dot = Mean Birth Weight", y=3000)

#Compares the different clusters using Mann-Withney U test 
library(coin)
wilcox_test(birth_weight~ rank, data=ranks1, distribution = "exact")




