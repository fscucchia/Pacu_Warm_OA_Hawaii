
#################  "HOBO Data"

#This script reads and plots temperature and light data from Hobo loggers 

## Load packages

library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(purrr)
library(lubridate)
library(ggplot2)
library(seacarb)
library(broom)
library(cowplot)
library(emmeans)
library(plotrix)
library(Rmisc)

setwd("....../HOBO")

## Read in Hobo logger files #JEss script

hobo <- read.csv("Hobo_allSquarical.csv", sep = ";")

hobo$Treatment <- factor(hobo$Treatment, levels=c("Control","Mid", "High"))
#hobo$Date<-as.Date(hobo$Date, "%d/%m %Y")
#hobo$Date_Time<- as.POSIXct(paste(hobo$Date, hobo$Time), "%Y-%m-%d %H:%M", tz = "GMT", usetz = FALSE)

hobo$Date <- format(as.Date(hobo$Date, format="%d/%m/%Y"),"%Y-%m-%d")

#bring together date and time column
hobo$Date_Time<- as.POSIXct(paste(hobo$Date, hobo$Time), "%Y-%m-%d %H:%M", tz = "GMT", usetz = FALSE)

sum(is.na(hobo))


temp<-hobo%>%
  group_by(Treatment,Date_Time)%>%
  summarize_at(vars(Temp), list(mean = mean, sd = sd), na.rm=T)
write.csv(temp, file = "temperature_dailymean.csv")

#summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
temp2 <- summarySE(hobo, measurevar="Temp", groupvars=c("Treatment","Date_Time"))
temp3 <- summarySE(hobo, measurevar="Temp", groupvars=c("Treatment","Time"))

count = hobo%>%
  group_by(Treatment,Date)%>% summarise(n = n())
View(count)

#count = count %>% filter(n != 288)

temp$Date<- as.POSIXct(temp$Date)

temp2 <- temp2 [-c(1:5851), ]

#https://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# Standard error of the mean
temp.plot_SE2 <- ggplot(temp2, aes(y=Temp, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.1) +
  geom_point(data=temp2, size=0.1, alpha = 0.4)+
  geom_line(data=temp2, aes(y=Temp, x=Date_Time),size=0.1,alpha=0.4)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="blue", "Mid"= "orange", "High"= "red"))+
  scale_color_manual("Treatment", values=c("Control"="blue", "Mid"= "orange", "High"= "red"))+
  scale_y_continuous(limits = c(24, 35)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


temp.plot_SE2



temp.plot_SE <- ggplot(temp3, aes(y=Temp, x=Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.8) +
  #geom_point(size=0.05, alpha = 0.1)+
  geom_line(data=temp3, aes(y=Temp, x=Time),size=0.5,alpha=0.8)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="blue", "Mid"= "orange", "High"= "red"))+
  scale_color_manual("Treatment", values=c("Control"="blue", "Mid"= "orange", "High"= "red"))+
  scale_y_continuous(limits = c(24, 35)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


temp.plot_SE


####### Control temperature from beginning until the 10th of July (start of the experiment)

#end of July 9th in row 13526

control_temp <-filter(hobo, Treatment == "Control")

control_temp_filt <- control_temp[-c(13526:23010), ]

control_temp_filt2 <- summarySE(control_temp_filt, measurevar="Temp", groupvars=c("Treatment","Date_Time"))
control_temp_filt3 <- control_temp_filt2[-c(5851:7665), ]

control_temp_filt4 <- na.omit(control_temp_filt)
control_temp_filt4 <- summarySE(control_temp_filt4, measurevar="Temp", groupvars=c("Treatment","Time"))



control_temp.plot <- ggplot(control_temp_filt3, aes(y=Temp, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.1) +
  #geom_point(size=0.05, alpha = 0.1)+
    geom_line(data=control_temp_filt3, aes(y=Temp, x=Date_Time),size=0.5,alpha=0.8)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="sandybrown"))+
  scale_color_manual("Treatment", values=c("Control"="sandybrown"))+
  scale_y_continuous(limits = c(24, 32)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


control_temp.plot

library(scales) 

control_temp.plot <- ggplot(control_temp_filt4, aes(y=Temp, x=Time, group =Treatment, color=Treatment)) +
  geom_line(size=0.1, alpha = 0.4)+
  geom_point(size=0.3, alpha = 0.4)+
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.1) +
    #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="sandybrown"))+
  scale_color_manual("Treatment", values=c("Control"="sandybrown"))+
  #scale_y_continuous(limits = c(22, 30), breaks = c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)) +
  scale_y_continuous(limits = c(23.5, 29), breaks = c(23,24,25, 26, 27, 28,29)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


control_temp.plot



####### Temperature from the 10th of July (start of the experiment)

#end of July 9th in row 67216

Exp_temp <- hobo %>% filter(hobo$Date > "2022-07-09")     

Exp_temp2 <- summarySE(Exp_temp, measurevar="Temp", groupvars=c("Treatment","Date_Time"))

sum(is.na(Exp_temp2))

Exp_temp.plot <- ggplot(Exp_temp2, aes(y=Temp, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.1) +
  geom_point(size=0.1, alpha = 0.4)+
  geom_line(data=Exp_temp2, aes(y=Temp, x=Date_Time),size=0.1,alpha=0.4)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="blue","Mid"= "orange", "High"= "red"))+
  scale_color_manual("Treatment", values=c("Control"="blue","Mid"= "orange", "High"= "red"))+
  scale_y_continuous(limits = c(24, 35)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


Exp_temp.plot




####### Temperature from the 10th of July (start of the experiment)

#end of July 9th in row 67216

Exp_temp <- hobo %>% filter(hobo$Date > "2022-07-09")     

Exp_temp2 <- summarySE(Exp_temp, measurevar="Temp", groupvars=c("Treatment","Date_Time"))

sum(is.na(Exp_temp2))

Exp_temp.plot <- ggplot(Exp_temp2, aes(y=Temp, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.1) +
  geom_point(size=0.1, alpha = 0.4)+
  geom_line(data=Exp_temp2, aes(y=Temp, x=Date_Time),size=0.1,alpha=0.4)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="blue","Mid"= "orange", "High"= "red"))+
  scale_color_manual("Treatment", values=c("Control"="blue","Mid"= "orange", "High"= "red"))+
  scale_y_continuous(limits = c(24, 35)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


Exp_temp.plot


####### Temp from the 10th of July (start of the experiment)

######### Only control

Exp_temp_control <- hobo %>% filter(hobo$Date > "2022-07-09")    
Exp_temp_control <- Exp_temp_control %>% filter(Exp_temp_control$Treatment == "Control")  

Exp_temp_control2 <- summarySE(Exp_temp_control, measurevar="Temp", groupvars=c("Treatment","Date_Time"))

sum(is.na(Exp_temp_control2))


Exp_temp.plot <- ggplot(Exp_temp_control2, aes(y=Temp, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.1) +
  geom_point(size=0.3, alpha = 0.4)+
  geom_line(data=Exp_temp_control2, aes(y=Temp, x=Date_Time),size=0.1,alpha=0.4)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="sandybrown"))+
  scale_color_manual("Treatment", values=c("Control"="sandybrown"))+
  scale_y_continuous(limits = c(24, 35)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


Exp_temp.plot


####### Temp from the 10th of July (start of the experiment)

######### Only mid

Exp_temp_mid <- hobo %>% filter(hobo$Date > "2022-07-09")    
Exp_temp_mid <- Exp_temp_mid %>% filter(Exp_temp_mid$Treatment == "Mid")  

Exp_temp_mid2 <- summarySE(Exp_temp_mid, measurevar="Temp", groupvars=c("Treatment","Date_Time"))

sum(is.na(Exp_temp_mid2))


Exp_temp.plot <- ggplot(Exp_temp_mid2, aes(y=Temp, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.1) +
  geom_point(size=0.3, alpha = 0.4)+
  geom_line(data=Exp_temp_mid2, aes(y=Temp, x=Date_Time),size=0.1,alpha=0.4)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Mid"= "indianred1"))+
  scale_color_manual("Treatment", values=c("Mid"= "indianred1"))+
  scale_y_continuous(limits = c(24, 35)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


Exp_temp.plot



####### Temp from the 10th of July (start of the experiment)

######### Only High

Exp_temp_high <- hobo %>% filter(hobo$Date > "2022-07-09")    
Exp_temp_high <- Exp_temp_high %>% filter(Exp_temp_high$Treatment == "High")  

Exp_temp_high2 <- summarySE(Exp_temp_high, measurevar="Temp", groupvars=c("Treatment","Date_Time"))

sum(is.na(Exp_temp_high2))


Exp_temp.plot <- ggplot(Exp_temp_high2, aes(y=Temp, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=0.05, alpha=0.02) +
  geom_point(size=0.3, alpha = 0.2)+
  geom_line(data=Exp_temp_high2, aes(y=Temp, x=Date_Time),size=0.1,alpha=0.4)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("High"= "darkred"))+
  scale_color_manual("Treatment", values=c("High"= "darkred"))+
  scale_y_continuous(limits = c(24, 35)) +
  labs(y = "Temperature (°C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 


Exp_temp.plot




Light<-hobo%>%
  group_by(Treatment,Date_Time)%>%
  summarize_at(vars(Light), list(mean = mean, sd = sd), na.rm=T)
write.csv(temp, file = "light_dailymean.csv")


Light$Date<- as.POSIXct(Light$Date)

#summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
light2 <- summarySE(hobo, measurevar="Light", groupvars=c("Treatment","Date_Time"))
light3 <- light2 %>% filter(light2$Date > "2022-07-10")    
sum(is.na(light3))


light_mid <-filter(light3, Treatment == "Mid")

Light.plot_SE <- ggplot(light3, aes(y=Light, x=Date_Time, color=Treatment)) + 
  geom_errorbar(aes(ymin=Light-se, ymax=Light+se), width=0.05, alpha=0.1) +
  geom_point(size=0.1, alpha = 0.4)+
  #geom_line(data=light3, aes(y=Light, x=Date_Time),size=0.5,alpha=0.8)+
  #geom_smooth(method="loess", span=0.1, se=FALSE, aes(colour=Treatment), alpha=1, show.legend=TRUE)+
  scale_fill_manual("Treatment", values=c("Control"="sandybrown", "Mid"= "indianred1", "High"= "darkred"))+
  scale_color_manual("Treatment", values=c("Control"="sandybrown", "Mid"= "indianred1", "High"= "darkred"))+
  scale_y_continuous(limits = c(0, 8000)) +
  labs(y = "Light (lux)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),#angling the labels on the x-axis
        panel.background= element_rect(fill=NA, color='black'),#this is making the black box around the graph
        strip.background = element_blank(), 
        strip.text = element_blank(),
        #legend.title = element_blank(),
        #legend.text =element_text(size = 12),
        #legend.background = element_blank(),
        #legend.position=c(0.85,0.2),
        axis.text.y = element_text(vjust=0.5, size=12), #making the axis text larger 
        axis.title.x = element_blank(),#making the axis title larger 
        axis.title.y = element_text(size=12))#making the axis title larger 

Light.plot_SE
  

############## test whether Light intensity is statistically different across tanks within each treatment
  
####### remove outliers within each treatment 

hobo_exp <- hobo %>% filter(hobo$Date > "2022-07-10")

hobo_no_outliers <- hobo_exp %>%
  dplyr::group_by(Treatment, Date_Time) %>%
  dplyr::filter(
    Light > quantile(Light, 0.25, na.rm = TRUE) - 1.5 * IQR(Light, na.rm = TRUE) &
      Light < quantile(Light, 0.75, na.rm = TRUE) + 1.5 * IQR(Light, na.rm = TRUE)
  ) %>%
  ungroup()

# use hobo_no_outliers for statistics


# Assign tank numbers
hobo_no_outliers <- hobo_no_outliers %>%
  dplyr::group_by(Treatment, Date_Time) %>%
  dplyr::mutate(tank = row_number()) %>%
  ungroup()

library(lme4)
library(lmerTest)

# Fit a mixed model
#fit a linear mixed model with tank as a random effect and Treatment as a fixed effect
results <- lapply(unique(hobo_no_outliers$Treatment), function(tr) {
  dat <- filter(hobo_no_outliers, Treatment == tr)
  mod <- lmer(Light ~ 1 + (1|tank), data = dat)
  list(
    treatment = tr,
    summary = summary(mod)
  )
})

# Print variance for tank in each treatment
for (res in results) {
  cat("\nTreatment:", res$treatment, "\n")
  print(res$summary)
}

# Treatment: 1 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Light ~ 1 + (1 | tank)
#    Data: dat
# 
# REML criterion at convergence: 53626.5
# 
# Scaled residuals: 
#    Min     1Q Median     3Q    Max 
# -0.778 -0.136 -0.084 -0.001 34.409 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  tank     (Intercept)  866398   930.8  
#  Residual             6130457  2476.0  
# Number of obs: 2904, groups:  tank, 3
# 
# Fixed effects:
#             Estimate Std. Error    df t value Pr(>|t|)
# (Intercept)    855.7      539.4   2.0   1.586    0.254
# 
# Treatment: 2 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Light ~ 1 + (1 | tank)
# Data: dat
# 
# REML criterion at convergence: 44735.4
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -0.499 -0.300 -0.144  0.079 41.966 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# tank     (Intercept)   5859    76.55  
# Residual             252944   502.94  
# Number of obs: 2928, groups:  tank, 3
# 
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)  
# (Intercept)   196.60      45.16   2.00   2.353   0.0589
# 
# Treatment: 3 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Light ~ 1 + (1 | tank)
#    Data: dat
# 
# REML criterion at convergence: 50365.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -0.5631 -0.1431 -0.0770  0.0047 31.4796 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  tank     (Intercept)  154541   393.1  
#  Residual             2347142  1532.0  
# Number of obs: 2877, groups:  tank, 3
# 
# Fixed effects:
#             Estimate Std. Error    df t value Pr(>|t|)
# (Intercept)    413.7      228.8   2.0   1.808    0.212




#### test if if all tanks received the same amount of light throughout the experiment 

model_tank <- lmer(Light ~ 1 + (1|tank), data = hobo_no_outliers)
summary(model_tank)
# > summary(model_tank)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Light ~ 1 + (1 | tank)
#    Data: hobo_no_outliers
# 
# REML criterion at convergence: 155044.2
# 
# Scaled residuals: 
#    Min     1Q Median     3Q    Max 
# -0.444 -0.239 -0.138 -0.003 48.595 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  tank     (Intercept)   76618   276.8  
#  Residual             3156283  1776.6  
# Number of obs: 8709, groups:  tank, 3
# 
# Fixed effects:
#             Estimate Std. Error    df t value Pr(>|t|)  
# (Intercept)    488.1      160.9   2.0   3.033   0.0937 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


############# assess whether light intensity differed among all tanks throughout the experiment

library(lme4)
model <- lmer(Light ~ tank * Date_Time + (1|Treatment), data = hobo_no_outliers)
anova(model)
# > anova(model)
# Type III Analysis of Variance Table with Satterthwaite's method
#                  Sum Sq  Mean Sq NumDF  DenDF F value Pr(>F)  
# Date_Time      11950005 11950005     1 6446.7  3.8767 0.0590 
# tank:Date_Time  8296134  8296134     1 6291.2  2.6914 0.1009  
# tank                                                          

######## the interaction (tank:Date_Time) is not significant, it means Light changes similarly in all tanks over time.



















# ## Read in Hobo logger files 
# 
# #Control squarical 1
# C_sq1 <- read.csv("C_sq1 2022-06-28 16_02_49 -1000_complete2.csv", sep = ";")
# 
# colnames(C_sq1)[1] ="Date Time, GMT-10:00"
# colnames(C_sq1)[2] ="GMT-10:00"
# colnames(C_sq1)[3] ="Temp, °C"
# colnames(C_sq1)[4] ="Intensity, lux"
# #colnames(C_sq1)[2] ="Temp, °C"
# #colnames(C_sq1)[3] ="Intensity, lux"
# 
# #change date format
# C_sq1$"Date Time, GMT-10:00" <- format(as.Date(C_sq1$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# #bring together date and time column
# C_sq1$"Date Time, GMT-10:00" <- str_c(C_sq1$"Date Time, GMT-10:00"," ", C_sq1$"GMT-10:00")
# C_sq1 <- C_sq1[ -c(2,5:8) ]
# 
# #Control squarical 2
# C_sq2 <- read.csv("C_sq2 2022-06-28 16_00_18 -1000_complete2.csv", sep = ";")
# 
# colnames(C_sq2)[1] ="Date Time, GMT-10:00"
# colnames(C_sq2)[2] ="GMT-10:00"
# colnames(C_sq2)[3] ="Temp, °C"
# colnames(C_sq2)[4] ="Intensity, lux"
# 
# C_sq2$"Date Time, GMT-10:00" <- format(as.Date(C_sq2$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# C_sq2$"Date Time, GMT-10:00" <- str_c(C_sq2$"Date Time, GMT-10:00"," ", C_sq2$"GMT-10:00")
# C_sq2 <- C_sq2[ -c(2,5:8) ]
# 
# #Control squarical 3
# C_sq3 <- read.csv("C_sq3 2022-06-28 15_58_39 -1000_complete2.csv", sep = ";")
# 
# colnames(C_sq3)[1] ="Date Time, GMT-10:00"
# colnames(C_sq3)[2] ="GMT-10:00"
# colnames(C_sq3)[3] ="Temp, °C"
# colnames(C_sq3)[4] ="Intensity, lux"
# 
# C_sq3$"Date Time, GMT-10:00" <- format(as.Date(C_sq3$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# C_sq3$"Date Time, GMT-10:00" <- str_c(C_sq3$"Date Time, GMT-10:00"," ", C_sq3$"GMT-10:00")
# C_sq3 <- C_sq3[ -c(2,5:8) ]
# 
# #Mid squarical 1
# M_sq1 <- read.csv("M_sq1 2022-06-28 16_04_36 -1000_complete2.csv", sep = ";")
# 
# colnames(M_sq1)[1] ="Date Time, GMT-10:00"
# colnames(M_sq1)[2] ="GMT-10:00"
# colnames(M_sq1)[3] ="Temp, °C"
# colnames(M_sq1)[4] ="Intensity, lux"
# 
# M_sq1$"Date Time, GMT-10:00" <- format(as.Date(M_sq1$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# M_sq1$"Date Time, GMT-10:00" <- str_c(M_sq1$"Date Time, GMT-10:00"," ", M_sq1$"GMT-10:00")
# M_sq1 <- M_sq1[ -c(2,5:8) ]
# 
# 
# #Mid squarical 2
# M_sq2 <- read.csv("M_sq2 2022-06-28 16_05_38 -1000_complete2.csv", sep = ";")
# 
# colnames(M_sq2)[1] ="Date Time, GMT-10:00"
# colnames(M_sq2)[2] ="GMT-10:00"
# colnames(M_sq2)[3] ="Temp, °C"
# colnames(M_sq2)[4] ="Intensity, lux"
# 
# M_sq2$"Date Time, GMT-10:00" <- format(as.Date(M_sq2$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# M_sq2$"Date Time, GMT-10:00" <- str_c(M_sq2$"Date Time, GMT-10:00"," ", M_sq2$"GMT-10:00")
# M_sq2 <- M_sq2[ -c(2,5:8) ]
# 
# 
# #Mid squarical 3
# M_sq3 <- read.csv("M_sq3 2022-06-28 16_08_08 -1000_complete2.csv", sep = ";")
# 
# colnames(M_sq3)[1] ="Date Time, GMT-10:00"
# colnames(M_sq3)[2] ="GMT-10:00"
# colnames(M_sq3)[3] ="Temp, °C"
# colnames(M_sq3)[4] ="Intensity, lux"
# 
# M_sq3$"Date Time, GMT-10:00" <- format(as.Date(M_sq3$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# M_sq3$"Date Time, GMT-10:00" <- str_c(M_sq3$"Date Time, GMT-10:00"," ", M_sq3$"GMT-10:00")
# M_sq3 <- M_sq3[ -c(2,5:8) ]
# 
# 
# #High squarical 1
# H_sq1 <- read.csv("H_sq1 2022-06-28 16_15_37 -1000_complete2.csv", sep = ";")
# 
# colnames(H_sq1)[1] ="Date Time, GMT-10:00"
# colnames(H_sq1)[2] ="GMT-10:00"
# colnames(H_sq1)[3] ="Temp, °C"
# colnames(H_sq1)[4] ="Intensity, lux"
# 
# H_sq1$"Date Time, GMT-10:00" <- format(as.Date(H_sq1$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# H_sq1$"Date Time, GMT-10:00" <- str_c(H_sq1$"Date Time, GMT-10:00"," ", H_sq1$"GMT-10:00")
# H_sq1 <- H_sq1[ -c(2,5:8) ]
# 
# #High squarical 2
# H_sq2 <- read.csv("H_sq2 2022-06-28 16_12_09 -1000_complete2.csv", sep = ";")
# 
# colnames(H_sq2)[1] ="Date Time, GMT-10:00"
# colnames(H_sq2)[2] ="GMT-10:00"
# colnames(H_sq2)[3] ="Temp, °C"
# colnames(H_sq2)[4] ="Intensity, lux"
# 
# H_sq2$"Date Time, GMT-10:00" <- format(as.Date(H_sq2$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# H_sq2$"Date Time, GMT-10:00" <- str_c(H_sq2$"Date Time, GMT-10:00"," ", H_sq2$"GMT-10:00")
# H_sq2 <- H_sq2[ -c(2,5:8) ]
# 
# 
# #High squarical 3
# H_sq3 <- read.csv("H_sq3 2022-06-28 16_10_50 -1000_complete2.csv", sep = ";")
# 
# colnames(H_sq3)[1] ="Date Time, GMT-10:00"
# colnames(H_sq3)[2] ="GMT-10:00"
# colnames(H_sq3)[3] ="Temp, °C"
# colnames(H_sq3)[4] ="Intensity, lux"
# 
# H_sq3$"Date Time, GMT-10:00" <- format(as.Date(H_sq3$"Date Time, GMT-10:00", format="%d/%m/%Y"),"%m/%d/%y")
# 
# H_sq3$"Date Time, GMT-10:00" <- str_c(H_sq3$"Date Time, GMT-10:00"," ", H_sq3$"GMT-10:00")
# H_sq3 <- H_sq3[ -c(2,5:8) ]
# 
# 
# ## Truncate dates so that date range is only capturing the experimental period
# 
# # C_sq1.1 <- C_sq1[-c(1:7670), ]
# # C_sq2.1 <- C_sq2[-c(1:7670), ]
# # C_sq3.1 <- C_sq3[-c(1:7670), ]
# # M_sq1.1 <- M_sq1[-c(1:7670), ]
# # M_sq2.1 <- M_sq2[-c(1:7670), ]
# # M_sq3.1 <- M_sq3[-c(1:7670), ]
# # H_sq1.1 <- H_sq1[-c(1:7670), ]
# # H_sq2.1 <- H_sq2[-c(1:7670), ]
# # H_sq3.1 <- H_sq3[-c(1:5564), ]
# 
# 
# ## Read in treatment information that assigns the loggers to a specific treatment
# 
# metadata <- read.csv("............/metadata.csv", sep = ";")
# #metadata$logger <- as.character(metadata$logger)
# 
# 
# ## Assign treatment information to logger
# 
# # loggers <- full_join(loggers, metadata, by = "logger")
# # loggers$Type <- as.factor(loggers$Type)
# # loggers$Incubator <- as.factor(loggers$Incubator)
# metadata$Treatment <- as.factor(metadata$Treatment)
# loggers$DateTime <- as.numeric(loggers$DateTime)
# 
# 
# ## Plot temperature by incubator colored by treatment 
# temp_plot1<-C_sq1%>%
#   ggplot(aes(x="Date Time, GMT-10:00", y="Temp, °C", colour=Treatment))+
#   geom_point()+
#   #ylim(26, 30)+
#   scale_colour_manual(values=c("blue", "red"), name="Temperature")+
#   ylab("Temperature (°C)")+
#   xlab("Date Time")+
#   theme_classic(); temp_plot1
# 
# 
# 
# 
