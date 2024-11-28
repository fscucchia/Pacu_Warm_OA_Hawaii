

#################  "Survival and Settlement Data - Statistic Analyses"  #################

#This script performs the statistic analyses for the survival and settlement data for Pacuta larvae and primary polyps


## Set directory

setwd("C:/Users/Federica/OneDrive - University of Rhode Island/Documenti/PhD related/Clean_workspace/Hawaii_2206/R_stats")

## Load packages

library(dplyr)
library(ggpubr)
library(onewaytests)

## Import data into R

my_data <- read.csv("survivorship_settlement.csv", sep = ";")

## Assess the normality of the data per each variable

#Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S. It is based on the correlation between the data and the corresponding normal scores.

survived <- filter(my_data, variable == "survived (swimming + settlers)")
shapiro.test(survived$count)

settled <- filter(my_data, variable == "settlers")
shapiro.test(settled$count)

settle_capacity <- filter(my_data, variable == "settle capacity")
shapiro.test(settle_capacity$count)

#Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution.

ggqqplot(survived$count)
ggqqplot(settled$count)
ggqqplot(settle_capacity$count)

## Assess homogeneity of variance using the Levene's test

leveneTest(count ~ condition, data = survived)
leveneTest(count ~ condition, data = settled)
leveneTest(count ~ condition, data = settle_capacity)

## Compute one-way ANOVA test

# Compute the analysis of variance
res.aov_survived <- aov(count ~ condition, data = survived)
res.aov_settled <- aov(count ~ condition, data = settled)
res.aov_settle_capacity <- aov(count ~ condition, data = settle_capacity)
# Summary of the analysis
summary(res.aov_survived)
summary(res.aov_settled)
summary(res.aov_settle_capacity)

## Tukey multiple pairwise-comparisons

TukeyHSD(res.aov_survived)
TukeyHSD(res.aov_settled)
TukeyHSD(res.aov_settle_capacity)


