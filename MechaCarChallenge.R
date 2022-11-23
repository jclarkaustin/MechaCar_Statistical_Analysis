library(dplyr)
library(tidyverse)

setwd("C:/Users/Homer/Module_15_R/ChallengeRedo")
mech <- read.csv('MechaCar_mpg.csv')
coil <- read.csv('Suspension_Coil.csv')

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = mech)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = mech))

Mean = mean(coil$PSI)
Median = median(coil$PSI)
Variance = var(coil$PSI)
SD = sd(coil$PSI)

total_summary <- data.frame(Mean, Median, Variance, SD)

lot_summary <- coil %>% group_by(Manufacturing_Lot) %>% 
  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

t.test((coil$PSI),mu=1500)

t.test(subset(coil, Manufacturing_Lot == "Lot1")$PSI, mu = 1500)
t.test(subset(coil, Manufacturing_Lot == "Lot2")$PSI, mu = 1500)
t.test(subset(coil, Manufacturing_Lot == "Lot3")$PSI, mu = 1500)