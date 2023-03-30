library(tidyverse)

mechacar_data <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_data)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_data))

suspension_data <- read.csv(file='Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
total_summary <- suspension_data %>% 
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
lot_summary <- suspension_data %>% 
  group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

t.test(suspension_data$PSI,mu=1500)
lot1 <- subset(suspension_data, Manufacturing_Lot == "Lot1")
lot2 <- subset(suspension_data, Manufacturing_Lot == "Lot2")
lot3 <- subset(suspension_data, Manufacturing_Lot == "Lot3")
t.test(lot1['PSI'],mu=1500)
t.test(lot2['PSI'],mu=1500)
t.test(lot3['PSI'],mu=1500)