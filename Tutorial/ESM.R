rm (list =ls())
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)

air<-read.csv("C:/Users/camer/Downloads/AirQuality.csv")

#site type w lines
ggplot(air, aes(x = Day, y = Conc, colour = Site_Type)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = 'Day', y = 'PM(2.5) Concentration (µg/m^3)', colour = 'Site Type') +
  theme_classic()

#each site
ggplot(air, aes(x = Day, y = Conc, colour = Site)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = 'Day', y = 'PM(2.5) Concentration (µg/m^3)', colour = 'Site') +
  theme_classic()

#site type no lines
ggplot(air, aes(x = Day, y = Conc, colour = Site_Type)) +
  geom_point() +
  labs(x = 'Day', y = 'PM(2.5) Concentration (µg/m^3)', colour = 'Site Type') +
  theme_classic()

airavg<-read.csv("C:/Users/camer/Downloads/AirAvg.csv")

#group urban and rural sites
airavg$Site <- factor(airavg$Site, levels=c('Warrington', 'Manchester Piccadilly', 'Leeds Centre', 
    'Northampton Spring Park', 'Dewsbury Ashworth Grange', 'High Muffles', 
    'Sibton', 'Tallington', 'Toft Newton', 'Weybourne'))

#Individual site avg
ggplot(airavg, aes(x = Site, y = Avg_Conc, colour = Site_Type)) +
  geom_point() +
  geom_errorbar(aes(ymin = Avg_Conc - SD, ymax = Avg_Conc + SD), width = 0.1) +
  labs(x = 'Site', y = 'Average PM(2.5) Concentration (µg/m^3)', colour = 'Site Type') +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

#avg 12.26944 urban
#avg 16.2892 rural

#standard modela
model2 <- lmer(Conc ~ Site_Type + (1|Day) + (1|Site), data = air)
model3<-lm(Conc ~Site_Type, data=air)

#glmm
model1 <- glmer(Conc ~ Site_Type + (1|Day) + (1|Site), data = air, family=Gamma(link='identity'))
summary(model1)

#check
plot(fitted(model1), residuals(model1))
hist(residuals(model1))
plot(model1)
