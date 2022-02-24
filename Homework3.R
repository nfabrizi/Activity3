### In-Class Prompts 3 ###

#install.packages("dplyr")

#install.packages("tidyverse")
library(tidyverse)
library(dplyr)

datTemp <- read.csv("/cloud/project/activity03/climate-change.csv")
datTemp$Date <- ymd(datTemp$Day)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
colnames(datCO2)[4] <- "CO2"
datCO2$Entity <- as.factor(datCO2$Entity)
name.Ent <- levels(datCO2$Entity)


# Prompt 1

plot(datTemp$Date, datTemp$temperature_anomaly)
unique(datTemp$Entity)

NorhternH <- datTemp[datTemp$Entity == "Northern Hemisphere",]
SouthernH <- datTemp[datTemp$Entity == "Southern Hemisphere",]

plot(NorhternH$Date, NorhternH$temperature_anomaly,
     type="l")

points (SouthernH$Date, SouthernH$temperature_anomaly,
        type="l", col = "tomato3")


ggplot(data = datTemp[datTemp$Entity != "World",], 
       aes(x = Date, 
                           y = temperature_anomaly,
                           color = Entity))+
  geom_line()+
  labs(x = "Year", y = "Temperature Anomaly")+
  theme_classic()
  

# Prompt 2

NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ 
  geom_point()+
  geom_line()+ 
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+
  theme_classic()


### Homework 3 ###


# Question 1

BiggestCO2 <- datCO2[datCO2$Entity == "Russia" |
                   datCO2$Entity == "Japan" |
                   datCO2$Entity == "United States" |
                   datCO2$Entity == "India" |
                   datCO2$Entity == "China", ]

ggplot(data = BiggestCO2, 
       aes(x = Year, y=CO2, color=Entity ) )+ 
  geom_line()+ 
  labs(x="Year", y="Fossil fuel emissions (tons CO2)")+
  theme_classic()

# Question 2

WorldTemp <- datTemp[datTemp$Entity == "World",]
ggplot(data = WorldTemp, 
       aes(x = Date, y=temperature_anomaly, color=Entity ) )+ 
  geom_line()+ 
  labs(x="Year", y="Global Tempreature Anomaly")+
  theme_classic()

WorldCO2 <- datCO2[datCO2$Entity == "World",]
ggplot(data = WorldCO2, 
       aes(x = Year, y=CO2, color=Entity ) )+ 
  geom_line()+ 
  scale_color_manual(values='Blue2') +
  labs(x="Year", y="Fossil fuel emissions (tons CO2)")+
  theme_classic()

# Question 3

USAPercep <- read.csv("/cloud/project/precipitation_fig-1.csv")
USPer <- USAPercep[-c(1, 2, 3, 4, 5, 6), ]
USPer_final <- USPer %>% 
  rename(Year = Figure.1..Precipitation.in.the.Contiguous.48.States..1901.2020,
    Percipitation.Anomaly = X)

plot(USPer_final$Year, USPer_final$Percipitation.Anomaly,
     type = "l", col = "dodgerblue", lwd = 1.5,
     xlab = "Year",
     ylab = "Percipitation Anomaly (inches)",
     main = "US Average Percipitation (1901 - 2020)")
legend(x = "topleft", legend = c("US Percipitation"),
       lty = c(1, 2), lwd = 1.5,
       col = "dodgerblue")






