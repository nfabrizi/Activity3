### Tutorial 3 ###

#The data
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
#install.packages("lubridate")

colnames(datCO2)
colnames(datCO2)[4] <- "CO2"

datCO2$Entity <- as.factor(datCO2$Entity)
name.Ent <- levels(datCO2$Entity)
name.Ent

#Plotting data in base R
plot(datCO2$Year, datCO2$CO2)

US <- datCO2[datCO2$Entity == "United States",]
ME <- datCO2[datCO2$Entity == "Mexico",]

plot(US$Year,
     US$CO2, 
     type = "b", 
     pch = 19, 
     ylab = "Annual fossil fuel emissions (tons CO2)",
     xlab = "Year",
     yaxt = "n")


axis(2, seq(0,6000000000, by=2000000000), 
     seq(0,6, by = 2), 
     las=2 )

points(ME$Year, 
       ME$CO2, 
       type = "b", 
       pch = 19, 
       col= "darkgoldenrod3")

plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n", # turn off y axis
     ylim = c(0,6200000000))
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")
legend("topleft",
       c("United States", "Mexico"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

# Plotting in ggplot2

ggplot(data = US, # data for plot
       aes(x = Year, y=CO2 ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+
  labs(x="Year", y="US fossil fuel emissions (tons CO2)") +
  theme_classic()

NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()


# Changing colors

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()+
  scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055"))

# Violin and box plots

compCO2 <- datCO2[datCO2$Year >= 1950 & datCO2$Entity == "France" |
                    datCO2$Year >= 1950 & datCO2$Entity == "India" |
                    datCO2$Year >= 1950 &  datCO2$Entity == "Russia" , ]

ggplot(data = compCO2 , aes(x=Entity, y=CO2))+ # look at CO2 by country
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.03,size=0.15, fill="grey90")+ 
  theme_classic()+ 
  labs(x = "Country", y="Annual emissions (tons CO2)")

# Area and ribbon plots

ggplot(data=compCO2, aes(x=Year, y=CO2, fill=Entity))+ # data
  geom_area(alpha = 0.5) +
  theme_classic()+
  labs(x="Year", y="Annual emissions (tons CO2)")


# Labels

b <- ggplot(data=compCO2,aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+
  geom_ribbon(alpha=0.5 )+
  labs(x="Year", y="Carbon emissions (tons CO2)") +
  theme_classic()

b + annotate("segment", # line label
             x=1991, # start x coordinate
             y=2450000000, # start y coordinate
             xend=1991, # end x coordinate
             yend=2600000000) + # end y coordinate
  annotate("text", # add text label
           x=1991, # center of label x coordinate
           y= 2700000000, # center of label y coordinate
           label="end of USSR") # label to add




















