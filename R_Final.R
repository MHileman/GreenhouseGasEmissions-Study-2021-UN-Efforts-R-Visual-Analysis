#
# Created by Matthew Hileman
# CS4720 - Data Visualization
# Final R Project - Part 2
# Due: 13 December 2021
#
# ----------------------------------------------------
# Data interpretation and visualization of:
# "Greenhouse Gas Emissions"
# with each dataset stored individually.
# taken from: http://data.un.org/Explorer.aspx?d=GHG
# ----------------------------------------------------
#

#----- libraries -----#
# load ggplot library
library(ggplot2)
# install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl")
library(readxl)
#install.packages("ggpubr")
library(ggpubr)
theme_set(theme_pubr())
#install.packages("packcircles")
library(packcircles)
#install.packages("viridis")
library(viridis)
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("readr")
library("dplyr")
library("plyr")
library("readr")



#----- datasets -----#
# all values are in kilotonne C02 equivalent
CH4 <- read.csv("data/UNdata_Export_20211207_170428068.csv", strip.white = TRUE)
CH4 <- data.frame(CH4, Gas = "CH4")
C02 <- read.csv("data/UNdata_Export_20211207_170450729.csv", strip.white = TRUE)
C02 <- data.frame(C02, Gas = "C02")
GHGs <- read.csv("data/UNdata_Export_20211207_170605693.csv", strip.white = TRUE)
GHGs <- data.frame(GHGs, Gas = "GHGs")
HFCs <- read.csv("data/UNdata_Export_20211207_170612620.csv", strip.white = TRUE)
HFCs <- data.frame(HFCs, Gas = "HFCs")
N20 <- read.csv("data/UNdata_Export_20211207_170630951.csv", strip.white = TRUE)
N20 <- data.frame(N20, Gas = "N20")
NF3 <- read.csv("data/UNdata_Export_20211207_170638347.csv", strip.white = TRUE)
NF3 <- data.frame(NF3, Gas = "NF3")
PFCs <- read.csv("data/UNdata_Export_20211207_170644222.csv", strip.white = TRUE)
PFCs <- data.frame(PFCs, Gas = "PFCs")
SF6 <- read.csv("data/UNdata_Export_20211207_170650224.csv", strip.white = TRUE)
SF6 <- data.frame(SF6, Gas = "SF6")

# solar and wind energy data
solar_energy <- read.csv("data/UNdata_Export_20211208_solar_energy.csv", strip.white = TRUE)
wind_energy <- read.csv("data/UNdata_Export_20211208_Wind_Energy.csv", strip.white = TRUE)

names(solar_energy)[2] <- "Electric_Type"
names(wind_energy)[2] <- "Electric_Type"

# create unified data frames
all_gas <- rbind(GHGs, CH4, C02, HFCs, N20, NF3, PFCs, SF6)
no_C02 <- rbind(CH4, HFCs, N20, NF3, PFCs, SF6)
small_gas <- rbind(SF6, PFCs, NF3)
wind_solar_energy <- rbind(solar_energy, wind_energy)
wind_solar_energy <- subset(wind_solar_energy, Year != 2019)

# ----------------------------------------------------
#####################  PART 2A #######################  
############## - Individual Analysis - ###############
# ----------------------------------------------------

# First - All Emissions from the UN since 1990
# plot all gasses
par(mar = c(1, 1, 2, 1))
ggplot(subset(all_gas, Gas != 'GHGs'), aes(x = Year, y = Value)) +
  theme_bw() +
  theme(plot.title = element_text(size=16, vjust = 1)) +
  geom_line(stat = "summary", fun = "mean", aes(group = Gas, color = Gas)) +
  scale_y_continuous(breaks = c(0,100000,200000,300000,400000,500000, 600000),
                     labels = c("0","100k","200k","300k","400k","500k","600k")) +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020)) +
  scale_color_manual(values=c('brown2', 'goldenrod1', 'chartreuse3', 'darkslategray3', 'dodgerblue3', 'darkorchid3', 'lavenderblush3')) +
  ggtitle("UN Greenhouse Gas Emissions") +
  labs(y = "killaton (C02 equivalent)",
       x = "year"
  )

# plot all but c02 / total
ggplot(no_C02, aes(x = Year, y = Value)) +
  theme_bw() +
  theme(plot.title = element_text(size=16, vjust = 1)) +
  geom_line(stat = "summary", fun = "mean", aes(group = Gas, color = Gas)) +
  scale_y_continuous(breaks = c(0,10000,20000,30000,40000,50000, 60000, 70000, 80000),
                     labels = c("0","10k","20k","30k","40k","50k","60k", "70k", "80k")) +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020)) +
  scale_color_manual(values=c('goldenrod1', 'chartreuse3', 'darkslategray3', 'dodgerblue3', 'darkorchid3', 'lavenderblush3')) +
  ggtitle("UN Greenhouse Gas Emissions - No C02") +
  labs(y = "killaton (C02 equivalent)",
       x = "year"
  )

# plot the lesser emissions independently
ggplot(small_gas, aes(x = Year, y = Value)) +
  theme_bw() +
  theme(plot.title = element_text(size=16, vjust = 1)) +
  geom_line(stat = "summary", fun = "mean", aes(group = Gas, color = Gas)) +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000),
                     labels = c("0","1k","2k","3k","4k")) +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020)) +
  scale_color_manual(values=c('dodgerblue3', 'darkorchid3', 'lavenderblush3')) +
  ggtitle("UN Greenhouse Gas Emissions - SF6, PFCs, NF3") +
  labs(y = "killaton (C02 equivalent)",
       x = "year"
  )

# plot total
ggplot(GHGs, aes(x = Year, y = Value)) +
  theme_bw() +
  theme(plot.title = element_text(size=16, vjust = 1)) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  scale_y_continuous(breaks = c(0, 500000, 525000, 550000, 575000, 600000),
                     labels = c("0","500k","525k","550k","575k","600k")) +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020)) +
  ggtitle("UN Greenhouse Gas Emissions Total") +
  labs(y = "killaton (C02 equivalent)",
       x = "year"
  )


# -------------------------------------------------------------------- #
# Now lets plot the gas types with a donut!
# Create Custom mean data frame
data <- data.frame(
  category = c("C02", "CH4", "HFCs", "N20", "NF3", "PFCs", "SF6"),
  count = c( mean(subset(C02, Year == 2018)$Value), mean(subset(CH4, Year == 2018)$Value), 
             mean(subset(HFCs, Year == 2018)$Value), mean(subset(N20, Year == 2018)$Value), 
             mean(subset(NF3, Year == 2018)$Value), mean(subset(PFCs, Year == 2018)$Value), 
             mean(subset(SF6, Year == 2018)$Value))
)

# Following percentage calculations taken from https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
# Compute percentages
data$fraction = data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)
# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Create Donut Chart
donut2018 <- ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c('brown2', 'goldenrod1', 'chartreuse3', 'darkslategray3', 'dodgerblue3', 'darkorchid3', 'lavenderblush3')) +
  ggtitle("2018") +
  labs(fill = "Gas Type") +
  xlim(c(1, 4)) +
  geom_text( x = 1.5, aes(y = 1, label = "80.7% C02"), size = 4, color = "red") +
  geom_text( x = 1.5, aes(y=.5, label = "(395,000 kilatons)"), size = 2, color = "firebrick2") + 
  theme_void() +
  theme(plot.title = element_text(size=14, vjust = -2, hjust = 0.5))

# -------------------------------------------------------------------- #
# ANOTHER DONUT, 2005
# Create Custom mean data frame
data <- data.frame(
  category = c("C02", "CH4", "HFCs", "N20", "NF3", "PFCs", "SF6"),
  count = c( mean(subset(C02, Year == 2005)$Value), mean(subset(CH4, Year == 2005)$Value), 
             mean(subset(HFCs, Year == 2005)$Value), mean(subset(N20, Year == 2005)$Value), 
             mean(subset(NF3, Year == 2005)$Value), mean(subset(PFCs, Year == 2005)$Value), 
             mean(subset(SF6, Year == 2005)$Value))
)

# Following percentage calculations taken from https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
# Compute percentages
data$fraction = data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)
# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Create Donut Chart
donut2005 <- ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c('brown2', 'goldenrod1', 'chartreuse3', 'darkslategray3', 'dodgerblue3', 'darkorchid3', 'lavenderblush3')) +
  ggtitle("2005") +
  xlim(c(1, 4)) +
  geom_text( x = 1.5, aes(y = 1, label = "82.0% C02"), size = 4, color = "red") +
  geom_text( x = 1.5, aes(y=.5, label = "(450,000 kilatons)"), size = 2, color = "firebrick2") +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=14, vjust = -2, hjust = 0.5))

# -------------------------------------------------------------------- #
# LAST DONUT, 1990
# Create Custom mean data frame
data <- data.frame(
  category = c("C02", "CH4", "HFCs", "N20", "NF3", "PFCs", "SF6"),
  count = c( mean(subset(C02, Year == 1990)$Value), mean(subset(CH4, Year == 1990)$Value), 
             mean(subset(HFCs, Year == 1990)$Value), mean(subset(N20, Year == 1990)$Value), 
             mean(subset(NF3, Year == 1990)$Value), mean(subset(PFCs, Year == 1990)$Value), 
             mean(subset(SF6, Year == 1990)$Value))
)

# Following percentage calculations taken from https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
# Compute percentages
data$fraction = data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)
# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Create Donut Chart
donut1990 <- ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c('brown2', 'goldenrod1', 'chartreuse3', 'darkslategray3', 'dodgerblue3', 'darkorchid3', 'lavenderblush3')) +
  ggtitle("1990") +
  xlim(c(1, 4)) +
  labs(fill = "Gas Type") +
  geom_text( x = 1.5, aes(y = 1, label = "78.6% C02"), size = 4, color = "red") +
  geom_text( x = 1.5, aes(y=.5, label = "(454,000 kilatons)"), size = 2, color = "firebrick2") +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=14, vjust = -2, hjust = 0.5))
# -------------------------------------------------------------------- #

# Place all the donuts into 1 visual
all_donuts <- ggarrange(donut1990, donut2005, donut2018,
          ncol = 3, nrow = 1,
          common.legend = TRUE, legend="bottom")

annotate_figure(all_donuts, top = text_grob("Greenhouse Gas Emissions in The UN", 
                                      color = "black", face = "bold", size = 14))


# -------------------------------------------------------------------- #
# Let's create a bubble plot by country. Let's choose a year as well (2005)
# Create subset of GHG, just from 2005
data <- subset(GHGs, Year == 2005)
#data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20))

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data$Value, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot() + 
  
  # Bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, size = Value, label = Country.or.Area)) +
  scale_size_continuous(range = c(1,4)) +
  ggtitle("Greenhouse Gas Emissions by Country from 2005") +
  theme_void() + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size=15, vjust = 1, hjust = 0.5)) +
  coord_equal()


# -------------------------------------------------------------------- #
# Now to look at solar and wind power generated.
# We will use this data to create a mixed model against the greenhouse 
#   gasses. We could measure either the gas or the energy as the DV.
par(mar = c(1, 1, 1, 1))
ggplot(wind_solar_energy, aes(x = Year, y = Quantity)) +
  theme_bw() +
  theme(plot.title = element_text(size=16, vjust = 1)) +
  geom_line(stat = "summary", fun = "sum", aes(group = Electric_Type, color = Electric_Type)) +
  
  scale_y_continuous(breaks = c(0,200000,400000, 600000, 800000, 1000000, 1200000, 1400000),
                     labels = c("0","200k","400k","600k","800k","1 million","1.2 million","1.4mil")) +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2018)) +
  
  ggtitle("Solar and Wind Energy Production in the UN per year") +
  scale_color_discrete("") +
  labs(y = "killawat hours (millions)",
       x = "year"
  )


# ----------------------------------------------------
#####################  PART 2B #######################  
############## - Mixed Model Analysis - ##############
# ----------------------------------------------------

# First, we need to make a data frame consisting of both energy output and 
#   gas emissions. To do this without having to worry about countries, we'll 
#   simply take the total of all countries for any given year.
# -------------------------------------------------------------------- #

# create empty data frame
data <- data.frame(Year = double(), 
                   GHGs = double(), 
                   solar = double(), 
                   wind = double())



# loop for each year to create sum
year_list = c(1990:2018)
for (i in year_list){
  # Year, GHGs emissions, solar production, wind production
  data[nrow(data) + 1,] = c(i, 
                            sum(subset(GHGs, Year == i)$Value), 
                            sum(subset(solar_energy, Year == i)$Quantity), 
                            sum(subset(wind_energy, Year == i)$Quantity))
}


# First comparison: Solar to GHGs linear
#fit linear mixed model using GHGs as response (DV)
linear_solar <- lm(GHGs ~ solar, data = data)
summary(linear_solar)

linear_wind <- lm(GHGs ~ wind, data = data)
summary(linear_wind)

linear_combined <- lm (GHGs ~ solar + wind, data = data)
summary(linear_combined)

# look at each residuals to decide if its significant
plot(linear_solar)
plot(linear_wind)
plot(linear_combined)

# we can do better. Let's use a polynomial base 4.
poly_wind <- lm(GHGs ~ poly(wind, 4), data = data)
summary(poly_wind)

poly_solar <- lm(GHGs ~ poly(solar, 4), data = data)
summary(poly_wind)

poly_combined <- lm(GHGs ~ poly(solar, 4) + poly(wind, 4), data = data)
summary(poly_combined)


# -------------------------------------------------------------------- #
# now to plot some of the data in an effective manner!
# first - wind
plot(data$wind, data$GHGs, main='Linear and Poly fits - Wind to GHGs', 
     xlab='Energy (killawat hours - millions)', ylab='Greenhouse Gas (killatons)')
points(data$wind, predict(poly_wind), type="l", col="purple", lwd=2)
points(data$wind, predict(linear_wind), type="l", col="red", lwd=2)

# second - solar
plot(data$solar, data$GHGs, main='Linear and Poly fits - Solar to GHGs', 
     xlab='Energy (killawat hours - millions)', ylab='Greenhouse Gas (killatons)')
points(data$solar, predict(poly_solar), type="l", col="purple", lwd=2)
points(data$solar, predict(linear_solar), type="l", col="red", lwd=2)

# finally, combined
# second - solar
plot(data$solar + data$wind, data$GHGs, main='Linear and Poly fits - Solar and Wind to GHGs', 
     xlab='Energy (killawat hours - millions)', ylab='Greenhouse Gas (killatons)')
points(data$solar + data$wind, predict(poly_combined), type="l", col="purple", lwd=2)
points(data$solar + data$wind, predict(linear_combined), type="l", col="red", lwd=2)
