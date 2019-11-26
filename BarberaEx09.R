
setwd("~/Desktop/Biocomputing_Junk/IBC_Exercise_09/")
library(tidyverse)
library(ggplot2)

# My data is daily Culiocoides collection counts  
# from years 1974 - 2012 
# from Preston site of Rothamsted Insect Survey
preston <- read.csv("preston.csv", header = T)

# I want to show the total number of midges 
# collected per year
whadupmidge = preston %>%
  group_by(Calendar.Year) %>%
  summarize(yearly_abundance = sum(Total.Cul.Caught))

# And now to plot
a = ggplot(data = whadupmidge, aes(x = Calendar.Year, y = yearly_abundance))
a + geom_point() + xlab("Year") + ylab("Abundance") + 
  ggtitle("Yearly Culicoides Abundance") +  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method = "lm", col = "turquoise4", fill = "tomato3") 
 

# Part 2
data <- read.csv("data.txt", header = T)

# I want to calculate the mean of each region
datasort = data %>%
  group_by(region) %>%
  summarize(mean = mean(as.numeric(observations)))

# For a bar plot of the means..
b = ggplot(data = datasort, aes(x = region, y = mean, fill = region)) 
b + xlab("Region") + ylab("Mean") +
  ggtitle("Population Means") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(data = datasort, stat="identity") +
  coord_cartesian(ylim=c(14,15.5)) + 
  scale_fill_manual(values = c("palevioletred4","tomato2","orange1","slateblue4"))
  
  
# And for the scatter plot of all observations
c = ggplot(data = data, aes(x = region, y = observations, color = region))
c + geom_jitter() + xlab("Region") + ylab("Mean") +
  scale_color_manual(values = c("slateblue4","palevioletred4","tomato2","orange1"))
  
# The bar plot and the scatter plot do give us different info
# The bar plot only shows means
# Which are pretty similar between the regions
# But the scatter plot shows all the observations
# And how they are distributed 
# The end 



            