# install packages
install.packages("tidyverse")
library(tidyverse)
install.packages("GGally")
library(GGally)
install.packages("reshape2")
library(reshape2)

# Import the data and look at the first few rows.
CovidData <- read.csv(file = '/Users/Marie/Desktop/R_visualisations_week6/country_wise_latest.csv')
head(CovidData)
summary(CovidData)
class(CovidData)
colnames(CovidData)

# Removing columns I am not interested in, or it becomes too complex
myCovidData <- CovidData[, c("Confirmed", "Deaths", "Recovered", "Active", "WHO.Region")]
summary(myCovidData)
head(myCovidData)

# summary of everything (let's see if those are even worth plotting...)
ggpairs(myCovidData)


# SPLITTING BY REGION AND OUTCOME

Covid_to_plot = myCovidData %>%
  group_by(WHO.Region) %>% 
  summarise(num = n(),
            totalConfirmed = sum(Confirmed),
            Active = sum(Active),
            Recovered = sum(Recovered),
            Death = sum(Deaths)
  )
Covid_to_plot

# plot overview
ggpairs(df_to_plot)


# LET'S GET TO PLOTTING

# simple plot of confirmed cases by region
ggplot(Covid_to_plot, aes(x=as.character(WHO.Region), y=totalConfirmed)) + geom_bar(stat='identity')

# remove the total_confirmed column
Covid_to_plot$totalConfirmed <- NULL

# tibble from wide to long format
Covid_to_plot_long = melt(Covid_to_plot, id.vars=c("WHO.Region"))
Covid_to_plot_long
Covid_to_plot_long = Covid_to_plot_long[-c(1, 2, 3, 4, 5, 6), ]


# MINIMAL WORKING EXAMPLE
# Stacked + percent
ggplot(Covid_to_plot_long, aes(fill=variable, y=value, x=as.character(WHO.Region))) + 
  geom_bar(position="fill", stat="identity")

# change names of axes
ggplot(Covid_to_plot_long, aes(fill=variable, y=value, x=as.character(WHO.Region))) + 
  geom_bar(position="fill", stat="identity") +
  xlab("WHO Region") +
  ylab("% of cases")

# change name of legend
ggplot(Covid_to_plot_long, aes(fill=variable, y=value, x=as.character(WHO.Region))) + 
  geom_bar(position="fill", stat="identity") +
  xlab("WHO Region") +
  ylab("% of cases") +
  labs(x = "", fill = "Outcome")

# change theme
ggplot(Covid_to_plot_long, aes(fill=variable, y=value, x=as.character(WHO.Region))) + 
  geom_bar(position="fill", stat="identity") +
  xlab("WHO Region") +
  ylab("% of cases") +
  labs(x = "", fill = "Outcome") +
  theme_bw()
  
# change colors of bars
ggplot(Covid_to_plot_long, aes(fill=variable, y=value, x=as.character(WHO.Region))) + 
  geom_bar(position="fill", stat="identity") +
  xlab("WHO Region") +
  ylab("% of cases") +
  labs(x = "", fill = "Outcome") +
  theme_bw() +
  scale_fill_manual(values = c(" lightskyblue4", "aquamarine3", "snow4"))


# x-axis labels at 45 degrees
ggplot(Covid_to_plot_long, aes(fill=variable, y=value, x=as.character(WHO.Region))) + 
  geom_bar(position="fill", stat="identity") +
  xlab("WHO Region") +
  ylab("% of cases") +
  labs(x = "", fill = "Outcome") +
  theme_bw() +
  scale_fill_manual(values = c(" lightskyblue4", "aquamarine3", "snow4")) +
  theme(axis.text.x=element_text(angle=45, hjust=1))
