# install packages
install.packages("tidyverse")
library(tidyverse)
install.packages("GGally")
library(GGally)
install.packages("reshape2")
library(reshape2)

# Import the data and look at the first few roads.
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



# SPLITTING BY OUTCOME

# 1. Calculate outcome by region

# Function to make inclementing easier
inc <- function(x, y)
{
  #eval.parent(substitute(x <- x + y))
  x = x + y
  return(x)
}

# unique regions
unique_regions = unique(myCovidData[c("WHO.Region")])
class(unique_regions)
unique_regions
regions = as.vector(unlist(unique_regions$WHO.Region))
class(regions)
regions

# confirmed cases by region - confirmed_by_region vector
confirmed_EasternMediterranean = 0
confirmed_Europe = 0
confirmed_Africa = 0
confirmed_Americas = 0
confirmed_WesternPacfic = 0
confirmed_SouthEastAsia = 0

myCovidData

for (item in myCovidData)
{
  region = myCovidData$WHO.Region
  print(region)
  ifelse(region == 'Eastern Mediterranean', (confirmed_EasternMediterranean = confirmed_EasternMediterranean + myCovidData$Confirmed),
         ifelse(region == 'Europe', (confirmed_Europe = confirmed_Europe + myCovidData$Confirmed),
                ifelse(region == 'Africa', (confirmed_Africa = confirmed_Africa + myCovidData$Confirmed),
                       ifelse(region == 'Americas', (confirmed_Americas = confirmed_Americas + myCovidData$Confirmed),
                              ifelse(region == 'Western Pacific', (confirmed_WesternPacfic = confirmed_WesternPacfic + myCovidData$Confirmed),
                                     (confirmed_SouthEastAsia = confirmed_SouthEastAsia + myCovidData$Confirmed))))))
}
confirmed_EasternMediterranean
myCovidData$Confirmed['Europe']



confirmed_by_region <- c(confirmed_EasternMediterranean, confirmed_Europe, confirmed_Africa, confirmed_Americas, confirmed_WesternPacfic, confirmed_SouthEastAsia)
confirmed_by_region

# active cases by region - active_by_region vector
active_EasternMediterranean = 0
active_Europe = 0
active_Africa = 0
active_Americas = 0
active_WesternPacfic = 0
active_SouthEastAsia = 0

for (region in myCovidData$WHO.Region)
{
  ifelse(region == 'Eastern Mediterranean', inc(active_EasternMediterranean, myCovidData$Active),
         ifelse(region == 'Europe', inc(active_Europe, myCovidData$Active),
                ifelse(region == 'Africa', inc(active_Africa, myCovidData$Active),
                       ifelse(region == 'Americas', inc(active_Americas, myCovidData$Active),
                              ifelse(region == 'Western Pacific', inc(active_WesternPacfic, myCovidData$Active),
                                     inc(active_SouthEastAsia, myCovidData$Active))))))
}

active_by_region <- c(active_EasternMediterranean, active_Europe, active_Africa, active_Americas, active_WesternPacfic, active_SouthEastAsia)


# recovered cases by region - recovered_by_region vector
recovered_EasternMediterranean = 0
recovered_Europe = 0
recovered_Africa = 0
recovered_Americas = 0
recovered_WesternPacfic = 0
recovered_SouthEastAsia = 0

for (region in myCovidData$WHO.Region)
{
  ifelse(region == 'Eastern Mediterranean', inc(recovered_EasternMediterranean, myCovidData$Recovered),
         ifelse(region == 'Europe', inc(recovered_Europe, myCovidData$Recovered),
                ifelse(region == 'Africa', inc(recovered_Africa, myCovidData$Recovered),
                       ifelse(region == 'Americas', inc(recovered_Americas, myCovidData$Recovered),
                              ifelse(region == 'Western Pacific', inc(recovered_WesternPacfic, myCovidData$Recovered),
                                     inc(recovered_SouthEastAsia, myCovidData$Recovered))))))
}

recovered_by_region <- c(recovered_EasternMediterranean, recovered_Europe, recovered_Africa, recovered_Americas, recovered_WesternPacfic, recovered_SouthEastAsia)


# death cases by region - death_by_region vector
death_EasternMediterranean = 0
death_Europe = 0
death_Africa = 0
death_Americas = 0
death_WesternPacfic = 0
death_SouthEastAsia = 0

for (region in myCovidData$WHO.Region)
{
  ifelse(region == 'Eastern Mediterranean', inc(death_EasternMediterranean, myCovidData$Deaths),
         ifelse(region == 'Europe', inc(death_Europe, myCovidData$Deaths),
                ifelse(region == 'Africa', inc(death_Africa, myCovidData$Deaths),
                       ifelse(region == 'Americas', inc(death_Americas, myCovidData$Deaths),
                              ifelse(region == 'Western Pacific', inc(death_WesternPacfic, myCovidData$Deaths),
                                     inc(death_SouthEastAsia, myCovidData$Deaths))))))
}

death_by_region <- c(death_EasternMediterranean, death_Europe, death_Africa, death_Americas, death_WesternPacfic, death_SouthEastAsia)


# Put it all together in one dataframe
df_to_plot <- data.frame(regions, confirmed_by_region, active_by_region, recovered_by_region, death_by_region)
head(df_to_plot)
summary(df_to_plot)

# plot overview
ggpairs(df_to_plot)


# LET'S GET TO PLOTTING

# simple plot of confirmed cases by region
ggplot(df_to_plot, aes(x=as.character(regions), y=confirmed_by_region)) + geom_bar(stat='identity')

# formatting the data from wide to long format for stacking
df_to_plot_long = melt(df_to_plot)


#outcome <- rep(c("Active" , "Recovered" , "Death") , 6)









