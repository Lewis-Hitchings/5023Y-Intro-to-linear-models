# Week-12-Intro-to-statistics


#____________________________ ----

# Import Packages ----

library(tidyverse)
library(here)
library(kableExtra)

#____________________________ ----

# Import Data ----

darwin <- read_csv(here("Data", "darwin.csv"))

#____________________________ ----

# Check Data

glimpse(darwin)

head(darwin)

colnames(darwin)

darwin %>% 
  duplicated() %>% 
  sum()

darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

darwin %>% 
  is.na() %>% 
  sum()

summary(darwin)


#____________________________ ----

# Clean Names

darwin <- janitor::clean_names(darwin)


#____________________________

# Visualuse Data

darwin %>% # Create point graph
  ggplot(aes(x=type,
             y=height))+
  geom_point()

darwin %>% # Create boxplot graph
  ggplot(aes(x=type,
             y=height))+
  geom_boxplot()

darwin %>% # Create violin graph
  ggplot(aes(x=type,
             y=height))+
  geom_violin()

darwin %>% # Create histogram for heights
  ggplot(aes(x=height))+
  geom_histogram(bins=15)


#____________________________

# Compare Groups

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))


#____________________________

# Make Plot to Compare Groups

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# create table of summary statistics
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


#____________________________ ----

# Estimate Differences Between Mean Heights Of Groups ----

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% # Moves each pair to the same row, then select the values from type and height
  mutate(difference = Cross - Self) # Subtract the height from cross from the height from self

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary %>% 
  mutate(se= sd/sqrt(n)) # Use the standard error equation 

# The mean difference in height between the Self and Cross plants was 2.62 +-1.22 inches.
#____________________________ ----

# Confidence Intervals

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI



