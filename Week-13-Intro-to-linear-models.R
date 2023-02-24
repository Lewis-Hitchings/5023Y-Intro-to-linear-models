# Week-13-Intro-to-linear-models

#____________________________ ----

# Source Week 12 Script ----

source("Week-12-Intro-to-statistics.R")

#____________________________ ----

# Import Additional Packages ----

library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

#____________________________ ----

# Least-Squares Linear Model ----

lsmodel0 <- # Conduct a Least-Squares Linear Model and assign the results to lsmodel0
  darwin %>%
  lm(height ~ 1, data=.) # As functions outside fo the tidyverse family are being used, a "." is used to represent the data in the pipe

# lm = formula
# height ~ 1 = returns only the estimate of the intercept



summary(lsmodel0) # Summaries the lease square model


broom::tidy(lsmodel0) # Summaries module components 

broom::glance(lsmodel0) # Summaries entire model

broom::augment(lsmodel0) # Summaries individual observations


#____________________________ ----

# Compare Means ----


mean(darwin$height) # Calculate mean height


lsmodel1 <- lm(height ~ type, data = darwin) # Perform a linear model for the height and type


broom::tidy(lsmodel1) # Tidy the linear model. Intercept represents Cross, and typeSelf represents the difference in mean for type Self with type Cross


summary(lsmodel1) # Summarise lsmodel


darwin %>% # Produce a graph showing the differences in height between the Cross and Self plants
  ggplot(aes(x=type,
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()


#____________________________ ----

# Confidence Intervals ----

confint(lsmodel1) # Outputs the 95% confidence intervals 

# Or, for Tidyverse...

broom::tidy(lsmodel1, conf.int=T) # As previous including the Estimate, Standard error, Standard error, and p value

GGally::ggcoef_model(lsmodel1, # Plots the confidence intervals for Self plants using Cross plants as the estimate.
                     show_p_values=FALSE, 
                     conf.level=0.95)

# As the confidence interval line does not cross 0, we ca see there is a significant difference between the mean heights of each group.


## Emmeans ----

# The package emmeans contains a function "emmeans()" that also performs a similar thing to "broom::tidy..."

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

means %>% # Produces a graph showing the means for the two groups and the confidene intervals aroud them.
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

# The overlapping confidence intervals overlap meaning we cannot use this graph to show a difference in mean height between the two groups.  


#____________________________ ----

# Assumption Checking ----

# It is necessary now that the linear model analyis has been conducted that we check that the assumptions we made are met. 

performance::check_model(lsmodel1) # Produces lots of graphs to check the assupions.


## Checking for... ----

### Normal Distribution ----

performance::check_model(lsmodel1, check=c("normality","qq")) # Produce a normality and a qq plot 

# or ...

plot(lsmodel1, which=c(2,2)) # plots a very basic QQ plot 

# The dots do not fall along the line at each end 


### Equality of Variance ----

# Specifically, equality of residual/unexplained variance between the two groups.

performance::check_model(lsmodel1, check="homogeneity") # Produces a plot of the standardized residuals (standardized residuals = residual error / standard deviation)

plot(lsmodel1, which=c(1,3))


### Outliers ----

performance::check_model(lsmodel1, check="outliers") # Plots the direction and distance of outliers 

plot(lsmodel1, which=c(4,4)) # Plots the distance (measured with Cooks Distnace) of outliers and their position within the data 

# Cooks distance shows how much imapct a datapoint has on the model (leverage) making it a useful way of looking at how outliers imapct the dataset


#____________________________ ----

# Summary ----

# So far the analysis has been done essentialy using a Student's t test rather than a Paired t-test. This ignores the experimental design having paired samples. 

darwin %>% # plot a graph showing the heights of the two groups, a line for the mean, and a dotted line the difference between the two
  ggplot(aes(x=type,
             y=height))+
  geom_jitter(width=0.1,
              pch=21,
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-1.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

