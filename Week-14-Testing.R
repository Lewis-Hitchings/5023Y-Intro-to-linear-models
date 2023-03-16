# Week 14 Testing

#____________________________ ----

# Source Week 13 Script ----

source("Week-13-Intro-to-linear-models.R")

#____________________________ ----

# Student's t-test

lm(y ~ 1)

lm (height~1)

lsmodel1 <- lm(height ~ type, data = darwin) # Run a linear model 

summary(lsmodel1) # Create summary statistics of model. Difference in plant heigh was 2.62 +- 1.07 inches. 


tidy_model1 <- broom::tidy(lsmodel1) # Tidy the information  

tidy_model1[[2,2]] / tidy_model1[[2,3]] # Divide the difference in height by the standard error. 

# The observed value of t is (-2.62 / 1.07 = -2.437113)

# report estimate, P value, and T value 

#____________________________ ----

# Paired t-test ----

# Add the factor for pairs (factor (pair)) to the linear model

lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)

summary(lsmodel_darwin)

# Row 2: Comparison of mean heights of crossed and selfed plants in the same pairs.
# Row 3 - 16: Average difference of each pair against 1 pair. 

lm(height ~ type + factor(pair), data = darwin) %>%  # Generate confidence intervals and show the first two rows
  broom::tidy(., conf.int=T) %>%
  slice(1:2) 


# Create figure to show confidence intervals between the paired and unpaired models... 

m1 <- lm(height ~ type, data = darwin) %>% # Unpaired linear model
  broom::tidy(., conf.int=T) %>%
  slice(2:2) %>%
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% # Paired linear model
  broom::tidy(., conf.int=T) %>%
  slice(2:2) %>%
  mutate(model="paired")

rbind(m1,m2) %>% # Combine the results from the two linear models and greate and plot the confidence intervals.
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  theme_minimal()+
  coord_flip()


#____________________________ ----

# Repeatability

# Run 20 sampling experiments, then calculating the estimated mean diffrence for each of them.  

set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) {
  
  x <- rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>%
    broom::tidy(conf.int=T)
  y <- rbind(y,temp)
  

}

y$'experiment number' <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments


y %>%
  mutate(ifelse()) %>%
  group_by(p.value < 0.049) %>%
  summrise()
  
  ---------
  
  



