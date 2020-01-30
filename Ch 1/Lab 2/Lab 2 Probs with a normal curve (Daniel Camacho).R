##################################################
#
# Plotting Probability Distributions and
# Calculating Probabilities with an Normal Curve
#
# Student name(s): Daniel Camacho
#
############## Setup ##############
# We will use tibbles, pipes, and ggplot
require(tidyverse)

######## Excercices - Part A ####################

######## Human Gestations #######################
#   Question (1): 
#   The length of human gestation has a normal 
#   distribution with a mean of 266 days and a standard 
#   deviation of 16 days.  What proportion of pregnancies
#   will last between 240 and 270 days?

# Set the parameters for the distribution in days
mu <- 266      # mean
sigma <- 16    # standard deviation
lower <- 240
upper <- 270

# Generate 100 x-values between mu +/- 4sigma 
# These will be used to generate points on the 
# normal curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(mu - 4*sigma,
                       mu + 4*sigma,
                       length = 100))

# plot the normal distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dnorm"  ("fun" = function)
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dnorm,                # shade the region
                args = list(mean = mu, 
                            sd = sigma),    # parameters for "dnorm" 
                xlim = c(lower,upper),
                geom = "area",
                fill = "lightblue")  +
  stat_function(fun = dnorm,                # draw the curve
                args = list(mean = mu,
                            sd = sigma)) +  # parameters for "dnorm"
  xlab("Days") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P(240 < x < 270) = P(z_lower < z < z_upper)
# That is, find the area shaded in blue
# For this we use the pnorm function
area <- pnorm(upper, mean = mu, sd = sigma) - 
  pnorm(lower, mean = mu, sd = sigma)

# add probability to the plot formatted properly
result <- paste("P(",lower,"< Days <",upper,") =",
                signif(area, digits=3))

myPlot + ggtitle("Normal Distribution: Human Gestation",
                 subtitle = result) 

##################################################

######### Test Scores ############################
#   Question (2): 
#   Entrance to a certain University is determined 
#   by a national test.  The scores on this test are 
#   normally distributed with mean 500 and standard   
#   deviation 100.  What is the probability that a  
#   student makes less than 585 on the exam?

# Set the parameters for the distribution of scores
mu <- 500      # mean
sigma <- 100   # standard deviation
lower <- 0     # There is a change student could get 0 score
upper <- 585

# Generate 100 x-values between mu +/- 4sigma 
# These will be used to generate points on the 
# normal curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(mu - 4*sigma,
                       mu + 4*sigma,
                       length = 100))

# plot the normal distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dnorm"  ("fun" = function)
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dnorm,                # shade the region
                args = list(mean = mu, 
                            sd = sigma),    # parameters for "dnorm" 
                xlim = c(lower,upper),
                geom = "area",
                fill = "lightblue")  +
  stat_function(fun = dnorm,                # draw the curve
                args = list(mean = mu,
                            sd = sigma)) +  # parameters for "dnorm"
  xlab(" Test Scores") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P(0 < x < 585) = P(z_lower < z < z_upper)
# That is, find the area shaded in blue
# For this we use the pnorm function
area <- pnorm(upper, mean = mu, sd = sigma)

# add probability to the plot formatted properly
result <- paste("P(",lower,"< Test Scores <",upper,") =",
                signif(area, digits=3))

myPlot + ggtitle("Normal Distribution: Test Scores",
                 subtitle = result) 

##################################################

######### Random Variable ########################
#   Question (3): 
#   If the mean of a random variable with a 
#   normal distribution is 81.1 and the standard 
#   deviation is 4.7, then find the probability 
#   of randomly selecting a value more than 83?

# Set the parameters for the distribution of scores
mu <- 81.1      # mean
sigma <- 4.7    # standard deviation
lower <- 83
upper <-  mu + 4*sigma # We would like to stop graph somewhere

# Generate 100 x-values between mu +/- 4sigma 
# These will be used to generate points on the 
# normal curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(mu - 4*sigma,
                       mu + 4*sigma,
                       length = 100))

# plot the normal distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dnorm"  ("fun" = function)
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dnorm,                # shade the region
                args = list(mean = mu, 
                            sd = sigma),    # parameters for "dnorm" 
                xlim = c(lower,upper),
                geom = "area",
                fill = "lightblue")  +
  stat_function(fun = dnorm,                # draw the curve
                args = list(mean = mu,
                            sd = sigma)) +  # parameters for "dnorm"
  xlab("Value") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P(83 < x < upper) = P(z_lower < z < z_upper)
# That is, find the area shaded in blue
# For this we use the pnorm function
area <- pnorm(upper, mean = mu, sd = sigma) - 
  pnorm(lower, mean = mu, sd = sigma)

# add probability to the plot formatted properly
result <- paste("P(Value >",lower,") =",
                signif(area, digits=3))

myPlot + ggtitle("Normal Distribution: Value of Random Variable",
                 subtitle = result) 

