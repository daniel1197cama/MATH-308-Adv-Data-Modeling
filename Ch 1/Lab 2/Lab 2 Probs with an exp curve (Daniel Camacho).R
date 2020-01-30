##################################################
#
# Plotting Probability Distributions and
# Calculating Probabilities with an Exponential Curve
#
# Student name(s): Daniel Camacho
#
############## Setup ##############
# We will use tibbles, pipes, and ggplot
require(tidyverse)


######## Excercices - Part B ####################

######## Computer Life ##########################
#   Question (4): 
#   a) On the average, a certain computer part 
#   lasts ten years. The length of time the computer 
#   part lasts is exponentially distributed. What is the
#   probability that the part will last between 7 and 11
#   years?
#   b) Eighty percent of these computer parts last 
#   at most how long?  (Hint: what R function is the 
#   inverse of pexp?)

# Set the parameters for the distribution
lambda <- 10      # lambda = average "lifetime"
rate <- 1/lambda     # parameter for "dexp" is rate = 1/lambda
lower <- 7
upper <- 11        # We have to stop the graph somewhere
percentile <- 80  

# Generate 100 x-values between 0 and 4*lambda
# These will be used to generate points on the 
# exponential curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(0, 4*lambda,length = 100))

# plot the exponential distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dexp" 
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dexp,                   
                args = list(rate = rate),     # parameters for "dexp"   
                xlim = c(lower,upper),
                geom = "area",                # shade the region
                fill = "lightgreen")  +
  stat_function(fun = dexp,                   # draw the curve
                args = list(rate = rate)) +   # parameters for "dexp" 
  xlab("Life of Computer (Years)") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P( x < upper) 
# That is, find the area shaded in green
# For this we use the pexp function
# Note that 
area <- pexp(upper, rate = 1/lambda) - pexp(lower, rate = 1/lambda)
time <- -plnorm(percentile, rate, lower = FALSE, log = TRUE)

# add probability to the plot formatted properly
result <- paste("P(",lower," < Life of Computer <",upper,") =",
                signif(area, digits=3))

# Eighty percent of these computer parts last almost 11.6 years
result2 <- paste("80th Percentile:", "Almost", 
                signif(time, digits=3), "years")


myPlot + labs(title = paste("Exponential distribution with lambda = ",
                       lambda),
                 subtitle = result, 
                 caption = result2) + theme(
                   plot.caption = element_text(hjust = 0, size = 12, face = "italic") # move caption to the left
                 )

##################################################

########## Chi-Square Random Variable########################################
#   Question (5): 
#   What is the probability that a chi-square random 
#   variable with 10 degrees of freedom is greater 
#   than 15.99?  (This curve can be graphed on the
#   interval [0,25].)

# Set the parameters for the distribution
dfchi <- 10     
lower <- 15.99
upper <- 25    

# Generate 100 x-values between 0 and 4*lambda
# These will be used to generate points on the 
# exponential curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(0, upper,length = 100))

# plot the chi-aquare distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dexp" 
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dchisq,                   
                xlim = c(lower,upper),
                args = list(df = dfchi),
                geom = "area",                # shade the region
                fill = "lightgreen")  +
  stat_function(fun = dchisq,                   # draw the curve
                args = list(df = dfchi)) +   # parameters for "dexp" 
  xlab("Values") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P( x < upper) 
# That is, find the area shaded in green
# For this we use the pexp function
# Note that 
area <- 1 - pchisq(lower, df)

# add probability to the plot formatted properly
result <- paste("P(Value >",lower,") =",
                signif(area, digits=3))

myPlot + ggtitle(paste("Chi-Square Distribution with df  = ",
                       dfchi),
                 subtitle = result) 

##################################################

######### F- Statistics Distribution ################
#   Question (6): 
#   Find the probability that an F-statistics is
#   less than 0.595 if the degrees of freedom are 
#   df = 11 and df = 6.  (This curve can be graphed on the
#   interval [0,5].)

# Set the parameters for the distribution
dfstat1 <- 11
dfstat2 <- 6
lower <- 0
upper <- 0.595    # We have to stop the graph somewhere 

# Generate 100 x-values between 0 and 4*lambda
# These will be used to generate points on the 
# exponential curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(0, 5,length = 100))

# plot the exponential distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dexp" 
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = df,                   
                args = list(df1 = dfstat1, df2 = dfstat2),     # parameters for "df"   
                xlim = c(lower,upper),
                geom = "area",                # shade the region
                fill = "lightgreen")  +
  stat_function(fun = df,                   # draw the curve
                args = list(df1 = dfstat1, df2 = dfstat2)) +   # parameters for "df" 
  xlab("Value") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P( x < upper) 
# That is, find the area shaded in green
# For this we use the pexp function
# Note that 
area <- pf(upper, df1 = dfstat1, df2 = dfstat2)

# add probability to the plot formatted properly
result <- paste("P(Value <", upper,") =",
                signif(area, digits=3))

myPlot + ggtitle(paste("F Distribution with df1 = ",
                       dfstat1,"and df2 =", dfstat2),
                 subtitle = result)
