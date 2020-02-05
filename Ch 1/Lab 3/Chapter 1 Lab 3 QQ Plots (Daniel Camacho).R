##### Chapter 1 Lab 3 --------------
#
# Creating QQ plots to determine data distribution
#
# Student name(s): Daniel Camacho
#
########### Directions ===============
#
# (1) read through and run the code to
# understand what it does.  Here we are plotting
# QQ plots "by hand" instead of using the black
# box commands of ggplot to plot them.
# (2) copy the code and revise it to plot 
# the QQ plot for a uniform distribution and 
# a chi-square (df = 3) distribution in addition to 
# the normal distribution.  Nota bene:  Only copy
# the code needed and change all comments to 
# reflect the distribution being tested
# (3) Add titles, captions, labels, etc., to the 
# plots
# (4) Use your code to visually determine the 
# best estimate # of the distributions in the 
# data you are given.
##### Answers =====================
#
# Choose from Normal, Chi-square, or Uniform

#  Dist1 is the UNIFORM distribution
#  Dist2 is the NONE distribution
#  Dist3 is the CHI-SQUARE distribution
#  Dist4 is the NORMAL distribution
#
############## Setup --------------
# We will use tibbles, pipes, and ggplot
require(tidyverse)

#  "percents" is a list 
# of the quantiles we will use in the QQ plots
percents <- seq(from = 0, to = 1, by = 0.01)

qqnormline <- function(mydata){
  # this function takes a vector and returns the slope
  # and the intercept of a line through the point of the QQ plot
  # at the 25th-percentile and the 75th-percentile
  # using a normal distribution for the comparison
  pt1 <- c(qnorm(0.25), quantile(mydata, 0.25))
  pt2 <- c(qnorm(0.75), quantile(mydata, 0.75)) 
  m <- (pt1[[2]] - pt2[[2]])/(pt1[[1]] - pt2[[1]])
  b <- -pt1[[1]]*m + pt1[[2]]
  
  # To use the output, the slope is in element [[1]] and
  # the y-intercept is in element [[2]]
  return(c(m, b))
}

qqunifline <- function(mydata){
  # this function takes a vector and returns the slope
  # and the intercept of a line through the point of the QQ plot
  # at the 25th-percentile and the 75th-percentile
  # using a uniform distribution for the comparison
  pt1 <- c(qunif(0.25), quantile(mydata, 0.25))
  pt2 <- c(qunif(0.75), quantile(mydata, 0.75)) 
  m <- (pt1[[2]] - pt2[[2]])/(pt1[[1]] - pt2[[1]])
  b <- -pt1[[1]]*m + pt1[[2]]
  
  # To use the output, the slope is in element [[1]] and
  # the y-intercept is in element [[2]]
  return(c(m, b))
}

qqchisqline <- function(mydata){
  # this function takes a vector and returns the slope
  # and the intercept of a line through the point of the QQ plot
  # at the 25th-percentile and the 75th-percentile
  # using a chi-square distribution for the comparison
  pt1 <- c(qchisq(0.25, df = 3), quantile(mydata, 0.25))
  pt2 <- c(qchisq(0.75, df = 3), quantile(mydata, 0.75)) 
  m <- (pt1[[2]] - pt2[[2]])/(pt1[[1]] - pt2[[1]])
  b <- -pt1[[1]]*m + pt1[[2]]
  
  # To use the output, the slope is in element [[1]] and
  # the y-intercept is in element [[2]]
  return(c(m, b))
}

####### The Data ###############
# Read in the assignment data, look at its structure,
# and pick one of the columns to test by storing it in "x"
myDists <- read_csv("Ch 1/Lab 3/ISLR Chapter 1 Lab 3 QQ Plot Data.csv")

str(myDists)

########### Example 1: dist1 ##########
# Strip off the first column of myDists
x1 <- myDists$Dist1

######## Compare to normal###

# Create a tibble with three columns: 
#    (1) the percentages for each quantile
#    (2) quantiles of the normal distribution and
#    (3) quantiles of the data in question
mytib <- tibble(percents,
                normal = qnorm(percents),
                uniform = qunif(percents),
                chisquared = qchisq(percents, df = 3),
                mydist = quantile(x1, percents))

# Calculate the line through the 25th and 75th
# quantile points on the QQ plot
linenorm <- qqnormline(x1)
lineunif <- qqunifline(x1)
linechisq <- qqchisqline(x1)

# The QQ plot for normal - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = normal, y = mydist)) +
  geom_abline(slope = linenorm[[1]], intercept = linenorm[[2]]) + 
  ggtitle("Normal Distribution Comparison with Dist1")
                                                                         

# The QQ plot for uniform - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = uniform, y = mydist)) +
  geom_abline(slope = lineunif[[1]], intercept = lineunif[[2]])+ 
  ggtitle("Uniform Distribution Comparison with Dist1")

# The QQ plot for chi-squared - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = chisquared, y = mydist)) +
  geom_abline(slope = linechisq[[1]], intercept = linechisq[[2]])+ 
  ggtitle("Chi-Square Distribution Comparison with Dist1")

########### Example 2: dist2 ##########
# Strip off the first column of myDists
x2 <- myDists$Dist2

######## Compare to normal  ###

# Create a tibble with three columns: 
#    (1) the percentages for each quantile
#    (2) quantiles of the normal distribution and
#    (3) quantiles of the data in question
mytib <- tibble(percents,
                normal = qnorm(percents),
                uniform = qunif(percents),
                chisquared = qchisq(percents, df = 3),
                mydist = quantile(x2, percents))

# Calculate the line through the 25th and 75th
# quantile points on the QQ plot
linenorm <- qqnormline(x2)
lineunif <- qqunifline(x2)
linechisq <- qqchisqline(x2)

# The QQ plot for normal - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = normal, y = mydist)) +
  geom_abline(slope = linenorm[[1]], intercept = linenorm[[2]]) + 
  ggtitle("Normal Distribution Comparison with Dist2")


# The QQ plot for uniform - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = uniform, y = mydist)) +
  geom_abline(slope = lineunif[[1]], intercept = lineunif[[2]])+ 
  ggtitle("Uniform Distribution Comparison with Dist2")

# The QQ plot for chi-squared - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = chisquared, y = mydist)) +
  geom_abline(slope = linechisq[[1]], intercept = linechisq[[2]])+ 
  ggtitle("Chi-Square Distribution Comparison with Dist2")

########### Example 3: dist3 ##########
# Strip off the first column of myDists
x3 <- myDists$Dist3

######## Compare to normal  ###

# Create a tibble with three columns: 
#    (1) the percentages for each quantile
#    (2) quantiles of the normal distribution and
#    (3) quantiles of the data in question
mytib <- tibble(percents,
                normal = qnorm(percents),
                uniform = qunif(percents),
                chisquared = qchisq(percents, df = 3),
                mydist = quantile(x3, percents))

# Calculate the line through the 25th and 75th
# quantile points on the QQ plot
linenorm <- qqnormline(x3)
lineunif <- qqunifline(x3)
linechisq <- qqchisqline(x3)

# The QQ plot for normal - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = normal, y = mydist)) +
  geom_abline(slope = linenorm[[1]], intercept = linenorm[[2]]) + 
  ggtitle("Normal Distribution Comparison with Dist3")


# The QQ plot for uniform - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = uniform, y = mydist)) +
  geom_abline(slope = lineunif[[1]], intercept = lineunif[[2]])+ 
  ggtitle("Uniform Distribution Comparison with Dist3")

# The QQ plot for chi-squared - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = chisquared, y = mydist)) +
  geom_abline(slope = linechisq[[1]], intercept = linechisq[[2]])+ 
  ggtitle("Chi-Square Distribution Comparison with Dist3")

########### Example 4: dist4 ##########
# Strip off the first column of myDists
x4 <- myDists$Dist4

######## Compare to normal  ###

# Create a tibble with three columns: 
#    (1) the percentages for each quantile
#    (2) quantiles of the normal distribution and
#    (3) quantiles of the data in question
mytib <- tibble(percents,
                normal = qnorm(percents),
                uniform = qunif(percents),
                chisquared = qchisq(percents, df = 3),
                mydist = quantile(x4, percents))

# Calculate the line through the 25th and 75th
# quantile points on the QQ plot
linenorm <- qqnormline(x4)
lineunif <- qqunifline(x4)
linechisq <- qqchisqline(x4)

# The QQ plot for normal - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = normal, y = mydist)) +
  geom_abline(slope = linenorm[[1]], intercept = linenorm[[2]]) + 
  ggtitle("Normal Distribution Comparison with Dist4")


# The QQ plot for uniform - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = uniform, y = mydist)) +
  geom_abline(slope = lineunif[[1]], intercept = lineunif[[2]])+ 
  ggtitle("Uniform Distribution Comparison with Dist4")

# The QQ plot for chi-squared - points and line
mytib %>% 
  ggplot() +
  geom_point(aes(x = chisquared, y = mydist)) +
  geom_abline(slope = linechisq[[1]], intercept = linechisq[[2]])+ 
  ggtitle("Chi-Square Distribution Comparison with Dist4")

