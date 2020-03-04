require(tidyverse)
require(class)
require(ISLR)

Default <- Default

Train = sample(1:2, 10000, replace = TRUE)

summary(Train)
Default$Train <- Train

TrainingSet <- Default %>%
  filter(Train == 1)

TrainingSet <- Default %>%
  filter(Train == 2)

TrainingSet <- head(TrainingSet, 4986)

set.seed(999)

neighbors <- 3
train.input <- TrainingSet %>%
  select(balance, income)
test.input <- TrainingSet %>%
  select(balance, income)
train.output <- TrainingSet$Default
length(train.output)

test.output$Default <- knn(train = train.input,
                test = test.input,
                cl = train.output,
                k = negihbors)
compare <- cbind(train.output, test.output)

table(data.frame(comprare))