library(corrplot)
library(h2o)
setwd("/Users/marceloalleo/Dataset FE")

################################################################################
################################### OLS ########################################
################################################################################

################################# Small Data ###################################
data <- read.csv("dataset.csv")
summary(data)

ols <- lm(FE ~ GCW + DS + S1 + S2 + S3, data = data)
summary(ols)

keeps <- c("FE", "GCW", "DS", "S1", "S2", "S3")
M = cor(data[keeps])
corrplot(M, method = 'number') 

################################## Full Data ###################################
ols <- lm(FE ~ GCW1 + GCW2 + GCW3 + GCW4 + GCW5 + GCW6 + GCW7 + GCW8 + GCW9 +
            GCW10 + GCW11 + GCW12 + GCW13 + GCW14 + GCW15 + GCW16 + GCW17 +
            GCW18 + GCW19 + GCW20 + GCW21 + GCW22 + GCW23 + GCW24 + GCW25 +
            GCW26 + GCW27 + GCW28 + GCW29 + S1 + S2 + S3 + SPEED1 + SPEED2 +
            SPEED3 + SPEED4 + SPEED5 + SPEED6 + SPEED7 + SPEED8 + SPEED9 +
            SPEED10 + SPEED11 + SPEED12 + SPEED13 + SPEED14 + SPEED15 +
            SPEED16 + SPEED17 + SPEED18 + SPEED19 + SPEED20, data = data_full)
summary(ols)

################################################################################
#################################### ML ########################################
################################################################################

############################### Small Data #####################################
data <- read.csv("dataset.csv")
summary(data)
h2o.init(nthreads = -1, max_mem_size = "16g")
data_h2o <- h2o.importFile("dataset.csv")
h2o.describe(data_h2o)
y <- "FE" 
x <- setdiff(names(data_h2o), y)
parts <- h2o.splitFrame(data_h2o, .80)
train <- parts[[1]]
test <- parts[[2]]
automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 120)
predictions <- h2o.predict(automodel@leader, test)
automodel@leader
predictions
print(automodel@leaderboard)

################################## Full Data ###################################
data_full <- read.csv("dataset_full.csv")
summary(data_full)
h2o.init(nthreads = -1, max_mem_size = "16g")
data_full_h2o <- h2o.importFile("dataset_full.csv")
h2o.describe(data_full_h2o)
y <- "FE" 
x <- setdiff(names(data_full_h2o), y)
parts <- h2o.splitFrame(data_full_h2o, .80)
train <- parts[[1]]
test <- parts[[2]]
automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 120)
predictions <- h2o.predict(automodel@leader, test)
predictions
automodel@leader

perf <- h2o.performance(automodel@leader, test)
perf
