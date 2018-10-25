# Header ------------------------------------------------------------------

# predict.R

# PROJECT: HW3_HONGYE_TERESA_OFER
# DATE: 2018-10-19

# Setup -------------------------------------------------------------------

source("library.R")

sqf <- read_csv("sqf_08_16.csv")

# Convert variable types as necessary
sqf <- sqf %>% mutate(suspect.build = as.factor(suspect.build),
                      suspect.sex = as.factor(suspect.sex),
                      location.housing = as.factor(location.housing),
                      day = as.factor(day),
                      month = as.factor(month),
                      time.period = as.factor(time.period),
                      precinct = as.factor(precinct))

# Part A ----------------------------------------------------------------

# Create training set 
train_set <- sqf %>% 
  filter(
    suspected.crime == "cpw",
    year == "2008"
  )

# Remember train set standarization values.
train_age_mean <- mean(train_set$suspect.age, na.rm = TRUE)
train_age_sd <- sd(train_set$suspect.age, na.rm = TRUE)
train_height_mean <- mean(train_set$suspect.height, na.rm=TRUE)
train_height_sd <- sd(train_set$suspect.height, na.rm=TRUE)
train_weight_mean <- mean(train_set$suspect.weight, na.rm=TRUE)
train_weight_sd <- sd(train_set$suspect.weight, na.rm=TRUE)
train_period_mean <- mean(train_set$observation.period, na.rm=TRUE)
train_period_sd <- sd(train_set$observation.period, na.rm=TRUE)

# Standardize train set.
train_set <- train_set %>%
  mutate(
    suspect.age = (suspect.age - train_age_mean) / train_age_sd,
    suspect.height = (suspect.height - train_height_mean) / train_height_sd,
    suspect.weight = (suspect.weight - train_weight_mean) / train_weight_sd,
    observation.period = (observation.period - train_period_mean) / train_period_sd
  )

# Fit logit regression on training set
train_set <- train_set %>% select(found.weapon, precinct, location.housing, 
                    contains('additional.'), contains('stopped.bc'), 
                    suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight, 
                    inside, radio.run, observation.period, day, month, time.period)
lmodel <- glm(found.weapon ~ ., data = train_set, family = "binomial")

# Caculate the probability of a specific instance
imaginary_friend <- data.frame(precinct="6", 
                               location.housing = "transit",
                               additional.report = F,
                               additional.investigation = F,
                               additional.proximity = T,  # close to high crime location?
                               additional.evasive = F,
                               additional.associating = F,
                               additional.direction = F,
                               additional.highcrime = F,
                               additional.time = F,
                               additional.sights = F,
                               additional.other = F,
                               stopped.bc.object = F,
                               stopped.bc.desc = F,
                               stopped.bc.casing = F,
                               stopped.bc.lookout = F,
                               stopped.bc.clothing = F,
                               stopped.bc.drugs = F,
                               stopped.bc.furtive = F,
                               stopped.bc.violent = F,
                               stopped.bc.bulge = T,
                               stopped.bc.other = F,
                               suspect.age = (30 - train_age_mean) / train_age_sd, 
                               suspect.build = "medium",
                               suspect.sex = "male",
                               suspect.height = (6 - train_height_mean) / train_height_sd, 
                               suspect.weight = (165 - train_weight_mean) / train_weight_sd, 
                               inside = F,
                               radio.run = F,
                               observation.period = (10 - train_period_mean) / train_period_sd,
                               day = "Thursday", month = "October", 
                               time.period = "6")

friend.prob.male <- predict(lmodel, newdata=imaginary_friend, type='response') 
paste0('imaginary friend (male) ex-ante probability of possessing a weapon: ', friend.prob.male)

imaginary_friend_female <- imaginary_friend
imaginary_friend_female$suspect.sex <- 'female'
friend.prob.female <- predict(lmodel, newdata=imaginary_friend_female, type='response') 
paste0('imaginary friend (female) ex-ante probability of possessing a weapon: ', friend.prob.female)

# Compute AUC on 2009 data, using ROCR package

test_09 <- sqf %>% 
  filter(year=="2009")

# preprocess as in training data.
test_09 <- test_09 %>%
  mutate(
    suspect.age = (suspect.age - train_age_mean) / train_age_sd,
    suspect.height = (suspect.height - train_height_mean) / train_height_sd,
    suspect.weight = (suspect.weight - train_weight_mean) / train_weight_sd,
    observation.period = (observation.period - train_period_mean) / train_period_sd
  )

test_09$predicted.probability <- predict(lmodel, newdata = test_09, type = "response")
test_09 <- test_09 %>% filter(!is.na(predicted.probability))

test_09.pred <- prediction(test_09$predicted.probability, test_09$found.weapon)
test_09.perf <- performance(test_09.pred, "auc")
auc <- 100*test_09.perf@y.values[[1]]
paste0("the auc score is ", auc)

# Proportion Pairs Comparison
true_samples <- test_09[test_09$found.weapon == TRUE,]
true_samples <- true_samples[sample(nrow(true_samples), 10000, replace=T),]

false_samples <- test_09[test_09$found.weapon == FALSE,]
false_samples <- false_samples[sample(nrow(false_samples), 10000, replace=T),]

approx_auc <- sum(true_samples$predicted.probability > false_samples$predicted.probability) / 10000
paste0('frequency that a randomly chosen true instance will be ranked higher than a randomly chosen false instance: ', approx_auc)

paste0('Confirmed: ', auc, ' is close to ', approx_auc*100)

# Part B ----------------------------------------------------------------

# set precincts that don't appear in training set to NA.
sqf$precinct <- factor(sqf$precinct, levels = levels(factor(train_set$precinct)))

# function for convenience.
compute_auc_for_year <- function(this_year) {
  test <- sqf %>% 
    filter(year==this_year)
  test <- test %>%
    mutate(
      suspect.age = (suspect.age - train_age_mean) / train_age_sd,
      suspect.height = (suspect.height - train_height_mean) / train_height_sd,
      suspect.weight = (suspect.weight - train_weight_mean) / train_weight_sd,
      observation.period = (observation.period - train_period_mean) / train_period_sd
    )
  
  test$predicted.probability <- predict(lmodel, newdata = test, type = "response")
  test <- test %>% filter(!is.na(predicted.probability))
  
  test.pred <- prediction(test$predicted.probability, test$found.weapon)
  test.perf <- performance(test.pred, "auc")
  auc <- 100*test.perf@y.values[[1]]
  auc
}

years <- c(2008:2016)
aucs <- foreach(year=years, .combine='c') %dopar% { compute_auc_for_year(year) }
qplot(years, aucs, geom='line', xlab='Year', ylab='AUC', ylim = c(0,100))

# AUC decrease because model was only trained on 2008 data. 
# Distribution of weapon posession might change over time, so model becomes less reliable.
# An option is to train the model based on sample data from all years so that any ecological changes are represented in the data

## Part C --- We need to do one for each group member!!

# Plots Group 1
# Choose target variable (contraband found) and restrict data to a relevant subset

train_set_contraband <- sqf %>% 
  filter(
   suspected.crime == c("criminal sale of controlled substance","criminal possesion of controlled substance"),
    year %in% c(2008:2009)
  )

# Splitting the data temporally, using 2008-2010 as the training data and any subsequent year as the test data. 
# This creates a split of about 60-40 between train and test sets which is acceptable

test_set_con_10_16 <- sqf %>% 
  filter(
    suspected.crime == c("criminal sale of controlled substance","criminal possesion of controlled substance"),
    year %in% c(2010:2016)
  )

# Remember train set standarization values.
train_age_mean_con <- mean(train_set_contraband$suspect.age, na.rm = TRUE)
train_age_sd_con <- sd(train_set_contraband$suspect.age, na.rm = TRUE)
train_height_mean_con <- mean(train_set_contraband$suspect.height, na.rm=TRUE)
train_height_sd_con <- sd(train_set_contraband$suspect.height, na.rm=TRUE)
train_weight_mean_con <- mean(train_set_contraband$suspect.weight, na.rm=TRUE)
train_weight_sd_con <- sd(train_set_contraband$suspect.weight, na.rm=TRUE)
train_period_mean_con <- mean(train_set_contraband$observation.period, na.rm=TRUE)
train_period_sd_con <- sd(train_set_contraband$observation.period, na.rm=TRUE)

# Standardize train set.
train_set_contraband <- train_set_contraband %>%
  mutate(
    suspect.age = (suspect.age - train_age_mean_con) / train_age_sd_con,
    suspect.height = (suspect.height - train_height_mean_con) / train_height_sd_con,
    suspect.weight = (suspect.weight - train_weight_mean_con) / train_weight_sd_con,
    observation.period = (observation.period - train_period_mean_con) / train_period_sd_con
  )

# Remember test set standarization values.
test_age_mean_con <- mean(test_set_con_10_16$suspect.age, na.rm = TRUE)
test_age_sd_con <- sd(test_set_con_10_16$suspect.age, na.rm = TRUE)
test_height_mean_con <- mean(test_set_con_10_16$suspect.height, na.rm=TRUE)
test_height_sd_con <- sd(test_set_con_10_16$suspect.height, na.rm=TRUE)
test_weight_mean_con <- mean(test_set_con_10_16$suspect.weight, na.rm=TRUE)
test_weight_sd_con <- sd(test_set_con_10_16$suspect.weight, na.rm=TRUE)
test_period_mean_con <- mean(test_set_con_10_16$observation.period, na.rm=TRUE)
test_period_sd_con <- sd(test_set_con_10_16$observation.period, na.rm=TRUE)

# Standardize test set.
test_set_con_10_16 <- test_set_con_10_16 %>%
  mutate(
    suspect.age = (suspect.age - test_age_mean_con) / test_age_sd_con,
    suspect.height = (suspect.height - test_height_mean_con) / test_height_sd_con,
    suspect.weight = (suspect.weight - test_weight_mean_con) / test_weight_sd_con,
    observation.period = (observation.period - test_period_mean_con) / test_period_sd_con
  )

# Fit logit regression on training set
train_set_contraband <- train_set_contraband %>% select(found.contraband, precinct, location.housing, 
                                  contains('additional.'), contains('stopped.bc'), 
                                  suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight, 
                                  inside, radio.run, observation.period, day, month, time.period)
con_lmodel <- glm(found.contraband ~ ., data = train_set_contraband, family = "binomial")
test_set_con_10_16$prediction <- predict(con_lmodel,test_set_con_10_16, type = "response")
# collceting performance tibble
con_model_perf <- tibble(threshold = numeric(),percent_above = numeric(),percent_pos=numeric())

for (threshold in seq(0,1,0.05)) {
  index<-threshold/0.05 + 1
  test_set_con_10_16 <- test_set_con_10_16 %>% mutate (over_thresh=case_when(
    prediction >= threshold ~ TRUE,
    prediction < threshold ~ FALSE)) %>% 
    mutate(true_positive = if_else( over_thresh & found.contraband, TRUE, FALSE)) %>% 
    mutate(pos_over_thresh = if_else(over_thresh & true_positive ,TRUE,FALSE))
  per_above <- mean(test_set_con_10_16$over_thresh,na.rm=TRUE)
  per_pos <- sum(test_set_con_10_16$pos_over_thresh,na.rm=TRUE)/sum(test_set_con_10_16$found.contraband,na.rm=TRUE)
  con_model_perf [index,] <- c(threshold,per_above,per_pos)
}

# Making performance plot
plot.data <- test_set_con_10_16 %>% arrange(desc(prediction)) %>% 
  mutate(numstops = row_number(), percent.outcome = cumsum(found.contraband)/sum(found.contraband),
         stops = numstops/n()) %>% select(stops, percent.outcome)

# create and save plot
theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of contraband captured", limits=c(0, 1), labels=scales::percent)
p
ggsave(plot=p, file='con_performance_plot.pdf', height=5, width=5)

# 2) make main calibration plot
plot.data <- test_set_con_10_16 %>% mutate(calibration = round(100*prediction)) %>% 
  group_by(calibration) %>% summarize(model.estimate = mean(prediction),
                                      numstops = n(),
                                      empirical.estimate = mean(found.contraband))

# create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p
ggsave(plot=p, file='con_calibration_main.pdf', height=5, width=5)
# In this case the focus is on contraband, the data was temporally split into 2, the years 08-09 and the rest.
# Data was restricted to stop reasons relating to selling or buying drugs. The results indicate the the model built 
# is not great but could be used to reduce the number of false positives. If tolerance for illeagel substances is 
# not zero then there is an option to reduce the number of stops. according to the performance plot for example
# 30% of the stops account for about 60% of the contraband.


# Plots Group 2
# Target variable: whether the suspect was searched

# Spliting 2008 and 2011 data in half for training and test sets
smp <- sqf %>% filter(year == c(2008:2011))
smp_size <- floor(0.5* nrow(smp))

set.seed(123)
train_ind <- sample(seq_len(nrow(smp)), size = smp_size)

train_searched <- smp[train_ind, ]
test_searched <- smp[-train_ind, ]

# Set standardization values
train_age_mean_s <- mean(train_searched$suspect.age, na.rm = TRUE)
train_age_sd_s <- sd(train_searched$suspect.age, na.rm = TRUE)
train_height_mean_s <- mean(train_searched$suspect.height, na.rm=TRUE)
train_height_sd_s <- sd(train_searched$suspect.height, na.rm=TRUE)
train_weight_mean_s <- mean(train_searched$suspect.weight, na.rm=TRUE)
train_weight_sd_s <- sd(train_searched$suspect.weight, na.rm=TRUE)
train_period_mean_s <- mean(train_searched$observation.period, na.rm=TRUE)
train_period_sd_s <- sd(train_searched$observation.period, na.rm=TRUE)

# Standardize train set
train_searched <- train_searched %>%
  mutate(
    suspect.age = (suspect.age - train_age_mean_s) / train_age_sd_s,
    suspect.height = (suspect.height - train_height_mean_s) / train_height_sd_s,
    suspect.weight = (suspect.weight - train_weight_mean_s) / train_weight_sd_s,
    observation.period = (observation.period - train_period_mean_s) / train_period_sd_s
  )

# Standardize test set
test_searched <- test_searched %>%
  mutate(
    suspect.age = (suspect.age - train_age_mean_s) / train_age_sd_s,
    suspect.height = (suspect.height - train_height_mean_s) / train_height_sd_s,
    suspect.weight = (suspect.weight - train_weight_mean_s) / train_weight_sd_s,
    observation.period = (observation.period - train_period_mean_s) / train_period_sd_s
  )

# Select independent variables
train_searched <- train_searched %>% select(
  searched,
  additional.proximity, additional.highcrime, additional.sights, additional.associating,
  contains("stopped.bc."),
  suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight, 
  inside, radio.run, observation.period, day, month, time.period
)

# Model fitting
smodel <- glm(searched ~., data = train_searched, family = "binomial")
test_searched$prediction <- predict(smodel, newdata = test_searched, type = "response")

# Performance plot
search_plot_data <- test_searched %>% arrange() %>% 
  mutate(pct_searched = )
  select(observation.period, searched)

theme_set(theme_bw())
s <- ggplot(data = search_plot_data, aes(x = observation.period, y = searched))


# Plots Group 3

# See if weapon possession and suspect attributes can be used to predict whether the incident occured in Manhattan.
# A classifier with high AUC would indicate that crimes in Manhattan are distinguishable from crimes outside the bustling city.

sqf_manhattan <- sqf %>% mutate(is.manhattan=(city=="MANHATTAN"))
sqf_manhattan <- sqf_manhattan %>% filter(suspected.crime == "cpw") %>% select(
  year,
  is.manhattan,
  contains('found.'),
  suspect.age,
  suspect.sex,
  suspect.height,
  suspect.weight,
  contains('additional.'),
  contains('stopped.bc'),
  day,
  month,
  time.period,
  radio.run)

train_set3 <- sqf_manhattan %>% 
  filter(
    year == 2008
  )

# Splitting the data temporally, using 2008-2010 as the training data and any subsequent year as the test data. 
# This creates a split of about 60-40 between train and test sets which is acceptable

test_set3 <- sqf_manhattan %>% 
  filter(
    year == 2009
  )

# Remember train set standarization values.
train_age_mean_con <- mean(train_set3$suspect.age, na.rm = TRUE)
train_age_sd_con <- sd(train_set3$suspect.age, na.rm = TRUE)
train_height_mean_con <- mean(train_set3$suspect.height, na.rm=TRUE)
train_height_sd_con <- sd(train_set3$suspect.height, na.rm=TRUE)
train_weight_mean_con <- mean(train_set3$suspect.weight, na.rm=TRUE)
train_weight_sd_con <- sd(train_set3$suspect.weight, na.rm=TRUE)

# Standardize train set.
train_set3 <- train_set3 %>%
  mutate(
    suspect.age = (suspect.age - train_age_mean_con) / train_age_sd_con,
    suspect.height = (suspect.height - train_height_mean_con) / train_height_sd_con,
    suspect.weight = (suspect.weight - train_weight_mean_con) / train_weight_sd_con,
  )
# Standardize test set.
test_set3 <- test_set3 %>%
  mutate(
    suspect.age = (suspect.age - train_age_mean_con) / train_age_sd_con,
    suspect.height = (suspect.height - train_height_mean_con) / train_height_sd_con,
    suspect.weight = (suspect.weight - train_weight_mean_con) / train_weight_sd_con,
  )

man_lmodel <- glm(is.manhattan ~ ., data = train_set3, family = "binomial")

# 1. generate predictions for test set
test_set3$predicted.probability <- predict(man_lmodel, newdata = test_set3, type='response') 

# 2. output predictions in decreasing order
test_set3 %>% arrange(desc(predicted.probability)) %>% select(is.manhattan,
                                                         predicted.probability)

# 1) make performance plot
plot.data <- test_set3 %>% arrange(desc(predicted.probability)) %>% 
  mutate(numstops = row_number(), percent.outcome = cumsum(is.manhattan)/sum(is.manhattan),
         stops = numstops/n()) %>% select(stops, percent.outcome)

# create and save plot
theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of Manhattan crimes", limits=c(0, 1), labels=scales::percent)
p
ggsave(plot=p, file='2c_3_performance_plot.pdf', height=5, width=5)

# 2) make main calibration plot
plot.data <- test_set3 %>% mutate(calibration = round(100*predicted.probability)) %>% 
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(is.manhattan))

# create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p
ggsave(plot=p, file='2c_3_calibration_main.pdf', height=5, width=5)


# Paragraph summary:
summary(man_lmodel)

# We suspected that crimes within Manhattan are peculiar and may be distinuishable from crimes outside
# of Manhattan. We built a classifier based on stop data and found that the classifier's performance is better
# than random, suggesting that to some degree, Manhattan crimes are different from non-Manhattan crimes.
# Looking at the learned weights of our logistic regression model, we interestingly find that
# possession of either a pistol or machine gun is a good indicator for Manhattan-based crimes, while
# possession of a gun negatives correlates with Manhattan-based crimes. The rest of our independent variables
# have much smaller weights compared to these.
