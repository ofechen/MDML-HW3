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

paste0('Confirmed: ', auc, ' is close to ', approx_auc)

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

years <- 2008:2016
aucs <- foreach(year=years, .combine='c') %dopar% { compute_auc_for_year(year) }
qplot(years, aucs, geom='line', xlab='Year', ylab='AUC')

# AUC decrease because model was trained on 2008 data. 
# Distribution of weapon posession changes over time, so model becomes less reliable.