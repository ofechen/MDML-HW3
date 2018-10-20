# Header ------------------------------------------------------------------

# predict.R

# PROJECT: HW3_HONGYE_TERESA_OFER
# DATE: 2018-10-19

# Setup -------------------------------------------------------------------

source(library.R)

df_clean <- read_csv(sqf_08_16.csv)

# Analysis ----------------------------------------------------------------

# Create training set 

train_set <- df_clean %>% 
  filter(
    suspected.crime == cpw,
    year == 2008
  ) %>% 
  mutate(
    suspect.age = standardize(suspect.age),
    suspect.height = standardize(suspect.height),
    suspect.weight = standardize(suspect.weight),
    observation.period = standardize(observation.period)
  )

# Fit logit regression on training set
lmodel <- glm(found.weapon ~ precinct + location.housing + additional.report + additional.investigation +
              additional.proximity + additional.evasive + additional.associating + additional.direction +
              additional.highcrime + additional.time + additional.sights + additional.other + stopped.bc.object+
              stopped.bc.desc + stopped.bc.casing + stopped.bc.lookout + stopped.bc.clothing +
              stopped.bc.drugs + stopped.bc.furtive + stopped.bc.violent + stopped.bc.bulge +
              stopped.bc.other + suspect.age + suspect.build 
              + suspect.sex + suspect.height + suspect.weight + inside + radio.run + observation.period +
                day + month + time.period, data = train_set, family = binomial)

# Caculate the probability of a specific instance
# NEED TO IMPROVE CODES --> These values are not standardized but the model is....
friend.prob <- predict(lmodel, suspect.sex = M,
                       suspect.age == 30, suspect.height == 6, suspect.weight == 165, 
                       location.housing == trainsit, day == 04, month == 10, 
                       time.period == 6, stopped.bc.bulge == T, additional.proximity == T, 
                       observation.period == 10, radio.run == F) 

# Compute AUC on 2009 data, using ROCR package

test_09 <- df_clean %>% 
  filter(year== 2009)

test_09$predicted.probability <- predict(lmodel, newdata = test_09, type = "response")

test_09.pred <- prediction(test_09$predicted.probability, test_09$found.weapon)
test_09.perf <- performance(test_09.pred, "auc")
cat("the auc score is", 100*test_09.perf@y.values[[1]], "\n")

# Proportion Pairs Comparison
true_prob <- 