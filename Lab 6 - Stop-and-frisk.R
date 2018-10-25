source('Lab 6/src/library.R')

### IMPORT DATA ####
sqf.data <- read_csv('Lab 6/data/output/sqf.csv')

### PREPARE DATA FOR MODELING ###
#sqf <- sqf.data %>% filter(suspected.crime=='cpw') %>% 

sqf <- sqf.data %>%
  select(id, year, found.weapon, found.gun, arrested, searched, suspect.race, suspect.age,
         suspect.build, suspect.sex, suspect.height, suspect.weight,
         stopped.bc.desc, stopped.bc.violent, stopped.bc.other, stopped.bc.object, 
         stopped.bc.casing, stopped.bc.lookout, stopped.bc.drugs, stopped.bc.clothing, 
         stopped.bc.furtive, stopped.bc.bulge, 
         precinct, inside, location.housing, observation.period, officer.uniform,
         additional.report, additional.investigation, additional.proximity, additional.evasive,
         additional.associating, additional.direction, additional.highcrime, additional.time, 
         additional.sights, additional.other, radio.run, day, month, time.period)

# Convert variable types as necessary
sqf <- sqf %>% mutate(suspect.race = as.factor(suspect.race), 
                      suspect.build = as.factor(suspect.build),
                      suspect.sex = as.factor(suspect.sex),
                      location.housing = as.factor(location.housing),
                      day = as.factor(day),
                      month = as.factor(month),
                      time.period = as.factor(time.period),
                      precinct = as.factor(precinct))
  
# Filter to complete cases, then split into train and test sets
sqf <- sqf %>% filter(complete.cases(sqf))

# shuffle and split randomly
#sqf <- sqf %>% slice(sample(1:n()))
#train <- sqf %>% slice(1:134000)
#test <- sqf %>% slice(134001:n())

# split by year
train <- sqf %>% filter(year==2008)
test <- sqf %>% filter(year==2009)

# separately standardize real-valued attributes for train/test sets
train <- train %>% mutate(suspect.height = standardize(suspect.height),
                          suspect.weight = standardize(suspect.weight),
                          suspect.age = standardize(suspect.age),
                          observation.period = standardize(observation.period))

test <- test %>% mutate(suspect.height = standardize(suspect.height),
                          suspect.weight = standardize(suspect.weight),
                          age = suspect.age,
                          suspect.age = standardize(suspect.age),
                          observation.period = standardize(observation.period))

### FIT LOGISTIC MODEL ON TRAINING SET ###
model <- glm(found.weapon ~ precinct + location.housing +  
               additional.report + additional.investigation + additional.sights +
               additional.proximity + additional.evasive + additional.associating +
               additional.direction + additional.highcrime + additional.time + 
               stopped.bc.object + stopped.bc.bulge + stopped.bc.desc + stopped.bc.violent +
               stopped.bc.casing + stopped.bc.lookout + stopped.bc.drugs + stopped.bc.clothing +
               stopped.bc.furtive + suspect.age + suspect.build + suspect.sex +
               suspect.height + suspect.weight + inside + observation.period +
               officer.uniform + radio.run + day + month + time.period, data=train, family = 'binomial')

# 1. generate predictions for test set
test$predicted.probability <- predict(model, newdata = test, type='response') 

# 2. output predictions in decreasing order
test %>% arrange(desc(predicted.probability)) %>% select(year, found.weapon,
                                                               predicted.probability)

# 3. generate confusion matrices for a given threshold
threshold <- 0.5
test <- test %>% mutate(prediction = case_when(
  predicted.probability < threshold ~ F,
  predicted.probability >= threshold ~ T
))
table(test$prediction, test$found.weapon)

# 4. output precision and recall for a threshold of 0.5
cat('At the threshold of ', threshold, ' the precision is ', 
    100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, prediction==T)),
    '%, and the recall is ', 100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, found.weapon==T)),
    '%\n')

# Change the threshold from 0 to 1 in increments of 0.25
# What do you expect will happen to precision? What do you expect will happen to recall?
# Write R code that checks this
for (threshold in c(seq(0,1,0.025))) {
  test <- test %>% mutate(prediction = case_when(
    predicted.probability < threshold ~ F,
    predicted.probability >= threshold ~ T
  ))
  
  cat('At the threshold of ', threshold, ' the precision is ', 
      100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, prediction==T)),
      '%, and the recall is ', 100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, found.weapon==T)),
      '%\n')
  }


# compute AUC using ROCR package
test.pred <- prediction(test$predicted.probability, test$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 


# generate predictions for training set
#train$predicted.probability <- predict(model, newdata = train, type='response') 

# compute AUC on training set
#train.pred <- prediction(train$predicted.probability, train$found.weapon)
#train.perf <- performance(train.pred, "auc")
#cat('the auc score is ', 100*train.perf@y.values[[1]], "\n") 

### MAKE PLOTS ###
# 1) make performance plot
plot.data <- test %>% arrange(desc(predicted.probability)) %>% 
  mutate(numstops = row_number(), percent.outcome = cumsum(found.weapon)/sum(found.weapon),
         stops = numstops/n()) %>% select(stops, percent.outcome)

# create and save plot
theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of weapons recovered", limits=c(0, 1), labels=scales::percent)
p
ggsave(plot=p, file='Lab 6/figs/performance_plot.pdf', height=5, width=5)

# 2) make main calibration plot
plot.data <- test %>% mutate(calibration = round(100*predicted.probability)) %>% 
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(found.weapon))

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
ggsave(plot=p, file='Lab 6/figs/calibration_main.pdf', height=5, width=5)

# 3) make precinct calibration plot
plot.data <- test %>% group_by(precinct) %>% 
  summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(found.weapon))

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
ggsave(plot=p, file='Lab 6/figs/calibration_precinct.pdf', height=5, width=5)

# 4) make gender/race/age facet plot
plot.data.sex <- test %>% group_by(suspect.sex) %>% 
  summarize(model.estimate = mean(predicted.probability), 
            empirical.estimate = mean(found.weapon)) %>% 
  gather('model.estimate', 'empirical.estimate', key = 'type', value = 'value') %>% 
  mutate(category = 'sex', group = suspect.sex) %>% select(-suspect.sex) 

plot.data.race <- test %>% group_by(suspect.race) %>% 
  summarize(model.estimate = mean(predicted.probability), 
            empirical.estimate = mean(found.weapon)) %>% 
  gather('model.estimate', 'empirical.estimate', key = 'type', value = 'value') %>% 
  mutate(category = 'race', group = suspect.race) %>% select(-suspect.race) 

plot.data.age <- test %>% mutate(age.binned = case_when(
  age < 18 ~ "Under 18",
  age >= 18 & age < 26 ~ "18 to 25",
  age >= 26 & age < 33 ~ "26 to 32",
  age >= 33 & age < 41 ~ "33 to 40",
  age >= 41 ~ "Over 40"
)) %>% group_by(age.binned) %>% summarize(model.estimate = mean(predicted.probability), 
                                          empirical.estimate = mean(found.weapon)) %>% 
  gather('model.estimate', 'empirical.estimate', key = 'type', value = 'value') %>% 
  mutate(category = 'age', group = age.binned) %>% select(-age.binned)

plot.data <- rbind(plot.data.sex, plot.data.race, plot.data.age)

# create and save plot
p <- ggplot(data = plot.data, aes(y=group, x=value))
p <- p + geom_point(alpha=0.5, aes(fill=type),shape=21)
p <- p + scale_fill_manual(values=c("black", "white"))
p <- p + facet_grid(category ~ ., scales="free_y", space="free_y")
p <- p + scale_size_area(guide='none')
p <- p + guides(colour = guide_legend(override.aes = list(alpha = 1)))
p <- p + scale_y_discrete('')
p <- p + scale_x_continuous('\n Hit rate', limits=c(0,.15), labels=scales::percent)
p <- p + theme(legend.title=element_blank(), legend.position=c(.83,.9), 
               legend.background = element_rect(fill="transparent"))
p
ggsave(plot=p, file='Lab 6/figs/calibration_facet.pdf', height=5, width=5)
