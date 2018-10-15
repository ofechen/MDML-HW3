source("~/Documents/GitHub/MD_and_ML/Labs/library.R")
sqf.data <- read.csv("~/Documents/GitHub/MD_and_ML/Labs/sqf.csv")

#limit to interesting variables
sqf <- sqf.data %>% filter(suspected.crime=='cpw') %>% 
  select(id,year,found.weapon,arrested,searched,suspect.race,suspect.age,suspect.build,
         suspect.sex,suspect.height,suspect.weight,stopped.bc.desc,stopped.bc.violent,stopped.bc.other,
         stopped.bc.object,stopped.bc.casing,stopped.bc.lookout,stopped.bc.drugs,stopped.bc.clothing,
         stopped.bc.furtive,stopped.bc.bulge,precinct,inside,location.housing,observation.period,
         officer.uniform,additional.report,additional.investigation,additional.proximity,additional.evasive,
         additional.associating,additional.direction,additional.highcrime,additional.time,additional.sights,
         additional.other,radio.run,day,month,time.period)

#convert to correct formats
sqf <- sqf %>% mutate(suspect.race = as.factor(suspect.race),
                      suspect.build = as.factor(suspect.build),
                      suspect.sex = as.factor(suspect.sex),
                      location.housing = as.factor(location.housing),
                      day = as.factor(day),
                      month = as.factor(month),
                      time.period = as.factor(time.period),
                      precinct = as.factor(precinct))

sqf <- sqf %>% filter(complete.cases(sqf))

train <- sqf %>% filter(year==2008)
test <- sqf %>% filter (year==2009)

model <- glm(found.weapon ~ precinct + location.housing + additional.report +
               arrested + searched + 
             additional.investigation + additional.proximity + additional.evasive +
             additional.associating + additional.direction + additional.highcrime + additional.time +
             additional.sights + additional.other + stopped.bc.desc + stopped.bc.violent +
             stopped.bc.other + stopped.bc.object + stopped.bc.casing + stopped.bc.lookout + 
             stopped.bc.drugs + stopped.bc.clothing + stopped.bc.furtive + stopped.bc.bulge + 
             radio.run + day + month + time.period + suspect.race + suspect.age + suspect.build + 
             suspect.sex + suspect.height + suspect.weight + officer.uniform + inside + observation.period, 
             data = train, family = 'binomial')

test$predicted.probability <- predict(model, newdata = test, type = 'response')

test.pred <- prediction(test$predicted.probability,test$found.weapon)
test.perf <- performance(test.pred, "auc")
cat ('the auc score is ', 100*test.perf@y.values[[1]], "\n")
