library(tidyverse)
library(caret)
library(kernlab)

cost_threshold = 5000
hmo = read_csv(url("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv"))

hmo = hmo %>% filter(!is.na(bmi), !is.na(hypertension)) %>% 
  mutate(smoker = factor(ifelse(smoker == "yes", "smoker", "nonsmoker")),
         exercise        = as.factor(exercise),
         gender          = as.factor(gender),
         expensive       = factor(ifelse(cost > cost_threshold, "yes", "no"))) %>%
  select(age, bmi, children, exercise, gender, smoker, expensive)
  
final_model = ksvm(expensive ~ .,data=trainSet,C=4,prob.model=TRUE,kernel = 'rbfdot')

