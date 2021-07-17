
# Code for Final Project:: TJ Haslam :: JHU Tidyverse Project   -----------

# Libraries I -------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(tm)


# Load data ---------------------------------------------------------------

data_complaints_train <- read_csv("data/final_project/data_complaints_train.csv")
# https://d3c33hcgiwev3.cloudfront.net/JhHJz2SSRCqRyc9kkgQqxA_8d34147955154de4a6176086946d07b3_data_complaints_train.csv?
data_complaints_test <- read_csv("data/final_project/data_complaints_test.csv")
# https://d3c33hcgiwev3.cloudfront.net/aEBWUxehSGyAVlMXoThsoQ_edf53641edca416fa00a78d9e4b16ced_data_complaints_test.csv


# Clean and pre-Tidy ------------------------------------------------------

data_complaints_train %>% glimpse() 

# names
dc_train <- data_complaints_train %>% clean_names()
dc_test <- data_complaints_test %>% clean_names()
# set as factors
dc_train$product <- factor(dc_train$product)
dc_train$company <- factor(dc_train$company)
dc_test$company <- factor(dc_test$company)
# simplify
dc_train <- dc_train %>% 
  rename(complaint = consumer_complaint_narrative )
dc_test<- dc_test %>% 
  rename(complaint = consumer_complaint_narrative )

# clean text
dc_train$complaint  <-  str_replace_all(dc_train$complaint, "[XX]+", " ")
dc_train$complaint  <-  str_replace_all(dc_train$complaint , "[xx]+", " ")
dc_train$complaint  <-  str_replace_all(dc_train$complaint , "[^[:alnum:]]", " ")
dc_train$complaint  <-  str_replace_all(dc_train$complaint , "\\d", " ")


dc_test$complaint  <-  str_replace_all(dc_test$complaint, "[XX]+", " ")
dc_test$complaint  <-  str_replace_all(dc_test$complaint , "[xx]+", " ")
dc_test$complaint  <-  str_replace_all(dc_test$complaint , "[^[:alnum:]]", " ")
dc_test$complaint  <-  str_replace_all(dc_test$complaint , "\\d", " ")

# Drop unneeded vars
dc_train <- dc_train %>% select(c(1:3))
dc_test <- dc_test %>% select(c(1:3))


# EDA case for new var ----------------------------------------------------

dc_train$company %>% 
  n_distinct() 

dc_train  %>% 
  group_by(product) %>% 
  distinct(company) %>% 
  count()

dc_train  %>% 
  count(product) %>% 
  mutate(ratio = n/sum(n))


# Set theory I
product_company <- dc_train %>% 
  group_by(product) %>% 
  distinct(company) 

credit_companies <- product_company %>% 
  filter(product == "Credit card or prepaid card")

mort_companies <- product_company %>% 
  filter(product == "Mortgage")

stu_companies <- product_company %>% 
  filter(product == "Student loan")

car_companies <- product_company %>% 
  filter(product == "Vehicle loan or lease")

# Set theory II

# Basic Set Theory
cc_op <- bind_rows(mort_companies, stu_companies,  car_companies)
cc_op$company %>% 
  n_distinct() # with below should add to 1497

cc_only <- credit_companies %>% 
  anti_join(cc_op, by = "company") 
cc_only %>% 
  n_distinct()  # with above should add to 1497

# Set again
mort_op <-  bind_rows(credit_companies, stu_companies,  car_companies)
  # mort_op$company %>% n_distinct() # 783
mort_only <- mort_companies %>% 
  anti_join(mort_op , by = "company") 
  # mort_only %>% n_distinct() # 714

  # Set again
stu_op <-  bind_rows(credit_companies, mort_companies, car_companies)
  # stu_op$company %>% n_distinct() # 1318
stu_only <- stu_companies %>% 
  anti_join(stu_op, by = "company") 
  # stu_only$company %>%  n_distinct()  179

  #Set again
car_op <- bind_rows(credit_companies, mort_companies, stu_companies)
  # car_op$company %>% n_distinct() #  1233
car_only <- car_companies %>% 
  anti_join(car_op  , by = "company") 
  #car_only$company %>% n_distinct()  #  1233


# Create new var ----------------------------------------------------------

# Reader version
dc_train <- dc_train %>%
  mutate(comp_domain = case_when(company %in% cc_only$company ~ "ccard_only",
                                 company %in% stu_only$company ~ "student_only",
                                 company %in% mort_only$company ~ "mort_only",
                                 company %in% car_only$company ~ "auto_only",
                                 TRUE ~ "mixed_lender") %>%
           factor() )


dc_test <- dc_test %>%
  mutate(comp_domain = case_when(company %in% cc_only$company ~ "ccard_only",
                                 company %in% stu_only$company ~ "student_only",
                                 company %in% mort_only$company ~ "mort_only",
                                 company %in% car_only$company ~ "auto_only",
                                 TRUE ~ "mixed_lender") %>%
           factor() )

# Modelling version
dc_train <- dc_train %>%
  mutate(dummy_var = case_when(comp_domain == "ccard_only" ~ 4,
                               comp_domain == "student_only" ~ 3,
                               comp_domain == "mort_only" ~ 2,
                               comp_domain == "auto_only" ~ 1,
                               TRUE ~ 0)) 

dc_test <- dc_test %>%
  mutate(dummy_var = case_when(comp_domain == "ccard_only" ~ 4,
                               comp_domain == "student_only" ~ 3,
                               comp_domain == "mort_only" ~ 2,
                               comp_domain == "auto_only" ~ 1,
                               TRUE ~ 0)) 

# New var results
dc_train %>% 
  count(comp_domain) %>% 
  mutate(ratio = n/sum(n)) %>%
  knitr::kable(caption = "Training Data Results with New Var")
  # almost 20% identified

#results
dc_test %>% 
  count(comp_domain)  %>% 
  mutate(ratio = n/sum(n))  %>% 
  knitr::kable(caption = "Test Data Results with New Var")
  # 25% identified


# Save data sets; clean enviro; load data ---------------------------------

save(dc_train, file = here::here("data", "final_project", "dc_train.rda"))
save(dc_test,  file = here::here("data", "final_project", "dc_test.rda"))
# clean out enivorment
rm(list = ls())
#load only needed data
load( here::here("data", "final_project", "dc_train.rda"))
load( here::here("data", "final_project", "dc_test.rda"))


# Start Modelling ---------------------------------------------------------
#
# Libraries II ------------------------------------------------------------
library(tidytext)
library(tidymodels)
library(textrecipes)
library(themis)
library(hardhat)


# Parameters, recipe, cv folds --------------------------------------------

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

smaller_lambda <- grid_regular(penalty(range = c(-4, 0)), levels = 10)


run_one <- recipe(product ~ complaint + dummy_var ,
                  data = dc_train) %>%
  step_tokenize(complaint) %>%
  step_tokenfilter(complaint, max_tokens = 1e3) %>%
  step_tfidf(complaint) #%>%
#themis::step_downsample(product)


set.seed(2023)
run_one_folds <- vfold_cv(dc_train, v = 10)


# Model spec and workflow -------------------------------------------------
run_one_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")


run1_lasso_wf <- workflow() %>%
  add_recipe(run_one, blueprint = sparse_bp) %>%
  add_model(run_one_spec)

run1_lasso_wf 


# Run time ----------------------------------------------------------------

set.seed(2023)
library(doParallel)
doParallel::registerDoParallel()

run1_lasso_rs <- tune_grid(
  run1_lasso_wf ,
  run_one_folds ,
  grid = smaller_lambda,
  control = control_resamples(save_pred = TRUE)
)


run1_lasso_rs  


# Results and best model --------------------------------------------------

best_acc <- run1_lasso_rs %>%
  show_best("accuracy")

best_acc %>% knitr::kable(caption = "Models ranked by Accuracy")

choose_acc <- run1_lasso_rs %>% 
  select_by_one_std_err(metric = "accuracy", -penalty)

choose_acc %>% knitr::kable(caption = "Selected Final Model")


# Plot Confusion Matrix I -------------------------------------------------

run1_lasso_rs  %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  filter(id == "Fold05") %>%
  conf_mat(product, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_fill_gradient(low = "#FFFAFA", high = "#006400") +
  ggtitle("CM: Best Model (Accuracy)", 
          subtitle = "Data from Fold 5 of 10")


# Plot CM II --------------------------------------------------------------

run1_lasso_rs  %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  filter(.pred_class != product) %>%
  filter(id == "Fold05") %>%
  conf_mat(product, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_fill_gradient(low = "#FFFAFA", high = "#FF0000") +
  ggtitle("CM: Mistaken Identifications", 
          subtitle = "Data from Fold 5 of 10")



# Useful CM summary stats -------------------------------------------------

# Correct ID's
cm_table <-  run1_lasso_rs  %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  filter(id == "Fold05") %>%
  conf_mat(product, .pred_class)

cm <- cm_table[["table"]] 
credit_card_tp <- cm[1 ,1] / sum(cm[ ,1])
mortage_tp <- cm[2,2] / sum(cm[ ,2])
stu_loan_tp <- cm[3,3] / sum(cm[ ,3])
car_loan_tp <- cm[4,4] / sum(cm[ ,4])

quick_glance <- data.frame("Credit_Card" = credit_card_tp,
                           "Mortage" = mortage_tp,
                           "Student_Loan" = stu_loan_tp,
                           "Vehicle_LL" = car_loan_tp)

quick_glance %>%
  knitr::kable(caption = "CM: Correct Identification Rate")


# False Positives
credit_card_fp <- 1 - (cm[1 ,1] / sum(cm[1, ]) )
mortage_fp <- 1 - (cm[2,2] / sum(cm[2, ]) )
stu_loan_fp <- 1 - (cm[3,3] / sum(cm[3, ]))
car_loan_fp <- 1 - (cm[4,4] / sum(cm[4, ]))


quick_glance_fp <- data.frame("Credit_Card" = credit_card_fp,
                              "Mortage" = mortage_fp,
                              "Student_Loan" = stu_loan_fp,
                              "Vehicle_LL" = car_loan_fp)


quick_glance_fp %>% 
  knitr::kable(caption = "CM: False Postive Rates")


# Fit final model; Predict on Test data -----------------------------------

final_wf <- finalize_workflow(run1_lasso_wf, choose_acc)

final_model <- fit(final_wf, dc_train)

results <- predict(final_model,  dc_test) 

results$problem_id <- dc_test$problem_id

results %>% 
  select(problem_id, .pred_class)  %>% 
  knitr::kable(caption ="Prediction Results on Test Data Set")


# Session info  -----------------------------------------------------------
ses_info <- sessioninfo::package_info() %>% tibble() 
ses_info %>% select(c(1,3,8,9))

### END
