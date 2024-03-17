# Start ----

# Libraries
library(tidyverse)
library(pROC)
library(pmsampsize)

# Initialize values
nfolds <- 10
nrepeats <- 20
niter <- 1

mat_nrow <- 800 # Used for making intentionally long matrix columns later

seed <- 19970507; set.seed(seed)

# Sample size
pmsampsize(type = 'b', parameters = 24, prevalence = 0.20, cstatistic = 0.80)

# Data
tfold_file_paths <- paste0("Data/CV-folds/tfold-", seq(1, nfolds*nrepeats, 1), ".csv")
vfold_file_paths <- paste0("Data/CV-folds/vfold-", seq(1, nfolds*nrepeats, 1), ".csv")

# Checks
nfolds*nrepeats == length(tfold_file_paths)
nfolds*nrepeats == length(vfold_file_paths)

# Predictor groups (NOTE THAT THEY ALL INCLUDE THE OUTCOME, "ci")
demographics <- c("ci", "trueage", "residenc", "a1", "f1", "f35", "f41_X2",
                  "f41_X3", "f41_X4", "f41_X5")
iadl <- c("ci","e7", "e8", "e9", "e10", "e11", "e12", "e13", "e14")
adl <- c("ci","e1", "e2", "e3", "e4", "e5", "e6")
diseases <- c("ci", "g15a1", "g15b1", "g15c1", "g15d1", "g15r1")
psych_factors <- c("ci","b21", "b22", "b23", "b24", "b25", "b26", "b27", "b28")
social_hobbies <- c("ci","d11b", "d11c", "d11d", "d11e", "d11f", "d11g", "d11h")
phys_wellbeing <- c("ci","d91", "d92", "g01", "g02")
diet <- c("ci", "d1_X2","d1_X3", "d1_X4", "d1_X5","d31", "d32", 
          "d34_X2", "d34_X3", "d34_X4", "d34_X5", "d34_X6", "d81", 
          "d85_X2", "d85_X3", "d85_X4", "d85_X5", "d85_X6","d86",
          "d4meat2", "d4fish2", "d4egg2", "d4suga2", "d4tea2") 
cognition <- c("ci", "mmse_orientation", "mmse_naming_foods", "mmse_immediate_recall", 
               "mmse_delayed_recall", "mmse_calculation", "mmse_drawing", "mmse_language")

# Predictors from existing models
hu_2021 <- c("ci","trueage", "mmse_score_baseline", "hu_iadl", "hu_f41") 
zhou_2020 <- c("ci","trueage", "zhou_adl_iadl", "mmse_score_baseline", 
               "g24", "g1", "g15d1", "zhou_tv", "zhou_gard_pets") 
wang_2022 <- c("ci","a1", "wang_edu", "wang_age" ,"wang_adl", "mmse_score_baseline", 
               "d11c", "d11d", "d11f", "d11g")

# Helper functions ----
reduce_glm <- function(glm_object){
  
  # Reduces the size of a glm model so it doesn't take up so much RAM
  #   - Object size will be roughly constant regardless of training data size
  #   - Allows for predict(), but not summary()
  
  glm_object$y = c()
  glm_object$model = c()
  
  glm_object$residuals = c()
  glm_object$fitted.values = c()
  glm_object$effects = c()
  glm_object$qr$qr = c()  
  glm_object$linear.predictors = c()
  glm_object$weights = c()
  glm_object$prior.weights = c()
  glm_object$data = c()
  
  glm_object$family$variance = c()
  glm_object$family$dev.resids = c()
  glm_object$family$aic = c()
  glm_object$family$validmu = c()
  glm_object$family$simulate = c()
  attr(glm_object$terms,".Environment") = c()
  attr(glm_object$formula,".Environment") = c()
  
  glm_object
}


# Initialize C-statistics/AUC ----
# For everyone
auc_dems_all <- numeric(nfolds*nrepeats)
auc_iadl_all <- numeric(nfolds*nrepeats)
auc_adl_all <- numeric(nfolds*nrepeats)
auc_dis_all <- numeric(nfolds*nrepeats)
auc_psych_all <- numeric(nfolds*nrepeats)
auc_social_all <- numeric(nfolds*nrepeats)
auc_exsl_all <- numeric(nfolds*nrepeats)
auc_diet_all <- numeric(nfolds*nrepeats)
auc_cogn_all <- numeric(nfolds*nrepeats)
auc_hu_all <- numeric(nfolds*nrepeats)
auc_zhou_all <- numeric(nfolds*nrepeats)
auc_wang_all <- numeric(nfolds*nrepeats)

# For men
auc_dems_men <- numeric(nfolds*nrepeats)
auc_iadl_men <- numeric(nfolds*nrepeats)
auc_adl_men <- numeric(nfolds*nrepeats)
auc_dis_men <- numeric(nfolds*nrepeats)
auc_psych_men <- numeric(nfolds*nrepeats)
auc_social_men <- numeric(nfolds*nrepeats)
auc_exsl_men <- numeric(nfolds*nrepeats)
auc_diet_men <- numeric(nfolds*nrepeats)
auc_cogn_men <- numeric(nfolds*nrepeats)
auc_hu_men <- numeric(nfolds*nrepeats)
auc_zhou_men <- numeric(nfolds*nrepeats)
auc_wang_men <- numeric(nfolds*nrepeats)

# For women
auc_dems_women <- numeric(nfolds*nrepeats)
auc_iadl_women <- numeric(nfolds*nrepeats)
auc_adl_women <- numeric(nfolds*nrepeats)
auc_dis_women <- numeric(nfolds*nrepeats)
auc_psych_women <- numeric(nfolds*nrepeats)
auc_social_women <- numeric(nfolds*nrepeats)
auc_exsl_women <- numeric(nfolds*nrepeats)
auc_diet_women <- numeric(nfolds*nrepeats)
auc_cogn_women <- numeric(nfolds*nrepeats)
auc_hu_women <- numeric(nfolds*nrepeats)
auc_zhou_women <- numeric(nfolds*nrepeats)
auc_wang_women <- numeric(nfolds*nrepeats)

# For rural
auc_dems_rural <- numeric(nfolds*nrepeats)
auc_iadl_rural <- numeric(nfolds*nrepeats)
auc_adl_rural <- numeric(nfolds*nrepeats)
auc_dis_rural <- numeric(nfolds*nrepeats)
auc_psych_rural <- numeric(nfolds*nrepeats)
auc_social_rural <- numeric(nfolds*nrepeats)
auc_exsl_rural <- numeric(nfolds*nrepeats)
auc_diet_rural <- numeric(nfolds*nrepeats)
auc_cogn_rural <- numeric(nfolds*nrepeats)
auc_hu_rural <- numeric(nfolds*nrepeats)
auc_zhou_rural <- numeric(nfolds*nrepeats)
auc_wang_rural <- numeric(nfolds*nrepeats)

# For urban
auc_dems_urban <- numeric(nfolds*nrepeats)
auc_iadl_urban <- numeric(nfolds*nrepeats)
auc_adl_urban <- numeric(nfolds*nrepeats)
auc_dis_urban <- numeric(nfolds*nrepeats)
auc_psych_urban <- numeric(nfolds*nrepeats)
auc_social_urban <- numeric(nfolds*nrepeats)
auc_exsl_urban <- numeric(nfolds*nrepeats)
auc_diet_urban <- numeric(nfolds*nrepeats)
auc_cogn_urban <- numeric(nfolds*nrepeats)
auc_hu_urban <- numeric(nfolds*nrepeats)
auc_zhou_urban <- numeric(nfolds*nrepeats)
auc_wang_urban <- numeric(nfolds*nrepeats)

# For those with no education
auc_dems_edu0 <- numeric(nfolds*nrepeats)
auc_iadl_edu0 <- numeric(nfolds*nrepeats)
auc_adl_edu0 <- numeric(nfolds*nrepeats)
auc_dis_edu0 <- numeric(nfolds*nrepeats)
auc_psych_edu0 <- numeric(nfolds*nrepeats)
auc_social_edu0 <- numeric(nfolds*nrepeats)
auc_exsl_edu0 <- numeric(nfolds*nrepeats)
auc_diet_edu0 <- numeric(nfolds*nrepeats)
auc_cogn_edu0 <- numeric(nfolds*nrepeats)
auc_hu_edu0 <- numeric(nfolds*nrepeats)
auc_zhou_edu0 <- numeric(nfolds*nrepeats)
auc_wang_edu0 <- numeric(nfolds*nrepeats)

# For those with some education
auc_dems_edu1 <- numeric(nfolds*nrepeats)
auc_iadl_edu1 <- numeric(nfolds*nrepeats)
auc_adl_edu1 <- numeric(nfolds*nrepeats)
auc_dis_edu1 <- numeric(nfolds*nrepeats)
auc_psych_edu1 <- numeric(nfolds*nrepeats)
auc_social_edu1 <- numeric(nfolds*nrepeats)
auc_exsl_edu1 <- numeric(nfolds*nrepeats)
auc_diet_edu1 <- numeric(nfolds*nrepeats)
auc_cogn_edu1 <- numeric(nfolds*nrepeats)
auc_hu_edu1 <- numeric(nfolds*nrepeats)
auc_zhou_edu1 <- numeric(nfolds*nrepeats)
auc_wang_edu1 <- numeric(nfolds*nrepeats)

# Initialize sens/spec metrics ---- 
# For everyone 
sens_dems_all <- spec_dems_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_iadl_all <- spec_iadl_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_adl_all <- spec_adl_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_dis_all <- spec_dis_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_psych_all <- spec_psych_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_social_all <- spec_social_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_exsl_all <- spec_exsl_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_diet_all <- spec_diet_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_cogn_all <- spec_cogn_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_hu_all <- spec_hu_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_zhou_all <- spec_zhou_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
sens_wang_all <- spec_wang_all <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)

groups_dems_all <- numeric(nfolds*nrepeats) # grouping variable for the upcoming plot
groups_iadl_all <- numeric(nfolds*nrepeats)
groups_adl_all <- numeric(nfolds*nrepeats)
groups_dis_all <- numeric(nfolds*nrepeats)
groups_psych_all <- numeric(nfolds*nrepeats)
groups_social_all <- numeric(nfolds*nrepeats)
groups_exsl_all <- numeric(nfolds*nrepeats)
groups_diet_all <- numeric(nfolds*nrepeats)
groups_cogn_all <- numeric(nfolds*nrepeats)
groups_hu_all <- numeric(nfolds*nrepeats)
groups_zhou_all <- numeric(nfolds*nrepeats)
groups_wang_all <- numeric(nfolds*nrepeats)

# Loop start ----
for(file in 1:length(tfold_file_paths)){
  
  # Read in the training and validation folds
  tfold <- read_csv(tfold_file_paths[file], show_col_types = FALSE)
  vfold <- read_csv(vfold_file_paths[file], show_col_types = FALSE) 
  
  # Loop glms ----
  # Train each model
  glm_dems <- reduce_glm(glm(ci ~., data = tfold[,demographics], family = binomial()))     # Demographics
  glm_iadl <- reduce_glm(glm(ci ~., data = tfold[,iadl], family = binomial()))             # IADLs
  glm_adl <- reduce_glm(glm(ci ~., data = tfold[,adl], family = binomial()))               # ADLs
  glm_dis <- reduce_glm(glm(ci ~., data = tfold[,diseases], family = binomial()))          # Chronic diseases
  glm_psych <- reduce_glm(glm(ci ~., data = tfold[,psych_factors], family = binomial()))   # Psychological factors
  glm_social <- reduce_glm(glm(ci ~., data = tfold[,social_hobbies], family = binomial())) # Social activities and hobbies
  glm_exsl <- reduce_glm(glm(ci ~., data = tfold[,phys_wellbeing], family = binomial()))   # Exercise and sleep
  glm_diet <- reduce_glm(glm(ci ~., data = tfold[,diet], family = binomial()))             # Diet
  glm_cogn <- reduce_glm(glm(ci ~., data = tfold[,cognition], family = binomial()))        # Cognition
  glm_hu <- reduce_glm(glm(ci ~., data = tfold[,hu_2021], family = binomial()))            # Hu (2021)
  glm_zhou <- reduce_glm(glm(ci ~., data = tfold[,zhou_2020], family = binomial()))        # Zhou (2020)
  glm_wang <- reduce_glm(glm(ci ~., data = tfold[,wang_2022], family = binomial()))        # Wang (2022)
  
  # Loop preds ----
  # Get predictions on each data subset for each model
  # For everyone
  glm_dems_preds_all <- predict(glm_dems, newdata = vfold, type = 'response')
  glm_iadl_preds_all <- predict(glm_iadl, newdata = vfold, type = 'response')
  glm_adl_preds_all <- predict(glm_adl, newdata = vfold, type = 'response')
  glm_dis_preds_all <- predict(glm_dis, newdata = vfold, type = 'response')
  glm_psych_preds_all <- predict(glm_psych, newdata = vfold, type = 'response')
  glm_social_preds_all <- predict(glm_social, newdata = vfold, type = 'response')
  glm_exsl_preds_all <- predict(glm_exsl, newdata = vfold, type = 'response')
  glm_diet_preds_all <- predict(glm_diet, newdata = vfold, type = 'response')
  glm_cogn_preds_all <- predict(glm_cogn, newdata = vfold, type = 'response')
  glm_hu_preds_all <- predict(glm_hu, newdata = vfold, type = 'response')
  glm_zhou_preds_all <- predict(glm_zhou, newdata = vfold, type = 'response')
  glm_wang_preds_all <- predict(glm_wang, newdata = vfold, type = 'response')
  
  # For men (a1 == 0, because we did -1 in the preprocessing)
  glm_dems_preds_men <- predict(glm_dems, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_iadl_preds_men <- predict(glm_iadl, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_adl_preds_men <- predict(glm_adl, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_dis_preds_men <- predict(glm_dis, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_psych_preds_men <- predict(glm_psych, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_social_preds_men <- predict(glm_social, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_exsl_preds_men <- predict(glm_exsl, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_diet_preds_men <- predict(glm_diet, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_cogn_preds_men <- predict(glm_cogn, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_hu_preds_men <- predict(glm_hu, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_zhou_preds_men <- predict(glm_zhou, newdata = vfold[vfold$a1 == 0,], type = 'response')
  glm_wang_preds_men <- predict(glm_wang, newdata = vfold[vfold$a1 == 0,], type = 'response')
  
  # For women (a1 == 1, because we did -1 in the preprocessing)
  glm_dems_preds_women <- predict(glm_dems, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_iadl_preds_women <- predict(glm_iadl, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_adl_preds_women <- predict(glm_adl, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_dis_preds_women <- predict(glm_dis, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_psych_preds_women <- predict(glm_psych, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_social_preds_women <- predict(glm_social, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_exsl_preds_women <- predict(glm_exsl, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_diet_preds_women <- predict(glm_diet, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_cogn_preds_women <- predict(glm_cogn, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_hu_preds_women <- predict(glm_hu, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_zhou_preds_women <- predict(glm_zhou, newdata = vfold[vfold$a1 == 1,], type = 'response')
  glm_wang_preds_women <- predict(glm_wang, newdata = vfold[vfold$a1 == 1,], type = 'response')
  
  # For rural (residenc == 3)
  glm_dems_preds_rural <- predict(glm_dems, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_iadl_preds_rural <- predict(glm_iadl, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_adl_preds_rural <- predict(glm_adl, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_dis_preds_rural <- predict(glm_dis, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_psych_preds_rural <- predict(glm_psych, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_social_preds_rural <- predict(glm_social, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_exsl_preds_rural <- predict(glm_exsl, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_diet_preds_rural <- predict(glm_diet, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_cogn_preds_rural <- predict(glm_cogn, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_hu_preds_rural <- predict(glm_hu, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_zhou_preds_rural <- predict(glm_zhou, newdata = vfold[vfold$residenc == 3,], type = 'response')
  glm_wang_preds_rural <- predict(glm_wang, newdata = vfold[vfold$residenc == 3,], type = 'response')
  
  # For urban (residenc %in% c(1,2)) meaning towns and cities are counted in the calculation
  glm_dems_preds_urban <- predict(glm_dems, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_iadl_preds_urban <- predict(glm_iadl, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_adl_preds_urban <- predict(glm_adl, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_dis_preds_urban <- predict(glm_dis, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_psych_preds_urban <- predict(glm_psych, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_social_preds_urban <- predict(glm_social, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_exsl_preds_urban <- predict(glm_exsl, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_diet_preds_urban <- predict(glm_diet, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_cogn_preds_urban <- predict(glm_cogn, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_hu_preds_urban <- predict(glm_hu, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_zhou_preds_urban <- predict(glm_zhou, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  glm_wang_preds_urban <- predict(glm_wang, newdata = vfold[vfold$residenc %in% c(1,2),], type = 'response')
  
  # For those with no education
  glm_dems_preds_edu0 <- predict(glm_dems, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_iadl_preds_edu0 <- predict(glm_iadl, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_adl_preds_edu0 <- predict(glm_adl, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_dis_preds_edu0 <- predict(glm_dis, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_psych_preds_edu0 <- predict(glm_psych, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_social_preds_edu0 <- predict(glm_social, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_exsl_preds_edu0 <- predict(glm_exsl, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_diet_preds_edu0 <- predict(glm_diet, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_cogn_preds_edu0 <- predict(glm_cogn, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_hu_preds_edu0 <- predict(glm_hu, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_zhou_preds_edu0 <- predict(glm_zhou, newdata = vfold[vfold$f1 == 0,], type = 'response')
  glm_wang_preds_edu0 <- predict(glm_wang, newdata = vfold[vfold$f1 == 0,], type = 'response')
  
  # For those with some education
  glm_dems_preds_edu1 <- predict(glm_dems, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_iadl_preds_edu1 <- predict(glm_iadl, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_adl_preds_edu1 <- predict(glm_adl, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_dis_preds_edu1 <- predict(glm_dis, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_psych_preds_edu1 <- predict(glm_psych, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_social_preds_edu1 <- predict(glm_social, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_exsl_preds_edu1 <- predict(glm_exsl, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_diet_preds_edu1 <- predict(glm_diet, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_cogn_preds_edu1 <- predict(glm_cogn, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_hu_preds_edu1 <- predict(glm_hu, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_zhou_preds_edu1 <- predict(glm_zhou, newdata = vfold[vfold$f1 > 0,], type = 'response')
  glm_wang_preds_edu1 <- predict(glm_wang, newdata = vfold[vfold$f1 > 0,], type = 'response')
    
  # Get labeled outcomes
  y <- vfold$ci
  y_men <- vfold[vfold$a1 == 0,]$ci
  y_women <- vfold[vfold$a1 == 1,]$ci
  y_rural <- vfold[vfold$residenc == 3,]$ci
  y_urban <- vfold[vfold$residenc %in% c(1,2),]$ci
  y_edu0 <- vfold[vfold$f1 == 0,]$ci
  y_edu1 <- vfold[vfold$f1 > 0,]$ci
  
  # Loop AUCS ----
  # Get AUCs (C-statistics) for everyone 
  # For everyone 
  auc_dems_all[file] <- auc(y, glm_dems_preds_all, direction = "<")
  auc_iadl_all[file] <- auc(y, glm_iadl_preds_all, direction = "<")
  auc_adl_all[file] <- auc(y, glm_adl_preds_all, direction = "<")  
  auc_dis_all[file] <- auc(y, glm_dis_preds_all, direction = "<")  
  auc_psych_all[file] <- auc(y, glm_psych_preds_all, direction = "<")  
  auc_social_all[file] <- auc(y, glm_social_preds_all, direction = "<")   
  auc_exsl_all[file] <- auc(y, glm_exsl_preds_all, direction = "<")  
  auc_diet_all[file] <- auc(y, glm_diet_preds_all, direction = "<")   
  auc_cogn_all[file] <- auc(y, glm_cogn_preds_all, direction = "<")  
  auc_hu_all[file] <- auc(y, glm_hu_preds_all, direction = "<")
  auc_zhou_all[file] <- auc(y, glm_zhou_preds_all, direction = "<")
  auc_wang_all[file] <- auc(y, glm_wang_preds_all, direction = "<")
  
  # For men (a1 == 0)
  auc_dems_men[file] <- auc(y_men, glm_dems_preds_men, direction = "<")
  auc_iadl_men[file] <- auc(y_men, glm_iadl_preds_men, direction = "<")
  auc_adl_men[file] <- auc(y_men, glm_adl_preds_men, direction = "<")  
  auc_dis_men[file] <- auc(y_men, glm_dis_preds_men, direction = "<")  
  auc_psych_men[file] <- auc(y_men, glm_psych_preds_men, direction = "<")  
  auc_social_men[file] <- auc(y_men, glm_social_preds_men, direction = "<")   
  auc_exsl_men[file] <- auc(y_men, glm_exsl_preds_men, direction = "<")  
  auc_diet_men[file] <- auc(y_men, glm_diet_preds_men, direction = "<")   
  auc_cogn_men[file] <- auc(y_men, glm_cogn_preds_men, direction = "<") 
  auc_hu_men[file] <- auc(y_men, glm_hu_preds_men, direction = "<")
  auc_zhou_men[file] <- auc(y_men, glm_zhou_preds_men, direction = "<")
  auc_wang_men[file] <- auc(y_men, glm_wang_preds_men, direction = "<")
  
  # For women (a1 == 1)
  auc_dems_women[file] <- auc(y_women, glm_dems_preds_women, direction = "<")
  auc_iadl_women[file] <- auc(y_women, glm_iadl_preds_women, direction = "<")
  auc_adl_women[file] <- auc(y_women, glm_adl_preds_women, direction = "<")  
  auc_dis_women[file] <- auc(y_women, glm_dis_preds_women, direction = "<")  
  auc_psych_women[file] <- auc(y_women, glm_psych_preds_women, direction = "<")  
  auc_social_women[file] <- auc(y_women, glm_social_preds_women, direction = "<")   
  auc_exsl_women[file] <- auc(y_women, glm_exsl_preds_women, direction = "<")  
  auc_diet_women[file] <- auc(y_women, glm_diet_preds_women, direction = "<")   
  auc_cogn_women[file] <- auc(y_women, glm_cogn_preds_women, direction = "<") 
  auc_hu_women[file] <- auc(y_women, glm_hu_preds_women, direction = "<")
  auc_zhou_women[file] <- auc(y_women, glm_zhou_preds_women, direction = "<")
  auc_wang_women[file] <- auc(y_women, glm_wang_preds_women, direction = "<")
  
  # For rural elderly (residenc == 3)
  auc_dems_rural[file] <- auc(y_rural, glm_dems_preds_rural, direction = "<")
  auc_iadl_rural[file] <- auc(y_rural, glm_iadl_preds_rural, direction = "<")
  auc_adl_rural[file] <- auc(y_rural, glm_adl_preds_rural, direction = "<")  
  auc_dis_rural[file] <- auc(y_rural, glm_dis_preds_rural, direction = "<")  
  auc_psych_rural[file] <- auc(y_rural, glm_psych_preds_rural, direction = "<")  
  auc_social_rural[file] <- auc(y_rural, glm_social_preds_rural, direction = "<")   
  auc_exsl_rural[file] <- auc(y_rural, glm_exsl_preds_rural, direction = "<")  
  auc_diet_rural[file] <- auc(y_rural, glm_diet_preds_rural, direction = "<")   
  auc_cogn_rural[file] <- auc(y_rural, glm_cogn_preds_rural, direction = "<") 
  auc_hu_rural[file] <- auc(y_rural, glm_hu_preds_rural, direction = "<")
  auc_zhou_rural[file] <- auc(y_rural, glm_zhou_preds_rural, direction = "<")
  auc_wang_rural[file] <- auc(y_rural, glm_wang_preds_rural, direction = "<")
  
  # For urban (residenc %in% c(1,2)) meaning towns and cities are counted in the calculation
  auc_dems_urban[file] <- auc(y_urban, glm_dems_preds_urban, direction = "<")
  auc_iadl_urban[file] <- auc(y_urban, glm_iadl_preds_urban, direction = "<")
  auc_adl_urban[file] <- auc(y_urban, glm_adl_preds_urban, direction = "<")  
  auc_dis_urban[file] <- auc(y_urban, glm_dis_preds_urban, direction = "<")  
  auc_psych_urban[file] <- auc(y_urban, glm_psych_preds_urban, direction = "<")  
  auc_social_urban[file] <- auc(y_urban, glm_social_preds_urban, direction = "<")   
  auc_exsl_urban[file] <- auc(y_urban, glm_exsl_preds_urban, direction = "<")  
  auc_diet_urban[file] <- auc(y_urban, glm_diet_preds_urban, direction = "<")   
  auc_cogn_urban[file] <- auc(y_urban, glm_cogn_preds_urban, direction = "<") 
  auc_hu_urban[file] <- auc(y_urban, glm_hu_preds_urban, direction = "<")
  auc_zhou_urban[file] <- auc(y_urban, glm_zhou_preds_urban, direction = "<")
  auc_wang_urban[file] <- auc(y_urban, glm_wang_preds_urban, direction = "<")
  
  # For those with no education (f1 == 0)
  auc_dems_edu0[file] <- auc(y_edu0, glm_dems_preds_edu0, direction = "<")
  auc_iadl_edu0[file] <- auc(y_edu0, glm_iadl_preds_edu0, direction = "<")
  auc_adl_edu0[file] <- auc(y_edu0, glm_adl_preds_edu0, direction = "<")  
  auc_dis_edu0[file] <- auc(y_edu0, glm_dis_preds_edu0, direction = "<")  
  auc_psych_edu0[file] <- auc(y_edu0, glm_psych_preds_edu0, direction = "<")  
  auc_social_edu0[file] <- auc(y_edu0, glm_social_preds_edu0, direction = "<")   
  auc_exsl_edu0[file] <- auc(y_edu0, glm_exsl_preds_edu0, direction = "<")  
  auc_diet_edu0[file] <- auc(y_edu0, glm_diet_preds_edu0, direction = "<")   
  auc_cogn_edu0[file] <- auc(y_edu0, glm_cogn_preds_edu0, direction = "<") 
  auc_hu_edu0[file] <- auc(y_edu0, glm_hu_preds_edu0, direction = "<")
  auc_zhou_edu0[file] <- auc(y_edu0, glm_zhou_preds_edu0, direction = "<")
  auc_wang_edu0[file] <- auc(y_edu0, glm_wang_preds_edu0, direction = "<")
  
  # For those with some education (f1 > 0)
  auc_dems_edu1[file] <- auc(y_edu1, glm_dems_preds_edu1, direction = "<")
  auc_iadl_edu1[file] <- auc(y_edu1, glm_iadl_preds_edu1, direction = "<")
  auc_adl_edu1[file] <- auc(y_edu1, glm_adl_preds_edu1, direction = "<")  
  auc_dis_edu1[file] <- auc(y_edu1, glm_dis_preds_edu1, direction = "<")  
  auc_psych_edu1[file] <- auc(y_edu1, glm_psych_preds_edu1, direction = "<")  
  auc_social_edu1[file] <- auc(y_edu1, glm_social_preds_edu1, direction = "<")   
  auc_exsl_edu1[file] <- auc(y_edu1, glm_exsl_preds_edu1, direction = "<")  
  auc_diet_edu1[file] <- auc(y_edu1, glm_diet_preds_edu1, direction = "<")   
  auc_cogn_edu1[file] <- auc(y_edu1, glm_cogn_preds_edu1, direction = "<") 
  auc_hu_edu1[file] <- auc(y_edu1, glm_hu_preds_edu1, direction = "<")
  auc_zhou_edu1[file] <- auc(y_edu1, glm_zhou_preds_edu1, direction = "<")
  auc_wang_edu1[file] <- auc(y_edu1, glm_wang_preds_edu1, direction = "<")
  
  # Loop ROC objs ----
  # Get ROC objects 
  # For everyone
  roc_dems_all <- roc(y, glm_dems_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_iadl_all <- roc(y, glm_iadl_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_adl_all <- roc(y, glm_adl_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_dis_all <- roc(y, glm_dis_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_psych_all <- roc(y, glm_psych_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_social_all <- roc(y, glm_social_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_exsl_all <- roc(y, glm_exsl_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_diet_all <- roc(y, glm_diet_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_cogn_all <- roc(y, glm_cogn_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_hu_all <- roc(y, glm_hu_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_zhou_all <- roc(y, glm_zhou_preds_all, direction = "<")[c("sensitivities", "specificities")]
  roc_wang_all <- roc(y, glm_wang_preds_all, direction = "<")[c("sensitivities", "specificities")]
  
  # For men
  roc_dems_men <- roc(y_men, glm_dems_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_iadl_men <- roc(y_men, glm_iadl_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_adl_men <- roc(y_men, glm_adl_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_dis_men <- roc(y_men, glm_dis_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_psych_men <- roc(y_men, glm_psych_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_social_men <- roc(y_men, glm_social_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_exsl_men <- roc(y_men, glm_exsl_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_diet_men <- roc(y_men, glm_diet_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_cogn_men <- roc(y_men, glm_cogn_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_hu_men <- roc(y_men, glm_hu_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_zhou_men <- roc(y_men, glm_zhou_preds_men, direction = "<")[c("sensitivities", "specificities")]
  roc_wang_men <- roc(y_men, glm_wang_preds_men, direction = "<")[c("sensitivities", "specificities")]
  
  # For women
  roc_dems_women <- roc(y_women, glm_dems_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_iadl_women <- roc(y_women, glm_iadl_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_adl_women <- roc(y_women, glm_adl_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_dis_women <- roc(y_women, glm_dis_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_psych_women <- roc(y_women, glm_psych_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_social_women <- roc(y_women, glm_social_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_exsl_women <- roc(y_women, glm_exsl_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_diet_women <- roc(y_women, glm_diet_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_cogn_women <- roc(y_women, glm_cogn_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_hu_women <- roc(y_women, glm_hu_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_zhou_women <- roc(y_women, glm_zhou_preds_women, direction = "<")[c("sensitivities", "specificities")]
  roc_wang_women <- roc(y_women, glm_wang_preds_women, direction = "<")[c("sensitivities", "specificities")]
  
  # For rural
  roc_dems_rural <- roc(y_rural, glm_dems_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_iadl_rural <- roc(y_rural, glm_iadl_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_adl_rural <- roc(y_rural, glm_adl_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_dis_rural <- roc(y_rural, glm_dis_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_psych_rural <- roc(y_rural, glm_psych_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_social_rural <- roc(y_rural, glm_social_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_exsl_rural <- roc(y_rural, glm_exsl_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_diet_rural <- roc(y_rural, glm_diet_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_cogn_rural <- roc(y_rural, glm_cogn_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_hu_rural <- roc(y_rural, glm_hu_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_zhou_rural <- roc(y_rural, glm_zhou_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  roc_wang_rural <- roc(y_rural, glm_wang_preds_rural, direction = "<")[c("sensitivities", "specificities")]
  
  # For urban
  roc_dems_urban <- roc(y_urban, glm_dems_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_iadl_urban <- roc(y_urban, glm_iadl_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_adl_urban <- roc(y_urban, glm_adl_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_dis_urban <- roc(y_urban, glm_dis_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_psych_urban <- roc(y_urban, glm_psych_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_social_urban <- roc(y_urban, glm_social_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_exsl_urban <- roc(y_urban, glm_exsl_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_diet_urban <- roc(y_urban, glm_diet_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_cogn_urban <- roc(y_urban, glm_cogn_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_hu_urban <- roc(y_urban, glm_hu_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_zhou_urban <- roc(y_urban, glm_zhou_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  roc_wang_urban <- roc(y_urban, glm_wang_preds_urban, direction = "<")[c("sensitivities", "specificities")]
  
  # For no education
  roc_dems_edu0 <- roc(y_edu0, glm_dems_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_iadl_edu0 <- roc(y_edu0, glm_iadl_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_adl_edu0 <- roc(y_edu0, glm_adl_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_dis_edu0 <- roc(y_edu0, glm_dis_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_psych_edu0 <- roc(y_edu0, glm_psych_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_social_edu0 <- roc(y_edu0, glm_social_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_exsl_edu0 <- roc(y_edu0, glm_exsl_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_diet_edu0 <- roc(y_edu0, glm_diet_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_cogn_edu0 <- roc(y_edu0, glm_cogn_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_hu_edu0 <- roc(y_edu0, glm_hu_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_zhou_edu0 <- roc(y_edu0, glm_zhou_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  roc_wang_edu0 <- roc(y_edu0, glm_wang_preds_edu0, direction = "<")[c("sensitivities", "specificities")]
  
  # For some education
  roc_dems_edu1 <- roc(y_edu1, glm_dems_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_iadl_edu1 <- roc(y_edu1, glm_iadl_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_adl_edu1 <- roc(y_edu1, glm_adl_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_dis_edu1 <- roc(y_edu1, glm_dis_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_psych_edu1 <- roc(y_edu1, glm_psych_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_social_edu1 <- roc(y_edu1, glm_social_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_exsl_edu1 <- roc(y_edu1, glm_exsl_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_diet_edu1 <- roc(y_edu1, glm_diet_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_cogn_edu1 <- roc(y_edu1, glm_cogn_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_hu_edu1 <- roc(y_edu1, glm_hu_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_zhou_edu1 <- roc(y_edu1, glm_zhou_preds_edu1, direction = "<")[c("sensitivities", "specificities")]
  roc_wang_edu1 <- roc(y_edu1, glm_wang_preds_edu1, direction = "<")[c("sensitivities", "specificities")]

  # Loop sens/spec ----
  # Get sensitivity and specificity vectors
  # Note we populate the unused column entries with NA
  # This is because the cut points for the sensitivity and specificity vectors vary 
  # Thus we needed to pad the end of each column with NA values
  
  # For everyone
  sens_dems_all[,file] <- c(roc_dems_all$sensitivities, rep(NA, mat_nrow - length(roc_dems_all$sensitivities)))
  sens_iadl_all[,file] <- c(roc_iadl_all$sensitivities, rep(NA, mat_nrow - length(roc_iadl_all$sensitivities)))
  sens_adl_all[,file] <- c(roc_adl_all$sensitivities, rep(NA, mat_nrow - length(roc_adl_all$sensitivities)))
  sens_dis_all[,file] <- c(roc_dis_all$sensitivities, rep(NA, mat_nrow - length(roc_dis_all$sensitivities)))
  sens_psych_all[,file] <- c(roc_psych_all$sensitivities, rep(NA, mat_nrow - length(roc_psych_all$sensitivities)))
  sens_social_all[,file] <- c(roc_social_all$sensitivities, rep(NA, mat_nrow - length(roc_social_all$sensitivities)))
  sens_exsl_all[,file] <- c(roc_exsl_all$sensitivities, rep(NA, mat_nrow - length(roc_exsl_all$sensitivities)))
  sens_diet_all[,file] <- c(roc_diet_all$sensitivities, rep(NA, mat_nrow - length(roc_diet_all$sensitivities)))
  sens_cogn_all[,file] <- c(roc_cogn_all$sensitivities, rep(NA, mat_nrow - length(roc_cogn_all$sensitivities)))
  sens_hu_all[,file] <- c(roc_hu_all$sensitivities, rep(NA, mat_nrow - length(roc_hu_all$sensitivities)))
  sens_zhou_all[,file] <- c(roc_zhou_all$sensitivities, rep(NA, mat_nrow - length(roc_zhou_all$sensitivities)))
  sens_wang_all[,file] <- c(roc_wang_all$sensitivities, rep(NA, mat_nrow - length(roc_wang_all$sensitivities)))
  
  spec_dems_all[,file] <- c(roc_dems_all$specificities, rep(NA, mat_nrow - length(roc_dems_all$specificities)))
  spec_iadl_all[,file] <- c(roc_iadl_all$specificities, rep(NA, mat_nrow - length(roc_iadl_all$specificities)))
  spec_adl_all[,file] <- c(roc_adl_all$specificities, rep(NA, mat_nrow - length(roc_adl_all$specificities)))
  spec_dis_all[,file] <- c(roc_dis_all$specificities, rep(NA, mat_nrow - length(roc_dis_all$specificities)))
  spec_psych_all[,file] <- c(roc_psych_all$specificities, rep(NA, mat_nrow - length(roc_psych_all$specificities)))
  spec_social_all[,file] <- c(roc_social_all$specificities, rep(NA, mat_nrow - length(roc_social_all$specificities)))
  spec_exsl_all[,file] <- c(roc_exsl_all$specificities, rep(NA, mat_nrow - length(roc_exsl_all$specificities)))
  spec_diet_all[,file] <- c(roc_diet_all$specificities, rep(NA, mat_nrow - length(roc_diet_all$specificities)))
  spec_cogn_all[,file] <- c(roc_cogn_all$specificities, rep(NA, mat_nrow - length(roc_cogn_all$specificities)))
  spec_hu_all[,file] <- c(roc_hu_all$specificities, rep(NA, mat_nrow - length(roc_hu_all$specificities)))
  spec_zhou_all[,file] <- c(roc_zhou_all$specificities, rep(NA, mat_nrow - length(roc_zhou_all$specificities)))
  spec_wang_all[,file] <- c(roc_wang_all$specificities, rep(NA, mat_nrow - length(roc_wang_all$specificities)))
  
  groups_dems_all[file] <- length(roc_dems_all$sensitivities) # Grouping variable
  groups_iadl_all[file] <- length(roc_iadl_all$sensitivities)
  groups_adl_all[file] <- length(roc_adl_all$sensitivities)
  groups_dis_all[file] <- length(roc_dis_all$sensitivities)
  groups_psych_all[file] <- length(roc_psych_all$sensitivities)
  groups_social_all[file] <- length(roc_social_all$sensitivities)
  groups_exsl_all[file] <- length(roc_exsl_all$sensitivities)
  groups_diet_all[file] <- length(roc_diet_all$sensitivities)
  groups_cogn_all[file] <- length(roc_cogn_all$sensitivities)
  groups_hu_all[file] <- length(roc_hu_all$sensitivities)
  groups_zhou_all[file] <- length(roc_zhou_all$sensitivities)
  groups_wang_all[file] <- length(roc_wang_all$sensitivities)
  
}

# Save AUCs ----
# Save AUC results in one data set
df_aucs <- data.frame(
  auc_dems_all, auc_dems_men, auc_dems_women, auc_dems_rural, auc_dems_urban, auc_dems_edu0, auc_dems_edu1,
  auc_iadl_all, auc_iadl_men, auc_iadl_women, auc_iadl_rural, auc_iadl_urban, auc_iadl_edu0, auc_iadl_edu1,
  auc_adl_all, auc_adl_men, auc_adl_women, auc_adl_rural, auc_adl_urban, auc_adl_edu0, auc_adl_edu1,
  auc_dis_all, auc_dis_men, auc_dis_women, auc_dis_rural, auc_dis_urban, auc_dis_edu0, auc_dis_edu1,
  auc_psych_all, auc_psych_men, auc_psych_women, auc_psych_rural, auc_psych_urban, auc_psych_edu0, auc_psych_edu1,
  auc_social_all, auc_social_men, auc_social_women, auc_social_rural, auc_social_urban, auc_social_edu0, auc_social_edu1,
  auc_exsl_all, auc_exsl_men, auc_exsl_women, auc_exsl_rural, auc_exsl_urban, auc_exsl_edu0, auc_exsl_edu1,
  auc_diet_all, auc_diet_men, auc_diet_women, auc_diet_rural, auc_diet_urban, auc_diet_edu0, auc_diet_edu1,
  auc_cogn_all, auc_cogn_men, auc_cogn_women, auc_cogn_rural, auc_cogn_urban, auc_cogn_edu0, auc_cogn_edu1
  )

write.csv(df_aucs, "Data/Derived-DFs/df-aucs.csv", row.names = FALSE)

df_aucs_models <- data.frame(auc_hu_all, auc_hu_men, auc_hu_women, auc_hu_rural, auc_hu_urban, auc_hu_edu0, auc_hu_edu1,
                             auc_zhou_all, auc_zhou_men, auc_zhou_women, auc_zhou_rural, auc_zhou_urban, auc_zhou_edu0, auc_zhou_edu1,
                             auc_wang_all, auc_wang_men, auc_wang_women, auc_wang_rural, auc_wang_urban, auc_wang_edu0, auc_wang_edu1
                             )

write.csv(df_aucs_models, "Data/Derived-DFs/df-aucs-models.csv", row.names = FALSE)

# Wrangling sens/spec matrices ----
# Flatten (columns purposefully too large) and group sensitivity, specificity matrices
# For everyone
sens_dems_all <- sens_dems_all[is.na(sens_dems_all) == FALSE] # Will flatten & remove NAs 
sens_iadl_all <- sens_iadl_all[is.na(sens_iadl_all) == FALSE]
sens_adl_all <- sens_adl_all[is.na(sens_adl_all) == FALSE]
sens_dis_all <- sens_dis_all[is.na(sens_dis_all) == FALSE]
sens_psych_all <- sens_psych_all[is.na(sens_psych_all) == FALSE]
sens_social_all <- sens_social_all[is.na(sens_social_all) == FALSE]
sens_exsl_all <- sens_exsl_all[is.na(sens_exsl_all) == FALSE]
sens_diet_all <- sens_diet_all[is.na(sens_diet_all) == FALSE]
sens_cogn_all <- sens_cogn_all[is.na(sens_cogn_all) == FALSE]
sens_hu_all <- sens_hu_all[is.na(sens_hu_all) == FALSE]
sens_zhou_all <- sens_zhou_all[is.na(sens_zhou_all) == FALSE]
sens_wang_all <- sens_wang_all[is.na(sens_wang_all) == FALSE]

spec_dems_all <- spec_dems_all[is.na(spec_dems_all) == FALSE] # Will flatten & remove NAs 
spec_iadl_all <- spec_iadl_all[is.na(spec_iadl_all) == FALSE]
spec_adl_all <- spec_adl_all[is.na(spec_adl_all) == FALSE]
spec_dis_all <- spec_dis_all[is.na(spec_dis_all) == FALSE]
spec_psych_all <- spec_psych_all[is.na(spec_psych_all) == FALSE]
spec_social_all <- spec_social_all[is.na(spec_social_all) == FALSE]
spec_exsl_all <- spec_exsl_all[is.na(spec_exsl_all) == FALSE]
spec_diet_all <- spec_diet_all[is.na(spec_diet_all) == FALSE]
spec_cogn_all <- spec_cogn_all[is.na(spec_cogn_all) == FALSE]
spec_hu_all <- spec_hu_all[is.na(spec_hu_all) == FALSE]
spec_zhou_all <- spec_zhou_all[is.na(spec_zhou_all) == FALSE]
spec_wang_all <- spec_wang_all[is.na(spec_wang_all) == FALSE]

# Getting vectors of the grouping variables
# Have not added all hu, zhou, wang stuff from here down ----

# For everyone
groups_dems_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_iadl_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_adl_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_dis_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_psych_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_social_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_exsl_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_diet_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)
groups_cogn_all_mat <- matrix(nrow = mat_nrow, ncol = nfolds*nrepeats)

# Populate matrices
group_num <- 1
for(i in 1:(nfolds*nrepeats)){
  
  # For everyone
  groups_dems_all_mat[,i] <- c(rep(group_num, groups_dems_all[i]), rep(NA, mat_nrow - groups_dems_all[i]))
  groups_iadl_all_mat[,i] <- c(rep(group_num, groups_iadl_all[i]), rep(NA, mat_nrow - groups_iadl_all[i]))
  groups_adl_all_mat[,i] <- c(rep(group_num, groups_adl_all[i]), rep(NA, mat_nrow - groups_adl_all[i]))
  groups_dis_all_mat[,i] <- c(rep(group_num, groups_dis_all[i]), rep(NA, mat_nrow - groups_dis_all[i]))
  groups_psych_all_mat[,i] <- c(rep(group_num, groups_psych_all[i]), rep(NA, mat_nrow - groups_psych_all[i]))
  groups_social_all_mat[,i] <- c(rep(group_num, groups_social_all[i]), rep(NA, mat_nrow - groups_social_all[i]))
  groups_exsl_all_mat[,i] <- c(rep(group_num, groups_exsl_all[i]), rep(NA, mat_nrow - groups_exsl_all[i]))
  groups_diet_all_mat[,i] <- c(rep(group_num, groups_diet_all[i]), rep(NA, mat_nrow - groups_diet_all[i]))
  groups_cogn_all_mat[,i] <- c(rep(group_num, groups_cogn_all[i]), rep(NA, mat_nrow - groups_cogn_all[i]))
  
  # Group number tracker
  group_num <- group_num + 1
  
}

# Flatten and remove NA values
# For everyone
groups_dems_all_vec <- groups_dems_all_mat[is.na(groups_dems_all_mat) == FALSE]
groups_iadl_all_vec <- groups_iadl_all_mat[is.na(groups_iadl_all_mat) == FALSE]
groups_adl_all_vec <- groups_adl_all_mat[is.na(groups_adl_all_mat) == FALSE]
groups_dis_all_vec <- groups_dis_all_mat[is.na(groups_dis_all_mat) == FALSE]
groups_psych_all_vec <- groups_psych_all_mat[is.na(groups_psych_all_mat) == FALSE]
groups_social_all_vec <- groups_social_all_mat[is.na(groups_social_all_mat) == FALSE]
groups_exsl_all_vec <- groups_exsl_all_mat[is.na(groups_exsl_all_mat) == FALSE]
groups_diet_all_vec <- groups_diet_all_mat[is.na(groups_diet_all_mat) == FALSE]
groups_cogn_all_vec <- groups_cogn_all_mat[is.na(groups_cogn_all_mat) == FALSE]

# Combine into data frames

# For everyone
ss_plt_df_dems_all <- data.frame(sens = sens_dems_all, spec = spec_dems_all, groups = groups_dems_all_vec)
ss_plt_df_iadl_all <- data.frame(sens = sens_iadl_all, spec = spec_iadl_all, groups = groups_iadl_all_vec)
ss_plt_df_adl_all <- data.frame(sens = sens_adl_all, spec = spec_adl_all, groups = groups_adl_all_vec)
ss_plt_df_dis_all <- data.frame(sens = sens_dis_all, spec = spec_dis_all, groups = groups_dis_all_vec)
ss_plt_df_psych_all <- data.frame(sens = sens_psych_all, spec = spec_psych_all, groups = groups_psych_all_vec)
ss_plt_df_social_all <- data.frame(sens = sens_social_all, spec = spec_social_all, groups = groups_social_all_vec)
ss_plt_df_exsl_all <- data.frame(sens = sens_exsl_all, spec = spec_exsl_all, groups = groups_exsl_all_vec)
ss_plt_df_diet_all <- data.frame(sens = sens_diet_all, spec = spec_diet_all, groups = groups_diet_all_vec)
ss_plt_df_cogn_all <- data.frame(sens = sens_cogn_all, spec = spec_cogn_all, groups = groups_cogn_all_vec)

# Save dfs
# For everyone
write.csv(ss_plt_df_dems_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/dems-all.csv", row.names = FALSE)
write.csv(ss_plt_df_iadl_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/iadl-all.csv", row.names = FALSE)
write.csv(ss_plt_df_adl_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/adl-all.csv", row.names = FALSE)
write.csv(ss_plt_df_dis_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/dis-all.csv", row.names = FALSE)
write.csv(ss_plt_df_psych_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/psych-all.csv", row.names = FALSE)
write.csv(ss_plt_df_social_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/social-all.csv", row.names = FALSE)
write.csv(ss_plt_df_exsl_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/exsl-all.csv", row.names = FALSE)
write.csv(ss_plt_df_diet_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/diet-all.csv", row.names = FALSE)
write.csv(ss_plt_df_cogn_all, "Data/Derived-DFs/Sensitivity-Specificity-Plot-DFs/cogn-all.csv", row.names = FALSE)

# Quick view of the results
# Sorting the AUC df and ordering it
df_aucs %>% select(ends_with("_all")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs %>% select(ends_with("_men")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs %>% select(ends_with("_women")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs %>% select(ends_with("_rural")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs %>% select(ends_with("_urban")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs %>% select(ends_with("_edu0")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs %>% select(ends_with("_edu1")) %>% colMeans() %>% sort(., decreasing = TRUE)

# Sorting the AUC models df and ordering it
df_aucs_models %>% select(ends_with("_all")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs_models %>% select(ends_with("_men")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs_models %>% select(ends_with("_women")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs_models %>% select(ends_with("_rural")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs_models %>% select(ends_with("_urban")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs_models %>% select(ends_with("_edu0")) %>% colMeans() %>% sort(., decreasing = TRUE)
df_aucs_models %>% select(ends_with("_edu1")) %>% colMeans() %>% sort(., decreasing = TRUE)

