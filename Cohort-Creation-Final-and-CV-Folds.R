# Libraries
library(tidyverse)
library(haven)
library(caret)
library(recipes)

# Initializing values:
mmse_cutoff_1 <- 18 # For those with no formal education (score < cutoff implies impairment)
mmse_cutoff_2 <- 21 # For those with 1-6 years of education (score < cutoff implies impairment)
mmse_cutoff_3 <- 25 # For those with >6 years of education (score < cutoff implies impairment)

nfolds <- 10
nrepeats <- 20
niter <- 1

seed <- 19970507; set.seed(seed)

# Data sets
df <- read_csv("Data/Derived-DFs/df-final.csv")

# Create cross-validation folds
data_recipe <- recipe(ci ~., data = df) %>% 
  step_impute_knn(
    all_predictors(), 
    neighbors = 1,
    impute_with = c(
      "mmse_score_baseline",
      "trueage", "residenc", "a1", "f1", "f35", "f41",                                      
      "e1", "e2", "e3", "e4", "e5", "e6",                  
      "e7", "e8", "e9", "e10", "e11", "e12", "e13", "e14",      
      "g15a1", "g15b1", "g15c1", "g15d1", "g15r1",                                     
      "b21", "b22", "b23", "b24", "b25", "b26", "b27", "b28",   
      "d11b", "d11c", "d11d", "d11e", "d11f", "d11g", "d11h", 
      "d91", "d92",                                 
      "g01", "g02",                              
      "d1", "d31", "d32", "d34", "d81", "d82", "d85", "d86",   
      "d4meat2", "d4fish2", "d4egg2", "d4suga2", "d4tea2",
      "g24",                                     
      "g1")
    ) %>% 
    step_mutate(
      a1 = a1 - 1,
      residenc = as.factor(residenc),
      f41 = as.factor(f41),
      g15a1 = g15a1 - 1,
      g15b1 = g15b1 - 1,
      g15c1 = g15c1 - 1,
      g15d1 = g15d1 - 1,
      g15r1 = g15r1 - 1,
      b28 = b28 - 1,
      d91 = d91 - 1,
      d92 = d92 - 1,
      d1 = as.factor(d1),
      d34 = as.factor(d34),
      d81 = d81 - 1,
      d81 = d82 - 1,
      d85 = as.factor(d85),
      g24 = g24 - 1,
      hu_f41 = as.factor(hu_f41),
      zhou_stroke = zhou_stroke - 1
    ) %>% 
  step_dummy(f41, d1, d34, d85)

# Loop to create folds
for(i in 1:nrepeats){
  
  folds <- createFolds(y = as.factor(df$ci), k = nfolds)
  
  for(k in 1:nfolds){
    
    tfold <- df[-folds[[k]], ]
    vfold <- df[folds[[k]], ]
    
    prepped_recipe <- prep(data_recipe, training = tfold, fresh = TRUE)
    
    tfold <- bake(prepped_recipe, new_data = tfold) 
    vfold <- bake(prepped_recipe, new_data = vfold) 
    
    tfold_filename <- paste0("Data/CV-folds/tfold-", niter, ".csv")
    vfold_filename <- paste0("Data/CV-folds/vfold-", niter, ".csv")
    
    write.csv(tfold, tfold_filename, row.names = FALSE)
    write.csv(vfold, vfold_filename, row.names = FALSE)
    
    niter <- niter + 1
  }
}