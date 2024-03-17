# Cohort Creation for the 2011-2014 data set

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

# Reading in the data
df <- read_sav("Data/CLHLS-dataset-2008-2018/CLHLS-2011-2018.sav")

# MMSE 2011 ----
mmse_df_2011 <- df %>% 
  select(
    id, c11, c12, c13, c14, c15, c16, c21a, c21b, c21c, c31a, c31b, c31c, 
    c31d, c31e, c32, c41a, c41b, c41c, c51a, c51b, c52, c53a, c53b, c53c
  )

# For the food naming question (name as many foods as you can in 1 minute)...
# Code "could not answer" as incorrect
mmse_df_2011$c16 <- ifelse(mmse_df_2011$c16 == 88, 0, mmse_df_2011$c16)

# Code "missing" as NA
mmse_df_2011$c16 <- ifelse(mmse_df_2011$c16 == 99, NA, mmse_df_2011$c16)

# Per the MMSE scoring, all scores >7 reduced to 7 (full points) 
mmse_df_2011$c16 <- ifelse(mmse_df_2011$c16 > 7, 7, mmse_df_2011$c16)

# For all other questions ...
# Now that the max(food naming) = 7, we can fill in values for 
#   8 = could not answer and 9 = missing in all other questions
mmse_df_2011[mmse_df_2011 == 8] <- 0  # Code "could not answer" as incorrect
mmse_df_2011[mmse_df_2011 == 9] <- NA # Code "missing" as NA

# Calculate MMSE scores for 2011
mmse_df_2011 <- mmse_df_2011 %>% 
  mutate(
    mmse_score_2011 = 
      c11+ c12+ c13+ c14+ c15+ c16+ c21a+ c21b+ c21c+ c41a+ c41b+ c41c+ 
      c31a+ c31b+ c31c+ c31d+ c31e+ c32+ c51a+ c51b+ c52+ c53a+ c53b+ c53c
    ) %>% 
  mutate(
    mmse_orientation = c11+ c12+ c13+ c14 +c15,
    mmse_naming_foods = c16,
    mmse_immediate_recall = c21a+ c21b +c21c,
    mmse_delayed_recall = c41a+ c41b +c41c,
    mmse_calculation = c31a+ c31b+ c31c+ c31d+ c31e,
    mmse_drawing = c32,
    mmse_language = c51a+ c51b+ c52+ c53a+ c53b+ c53c
  ) %>% 
  select(id, starts_with("mmse"))

# MMSE 2014 ----
mmse_df_2014 <- df %>% 
  select(
    id, 
    c11_14, c12_14, c13_14, c14_14, c15_14, c16_14, c21a_14, c21b_14, 
    c21c_14, c31a_14, c31b_14, c31c_14, c31d_14, c31e_14, c32_14, 
    c41a_14, c41b_14, c41c_14, c51a_14, c51b_14, c52_14, c53a_14, 
    c53b_14, c53c_14
  )

# For the food naming question (name as many foods as you can in 1 minute)...
# Code "could not answer" as incorrect
mmse_df_2014$c16_14 <- ifelse(mmse_df_2014$c16_14 == 88, 0, mmse_df_2014$c16_14)

# Code "missing" as NA
mmse_df_2014$c16_14 <- ifelse(mmse_df_2014$c16_14 == 99, NA, mmse_df_2014$c16_14)

# Per the MMSE scoring, all scores >7 reduced to 7 (full points) 
mmse_df_2014$c16_14 <- ifelse(mmse_df_2014$c16_14 > 7, 7, mmse_df_2014$c16_14)

# For all other questions ...
# Now that the max(food naming) = 7, we can fill in values for 
#   8 = could not answer and 9 = missing in all other questions
mmse_df_2014[mmse_df_2014 == 8] <- 0  # Code "could not answer" as incorrect
mmse_df_2014[mmse_df_2014 == 9] <- NA # Code "missing" as NA

# Calculate MMSE scores for 2014
mmse_df_2014 <- mmse_df_2014 %>% 
  mutate(
    mmse_score_2014 = 
      c11_14+ c12_14+ c13_14+ c14_14+ c15_14+ c16_14+ c21a_14+ 
      c21b_14+ c21c_14+ c41a_14+ c41b_14+ c41c_14+ c31a_14+ c31b_14+ 
      c31c_14+ c31d_14+ c31e_14+ c32_14+ c51a_14+ c51b_14+ c52_14+
      c53a_14+ c53b_14+ c53c_14
  ) %>% 
  select(id, mmse_score_2014)

# Predictors 2011 ----
predictors_df <- df %>% 
  select(
    id,                                        # Individual ID
    trueage,                                   # Age
    residenc,                                  # Residence
    a1,                                        # Sex 
    f1,                                        # Years of schooling
    f35,                                       # Household income last year
    f41,                                       # Marital status
    e1, e2, e3, e4, e5, e6,                    # ADL
    e7, e8, e9, e10, e11, e12, e13, e14,       # IADL
    g15a1, g15b1, g15c1, g15d1, g15r1,         # Chronic disease diagnoses                              
    b21, b22, b23, b24, b25, b26, b27, b28,    # Psychological (personality and mood) questions
    d11b, d11c, d11d, d11e, d11f, d11g, d11h,  # Social activities and hobbies
    d91, d92,                                  # Exercise (current and past)
    g01, g02,                                  # Sleep quality and sleep duration
    d1, d31, d32, d34, d81, d82, d85, d86,     # Diet and cooking
    d4meat2, d4fish2, d4egg2, d4suga2, d4tea2,
    g24,                                       # Chewing ability
    g1                                         # Visual ability
  )

# Set 'I don't know" and "missing" to NA for income
# Set '>100000 to 1000001
predictors_df$f35 <- ifelse(predictors_df$f35 == 88888, NA, predictors_df$f35)
predictors_df$f35 <- ifelse(predictors_df$f35 == 99999, NA, predictors_df$f35)
predictors_df$f35 <- ifelse(predictors_df$f35 == 99998, 100001, predictors_df$f35)

# Set "don't know" and "could not answer" to NA for years of schooling
predictors_df$f1 <- ifelse(predictors_df$f1 == 88, NA, predictors_df$f1)
predictors_df$f1 <- ifelse(predictors_df$f1 == 99, NA, predictors_df$f1)

# 0 is not in the codebook for this question, replacing with NA
predictors_df$g15c1 <- ifelse(predictors_df$g15c1 == 0, NA, predictors_df$g15c1)

# Set "I don't know" to missing for all chronic diseases
predictors_df$g15a1 <- ifelse(predictors_df$g15a1 == 8, NA, predictors_df$g15a1)
predictors_df$g15b1 <- ifelse(predictors_df$g15b1 == 8, NA, predictors_df$g15b1)
predictors_df$g15c1 <- ifelse(predictors_df$g15c1 == 8, NA, predictors_df$g15c1)
predictors_df$g15d1 <- ifelse(predictors_df$g15d1 == 8, NA, predictors_df$g15d1)
predictors_df$g15r1 <- ifelse(predictors_df$g15r1 == 8, NA, predictors_df$g15r1)

# For the psychological variables change "could not answer" to missing
predictors_df$b21 <- ifelse(predictors_df$b21 == 8, NA, predictors_df$b21)
predictors_df$b22 <- ifelse(predictors_df$b22 == 8, NA, predictors_df$b22)
predictors_df$b23 <- ifelse(predictors_df$b23 == 8, NA, predictors_df$b23)
predictors_df$b24 <- ifelse(predictors_df$b24 == 8, NA, predictors_df$b24)
predictors_df$b25 <- ifelse(predictors_df$b25 == 8, NA, predictors_df$b25)
predictors_df$b26 <- ifelse(predictors_df$b26 == 8, NA, predictors_df$b26)
predictors_df$b27 <- ifelse(predictors_df$b27 == 8, NA, predictors_df$b27)
predictors_df$b28 <- ifelse(predictors_df$b28 == 8, NA, predictors_df$b28)

# For the adl/iadl variables change 8,9 to missing
predictors_df$e1 <- ifelse(predictors_df$e1 %in% c(8,9), NA, predictors_df$e1)
predictors_df$e2 <- ifelse(predictors_df$e2 %in% c(8,9), NA, predictors_df$e2)
predictors_df$e3 <- ifelse(predictors_df$e3 %in% c(8,9), NA, predictors_df$e3)
predictors_df$e4 <- ifelse(predictors_df$e4 %in% c(8,9), NA, predictors_df$e4)
predictors_df$e5 <- ifelse(predictors_df$e5 %in% c(8,9), NA, predictors_df$e5)
predictors_df$e6 <- ifelse(predictors_df$e6 %in% c(8,9), NA, predictors_df$e6)
predictors_df$e7 <- ifelse(predictors_df$e7 %in% c(8,9), NA, predictors_df$e7)
predictors_df$e8 <- ifelse(predictors_df$e8 %in% c(8,9), NA, predictors_df$e8)
predictors_df$e9 <- ifelse(predictors_df$e9 %in% c(8,9), NA, predictors_df$e9)
predictors_df$e10 <- ifelse(predictors_df$e10 %in% c(8,9), NA, predictors_df$e10)
predictors_df$e11 <- ifelse(predictors_df$e11 %in% c(8,9), NA, predictors_df$e11)
predictors_df$e12 <- ifelse(predictors_df$e12 %in% c(8,9), NA, predictors_df$e12)
predictors_df$e13 <- ifelse(predictors_df$e13 %in% c(8,9), NA, predictors_df$e13)
predictors_df$e14 <- ifelse(predictors_df$e14 %in% c(8,9), NA, predictors_df$e14)

# Set "could not answer" for alcohol consumption to NA
predictors_df$d81 <- ifelse(predictors_df$d81 == 8, NA, predictors_df$d81)

# If someone reported that they don't drink, set their alcohol type to 0 (new category = does not drink)
predictors_df$d85 <- ifelse(predictors_df$d81 == 2, 0, predictors_df$d85)

# If someone reported that they don't drink, set their daily consumption to zero
predictors_df$d86 <- ifelse(predictors_df$d81 == 2, 0, predictors_df$d86)

# Set "don't know" and "could not answer" to NA for number of drinks
predictors_df$d86 <- ifelse(predictors_df$d86 %in% c(88,99), NA, predictors_df$d86)

# For garden work 8 is not a valid entry, set to NA
predictors_df$d11c <- ifelse(predictors_df$d11c == 8, NA, predictors_df$d11c)

# For playing mahjong 8 is not a valid entry, set to NA
predictors_df$d11f <- ifelse(predictors_df$d11f == 8, NA, predictors_df$d11f)

# For taking part in social activities 8 is not a valid entry, set to NA
predictors_df$d11h <- ifelse(predictors_df$d11h == 8, NA, predictors_df$d11h)

# For exercise (current and past), set "don't know" values to NA
predictors_df$d91 <- ifelse(predictors_df$d91 == 8, NA, predictors_df$d91)
predictors_df$d92 <- ifelse(predictors_df$d92 == 8, NA, predictors_df$d92)

# For sleep quality set "could not answer" to NA
predictors_df$g01 <- ifelse(predictors_df$g01 == 8, NA, predictors_df$g01)

# For sleep duration, set "don't know" to missing, NOTE THIS HAS 99 FOR MISSING BUT NO ENTRIES
predictors_df$g02 <- ifelse(predictors_df$g02 == 88, NA, predictors_df$g02)

# For sleep duration, round to the first decimal since thre are some wacky values
predictors_df$g02 <- round(predictors_df$g02, 1)

# For fresh fruit and vegetable consumption set "I don't know" to missing
predictors_df$d31 <- ifelse(predictors_df$d31 == 8, NA, predictors_df$d31)
predictors_df$d32 <- ifelse(predictors_df$d32 == 8, NA, predictors_df$d32)

# For cooking flavor, set "don't know" to missing
predictors_df$d34 <- ifelse(predictors_df$d34 == 8, NA, predictors_df$d34)

# For current drinking statu, set "don't know" to NA
predictors_df$d82 <- ifelse(predictors_df$d82 == 8, NA, predictors_df$d82)

# For meat consumption, set "don't know" to missing 
predictors_df$d4meat2 <- ifelse(predictors_df$d4meat2 == 8, NA, predictors_df$d4meat2)

# For fish consumption, set "don't know" to missing 
predictors_df$d4fish2 <- ifelse(predictors_df$d4fish2 == 8, NA, predictors_df$d4fish2)

# For egg consumption, set "don't know" to missing 
predictors_df$d4egg2 <- ifelse(predictors_df$d4egg2 == 8, NA, predictors_df$d4egg2)

# For sugar consumption, set "don't know" to missing, and 7 (unspecified value) to missing 
predictors_df$d4suga2 <- ifelse(predictors_df$d4suga2 == 8, NA, predictors_df$d4suga2)
predictors_df$d4suga2 <- ifelse(predictors_df$d4suga2 == 7, NA, predictors_df$d4suga2)

# For tea consumption, set "don't know" to missing
predictors_df$d4tea2 <- ifelse(predictors_df$d4tea2 == 8, NA, predictors_df$d4tea2)

# For all variables replace 9 with NA 
# (EXCEPT FOR SLEEP, YEARS OF SCHOOLING, NUMBER OF DRINKS, and INCOME BECAUSE 9 IS A VALID ENTRY)
sleep_duration <- predictors_df$g02
years_of_schooling <- predictors_df$f1
household_income <- predictors_df$f35
num_drinks <- predictors_df$d86

predictors_df <- predictors_df[,!names(predictors_df) %in% c("g02", "f1", "f35", "d86")]
predictors_df[predictors_df == 9] <- NA 

predictors_df$g02 <- sleep_duration 
predictors_df$f1 <- years_of_schooling
predictors_df$f35 <- household_income
predictors_df$d86 <- num_drinks

# Create adl_iadl score from zhou (2020)
predictors_df$zhou_adl_iadl <- 
  predictors_df$e1+
  predictors_df$e2+
  predictors_df$e3+
  predictors_df$e4+
  predictors_df$e5+
  predictors_df$e6+
  predictors_df$e7+
  predictors_df$e8+
  predictors_df$e9+
  predictors_df$e10+
  predictors_df$e11+
  predictors_df$e12+
  predictors_df$e13+
  predictors_df$e14

# The chewing variable has 8s and 9s, set to NA
predictors_df$g24 <- ifelse(predictors_df$g24 %in% c(8,9), NA, predictors_df$g24)

# Binarizing the visual function variable
#  0 = can see, 1 = cannot see/blind
predictors_df$g1 <- ifelse(predictors_df$g1 %in% c(1,2), 0, predictors_df$g1)
predictors_df$g1 <- ifelse(predictors_df$g1 %in% c(3,4), 1, predictors_df$g1)
predictors_df$g1 <- ifelse(predictors_df$g1 %in% c(8,9), NA, predictors_df$g1)

# Creating the IADL score for the zhou paper
predictors_df$iadl1 <- ifelse(predictors_df$e7 %in% c(3), 1, ifelse(predictors_df$e7 %in% c(1,2), 0, NA))
predictors_df$iadl2 <- ifelse(predictors_df$e8 %in% c(3), 1, ifelse(predictors_df$e8 %in% c(1,2), 0, NA))
predictors_df$iadl3 <- ifelse(predictors_df$e9 %in% c(3), 1, ifelse(predictors_df$e9 %in% c(1,2), 0, NA))
predictors_df$iadl4 <- ifelse(predictors_df$e10 %in% c(3), 1, ifelse(predictors_df$e10 %in% c(1,2), 0, NA))
predictors_df$iadl5 <- ifelse(predictors_df$e11 %in% c(3), 1, ifelse(predictors_df$e11 %in% c(1,2), 0, NA))
predictors_df$iadl6 <- ifelse(predictors_df$e12 %in% c(3), 1, ifelse(predictors_df$e12 %in% c(1,2), 0, NA))
predictors_df$iadl7 <- ifelse(predictors_df$e13 %in% c(3), 1, ifelse(predictors_df$e13 %in% c(1,2), 0, NA))
predictors_df$iadl8 <- ifelse(predictors_df$e14 %in% c(3), 1, ifelse(predictors_df$e14 %in% c(1,2), 0, NA))

predictors_df$hu_iadl <- 
  predictors_df$iadl1+
  predictors_df$iadl2+
  predictors_df$iadl3+
  predictors_df$iadl4+
  predictors_df$iadl5+
  predictors_df$iadl6+
  predictors_df$iadl7+
  predictors_df$iadl8

predictors_df <- predictors_df %>% 
  select(-iadl1, -iadl2, -iadl3, -iadl4, -iadl5, -iadl6, -iadl7, -iadl8)

# The chewing variable has 8s and 9s, set to NA
predictors_df$g24 <- ifelse(predictors_df$g24 %in% c(8,9), NA, predictors_df$g24)

# Creating marriage variable from Hu (2020)
predictors_df$hu_f41 <- ifelse(predictors_df$f41 %in% c(1,2), 1, 
                               ifelse(predictors_df$f41 %in% c(4), 2,
                                      ifelse(predictors_df$f41 %in% c(3,5), 3, predictors_df$f41)))

# Creating the TV/radio variable from Zhou 2020
predictors_df$zhou_tv <- ifelse(predictors_df$d11g %in% c(1), 1,
                                ifelse(predictors_df$d11g %in% c(2,3,4), 2,
                                       ifelse(predictors_df$d11g %in% c(5), 3, predictors_df$d11g)))

# Creating the gardening variable from Zhou 2020
# "Growing flowers OR raising pets"
# 1 = often, 2 = sometimes, 3 = never
predictors_df$zhou_gardening <- ifelse(predictors_df$d11c %in% c(1), 1,
                                       ifelse(predictors_df$d11c %in% c(2,3,4), 2,
                                              ifelse(predictors_df$d11c %in% c(5), 3, predictors_df$d11c)))

predictors_df$zhou_pets <- ifelse(predictors_df$d11e %in% c(1), 1,
                                  ifelse(predictors_df$d11e %in% c(2,3,4), 2,
                                         ifelse(predictors_df$d11e %in% c(5), 3, predictors_df$d11e)))

predictors_df$zhou_gard_pets <- ifelse(predictors_df$zhou_gardening <= predictors_df$zhou_pets, 
                                       predictors_df$zhou_gardening, predictors_df$zhou_pets)

predictors_df <- predictors_df %>% select(-zhou_gardening, -zhou_pets)

# Creating stroke variable for zhou 2020
predictors_df$zhou_stroke <- ifelse(predictors_df$g15d1 == 8, NA, predictors_df$g15d1)

# Creating the age group variable for wang 2022
predictors_df$wang_age <- ifelse(predictors_df$trueage <= 79, 1,
                                 ifelse(predictors_df$trueage %in% seq(80, 89, 1), 2,
                                        ifelse(predictors_df$trueage %in% seq(90, 99, 1), 3,
                                               ifelse(predictors_df$trueage >= 100, 4, predictors_df$trueage))))

# Creating the adl variable for wang 2022
predictors_df$adl1 <- ifelse(predictors_df$e1 == 3, 1, ifelse(predictors_df$e1 < 3, 0, predictors_df$e1)) 
predictors_df$adl2 <- ifelse(predictors_df$e2 == 3, 1, ifelse(predictors_df$e2 < 3, 0, predictors_df$e2)) 
predictors_df$adl3 <- ifelse(predictors_df$e3 == 3, 1, ifelse(predictors_df$e3 < 3, 0, predictors_df$e3)) 
predictors_df$adl4 <- ifelse(predictors_df$e4 == 3, 1, ifelse(predictors_df$e4 < 3, 0, predictors_df$e4)) 
predictors_df$adl5 <- ifelse(predictors_df$e5 == 3, 1, ifelse(predictors_df$e5 < 3, 0, predictors_df$e5)) 
predictors_df$adl6 <- ifelse(predictors_df$e6 == 3, 1, ifelse(predictors_df$e6 < 3, 0, predictors_df$e6)) 

predictors_df$wang_adl <- 
  predictors_df$adl1+
  predictors_df$adl2+
  predictors_df$adl3+
  predictors_df$adl4+
  predictors_df$adl5+
  predictors_df$adl6

# If any difficulty across the 6 ADLs the score = 1, otherwise 0
predictors_df$wang_adl <- ifelse(predictors_df$wang_adl >= 1, 1,
                                 ifelse(predictors_df$wang_adl == 0, 0, predictors_df$wang_adl))

predictors_df <- predictors_df %>% 
  select(-adl1, -adl2, -adl3, -adl4, -adl5, -adl6)

# Creating Wang 2022 educational variable
predictors_df$wang_edu <- ifelse(predictors_df$f1 == 0, 1,
                                 ifelse(predictors_df$f1 %in% seq(1,6), 2,
                                        ifelse(predictors_df$f1 >= 7, 3, predictors_df$f1)))

# Final DF ----
df_final <- inner_join(mmse_df_2014, mmse_df_2011, by = "id")
df_final <- inner_join(df_final, predictors_df, by = "id")

# Exclusion criteria and MMSE cutoffs (education specific, see the Shanghai paper)
df_final <- df_final %>% 
  filter(trueage >= 60) %>% 
  drop_na(mmse_score_2011, mmse_score_2014) %>% 
  mutate(
    mmse_cutoff = 
      case_when(
        f1 < 1 ~ mmse_cutoff_1,
        f1 %in% c(1,2,3,4,5,6) ~ mmse_cutoff_2,
        f1 > 6 ~ mmse_cutoff_3
      )
  ) %>% 
  filter(mmse_score_2011 >= mmse_cutoff) %>% 
  mutate(ci = ifelse(mmse_score_2014 < mmse_cutoff, 1, 0)) %>% 
  select(-mmse_cutoff) %>% 
  rename(mmse_score_baseline = mmse_score_2011,
         mmse_score_followup = mmse_score_2014)

# Saving the final df for the 2011-2014 cohort
write.csv(df_final, "Data/Derived-DFs/df-final.csv", row.names = FALSE)
