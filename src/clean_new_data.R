library(tidyverse)
library(haven)

diq  <- read_xpt("new_data/external/DIQ_J.xpt")      
demo <- read_xpt("new_data/external/DEMO_J.xpt")     
ghb  <- read_xpt("new_data/external/GHB_J.xpt")  
bmx  <- read_xpt("new_data/external/BMX_J.xpt")     
trigly <- read_xpt("new_data/external/TRIGLY_J.xpt")   
insulin <- read_xpt("new_data/external/INS_J.xpt")       


nhanes_merged <- reduce(
  list(diq, demo, ghb, bmx,trigly, insulin),
  full_join,
  by = "SEQN"
)

write_csv(nhanes_merged, "new_data/interim/nhanes_merged.csv")



# Clean & rename columns
nhanes_clean <- nhanes_merged %>%
  select(
    a1c = LBXGH,
    waist_cm = BMXWAIST,
    height_cm = BMXHT,
    gender = RIAGENDR,
    age = RIDAGEYR,
    ethnicity = RIDRETH3,
    income_poverty_ratio = INDFMPIR,
    family_history = DIQ170,
    pregnancy = RIDEXPRG,
    triglycerides = LBXTR,
    insulin = LBXIN  
  ) %>%
  filter(age >= 18) %>%
  filter(!is.na(a1c)) %>%
  filter(pregnancy != 1 | is.na(pregnancy)) %>%
  select(-pregnancy) %>%
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    ethnicity = factor(
      ethnicity,
      levels = c(1, 2, 3, 4, 6, 7),
      labels = c(
        "Mexican American", "Other Hispanic", "Non-Hispanic White",
        "Non-Hispanic Black", "Non-Hispanic Asian", "Other/Multi-Racial"
      )
    ),
    ethnicity = relevel(ethnicity, ref = "Non-Hispanic White"),
    family_history = case_when(
      family_history == 1 ~ "Yes",
      family_history == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    family_history = factor(family_history)
  ) %>%
  drop_na()

dim(nhanes_clean)
write_csv(nhanes_clean, "new_data/processed/nhanes_clean.csv")
