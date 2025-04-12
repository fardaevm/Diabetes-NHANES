library(tidyverse)
library(haven)

diq  <- read_xpt("new_data/external/DIQ_J.xpt")      # Family history
demo <- read_xpt("new_data/external/DEMO_J.xpt")     # Demographics
ghb  <- read_xpt("new_data/external/GHB_J.xpt")      # A1C
bmx  <- read_xpt("new_data/external/BMX_J.xpt")      # Body measures
smq  <- read_xpt("new_data/external/SMQ_J.xpt")      # Smoking

trigly <- read_xpt("new_data/external/TRIGLY_J.xpt")     # LBXTR, LBDLDLN
chol   <- read_xpt("new_data/external/HDL_J.xpt")        # LBDHDD
insulin <- read_xpt("new_data/external/INS_J.xpt")       # LBXIN
sleep  <- read_xpt("new_data/external/SLQ_J.xpt")        # SLD012, SLD013

nhanes_merged <- reduce(
  list(diq, demo, ghb, bmx, smq, trigly, chol, insulin, sleep),
  full_join,
  by = "SEQN"
)

dim(nhanes_merged)

write_csv(nhanes_merged, "new_data/interim/nhanes_merged.csv")



# Clean & rename columns
nhanes_clean <- nhanes_merged %>%
  select(
    SEQN,
    a1c = LBXGH,
    waist_cm = BMXWAIST,
    height_cm = BMXHT,
    gender = RIAGENDR,
    age = RIDAGEYR,
    ethnicity = RIDRETH3,
    income_poverty_ratio = INDFMPIR,
    education_level = DMDEDUC2,
    family_history = DIQ170,
    smoking = SMQ040,
    pregnancy = RIDEXPRG,
    triglycerides = LBXTR,
    ldl = LBDLDLN,
    hdl = LBDHDD,
    insulin = LBXIN,
    sleep_hours_weekdays = SLD012,
    sleep_hours_weekends = SLD013
  ) %>%
  filter(!is.na(a1c)) %>%
  filter(pregnancy != 1 | is.na(pregnancy)) %>%
  select(-pregnancy) %>%
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    ethnicity = factor(ethnicity, levels = c(1, 2, 3, 4, 6, 7), labels = c(
      "Mexican American", "Other Hispanic", "Non-Hispanic White",
      "Non-Hispanic Black", "Non-Hispanic Asian", "Other/Multi-Racial"
    )),
    ethnicity = relevel(ethnicity, ref = "Non-Hispanic White"),
    family_history = case_when(
      family_history == 1 ~ "Yes",
      family_history == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    family_history = factor(family_history),
    education_level = factor(education_level, levels = c(1, 2, 3, 4, 5),
                             labels = c("Less than 9th grade", "9-11th grade", 
                                        "High school/GED", "Some college/AA", 
                                        "College graduate or above")),
    smoking = case_when(
      smoking == 1 ~ "Every day",
      smoking == 2 ~ "Some days",
      smoking == 3 ~ "Not at all",
      TRUE ~ NA_character_
    ),
    smoking = factor(smoking)
  ) %>%
  drop_na()

dim(nhanes_clean)
write_csv(nhanes_clean, "new_data/processed/nhanes_clean.csv")