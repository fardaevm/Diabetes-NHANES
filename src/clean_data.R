widyverse)
library(haven)

diq  <- read_xpt("data/external/DIQ_L.xpt")     
demo <- read_xpt("data/external/DEMO_L.xpt")
ghb  <- read_xpt("data/external/GHB_L.xpt")  
glu  <- read_xpt("data/external/GLU_L.xpt")     
bmx  <- read_xpt("data/external/BMX_L.xpt")   

nhanes_merged <- reduce(list(diq, demo, ghb, glu, bmx), full_join, by = "SEQN")

write_csv(nhanes_merged, "data/interim/nhanes_merged.csv")
sum
nhanes_clean <- nhanes_merged %>%
  select(
    LBXGH, BMXBMI, BMXWT, BMXHT, BMXWAIST, RIDAGEYR,
    RIAGENDR, RIDRETH3, INDFMPIR, RIDEXPRG
  ) %>%
  rename(
    a1c           = LBXGH,
    bmi           = BMXBMI,
    weight_kg     = BMXWT,
    height_cm     = BMXHT,
    waist_cm      = BMXWAIST,
    age           = RIDAGEYR,
    gender        = RIAGENDR,
    ethnicity     = RIDRETH3,
    income_poverty_ratio = INDFMPIR,
    pregnancy     = RIDEXPRG
  ) %>%
  filter(!is.na(a1c)) %>%
  filter(pregnancy != 1 | is.na(pregnancy)) %>%
  select(-pregnancy) %>%
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    ethnicity = factor(ethnicity, levels = c(1, 2, 3, 4, 6, 7), labels = c(
      "Mexican American", "Other Hispanic", "Non-Hispanic White",
      "Non-Hispanic Black", "Non-Hispanic Asian", "Other/Multi-Racial"
    ))
  ) %>%
  drop_na()

nhanes_clean$ethnicity <- relevel(nhanes_clean$ethnicity, ref = "Non-Hispanic White")

colnames(nhanes_clean)
write_csv(nhanes_clean, "data/processed/nhanes_clean.csv")

