#install.packages("")

library(tidyverse)
library(haven)
library(viridis)
library(paletteer)
library(ggthemes)
library(finalfit)  #for labels 
library(interva)


NUHDSS <- read_dta("Verbalautopsy_2002-2015 NUHDSS.dta") %>% as_factor()
#symptoms_nuhdss <- read_dta("verbalautopsy_symptoms_anon_NUHDSS.dta")
str(NUHDSS)
nuhdss_va <- NUHDSS %>% select(7:10,15, 348:351)
#change labels to variable names - finalfit package
nuhdss_va <- nuhdss_va %>% labels_to_column()
#remove labels
nuhdss_va <- nuhdss_va %>% remove_labels()
unique(nuhdss_va$`age group at death`)
# age categories 
nuhdss_va <- nuhdss_va %>% mutate(
  age_cat = case_when(
    `age group at death` == "neonate:0-28 days" ~"neonate",
    `age group at death` == "infant:<1 Year" ~"infant",
    `age group at death` == "child:1-4" ~"1-4",
    `age group at death` == "5-9" ~"5-14",
    `age group at death` == "10-14" ~"5-14",
    `age group at death` == "15-19" ~"15-49",
    `age group at death` == "20-24" ~"15-49",
    `age group at death` == "25-29" ~"15-49",
    `age group at death` == "30-34" ~"15-49",
    `age group at death` == "35-39" ~"15-49",
    `age group at death` == "40-44" ~"15-49",
    `age group at death` == "45-49" ~"15-49",
    `age group at death` == "50-54" ~"50-64",
    `age group at death` == "55-59" ~"50-64",
    `age group at death` == "60-64" ~"50-64",
    `age group at death` == "65-69" ~"65+",
    `age group at death` == "70-74" ~"65+",
    `age group at death` == "75-79" ~"65+",
    `age group at death` == ">=80" ~"65+"))

unique(nuhdss_va$`broad 1st cause of death by InterVA`)

#getting icd10 codes using interva package
