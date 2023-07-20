#install.packages("table1")
#install.packages("sjmisc")

library(easypackages)

libraries("dplyr", "ggplot2", "openxlsx","flextable", "gtsummary", "table1", "sjmisc")
source("helperfuns.R")

CODwithSES1 <- read_dta("CODwithSES1.dta") %>% as_factor()

wb <- createWorkbook()
filename <- "tables_descriptives.xlsx"

df_tab <- CODwithSES1
tab2 <- df2
sex_tab <- generateTab(tab2
                        , vars = c("age_cat","new_code" )
                        , by="gender"
                        , add.p=FALSE
                        , add.ci=FALSE
                        , save.excel = FALSE
                        , add.plot=FALSE
                        , wb=wb
                        , filename=filename
                        , caption="Sex"
)
sex_tab

poor_tab <- generateTab(df_tab
  , vars = c("quintile2","incomesrc2", "educlevel")
  , by="gender"
  , add.p=FALSE
  , add.ci=FALSE
  , save.excel = FALSE
  , add.plot=FALSE
  , wb=wb
  , filename=filename
  , caption="Sex"
)
poor_tab
