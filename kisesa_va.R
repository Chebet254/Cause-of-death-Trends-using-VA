#KISESA VA
library(dplyr)
kisesa_va <- (read_dta("insilico_maincause_Kisesa_origid.dta")) %>% as_factor
