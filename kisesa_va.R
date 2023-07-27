#KISESA VA
library(dplyr)
library(tidyverse)
library(lubridate)

kisesa <- read.csv("kisesa.csv")

#calculate age
kisesa_va <- kisesa %>% mutate(
  Age_years = interval(dob, dod) %/% years(1))
kisesa_va <- kisesa_va %>% mutate(
  age_days = interval(dob, dod) / days(1))
kisesa_va <- kisesa_va[kisesa_va$age_days >= 0, ]  #remove neg values

#age cat
kisesa_va <- kisesa_va %>% mutate(
  age_cat = case_when(
    age_days >=0 & age_days <29 ~"neonate",
    age_days >28 & age_days <= 365 ~"infant",
    Age_years >= 1 & Age_years <=4~"1-4",
    Age_years >= 5 & Age_years <=14~"5-14",
    Age_years >= 15 & Age_years <=49~"15-49",
    Age_years >= 50 & Age_years <=64~"50-64",
    Age_years >= 65 ~"65+"))

#extraxt yod
kisesa_va$yearofdeath <- year(kisesa_va$dod)
kisesa_va <- kisesa_va %>% select(dod, age_cat, Cleaned_COD, sex,yearofdeath)



#total cod
kisesa_total <- kisesa_va %>%
  group_by( Cleaned_COD) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  mutate(percentage = count / sum(count) * 100)
kisesa_total_ages <- kisesa_va %>% select(age_cat, Cleaned_COD)
kisesa_total_ages <- kisesa_total_ages %>%
  group_by(age_cat, Cleaned_COD) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
kisesa_total_ages <- na.omit(kisesa_total_ages)
kisesa_top10_data <- kisesa_total_ages %>%
  group_by(age_cat) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

kisesa_top10_data$age_cat <- factor(kisesa_top10_data$age_cat,
                                                  levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))

ggplot(kisesa_top10_data, aes(x =as.factor(age_cat), y = percentage, fill =fct_reorder(Cleaned_COD, -percentage))) +
  geom_bar(aes(group = age_cat),stat = "identity") +
  ylab("Percentage") +
  ggtitle("Overall Top 5 Causes of Death by Age Group- KISESA HDSS")+
  scale_fill_manual(values = col3)+
  guides(fill=guide_legend(ncol=1))+
  # my_theme+
  labs(fill = "Causes of Death")

#COD over time 
kisesa_data_subset <- kisesa_va %>% filter(yearofdeath >2007)
kisesa_data_subset = (kisesa_data_subset
                                    %>% select("yearofdeath",  "Cleaned_COD")
                                    %>% group_by(yearofdeath, Cleaned_COD)
                                    %>% count()
                                    %>% ungroup()
                                    %>% group_by(yearofdeath)
                                    %>% mutate(percentage = n / sum(n) * 100)
                                    %>% arrange(desc(n),.by_group = TRUE)
                                    %>% mutate(index = 1:n())
                                    %>% filter(index <= top_n_cod)
                                    %>% ungroup()
                                    )
K1 = c('2008','2009','2010','2011')
K2 = c('2012','2013','2014','2015')
K3 = c('2016','2017','2018','2019' )
kisesa_data_subset <- (kisesa_data_subset
                                     %>% mutate(.new_group = as.factor(yearofdeath)
                                                              , .new_group = fct_collapse(.new_group
                                                               , `2008 - 2011` = K1
                                                                , `2012 - 2015` = K2
                                                                , `2016 - 2019` = K3)))
ggplot(data = kisesa_data_subset,
                     #%>% filter(dodyear %in% t1),
                       aes(x=as.factor(yearofdeath), y=percentage, fill = fct_reorder(Cleaned_COD, -n)))+
  geom_bar(aes(group = yearofdeath),stat="identity", position = "dodge2")+
  facet_wrap(~.new_group, scales = "free_x", ncol = 1)+
  theme_light()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1)) +
  labs(x ='', y = '', fill = "Causes of Death")+
  #my_theme+
  scale_fill_manual(values = col3)+
  ggtitle("CAUSES OF DEATH OVER THE YEARS- KISESA")





















