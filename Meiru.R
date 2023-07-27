library(haven)
library(finalfit)
meiru_dta <- read_dta("Meiru/meiru_inspireJune23.dta") #%>% as.factor()
meiru_dta_labs <- meiru_dta %>%  extract_labels()
meiru_inspireJune23 <- read_csv("Meiru/meiru_inspireJune23.csv")
meiru_inspireJune23 <- meiru_inspireJune23[complete.cases(meiru_inspireJune23$dod),]
#write.csv(meiru_inspireJune23, 'Meiru.csv', row.names = FALSE)
Meiru <- read.csv("Meiru.csv")
#Meiru$birth_date <- ymd(Meiru$birth_date)
Meiru <- Meiru %>% mutate(
  Age_years = interval(birth_date, dod) %/% years(1))
Meiru$age_days <-as.numeric(difftime(Meiru$dod, Meiru$birth_date, units = "days"))

#AGE 
str(Meiru)
#Meiru$Age <- as.character(Meiru$Age)
Meiru <- Meiru %>% mutate(
  age_cat = case_when(
    age_days >=0 & age_days <29 ~"neonate",
    age_days >28 & age_days <= 365 ~"infant",
    Age_years >= 1 & Age_years <=4~"1-4",
    Age_years >= 5 & Age_years <=14~"5-14",
    Age_years >= 15 & Age_years <=49~"15-49",
    Age_years >= 50 & Age_years <=64~"50-64",
    Age_years >= 65 ~"65+"))
#extraxt yod
Meiru$yearofdeath <- year(Meiru$dod)
Meiru <- Meiru %>% select(dod, age_cat, Cleaned_COD, sex,yearofdeath)
meiru_total_ages <- Meiru %>% select(age_cat, Cleaned_COD)
meiru_total_ages <- meiru_total_ages %>%
  group_by(age_cat, Cleaned_COD) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
meiru_total_ages <- na.omit(meiru_total_ages)
meiru_top10_data <- meiru_total_ages %>%
  group_by(age_cat) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)
meiru_top10_data$age_cat <- factor(meiru_top10_data$age_cat,
                                                 levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))

 ggplot(meiru_top10_data, aes(x =as.factor(age_cat), y = percentage, fill =fct_reorder(Cleaned_COD, -percentage))) +
   geom_bar(aes(group = age_cat), stat = "identity") +
   ylab("Percentage") +
   ggtitle("Overall Top 5 Causes of Death by Age Group- MEIRU HDSS")+
   scale_fill_manual(values = col3)+
   guides(fill=guide_legend(ncol=1))+
   my_theme+
   labs(fill = "Causes of Death")
 #extraxt yod
 Meiru$yearofdeath <- year(Meiru$dod)
 Meiru <- Meiru %>% select(dod, age_cat, Cleaned_COD, sex,yearofdeath)
 #total cod
   meiru_total <- Meiru %>%
   group_by( Cleaned_COD) %>%
   summarize(count = n()) %>%
   arrange(desc(count))%>%
   mutate(percentage = count / sum(count) * 100)
#total age cats
   meirutotalAge <- Meiru %>%
   group_by( age_cat) %>%
   summarize(count = n()) %>%
   arrange(desc(count))%>%
   mutate(percentage = count / sum(count) * 100)
 #COD over time
 meiru_data_subset <- Meiru %>% filter(yearofdeath >2009)
 meiru_data_subset = (meiru_data_subset
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
 M1 = c('2010','2011','2012','2013')
 M2 = c('2014','2015','2016','2017' )
 M3 = c('2018', '2019','2020', '2021')
 meiru_data_subset <- (meiru_data_subset
                                     %>% mutate(.new_group = as.factor(yearofdeath)
                                                              , .new_group = fct_collapse(.new_group
                                                                 , `2010 - 2013` = M1
                                                                 , `2014 - 2017` = M2
                                                                 , `2018 - 2021` = M3)))
ggplot(data = meiru_data_subset,
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
   ggtitle("CAUSES OF DEATH OVER THE YEARS- KARONGA") 
