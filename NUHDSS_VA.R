#("causeofdeath")

library(tidyverse)
library(haven)
library(viridis)
library(paletteer)
library(ggthemes)
library(finalfit)  #for labels 
library(stringr)
library(openxlsx)


NUHDSS <- read_dta("Verbalautopsy_2002-2015 NUHDSS.dta") %>% as_factor()
labels <- NUHDSS %>% extract_labels()
HH_Poverty_Indicators <- read_dta("HH Poverty Indicators - ANON.dta") %>% as_factor()
hhamenities_hhpossessions<- read_dta("hhamenities_hhpossessions_anon_2015_2018 (1).dta")%>% as_factor()
NUHDSS_hhamenitiescharacteristics <- read_dta("NUHDSS_hhamenitiescharacteristics_anon.dta") %>% as_factor()
amenities <- NUHDSS_hhamenitiescharacteristics %>% extract_labels()
#NUHDSSWITH_ICD <- read_dta("NUHDSS data/NUHDSSWITH ICD.dta")%>% as_factor()
#write.xlsx(NUHDSSWITH_ICD, "nuhdss_data.xlsx", rowNames = F)
nuhdss_df <- readxl::read_excel("nuhdss_data.xlsx")


# age categories 
nuhdss_va <- nuhdss_df %>% mutate(
  age_cat = case_when(
    agegroupatdeath == "neonate:0-28 days" ~"neonate",
    agegroupatdeath == "infant:<1 Year" ~"infant",
    agegroupatdeath == "child:1-4" ~"1-4",
    agegroupatdeath == "5-9" ~"5-14",
    agegroupatdeath == "10-14" ~"5-14",
    agegroupatdeath == "15-19" ~"15-49",
    agegroupatdeath == "20-24" ~"15-49",
    agegroupatdeath == "25-29" ~"15-49",
    agegroupatdeath == "30-34" ~"15-49",
    agegroupatdeath == "35-39" ~"15-49",
    agegroupatdeath == "40-44" ~"15-49",
    agegroupatdeath == "45-49" ~"15-49",
    agegroupatdeath == "50-54" ~"50-64",
    agegroupatdeath == "55-59" ~"50-64",
    agegroupatdeath == "60-64" ~"50-64",
    agegroupatdeath == "65-69" ~"65+",
    agegroupatdeath == "70-74" ~"65+",
    agegroupatdeath == "75-79" ~"65+",
    agegroupatdeath == ">=80" ~"65+"))


#extract dod
#nuhdss_va <- nuhdss_va %>% mutate(dodyear = year(vau_datedeath))


#getting icd10 codes using interva package
#top 10 causes over the years
top_n_cod <- 10
nuhdss_va <- subset(nuhdss_va, Cleaned_COD != 'missing:impute' )
nuhdss_data_subset = (nuhdss_va
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

nuhdss_data_subset <- nuhdss_data_subset %>% filter(yearofdeath >2005)
nuhdss_data_subset <- subset(nuhdss_data_subset, yearofdeath != 'missing:impute' )
N1 = c('2006','2007','2008','2009')
N2 = c('2010','2011','2012','2013' )
N3 = c('2014','2015', '2016')


nuhdss_data_subset <- (nuhdss_data_subset
                %>% mutate(.new_group = as.factor(yearofdeath)
                           , .new_group = fct_collapse(.new_group
                                                       , `2006 - 2009` = N1
                                                       , `2010 - 2013` = N2
                                                       , `2014 - 2016` = N3)))


ggplot(data = nuhdss_data_subset, 
       #%>% filter(dodyear %in% t1), 
       aes(x=as.factor(yearofdeath), y=percentage, fill = fct_reorder(Cleaned_COD, -n)))+
  geom_bar(aes(group = yearofdeath),stat="identity", position = "dodge2")+
  facet_wrap(~.new_group, scales = "free_x", ncol = 1)+
  theme_light()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1)) + 
  labs(fill = "Causes of Death")+
  my_theme+
  scale_fill_manual(values = my_colors)+
  ggtitle("ICD10 CAUSES OF DEATH OVER THE YEARS- NUHDSS")

#gen causes of death
nuhdss_va <- subset(nuhdss_va, Cleaned_COD != 'va not done' ) 
nuhdss_va <- subset(nuhdss_va, Cleaned_COD != 'Indeterminate' ) 
nai <- nuhdss_va %>% select(yearofdeath, age_cat, ICD_10, genderofnuhdssindividual, Cleaned_COD)

#nai <- nai %>% filter(yearofdeath >= 2007 & yearofdeath <= 2016)
nuhdss_total <- nai %>%
  group_by( Cleaned_COD) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))%>%
  mutate(percentage = count / sum(count) * 100)#%>%
  #top_n(n = 15, wt = count)

#write.csv(nuhdss_total, 'nuhdssTop15.csv')

#those who had AIDS & PTB
TBAIDS <- NUHDSS %>% select(icod_1, vau_illwith_hivaids)
TBAIDS <- subset(TBAIDS, vau_illwith_hivaids == 'yes')
#TBAIDS <- subset(TBAIDS, icod_1 == '01.09 pulmonary tuberculosis')
# Compute the position of labels
TBAIDS <- TBAIDS %>% 
  group_by(icod_1)%>%
  summarize(count = n()) %>% 
  arrange(desc(count)) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  top_n(n = 10, wt = count)

# Basic piechart
new_row_names <- c("PTB", "HIV", "Pneumonia", "Cardiac Dis", "Anaemia")
row.names(TBAIDS)[2,] <-  c("PTB", "HIV", "Pneumonia", "Cardiac Dis", "Anaemia")

ggplot(TBAIDS, aes(x="", y=icod_1, fill= percentage)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+ 
  theme(legend.position="bottom") +
  geom_text(aes(label = icod_1), position = position_stack(vjust = 0.5))



#COD BY AGE CATEGORIES TOP 5
str(nuhdss_va)
nuhdss_total_ages <- nuhdss_va %>% select(age_cat,Cleaned_COD )
nuhdss_total_ages <- nuhdss_total_ages %>%
  group_by(age_cat, Cleaned_COD) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

nuhdss_total_ages <- na.omit(nuhdss_total_ages)

nuhdss_top10_data <-nuhdss_total_ages %>%
  group_by(age_cat) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

nuhdss_top10_data$age_cat <- factor(nuhdss_top10_data$age_cat, 
                                    levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))

ggplot(nuhdss_top10_data, aes(x =as.factor(age_cat), y = percentage, fill =fct_reorder(Cleaned_COD, -percentage))) +
  geom_bar(aes(group = age_cat),stat = "identity") +
  ylab("Percentage") +
  ggtitle("Overall Top 5 Causes of Death by Age Group- NUHDSS")+
  scale_fill_manual(values = my_colors2)+
  guides(fill=guide_legend(ncol=1))+
  my_theme+
  labs(fill = "Causes of Death")

#GENDER GROUPING
#by gender
gender_nuhdss <- nuhdss_va %>% select(age_cat, Cleaned_COD, genderofnuhdssindividual)
gender_nuhdss <- gender_nuhdss %>%
  group_by(age_cat, Cleaned_COD, genderofnuhdssindividual) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

gender_nuhdss <- na.omit(gender_nuhdss)

gender_nuhdss <- gender_nuhdss %>%
  group_by(age_cat, genderofnuhdssindividual) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

gender_nuhdss$age_cat <- factor(gender_nuhdss$age_cat, 
                               levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))

ggplot(gender_nuhdss, aes(x = as.factor(Cleaned_COD), y = percentage, fill = fct_reorder(Cleaned_COD, -percentage))) +
  geom_bar(aes(group = Cleaned_COD), stat = "identity") +
  facet_grid(rows = vars(genderofnuhdssindividual), cols = vars(age_cat), scales = "free") +
  theme(strip.text = element_text(size = 20, face = "bold")) +
  labs(fill = "Causes of Death") +
  ggtitle("Top 5 Causes of Death by Age Group, Grouped by Gender- NUHDSS")+
  guides(fill=guide_legend(ncol=1),
         guide = guide_legend(order = -1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "right")+
  my_theme+
  scale_fill_manual(values = my_colors)


#MERGED DATASETS 
iganga <- df2 %>% select(dodyear, age_cat, sex, Cleaned_COD)
colnames(iganga) <- c("dodyear", "age_cat", "sex", "causeofdeath")
iganga <- iganga %>% mutate(flag = "IGANGA")
iganga <-  na.omit(iganga)
iganga$dodyear <- as.character(iganga$dodyear)

nai <- nuhdss_va %>% select(yearofdeath, age_cat, genderofnuhdssindividual, Cleaned_COD)
colnames(nai) <- c("dodyear", "age_cat", "sex", "causeofdeath")
nai <- subset(nai, causeofdeath != 'va not done')
nai <- nai %>% mutate(flag = "NUHDSS")
nai$dodyear <- as.character(nai$dodyear)

meiru <- Meiru %>% select(yearofdeath, age_cat, sex, Cleaned_COD)
colnames(meiru) <- c("dodyear", "age_cat", "sex", "causeofdeath")
meiru <- meiru %>% mutate(flag = "MEIRU")
meiru$dodyear <- as.character(meiru$dodyear)

mwanza <- kisesa_va %>% select(yearofdeath, age_cat, sex, Cleaned_COD)
colnames(mwanza) <- c("dodyear", "age_cat", "sex", "causeofdeath")
mwanza <- mwanza %>% mutate(flag = "MAGU")
mwanza$dodyear <- as.character(mwanza$dodyear)

stacked_df <- bind_rows(nai, iganga, meiru, mwanza)


total_stacked <- stacked_df %>%
  group_by(age_cat, causeofdeath, flag) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

total_stacked <- na.omit(total_stacked)
 
top10_stacked <- total_stacked %>%
  group_by(age_cat, flag) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

#remove numericals 
top10_stacked$causeofdeath <- gsub("[0-9.]", "", top10_stacked$causeofdeath)
top10_stacked$age_cat <- factor(top10_stacked$age_cat, 
                             levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))

#plot
ggplot(top10_stacked, aes(x = as.factor(age_cat), y = percentage, fill = fct_reorder(causeofdeath, -percentage))) +
  geom_bar( aes(group = age_cat), stat = "identity", position = "dodge2") +
  facet_grid(rows = vars(flag),cols = vars(age_cat), scales = "free") +
  theme(strip.text = element_text(size = 20, face = "bold")) +
  labs(x = "Year", y = "Percent", fill = '') +
  ggtitle("Top 5 Causes of Death by Age Group across four Sites")+
  guides(fill=guide_legend(ncol=1),
  guide = guide_legend(order = 1))+
  #theme_few()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.text = element_text(size = 10),
        legend.position = 'right',
        axis.title  = element_text(size=20,color='white',face='bold'),
        title = element_text(size = 22, face = 'bold'))+
  scale_fill_manual(values = col3)+
  #my_theme+
  theme(legend.position = "right")

  