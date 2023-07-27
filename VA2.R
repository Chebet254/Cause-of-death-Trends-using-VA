library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(haven)
library(viridis)
library(paletteer)
library(ggthemes)
#data
#CoD3 <- read_csv("va_updated.csv") 
#CoD3 <- read_dta("va_updated.dta") %>% as_factor()
#ylll <- read_dta("YLLL.dta") %>% as_factor()

#USING ICD 10 CAUSES 
df2 <- readxl::read_excel("data.xlsx")
#total deaths by age group
total_ages <- df2 %>% select(age_cat, Cleaned_COD)
total_ages <- total_ages %>%
  group_by(age_cat, Cleaned_COD) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

total_ages <- na.omit(total_ages)

top10_data <- total_ages %>%
  group_by(age_cat) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

my_colors <- c( "lawngreen", "blue", "yellow", "purple","hotpink", "powderblue","darkblue", "deepskyblue",
               "cyan", "royalblue1","yellowgreen", "brown","peachpuff4","aquamarine4","skyblue","palegreen",
               "orange", "lavender", "pink", "springgreen","cornflowerblue","azure4", "palegoldenrod",
               "yellow4", "black", "gold2",  "peru", "lightslateblue", "darkgreen", "cyan3", 
               "aquamarine", "darkslategray", "indianred2", "olivedrab1", "violet", "seagreen","magenta", "thistle",
               "purple4", "gray30", "darkolivegreen", "beige")

my_colors2 <- c("blue", "yellow", "purple","hotpink", "olivedrab1","darkblue", "deepskyblue",
                "cyan", "royalblue1","yellowgreen", "brown","peachpuff4","aquamarine4","skyblue","peru",
                "orange", "yellow4", "pink", "springgreen","darkslategray", "black", "gold", "beige")

col3 <-  c('Malaria' = 'yellowgreen', 'Still birth' = 'lawngreen', 'HIV/AIDS' = 'yellow', 'Hypertension'= 'purple',
          'Diarrhoeal diseases' = "hotpink", 'Prematurity'= "powderblue",'Birth injury and or asphyxia'= "darkblue",
          'Pneumonia'="deepskyblue", 'perinatal causes'=  "cyan",'Malnutrition'= "royalblue1",'Road traffic accident'= "blue",
           'Diabetes'=  "brown",'Anaemia'= "peachpuff4",'Acute abdominal conditions' ="aquamarine4",'Acute cardiac disease'= "skyblue",
           'Other and unspecified cardiac dis' = "palegreen",'Assault'= "orange",'Tetanus'= "lavender",'Pulmonary Tuberculosis'=  "pink",
          'Meningitis and Encephalitis'=  "springgreen", 'Accidental exposure to smoke, fire & flame'= "cornflowerblue",
           'Other and unspecified infect dis'= "azure4",'Neonatal pneumonia'= "palegoldenrod",
           'Neonatal sepsis'= "yellow4",'Unspecified neonatal COD'= "black",'Respiratory neoplasms'= "gold2",
           'COPD'= "peru",'Other and unspecified infect dis'= "lightslateblue",'Pertussis'= "darkgreen",
           'Digestive neoplasms'= "cyan3",'Stroke'= "aquamarine", 'Reproductive neoplasms' = "olivedrab1", 'Accid drowning and submersion' ="indianred2" ,
           'Fall' = "violet", 'Other and unspecified neoplasms' =   "seagreen",'External cause'= "magenta",'Liver cirrhosis'= "thistle",
           'Severe malnutrition'= "purple4", 'Epilepsy'= "gray30")

my_theme <- theme(axis.title.x=element_blank(),
                  legend.text = element_text(size = 30),
                  legend.title = element_text(size = 32, face = "bold"),
                  legend.key.size = unit(1.5, "cm"),
                  title = element_text(size = 40, face = 'bold'),
                  axis.text = element_text(size = 20, face = 'bold'), 
                  axis.text.y.right = element_text(size = 30))

BlankSettings <- theme(legend.position = "none", 
                       title =element_text(size=12, face='bold'),
                       plot.margin = unit(c(0,0, 0, 0), "npc"), 
                       panel.margin = unit(c(0,0, 0, 0), "npc"),              
                       axis.text.x = element_text(color='white'),                             
                       axis.text.y = element_blank(), 
                       axis.ticks.x = element_line(color = "white"),
                       axis.ticks.y=element_blank(),
                       axis.title.x = element_text(size=12,color='white',face='bold'),
                       panel.grid = element_blank(),panel.grid.major = element_blank(),panel.background = element_blank()
)
top10_data$age_cat <- factor(top10_data$age_cat, 
                               levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))

ggplot(top10_data, aes(x =as.factor(age_cat), y = percentage, fill =fct_reorder(Cleaned_COD, -percentage))) +
  geom_bar(aes(group = age_cat),stat = "identity") +
  ylab("Percentage") +
  ggtitle("Overall Top 5 Causes of Death by Age Group- Iganga Mayuge HDSS")+
  scale_fill_manual(values = my_colors2)+
  guides(fill=guide_legend(ncol=1))+
  my_theme+
  labs(fill = "Causes of Death")
#gen causes of death
iganga <- df2 %>% select(dodyear, age_cat, ICD10codes, sex, Cleaned_COD)
iganga_total <- iganga %>%
  group_by( Cleaned_COD) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  mutate(percentage = count / sum(count) * 100)


#by gender
gender_grouping <- df2 %>% select(age_cat, Cleaned_COD, sex)
gender_group <- gender_grouping %>%
  group_by(age_cat, Cleaned_COD, sex) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

gender_group <- na.omit(gender_group)

gender_group <- gender_group %>%
  group_by(age_cat, sex) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

gender_group$age_cat <- factor(gender_group$age_cat, 
                                levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))

ggplot(gender_group, aes(x = as.factor(Cleaned_COD), y = percentage, fill = fct_reorder(Cleaned_COD, -percentage))) +
  geom_bar(aes(group = Cleaned_COD), stat = "identity") +
  facet_grid(rows = vars(sex), cols = vars(age_cat), scales = "free") +
  theme(strip.text = element_text(size = 20, face = "bold")) +
  labs(fill = "Causes of Death") +
  ggtitle("Top 5 Causes of Death by Age Group, Grouped by Gender- IGANGA")+
  guides(fill=guide_legend(ncol=1),
 guide = guide_legend(order = -1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "right")+
  my_theme+
  scale_fill_manual(values = my_colors)
  



#deaths per ages over time
ages_range_data <- CoD3 %>% select(age_cat, ICD10causeofdeaths, dodyear) #%>% filter(dodyear>2011)

ages_over_time <- ages_range_data %>%
  group_by(age_cat, ICD10causeofdeaths, dodyear) %>%
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup()

ages_over_time <- na.omit(ages_over_time)

ages_over_time <- ages_over_time %>%
  group_by(age_cat, dodyear) %>%
  top_n(n = 4, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)


g1 <- ages_over_time %>% filter(dodyear>=2008 & dodyear<=2010)
g2 <- ages_over_time %>% filter(dodyear>=2011 & dodyear<=2012)
g3 <- ages_over_time %>% filter(dodyear>=2013& dodyear<=2015)
g4 <- ages_over_time %>% filter(dodyear>=2016& dodyear<=2018)
g5 <- ages_over_time %>% filter(dodyear>=2019)
  
  
ggplot(g1, aes(x = as.factor(dodyear), y = percentage, fill = fct_reorder(ICD10causeofdeaths, -percentage))) +
  geom_bar( aes(group = dodyear), stat = "identity", position = "dodge2") +
  facet_grid(rows = vars(age_cat),cols = vars(dodyear), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold")) +
  labs(x = "Year", y = "Percent", fill = '') +
  ggtitle("Top 5 Causes of Death by Age Group Over Time")+
  guides(fill=guide_legend(ncol=1))+
  theme_few()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.text = element_text(size = 7),
        legend.position = 'right')+
  scale_fill_manual(values = my_colors)


#mid year population
mid_year_pop <- readxl::read_excel("AllDeathsage.xlsx", sheet = 2, skip = 1) 
mid_year_pop <- mid_year_pop %>% mutate(
  age_cat = case_when(
    Age == "<1" ~"neonate",
    Age == "1-4" ~"infant",
    Age == "5-9" ~"5-14",
    Age == "10-14" ~"5-14",
    Age == "15-19" ~"15-49",
    Age == "20-24" ~"15-49",
    Age == "25-29" ~"15-49",
    Age == "30-34" ~"15-49",
    Age == "35-39" ~"15-49",
    Age == "40-44" ~"15-49",
    Age == "45-49" ~"15-49",
    Age == "50-54" ~"50-64",
    Age == "55-59" ~"50-64",
    Age == "60-64" ~"50-64",
    Age == "65-69" ~"65+",
    Age == "70-74" ~"65+",
    Age == "75-79" ~"65+",
    Age == "80-84" ~"65+",
    Age == "85+" ~"65+"))

mid_year_pop <- mid_year_pop %>% pivot_longer(
  cols = starts_with("20"),
  names_to ="dodyear",
  values_to = "value")
mid_year_pop <- mid_year_pop %>% select(-Age)
mid_year_pop$dodyear <- as.numeric(mid_year_pop$dodyear)

#USING INTER VA CODES 
muchap_gen_causes <- CoD3 %>% select(causeofdeath)

muchap_gen_causes <- muchap_gen_causes %>%
  group_by(causeofdeath) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

#muchap_gen_causes <- na.omit(muchap_gen_causes)

muchap_gen_causes <- muchap_gen_causes %>%
 # group_by(causeofdeath) %>%
  top_n(n = 20, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

#plot
ggplot(
  muchap_gen_causes, aes(area = percentage, fill = percentage, label = str_wrap(causeofdeath)))+
  geom_treemap()+
  geom_treemap_text(place = "centre", size = 14, 
                    colour = c(rep("white", 1)))+
  scale_fill_viridis_c(direction = 1)
comp <- CoD3 %>% select(ICD10causeofdeaths, causeofdeath)
