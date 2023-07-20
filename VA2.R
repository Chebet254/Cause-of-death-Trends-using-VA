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
CoD3 <- read_dta("va_updated.dta") %>% as_factor()
#ylll <- read_dta("YLLL.dta") %>% as_factor()
#total deaths by age group
total_ages <- CoD3 %>% select(age_cat, ICD10causeofdeaths)
total_ages <- total_ages %>%
  group_by(age_cat, ICD10causeofdeaths) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

total_ages <- na.omit(total_ages)

top10_data <- total_ages %>%
  group_by(age_cat) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

my_colors <- c("red", "lawngreen", "blue", "yellow", "purple","hotpink", "powderblue","darkblue", "deepskyblue",
               "cyan", "royalblue1","yellowgreen", "brown","peachpuff4","aquamarine4","skyblue","palegreen",
               "orange", "lavender", "pink", "springgreen","cornflowerblue","azure4", "palegoldenrod",
               "yellow4", "black", "gold2",  "peru", "lightslateblue", "darkgreen", "cyan3", 
               "aquamarine", "darkslategray", "indianred2", "olivedrab1", "violet", "seagreen","magenta", "thistle",
               "purple4", "gray30", "darkolivegreen", "beige")

my_colors2 <- c("red", "lawngreen", "blue", "yellow", "purple","hotpink", "olivedrab1","darkblue", "deepskyblue",
                "cyan", "royalblue1","yellowgreen", "brown","peachpuff4","aquamarine4","skyblue","peru",
                "orange", "yellow4", "pink", "springgreen","darkslategray", "black", "gold", "beige")

ggplot(top10_data, aes(x =as.factor(age_cat), y = percentage, fill =fct_reorder(ICD10causeofdeaths, -percentage))) +
  geom_bar(aes(group = age_cat),stat = "identity") +
  ylab("Percentage") +
  ggtitle("Overall Top 5 Causes of Death by Age Group")+
  scale_fill_manual(values = my_colors2)+
  guides(fill=guide_legend(ncol=1))+
  theme(axis.title.x=element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key.size = unit(1.5, "cm"))+
  labs(fill = "Causes of Death")


#by gender
gender_grouping <- CoD3 %>% select(age_cat, ICD10causeofdeaths, sex)
gender_group <- gender_grouping %>%
  group_by(age_cat, ICD10causeofdeaths, sex) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

gender_group <- na.omit(gender_group)

gender_group <- gender_group %>%
  group_by(age_cat, sex) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

ggplot(gender_group, aes(x = as.factor(ICD10causeofdeaths), y = percentage, fill = fct_reorder(ICD10causeofdeaths, -percentage))) +
  geom_bar(aes(group = ICD10causeofdeaths), stat = "identity") +
  facet_grid(rows = vars(sex), cols = vars(age_cat), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold")) +
  labs(x = "Cause of Death", y = "Percentage") +
  ggtitle("Top 5 Causes of Death by Age Group, Grouped by Gender")+
  guides(fill=guide_legend(ncol=1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  scale_fill_manual(values = my_colors2)
  



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

ggplot(g2, aes(x = as.factor(dodyear), y = percentage, fill = fct_reorder(ICD10causeofdeaths, -percentage))) +
  geom_bar(aes(group = dodyear),stat = "identity", position = "dodge2") +
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

ggplot(g3, aes(x = as.factor(dodyear), y = percentage, fill = fct_reorder(ICD10causeofdeaths, -percentage))) +
  geom_bar(aes(group = dodyear),stat = "identity", position = "dodge2") +
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

ggplot(g4, aes(x = as.factor(dodyear), y = percentage, fill = fct_reorder(ICD10causeofdeaths, -percentage))) +
  geom_bar(aes(group = dodyear), stat = "identity", position = "dodge2") +
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

ggplot(g5, aes(x = as.factor(dodyear), y = percentage, fill = fct_reorder(ICD10causeofdeaths, -percentage))) +
  geom_bar(aes(group = dodyear), stat = "identity", position = "dodge2") +
  facet_grid(rows = vars(age_cat),cols = vars(dodyear), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold")) +
  labs(x = "Year", y = "Percent", fill='') +
  ggtitle("Top 5 Causes of Death by Age Group Over Time")+
  guides(fill=guide_legend(ncol=1))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.text = element_text(size = 7),
        legend.position = 'right')+
  scale_fill_manual(values = my_colors2)
str(mid_year_pop)
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
