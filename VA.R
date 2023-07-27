#install.packages("paletteer")
#install.packages("ggthemes")
library(tidyverse)
library(haven)
library(viridis)
library(paletteer)
library(ggthemes)
verbal_autopsies <-  read_dta("CoDQN2b.dta")%>% as_factor()
age_grp <- read_dta("CoDQN2b-with age range.dta") %>% as_factor()
CoD3 <- read_dta("CoD3.dta")%>% as_factor()
#adult_cdm_36 <- read_dta("adult_cdm_36.dta")
table(verbal_autopsies$ICD10causeofdeaths, verbal_autopsies$sex)


counts <- (df2 
           %>% group_by(Cleaned_COD, sex) 
           %>% count() 
           %>% arrange(desc(n))
           %>% head(20)
)

#bar plot to show rate of htn
plot<- counts%>% 
  ggplot(aes(x = Cleaned_COD, y = n, fill = sex))+
  geom_bar(stat = "identity", 
           position = "stack",
           col = "lightcoral")+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "right")+
  geom_text(mapping = aes(label = ''), 
            hjust = .2, nudge_x = -.1, colour = "black", vjust = -.2)+
  labs(title = "TOP 10 CAUSES OF DEATH- IGANGA MAYUGE",
       x = "ICD 10 CAUSES OF DEATH")+
  guides(fill=guide_legend(ncol=3),
         guide = guide_legend(order = 1))

plot

#icd 10 & years
#top_n <- 1:10
counts3 <- (df 
  %>% select(ICD10causeofdeaths, dodyear)
  %>% group_by(ICD10causeofdeaths, dodyear )
  %>% count()
  %>% ungroup()
  %>% group_by(ICD10causeofdeaths)
  %>% mutate(y = sum(n))
  %>% arrange(desc(y))
  %>% ungroup()
  %>% group_by(y)
  %>% mutate(index = 1:n()) 
  #%>% head(10)
  
)
data_new <- counts3[1:140, ]

ggplot(data_new, aes(x = dodyear, y = n,
                      group = (ICD10causeofdeaths),
                      colour = ICD10causeofdeaths)) +
  geom_line(linewidth = 2, alpha = 2, linetype = 1)+
  theme_light()+
  ggtitle("ICD10 CAUSES OF DEATH OVER THE YEARS (2007-2022)")+
  scale_x_continuous(breaks = seq(2005, 2022, by = 2))+
  scale_y_continuous(breaks = seq(0, 200, by = 20))+
  theme(legend.position = 'bottom')


#MIKE
#top ten per year
#top 10 per year
top_n_year <- 10
data_subset = (df2
               %>% select("dodyear",  "Cleaned_COD")
               %>% group_by(dodyear, Cleaned_COD)
               %>%count()
               %>% ungroup()
               %>% group_by(dodyear)
               %>% mutate(percentage = n / sum(n) * 100)
               %>%arrange(desc(n),.by_group = TRUE)
               %>% mutate(index = 1:n())
               %>% filter(index <= top_n_year)
               %>% ungroup()
)
colnames(data_subset) <- c('yearofdeath', 'Cleaned_COD', 'n', 'percentage', 'index')

data_subset <- data_subset %>% filter(yearofdeath >2010)
t1 = c('2011','2012','2013','2014')
t2 = c('2015','2016','2017','2018' )
t3 = c('2019','2020','2021', '2022')


data_subset <- (data_subset
  %>% mutate(.new_group = as.factor(yearofdeath)
    , .new_group = fct_collapse(.new_group
      , `2012 - 2015` = t1
      , `2016 - 2019` = t2
      , `2020 - 2022` = t3
    )
  )
)


ggplot(data = data_subset, 
       #%>% filter(dodyear %in% t1), 
       aes(x=as.factor(yearofdeath), y=n, fill = fct_reorder(Cleaned_COD, -n)))+
  geom_bar(aes(group = yearofdeath),stat="identity", position = "dodge2")+
 facet_wrap(~.new_group, scales = "free_x", ncol = 1)+
  theme_light()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1),
         guide = guide_legend(order = 1)  ) + 
  labs(x ='', y = '', fill = "Causes of Death")+
  my_theme+
scale_fill_manual(values = col3)+
  ggtitle("ICD10 CAUSES OF DEATH OVER THE YEARS- IGANGA")
  
#AGE 

ages_over_time <- (top10_data
                %>% mutate(.new_group = as.factor(dodyear)
                           , .new_group = fct_collapse(.new_group
                                                       , `2012 - 2015` = t1
                                                       , `2016 - 2019` = t2
                                                       , `2020 - 2022` = t3
                           )
                )
)

ggplot(data = top10_data,  
       aes(x=as.factor(age_range), y=n, fill = fct_reorder(ICD10causeofdeaths, -percentage)))+
  geom_bar(aes(group = dodyear),stat="identity", position = "dodge2")+
  facet_wrap(~.new_group, scales = "free_x", ncol = 1)+
  theme_light()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1)) + 
  labs(fill = "Causes of Death")+
  my_theme+
  scale_fill_manual(values = my_colors)


#AGE GROUPS 
#NEONATES
age_data <- (age_grp 
            %>% select(ICD10causeofdeaths, dodyear, age_range)
            %>% group_by(ICD10causeofdeaths, dodyear, age_range)
            %>% count()
            %>% filter(age_range == 'Neonate')
            %>% ungroup()
            %>% group_by(ICD10causeofdeaths)
            %>% mutate(y = sum(n))
            %>% arrange(desc(y))
            %>% ungroup()
            %>% group_by(y)
            %>% mutate(index = 1:n()) 
            #%>% head(10)
            
)
neon <- age_data %>% filter(age_range == 'Neonate')
neon <- neon[1:44, ]

ggplot(neon, aes(x = dodyear, y = n,
                     group = (ICD10causeofdeaths),
                     colour = ICD10causeofdeaths)) +
  geom_bar(stat="identity", position = "dodge2")+
  theme_light()+
  ggtitle("ICD10 NEONATE CAUSES OF DEATH OVER THE YEARS (2007-2022)")+
  scale_x_continuous(breaks = seq(2012, 2022, by = 2))+
  scale_fill_manual(values = palettes)

#MERGED
top10_data <- top10_data%>% mutate(flag = 'Iganga')
nuhdss_top10_data <- nuhdss_top10_data%>% mutate(flag = 'Nuhdss')
meiru_top10_data <- meiru_top10_data%>% mutate(flag = 'Karonga')
kisesa_top10_data <- kisesa_top10_data%>% mutate(flag = 'Magu')

AgecatMerged <- rbind(top10_data, nuhdss_top10_data, meiru_top10_data, kisesa_top10_data)

ggplot(AgecatMerged, aes(x = as.factor(age_cat), y = percentage,
                         fill = fct_reorder(Cleaned_COD, -percentage))) +
  geom_bar( aes(group = age_cat), stat = "identity") +
  facet_grid(rows = vars(flag),cols = NULL, scales = "free") +
  theme(strip.text = element_text(size = 20, face = "bold")) +
  labs(x = "", y = "", fill = 'Cause of Death') +
  ggtitle("Overall Top 5 Causes of Death by Age Group across the four sites")+
  guides(fill=guide_legend(ncol=1),
                       guide = guide_legend(order = 1))+
  theme_bw()+
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      legend.text = element_text(size =30),
      legend.title = element_text(size = 32, face = "bold"),
      legend.key.size = unit(1, "cm"),
      legend.position = 'right',
      axis.text = element_text(size = 20, face = 'bold'),
      axis.title  = element_text(size=18,color='white',face='bold'),
      title = element_text(size = 40, face = 'bold'))+
  scale_fill_manual(values = col3)+
  theme(legend.position = "right")


#COD OVER TIME MERGED
data_subset <- data_subset %>% mutate(flag = 'Iganga')
nuhdss_data_subset <-nuhdss_data_subset %>% mutate(flag = 'Nuhdss')
meiru_data_subset <-meiru_data_subset %>% mutate(flag = 'Karonga')
kisesa_data_subset <-kisesa_data_subset %>% mutate(flag = 'Magu')
MergedSubset <-  rbind(data_subset, nuhdss_data_subset, meiru_data_subset, kisesa_data_subset)

ggplot(MergedSubset, aes(x = as.factor(yearofdeath), y = percentage,
                         fill = fct_reorder(Cleaned_COD, -percentage))) +
  geom_bar( aes(group = yearofdeath), stat = "identity", position = "dodge2") +
  facet_grid(rows = vars(flag),cols = NULL, scales = "free") +
  theme(strip.text = element_text(size = 35, face = "bold")) +
  labs(x = "", y = "", fill = 'Cause of Death') +
  ggtitle("CAUSES OF DEATH OVER THE YEARS")+
  guides(fill=guide_legend(ncol=1),
         guide = guide_legend(order = 1))+
  #theme_bw()+
theme(axis.title.x=element_blank(),
      legend.text = element_text(size =30),
      legend.title = element_text(size = 32, face = "bold"),
      legend.key.size = unit(1, "cm"),
      legend.position = 'right',
      axis.text = element_text(size = 20, face = 'bold'),
      axis.title  = element_text(size=18,color='white',face='bold'),
      title = element_text(size = 40, face = 'bold'))+
  scale_fill_manual(values = col3)+
theme(legend.position = "right")
