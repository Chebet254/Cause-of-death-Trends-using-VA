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


counts <- (verbal_autopsies 
           %>% group_by(ICD10causeofdeaths, sex) 
           %>% count() 
           %>% arrange(desc(n))
           %>% head(20)
)

#bar plot to show rate of htn
plot<- counts%>% 
  ggplot(aes(x = ICD10causeofdeaths, y = n, fill = sex))+
  geom_bar(stat = "identity", 
           position = "stack",
           col = "lightcoral")+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "right")+
  geom_text(mapping = aes(label = ''), 
            hjust = .2, nudge_x = -.1, colour = "black", vjust = -.2)+
  labs(title = "TOP 10 CAUSES OF DEATH",
       x = "ICD 10 CAUSES OF DEATH")

plot

#icd 10 & years
#top_n <- 1:10
counts3 <- (verbal_autopsies 
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

#top 10 per year
# top_n_year <- 10
# data_subset = (verbal_autopsies 
#                %>% select("dodyear",  "ICD10causeofdeaths") 
#                %>% group_by(dodyear, ICD10causeofdeaths)
#                %>%count()
#                %>% ungroup()
#                %>% group_by(dodyear)
#                %>%arrange(desc(n),.by_group = TRUE)
#                %>% mutate(index = 1:n())
#                %>% filter(index <= top_n_year)
#                %>% ungroup()
# 
# )
# 
# # Stacked
# ggplot(data_subset, aes(fill=fct_reorder(ICD10causeofdeaths, -n), y=n, x=as.factor(dodyear))) + 
#   geom_bar(aes(group = dodyear), position="dodge2", stat="identity")+
#   #scale_fill_viridis(discrete = T, option = 'D')+
#   scale_color_brewer(palette = "Pastel1")+
#   #scale_colour_paletteer_d(ggsci, nrc_npg)
#   theme_bw()+
#   theme(legend.position = 'bottom')
# 

#MIKE
#top ten per year
#top 10 per year
top_n_year <- 10
data_subset = (age_grp
               %>% select("dodyear",  "ICD10causeofdeaths")
               %>% group_by(dodyear, ICD10causeofdeaths)
               %>%count()
               %>% ungroup()
               %>% group_by(dodyear)
               %>%arrange(desc(n),.by_group = TRUE)
               %>% mutate(index = 1:n())
               %>% filter(index <= top_n_year)
               %>% ungroup()
)

data_subset <- data_subset %>% filter(dodyear >2011)
t1 = c('2012','2013','2014','2015')
t2 = c('2016','2017','2018','2019' )
t3 = c('2020','2021', '2022')


data_subset <- (data_subset
  %>% mutate(.new_group = as.factor(dodyear)
    , .new_group = fct_collapse(.new_group
      , `2012 - 2015` = t1
      , `2016 - 2019` = t2
      , `2020 - 2022` = t3
    )
  )
)


#palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]][["Tableau 20"]]$value

ggplot(data = data_subset, 
       #%>% filter(dodyear %in% t1), 
       aes(x=as.factor(dodyear), y=n, fill = fct_reorder(ICD10causeofdeaths, -n)))+
  geom_bar(aes(group = dodyear),stat="identity", position = "dodge2")+
 facet_wrap(~.new_group, scales = "free_x", ncol = 1)+
  theme_light()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1)) + 
  labs(fill = "Causes of Death")+
  theme(axis.title.x=element_blank())+
scale_fill_manual(values = my_colors)
  
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
  theme(axis.title.x=element_blank())+
  scale_fill_manual(values = my_colors)

# ggplot(data = data_subset 
#        %>% filter(dodyear %in% t2), 
#        aes(x=dodyear, y=n, fill = ICD10causeofdeaths))+
#   geom_bar(stat="identity", position = "dodge2")+
#   # facet_wrap(vars(ICD10causeofdeaths), ncol = 1)+
#   theme_light()+
#   theme(legend.position = "right"
#   )
#   

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

#STILL BIRTHS
SB <- age_grp %>% filter(age_range == 'Stillbirth')
SB <- (SB  %>% select(ICD10causeofdeaths, dodyear, age_range)
             %>% group_by(ICD10causeofdeaths, dodyear, age_range)
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

SB <- SB[1:56, ]

ggplot(SB, aes(x = dodyear, y = n,
                 group = (ICD10causeofdeaths),
                 colour = ICD10causeofdeaths)) +
  geom_line(linewidth = 2, alpha = 2, linetype = 1)+
  theme_light()+
  ggtitle("ICD10 STILL BIRTH CAUSES OF DEATH OVER THE YEARS (2007-2022)")+
  scale_x_continuous(breaks = seq(2005, 2022, by = 2))+
  scale_y_continuous(breaks = seq(0, 200, by = 20))

#ADULTS 
Adults <- age_grp %>% filter(age_range == 'Adult')
Adults <- (Adults %>% select(ICD10causeofdeaths, dodyear, age_range)
       %>% group_by(ICD10causeofdeaths, dodyear, age_range)
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
Adults<- Adults[1:128, ]

ggplot(Adults, aes(x = dodyear, y = n,
               group = (ICD10causeofdeaths),
               colour = ICD10causeofdeaths)) +
  geom_line(linewidth = 2, alpha = 2, linetype = 1)+
  theme_light()+
  ggtitle("ICD10 ADULTS CAUSES OF DEATH OVER THE YEARS (2007-2022)")+
  scale_x_continuous(breaks = seq(2005, 2022, by = 2))+
  scale_y_continuous(breaks = seq(0, 200, by = 20))

#CHILD
child <- age_grp %>% filter(age_range == 'Child', dodyear >2012)
child <- (child %>% select(ICD10causeofdeaths, dodyear, age_range)
           %>% group_by(ICD10causeofdeaths, dodyear, age_range)
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
child<- child[1:63, ]

ggplot(child, aes(x = dodyear, y = n,
                   group = (ICD10causeofdeaths),
                   colour = ICD10causeofdeaths)) +
  geom_line(linewidth = 2, alpha = 2, linetype = 1)+
  theme_light()+
  ggtitle("ICD10 CHILD CAUSES OF DEATH OVER THE YEARS (2007-2022)")+
  scale_x_continuous(breaks = seq(2005, 2022, by = 2))+
  scale_y_continuous(breaks = seq(0, 200, by = 20))




# theme(axis.title.x=element_blank()
#       , axis.text.x=element_blank()
#       , axis.ticks.x=element_blank()
#       , legend.position = "right"
# )

# data_new <- counts3[order(counts3$y, decreasing = TRUE), ]
# data_new <- Reduce(rbind,                                 # Top N highest values by group
#                     by(data_new,
#                        data_new["y"],
#                        head,
#                        n = 5))

