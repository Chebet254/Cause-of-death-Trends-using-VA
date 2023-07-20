#install.packages("treemapify")

library(forcats)
library(treemap)
library(treemapify)
library(stringr)
library(ggplot2)

CoD3 <- read_dta("va_updated.dta") %>% as_factor()
grouping <- CoD3 #%>% select(ICD10codes)
code_df <- readxl::read_excel("ICD_CODES.xlsx", sheet = 1)
getcodes <- function(x) {
  x <- strsplit(x, "–")[[1]]
  les <- gsub("[0-9]| ", "", x)
  digs <- as.numeric(gsub("[aA-zZ]| ", "", x))*10
  digs <- seq(digs[1], digs[2], by=1)
  digs2 <- formatC(digs, width = 3, format = "d", flag = "0")
  all_n <- unique(c(c(paste0(les[1], digs), paste0(les[2], digs)), c(paste0(les[1], digs2), paste0(les[2], digs2))))
  return(all_n)
}
code_list <- list()
for (l in 1:length(code_df$code)) {
  code_list[[code_df$label[[l]]]] <- getcodes(code_df$code[[l]])
}
all_codes <- unlist(code_list)

df <- grouping
df$new_code <- "Not found"
for (i in 1:NROW(df)) {
  pic <- names(all_codes)[all_codes %in% df$ICD10codes[i]]
  df$new_code[i] <- ifelse(length(pic), pic, "Not found")
}
df$new_code <- gsub("[0-9]", "", df$new_code)

#filter not found
not_found <- df %>% filter(new_code == "Not found")
#write.csv2(df, "data.csv", row.names = F)

#read csv
df2 <- readxl::read_excel("data.xlsx")

#TOTAL DISEASES BY AGE GROUP
#total deaths by age group
df2$age_cat <- factor(df2$age_cat, 
               levels=c("neonate", "infant", "1-4", "5-14", "15-49", "50-64", "65+"))


merged <- df2 %>% select(age_cat, new_code)
merged <- na.omit(merged)

total_ICDchapters <- merged %>%
  group_by(age_cat, new_code) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))


top10_chapters <- total_ICDchapters %>%
  group_by(age_cat) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)



ggplot(top10_chapters, aes(x =as.factor(age_cat), y = percentage, fill =fct_reorder(new_code, -percentage))) +
  geom_bar(aes(group = age_cat),stat = "identity") +
  ylab("Percentage") +
  ggtitle("Overall Top 5 Causes of Death by Age Groups")+
  scale_fill_manual(values = my_colors2,
                    breaks = top10_chapters$new_code)+
  guides(fill=guide_legend(ncol=1), 
         guide = guide_legend(order = 1))+
  theme(axis.title.x=element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"))+
  labs(fill = "General Causes of Death")

#by gender
#by gender
general_gender <- df2 %>% select(age_cat, new_code, sex)

general_gender <- general_gender %>%
  group_by(age_cat, new_code, sex) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

general_gender <- na.omit(general_gender)

general_gender <- general_gender %>%
  group_by(age_cat, sex) %>%
  top_n(n = 5, wt = count)%>%
  mutate(percentage = count / sum(count) * 100)

ggplot(general_gender, aes(x = as.factor(new_code), y = percentage, fill = fct_reorder(new_code, percentage))) +
  geom_bar(aes(group = new_code), stat = "identity")+
  facet_grid(rows = vars(sex), cols = vars(age_cat), scales = "free_x") +
  theme(strip.text = element_text(size = 10, face = "bold")) +
  labs(x = "Cause of Death", y = "Percentage") +
  ggtitle("Top 5 Causes of Death by Age Group, Grouped by Gender")+
  guides(fill=guide_legend(ncol=1),
         guide = guide_legend(order = 1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  scale_fill_manual(values = my_colors2)+
  labs(fill = "Causes of Death")

#under 5 deaths over time- add population
xx <- df2 %>% select(dodyear, age_cat)
xx <- na.omit(xx)
under_five <- xx %>% filter(age_cat == "neonate" | age_cat == "1-4" | age_cat == "infant")
table(under_five)

under_five <- under_five %>%
  group_by(dodyear, age_cat) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))%>%
  mutate(percentage = count / sum(count) * 100)

under_five$dodyear <- as.factor(under_five$dodyear)

# Create the ggplot
ggplot(under_five, aes(x = dodyear, y = percentage, group = age_cat, color = age_cat)) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  labs(x = "Year", y = "Under Five Deaths Rate", title = "Under Five Deaths Rates by Age Group") +
  theme_minimal()

#adults 
adults <- xx %>% filter(age_cat == "5-14" | age_cat == "15-49" | age_cat == "50-64" | age_cat == "65+")
table(adults)

adults <- adults %>%
  group_by(dodyear, age_cat) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))%>%
  mutate(percentage = count / sum(count) * 100)

adults$dodyear <- as.factor(adults$dodyear)

# Create the ggplot
ggplot(adults, aes(x = dodyear, y = percentage, group = age_cat, color = age_cat)) +
  geom_line(linewidth = 0.8)+
  geom_point() +
  labs(x = "Year", y = "Deaths Rate", title = "Adult Deaths Rates by Age Group") +
  theme_minimal()

#treemap
general_chapters <- df2 %>% select(new_code)
general_chapters <- general_chapters %>%
  group_by(new_code) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))%>%
  mutate(percentage = count / sum(count) * 100)


ggplot(
  general_chapters, aes(area = percentage, fill = percentage, label = str_wrap(new_code,5)))+
  geom_treemap()+
  geom_treemap_text(place = "centre", size = 14, 
                    colour = c(rep("white", 1)))+
  scale_fill_viridis_b(direction = 1)

#general chapters with gender

general_chapters_gender <- df2 %>% select(new_code, sex)
general_chapters_gender <- general_chapters_gender %>%
  group_by(new_code, sex) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))%>%
  mutate(percentage = count / sum(count) * 100)


ggplot(
  general_chapters_gender, aes(area = percentage, fill = percentage, label = str_wrap(new_code,5)))+
  geom_treemap()+
  geom_treemap_text(place = "centre", size = 14, 
                    colour = c(rep("white", 1)))+
  scale_fill_viridis_b(direction = 1)+
  facet_wrap(~sex)

#SES data
df3 <- inner_join(df2, mid_year_pop, by = c("dodyear", "age_cat"), relationship = "many-to-many")
df3 <- distinct(df3)
duplicates <- df3[duplicated(df3$age_cat), ]
