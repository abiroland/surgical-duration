#Regression analysis project: Exploratory analysis
#Edited on 05-01-2023
#by: Roland Abi and Prasana

# Prep -------------------------------------------------------------------------
library(tidyverse)
library(bbplot)
library(viridis)
library(ggpubr)

# Distribution of patients based on gender and ASAstatus
plt1 <- proj_df %>% 
  mutate(
    ASAstatus = fct_recode(ASAstatus,
                           "Normal patient" = "Normal Healthy Patient",
                           "Mild disease" = "Mild Systemic Disease",
                           "Severe disease" = "Severe Systemic Disease"),
    ASAstatus = factor(ASAstatus, levels = c("Normal patient",
                                             "Mild disease",
                                             "Severe disease"))
  )%>%
  with(table(ASAstatus, Gender)) %>%
  prop.table(1) %>%
  data.frame() %>%
  mutate(
    Freq = round(Freq*100,1)
  ) %>%
  ggplot(aes(x = Freq, y = ASAstatus, fill = Gender))+
  scale_fill_manual(values = c("#FAAB18", "#1380A1")) +
  geom_bar(stat = "identity") +
  bbc_style() +
  labs(title = "ASA status and Gender",
       subtitle = "Patients health status based on ASA status and Gender",
       x = "Percentage",
       y = "ASA status") +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = T)) +
  coord_flip()+
  geom_text(aes(label = Freq), color = "white", size = 8,
            position = position_stack(vjust = .5))


# Distribution of surgical duration by characteristics--------------------------

colSums(is.na(proj_df))

#plot for gender
plt2 <- proj_df %>% 
  #creating race groups
  mutate(
    newage = cut(Age,
                 breaks = seq(20, 100, by = 20),
                 labels = c("<20", "20-40", "40-60", "60+"))) %>%
  mutate(
    Race = factor(Race, levels = c("Others", "Black", "White"))) %>%
  na.omit() %>%
  group_by(Race, newage) %>%
  ggplot(aes(x = newage, y = DurationSurgery,
             fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#805D30", "#FFBB62", "#BF8C49")) +
  geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
  bbc_style() +
  guides(fill = guide_legend(reverse = F)) +
  labs(
    title = "Race")

#Surgical duration age vs diabetes
plt3 <- proj_df %>% 
  mutate(
    newage = cut(Age,
                 breaks = seq(20, 100, by = 20),
                 labels = c("<20", "20-40", "40-60", "60+"))) %>%
  na.omit() %>%
  group_by(Diabetes, newage) %>%
  ggplot(aes(x = newage, y = DurationSurgery,
             fill = Diabetes)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#F3DEBA", "#A9907E")) +
  geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
  bbc_style() +
  guides(fill = guide_legend(reverse = F)) +
  labs(
    title = "Diabetes")

#surgical duration vs chronic renal failure
plt4 <- proj_df %>% 
  mutate(
    newage = cut(Age,
                 breaks = seq(20, 100, by = 20),
                 labels = c("<20", "20-40", "40-60", "60+"))) %>%
  na.omit() %>%
  group_by(ChronicRenalFailure, newage) %>%
  ggplot(aes(x = newage, y = DurationSurgery,
             fill = ChronicRenalFailure)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#D3756B", "#F0997D")) +
  geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
  bbc_style() +
  guides(fill = guide_legend(reverse = F)) +
  labs(
    title = "Chronic Renal failure") 


#surgical duration vs ASA status
plt5 <- proj_df %>% 
  mutate(
    ASAstatus = fct_recode(ASAstatus,
                           "Normal patient" = "Normal Healthy Patient",
                           "Mild disease" = "Mild Systemic Disease",
                           "Severe disease" = "Severe Systemic Disease"),
    ASAstatus = factor(ASAstatus, levels = c("Normal patient",
                                             "Mild disease",
                                             "Severe disease"))
  )%>%
  mutate(
    newage = cut(Age,
                 breaks = seq(20, 100, by = 20),
                 labels = c("<20", "20-40", "40-60", "60+"))) %>%
  na.omit() %>%
  group_by(ASAstatus, newage) %>%
  ggplot(aes(x = newage, y = DurationSurgery,
             fill = ASAstatus)) +
  geom_bar(stat = "identity", position = "dodge", size = 0.5) +
  scale_fill_manual(values = c("#EBD8B2", "#B7B7B7", "#E8D5C4")) +
  geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
  bbc_style() +
  guides(fill = guide_legend(reverse = F)) +
  labs(
    title = "ASA status") 

#combine plots -----------------------------------------------------------------
plt6 <- ggarrange(plt2, plt3, plt4, plt5, 
          nrow = 2, ncol = 2)

#Age BMI vs surgical duration---------------------------------------------------

#roj_line_df2 %>%
  #ggplot(aes(x = Male, xend = Female, 
    #         y = grpbmi)) +
  #geom_dumbbell(color = "#dddddd",
   #             size = 6,
                #colour_x = "#faab18",
               #colour_xend = "#1380A1",) +
  #bbc_style() +
  #labs(
    #title = "Surgical duration, BMI and gender",
  #) 


plt7 <- ggplot(proj_line_df, aes(x=Age, y = DurationSurgery, 
                               color = Gender)) +
  geom_line(linewidth = 1) +
  facet_wrap(.~ grpbmi) +
  geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
  scale_color_manual(values = c("#FAAB18", "#1380A1")) +
  bbc_style() +
  labs(title = "Surgical duration and BMI", 
       subtitle = "for patients age >= 60")
