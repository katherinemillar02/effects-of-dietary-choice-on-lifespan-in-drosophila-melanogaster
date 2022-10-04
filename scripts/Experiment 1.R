
 #------------------------------Data from four experiments-------------------#

#-------- Experiment 1 (Males and Females)
#---- Male day 1 
#---- Male day 2 
#---- Female day 1 
#---- Female day 2 

#-------- Experiment 2 (Mated Females and Virgin Females)
#---- Mated Female day 1 
#---- Mated Female day 2
#---- Virgin Female day 1
#---- Virgin Female day 2 

#--------Experiment 3 (Mated Females and (Males + Females))
#---- Males and Females day 1 
#---- Males and Females day 2 
#---- Mated Female day 1 
#---- Mated Female day 2 

#--------Experiment 4: repeat of experiment 2 (Mated Females and Virgin Females)
#---- Mated Female day 1 
#---- Mated Female day 2
#---- Virgin Female day 1
#---- Virgin Female day 2 

#________________________ Installing appropriate packages__________________

library(tidyverse)
library(readxl)
library(kableExtra)
library(performance)
library(see)
library(patchwork)
library(usethis)
library(devtools)
library(knitr)
library(emmeans)
library(here)

#_________________________________ Experiment 1 _____________________________# 

#____________________________ Egg counting

egg_counting_data <- read_csv("data/EggCountingE1.csv", col_select = 2:5 ) %>% drop_na()

long_egg_counting1 <- egg_counting_data %>% 
pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")

egg_counting1_summary <- long_egg_counting1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))


#---------- Visualise the data of egg counting (experiment 1)

egg_counting1_plot <- egg_counting1_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting1,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,200)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs")+
  theme_minimal()


#----------- Data Analysis of egg counting (experiment 1)

egg_counting1_summary  <- long_egg_counting1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

egg_counting1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

eggcountingls1 <- lm(egg_numbers ~ diet, data = long_egg_counting1)

eggcountingls1
summary(eggcountingls1)
anova(eggcountingls1)
confint(eggcountingls1)

broom::tidy(eggcountingls1,  
            exponentiate=T, 
            conf.int=T)


performance::check_model(eggcountingls1)


eggcountingls1_table <- eggcountingls1 %>% broom::tidy(conf.int = T) %>% 
  select(-`std.error`) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kbl(col.names = c("Predictors",
                    "Estimates",
                    "Z-value",
                    "P",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "", 
      booktabs = TRUE) %>% 
  kable_styling(full_width = FALSE, font_size=16, latex_options = c("striped", "hold_position"))
  


 


#---------------- Female feeding behaviour (Day 1) 

female_feedingd1 <- read_excel("data/MatedFemalesE1D1.xlsx", 
                               filter("1":"5"))


long_female_feedingd1 <- female_feedingd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

female_feedingd1_summary <- long_female_feedingd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


#---------- Visualise the data of female feeding behaviour (experiment 1, day 1)

female_feedingd1_plot <- female_feedingd1_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_female_feedingd1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 1*",
       y = "Mean (+/- S.E.) number of flies on each patch")+
  theme_minimal()

#------- Data Analysis of female feeding behaviour (experiment 1, day 1)

#female_feedingd1_summary <- long_female_feedingd1 %>%
# group_by(diet) %>%
# summarise(mean = mean(fly_numbers),
#           sd=sd(fly_numbers))


  #female_feedingd1_summary %>%
# kbl(caption=" ") %>% 
# kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

  #female_feedingd1_ls1 <- lm(fly_numbers ~ diet, data = long_female_feedingd1)

  #summary(female_feedingd1_ls1)

  #female_feedingd1_ls1
  #confint(female_feedingd1_ls1)
  #anova(female_feedingd1_ls1)


  #performance::check_model(female_feedingd1_ls1)

  #broom::tidy(female_feedingd1_ls1,  
  # exponentiate=T, 
  #         conf.int=T)

#female_feedingd1_table <- female_feedingd1_ls1 %>% broom::tidy(conf.int = T) %>% 
#select(-`std.error`) %>% 
# mutate_if(is.numeric, round, 2) %>% 
# kbl(col.names = c("Predictors",
#                   "Estimates",
#                   "Z-value",
#                   "P",
#                   "Lower 95% CI",
#                   "Upper 95% CI"),
#     caption = "", 
#     booktabs = TRUE) %>% 
# kable_styling(full_width = FALSE, font_size=16, latex_options = c("striped", "hold_position"))








#---------------- Female feeding behaviour (Day 2) 


female_feedingd2 <- read_excel("data/MatedFemalesE1D2.xlsx")

long_female_feedingd2 <- female_feedingd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

female_feedingd2_summary <- long_female_feedingd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


#---------------- Visualise the data of female feeding (experiment 1, day 2)

female_feedingd2_plot <- female_feedingd2_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_female_feedingd2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 2*", 
       y = "Mean +/- SE number of flies on each patch")+ 
  theme_minimal()

female_feedingd2_plot + male_feedingd2_plot

#---------------Data analysis of female feeding behaviour (exp 1, day 2)

#female_feedingd2_summary <- long_female_feedingd2 %>% 
#  group_by(diet) %>% 
# summarise(mean = mean(fly2_numbers),
#           sd = sd(fly2_numbers),
#           n = n(),
#        se = sd/sqrt(n))


  #female_feedingd2_ls1 <- lm(fly2_numbers ~ diet, data = long_female_feedingd2)

  #summary(female_feedingd2_ls1)

  #female_feedingd2_ls1
  #confint(female_feedingd2_ls1)
  #anova(female_feedingd2_ls1)


#performance::check_model(female_feedingd1_ls1)
#broom::tidy(female_feedingd2_ls1,  
#   exponentiate=T, 
#   conf.int=T)


#--------------------------- Male feeding behaviour (Day 1)

male_feedingd1 <- read_excel("data/MatedMalesE1D1.xlsx")


long_male_feedingd1 <- male_feedingd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

male_feedingd1_summary <- long_male_feedingd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


#----------- Visualise the data of male feeding behaviour (experiment 1, day 1)

male_feedingd1_plot <- male_feedingd1_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#A8DBAF",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#A8DBAF",
                width = 0.2)+
  geom_jitter(data = long_female_feedingd1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 1*", 
       y = "" )+
  theme_minimal()

female_feedingd1_plot + male_feedingd1_plot


#-------------- Data Analysis of male feeding behaviour (experiment 1, day 1)


#male_feedingd1_summary <- long_male_feedingd1 %>%
#  group_by(diet) %>%
# summarise(mean = mean(fly_numbers),
#           sd=sd(fly_numbers))

  #male_feedingd1_summary %>%
  #kbl(caption=" ") %>% 
  #kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

  #male_feedingd1_ls1 <- lm(fly_numbers ~ diet, data = long_male_feedingd1)

  #male_feedingd1_ls1
  #summary(male_feedingd1_ls1)

  #male_feedingd1_ls2 <- glm(formula = fly_numbers ~ diet,
  #    family = quasipoisson(), data = long_male_feedingd1)

#summary(male_feedingd1_ls2)
#performance::check_model(male_feedingd1_ls2, check=c("homogeneity", "qq"))


#performance::check_model(male_feedingd1_ls2)


#broom::tidy(male_feedingd1_ls2)
#anova(male_feedingd1_ls2)


#------------------------ Male feeding behaviour (Day 2)

male_feedingd2 <- read_excel("data/MatedMalesE1D2.xlsx")



long_male_feedingd2 <- male_feedingd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "mfly2_numbers")

male_feedingd2_summary <- long_male_feedingd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(mfly2_numbers),
            sd = sd(mfly2_numbers),
            n = n(),
            se = sd/sqrt(n))

#------------Visualise the data of male feeding behaviour (experiment 1, day 2)

male_feedingd2_plot <- male_feedingd2_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#A8DBAF",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#A8DBAF",
                width = 0.2)+
  geom_jitter(data = long_male_feedingd2,
              aes(x = diet,
                  y = mfly2_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 2*", 
       y = "")+
  theme_minimal()

#-------------------------  Analysis of flies not on a plate (experiment 1)

#----------------- Data for female flies not feeding (exp 1)

female_notfeedinge1 <- read_csv("data/FemaleNotFeedingE1.csv", col_select = 1:9)  %>% drop_na()

long_female_notfeedinge1 <- female_notfeedinge1 %>% 
  pivot_longer(cols = ("1":"8"), names_to = "plate", values_to = "fnf")

female_notfeedinge1_summary <- long_female_notfeedinge1 %>% 
  group_by(plate) %>% 
  summarise(mean = mean(fnf),
            sd = sd(fnf),
            n = n(),
            se = sd/sqrt(n))

#--------------------- Visualising data for female flies not feeding (exp 1)

female_notfeedinge1_plot <- female_notfeedinge1_summary %>% 
  ggplot(aes(x = plate, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_female_notfeedinge1,
              aes(x = plate,
                  y = fnf),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Plate", 
       y = "Mean (+/- S.E.) flies per plate not on a plate")+
  theme_minimal()




#----------------- Data for male flies not feeding (exp 1)


male_notfeedinge1 <- read_csv("~/Documents/drosophilaresearchproject/data/MaleNotFeedingE1.csv", col_select = 1:9)  %>% drop_na()


long_male_notfeedinge1 <- male_notfeedinge1 %>% 
  pivot_longer(cols = ("1":"8"), names_to = "plate", values_to = "mnf")

male_notfeedinge1_summary <- long_male_notfeedinge1 %>% 
  group_by(plate) %>% 
  summarise(mean = mean(mnf),
            sd = sd(mnf),
            n = n(),
            se = sd/sqrt(n))


#--------------- Visualising data for male flies not feeding (exp 1)


male_notfeedinge1_plot <- male_notfeedinge1_summary %>% 
  ggplot(aes(x = plate, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#A8DBAF",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#A8DBAF",
                width = 0.2)+
  geom_jitter(data = long_male_notfeedinge1,
              aes(x = plate,
                  y = mnf),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Plate", 
       y = "")+
  theme_minimal()

female_notfeedinge1_plot + male_notfeedinge1_plot


#----------------------OVERALL DATA ANALYSIS FOR EXPERIMENT 1 ----------------#

#expls1 <- lm(long_female_feedingd1$fly_numbers ~ long_male_feedingd1$fly_numbers)
# Error in variable lengths 
# so merged instead 
#exp1.df <- merge(long_female_feedingd1, long_male_feedingd1, by=c("diet","diet"))

# Mutating a sex variable 
exp1male <- long_male_feedingd1 %>% mutate(sex = "male")

exp1female <- long_female_feedingd1 %>% mutate(sex = "female")


#using r bind to make two data sets one data set 


exp1 <- rbind(exp1male, exp1female)

# Making a linear model
exp1ls1 <- lm(fly_numbers ~ diet + sex, data = exp1)

performance::check_model(exp1ls1)

broom::tidy(exp1ls1,  
            exponentiate=T, 
            conf.int=T)

# Same linear model but with sqrt 
exp1ls1a <- lm(sqrt(fly_numbers) ~ diet + sex , data = exp1)

performance::check_model(exp1ls1a)

broom::tidy(exp1ls1a,  
            exponentiate=T, 
            conf.int=T)


# Making a second linear model (with an interaction effect)

exp1ls2 <- lm(fly_numbers ~ diet * sex, data = exp1)

performance::check_model(exp1ls2)

broom::tidy(exp1ls2,  
            exponentiate=T, 
            conf.int=T)

emmeans::emmeans



# Same linear model but with sqrt 

exp1ls2a <- lm(sqrt(fly_numbers) ~ diet * sex , data = exp1)

performance::check_model(exp1ls2a)

broom::tidy(exp1ls2a,  
            exponentiate=T, 
            conf.int=T)

# From performance::check, exp 2a looks better but probably use (2) 

# Day 2 
exp1male2 <- long_male_feedingd2 %>% mutate(sex = "male")

exp1female2 <- long_female_feedingd2 %>% mutate(sex = "female")

exp1d2 <- rbind(exp1male2, exp1female2)


# Combining the days 
# Females 
exp1females1 <- long_female_feedingd1 %>% mutate(sex = "female") %>% mutate(day = "1")
exp1females2 <- long_female_feedingd2 %>% mutate(sex = "female") %>% mutate(day = "2")

# Combining 
exp1femaleall <- rbind(exp1females1, exp1females2)





