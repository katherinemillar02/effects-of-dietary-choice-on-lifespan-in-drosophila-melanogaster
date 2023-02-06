

#----------------------📦  Installing appropriate packages 📦----------
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
library(sjPlot)
library(gtsummary)
library(knitr)
library(rphylopic)

tinytex::install_tinytex()

#--------------------------------- ♀️ Experiment 1 ♂️----------------------------------
#---- 🥚 Egg counting ----
#____ Reading the data in 
egg_counting_data <- (read_excel(path = "data/EggCountingDataExp1.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting1 <- egg_counting_data %>% 
pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting1_summary <- long_egg_counting1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
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
       y = "Mean (+/- S.E.) number of eggs laid on each patch")+
  theme_minimal()
#----------- 📊🥚 Data analysis of Egg Counting ---------------------- 
#-- Making a linear model 
eggcountingls1 <- lm(egg_numbers ~ diet, data = long_egg_counting1)
#---- Checking the model 
performance::check_model(eggcountingls1)
#---- summarising the data 
summary(eggcountingls1)
#----  doing tests 
anova(eggcountingls1) 
confint(eggcountingls1)
#tidy verse summary
broom::tidy(eggcountingls1,  
            exponentiate=T, 
            conf.int=T)




#- ------ FEEDING BEHAHAVIOUR 
#---------------- ♀️ Female feeding behaviour -----------------------------------
#----- Day 1 
#-------- Reading the data in
female_feedingd1 <- read_excel("data/MatedFemalesE1D1.xlsx")
#---- Making the data long
long_female_feedingd1 <- female_feedingd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 1 
exp1female1_summary <- long_female_feedingd1 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#----- Day 2 
#-------- Reading the data in
female_feedingd2 <- read_excel("data/MatedFemalesE1D2.xlsx")
#---- Making the data long
long_female_feedingd2 <- female_feedingd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 2 
exp1female2_summary <- long_female_feedingd2 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Mutating a variable for sex (female) and day 
exp1females1 <- long_female_feedingd1 %>% mutate(sex = "female") %>% mutate(day = "1")
exp1females2 <- long_female_feedingd2 %>% mutate(sex = "female") %>% mutate(day = "2")
#------- Combining the days 
exp1femaleall <- rbind(exp1females1, exp1females2)
#------- Summarising the data 
exp1femaleall_summary <- exp1femaleall %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Visualising the data for female feeding----------------#
exp1_femaleall_plot <- exp1femaleall_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1femaleall,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "Female")+
  theme_minimal() 




#--------------------------- ♂️ Male feeding behaviour ---- 
#----------------- Day 1 
#------ Reading the data in 
male_feedingd1 <- read_excel("data/MatedMalesE1D1.xlsx")
#--- Making the data long 
long_male_feedingd1 <- male_feedingd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#----------------- Day 2 
#------ Reading the data in 
male_feedingd2 <- read_excel("data/MatedMalesE1D2.xlsx")
#---Making the data long 
long_male_feedingd2 <- male_feedingd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#------- Mutating a variable for sex (male) and day 
exp1males1 <- long_male_feedingd1 %>% mutate(sex = "male") %>% mutate(day = "1")
exp1males2 <- long_male_feedingd2 %>% mutate(sex = "male") %>% mutate(day = "2")
#------- Combining the days 
exp1maleall <- rbind(exp1males1, exp1males2)
#------- Summarising the data 
exp1maleall_summary <- exp1maleall %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Visualising the data for male feeding experiment 1 
exp1_maleall_plot <- exp1maleall_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#077535",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#077535",
                width = 0.2)+
  geom_jitter(data = exp1maleall,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch", 
       title = "Male")+
  theme_minimal()
#------- Using patchwork to combine the two parts of data 

exp1_femaleall_plot + exp1_maleall_plot


#------------------------------- Analysis of flies not on a plate (experiment 1)
#----------------- Data for female flies not feeding (exp 1)
#------ Reading the data in 
female_notfeedinge1 <- read_excel("data/FemaleNotFeedingExp1.xlsx")  %>% drop_na()
#------ Making the data long 
long_female_notfeedinge1 <- female_notfeedinge1 %>% 
  pivot_longer(cols = ("1":"8"), names_to = "plate", values_to = "fnf")
#------ Summarising the data 
female_notfeedinge1_summary <- long_female_notfeedinge1 %>% 
  group_by(plate) %>% 
  summarise(mean = mean(fnf),
            sd = sd(fnf),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualising data for female flies not feeding (exp 1)
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
       y = "Mean (+/- S.E.) number of female flies/plate not on a patch")+
  theme_minimal()
#----------------- Data for male flies not feeding (exp 1)
#------ Reading the data in 
male_notfeedinge1 <- read_excel("data/MalesNotFeedingExp1.xlsx")%>% drop_na()
#------ Making the data long 
long_male_notfeedinge1 <- male_notfeedinge1 %>% 
  pivot_longer(cols = ("1":"8"), names_to = "plate", values_to = "mnf")
#------ Summarising the data 
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
       y = "Mean (+/- S.E.) number of male flies/plate not on a patch")+
  theme_minimal()

#------------ Using patchwork to combine the male and female plots ------------# 

female_notfeedinge1_plot + male_notfeedinge1_plot





#----------------- 📊 Data Analysis of feeding behaviours  ---- 
# fly_numbers: number of flies on a flies on a food patch
# sex: whether the fly is male or female 
# diet: one of the four P:C ratios 


# Binding the combined days data of males and females 
exp1all <- rbind(exp1femaleall, exp1maleall) 
# viewing the whole data set 
GGally::ggpairs(exp1all)

# trying normal linear model 
exp1alllm <- lm(fly_numbers ~ diet * sex + day, data = exp1all)
# Checking the model 
# lm looks better from patchwork but do glm anyway 
performance::check_model(exp1alllm)
# using summary function to look at values 
summary(exp1alllm)
# using broom::tidy
broom::tidy(exp1alllm)
# forming a table 
tab_model(exp1alllm)


# ---glm with interaction effect
exp1allglm <- glm(fly_numbers ~ diet * sex + day, data = exp1all, family = poisson())
# --- summarising the data 
summary(exp1allglm)
# - overdispersion so use quasi <1 
exp1allglm2 <- glm(fly_numbers ~ diet * sex + day, data = exp1all, family = quasipoisson())
# Checking the new model 
performance::check_model(exp1allglm2)
# using summary function to look at values 
summary(exp1allglm2)
# testing for significance of day
drop1(exp1allglm2, test = "F")
#  day is only just significant  but keep anyway?? 

# final model choice = linear model with interaction effect and day 
performance::check_model(exp1alllm)
performance::check_model(exp1allglm)
performance::check_model(exp1allglm, check = c("qq"))
performance::check_model(exp1alllm, check = c("qq"))

# use drop1 function to remove top-level terms
drop1(exp1alllm, test = "F")
# both the interaction effect and day are relevant 

# making a table 
tab_model(exp1allglm)
# what is incidence rate ratios vs. estimates? 
# happens in the table when glm over lm is used 


# doing an ANOVA for having more than two groups 
anova(exp1alllm)

# comparing with summary function
summary(exp1alllm)

# variance inflation factor - 
car::vif(exp1alllm)
# what am i looking at? relevance? 

# experiment 1 feeding behaviour is quite complicated so probaly best to ignore what summary gives. 

# forgot to include day!!!
# using emmeans to test all the factors
emmeans::emmeans(exp1alllm, specs = pairwise ~ sex + day + diet + diet * sex  ) 
# emmeans - tukey test contrasts - will include posthoc pairwise comparisons between all levels 
# is there a better way to write this code? 
# doesn't show one of the f-statistics that is needed? 
# is doing emmeans with pairwise just a tukey test then? 

# do not use for interaction model?? - doesnt run 
#meansf <- emmeans::emmeans(exp1femaleall, specs = ~ diet)
#meansm <- emmeans::emmeans(exp1maleall, specs = ~ diet)

# creating a table including f-statistic stuff 
tableexp1feeding <- exp1alllm %>% broom::tidy(conf.int = T) %>% 
  select(-`std.error`) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kbl(col.names = c("Predictors",
                    "Estimates",
                    "Z-value",
                    "P",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "Linear model coefficients", 
      booktabs = T) %>% 
  kable_styling(full_width = FALSE, font_size=16)

#  analyse what table is showing 




#--------------------- IGNORE THE HASHTAGGED OUT CODE
#expls1 <- lm(long_female_feedingd1$fly_numbers ~ long_male_feedingd1$fly_numbers)
# Error in variable lengths 
# so merged instead 
#exp1.df <- merge(long_female_feedingd1, long_male_feedingd1, by=c("diet","diet"))
# Mutating a sex variable 
#exp1male <- long_male_feedingd1 %>% mutate(sex = "male")
#exp1female <- long_female_feedingd1 %>% mutate(sex = "female")
#using r bind to make two data sets one data set 
#exp1 <- rbind(exp1male, exp1female)
#female_feedingd1_summary <- long_female_feedingd1 %>% 
#group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#          se = sd/sqrt(n))
#---------- Visualising the data of female feeding behaviour (experiment 1, day 1)
#female_feedingd1_plot <- female_feedingd1_summary %>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "#FF6863",
#          alpha = 0.6)+
#geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "#FF6863",
#              width = 0.2)+
# geom_jitter(data = long_female_feedingd1,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0.0, 4.0)+
# labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 1*",
#      y = "Mean (+/- S.E.) number of flies on each patch")+
# theme_minimal()

#male_feedingd1_summary <- long_male_feedingd1 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#            n = n(),
#           se = sd/sqrt(n))
#----------- Visualise the data of male feeding behaviour (experiment 1, day 1)
#male_feedingd1_plot <- male_feedingd1_summary %>% 
#ggplot(aes(x = diet, y = mean))+
#geom_bar(stat = "identity",
#        fill = "skyblue",
#        colour = "#A8DBAF",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "#A8DBAF",
#               width = 0.2)+
# geom_jitter(data = long_female_feedingd1,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0.0, 2.0)+
# labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 1*", 
#      y = "" )+
# theme_minimal()
#female_feedingd1_plot + male_feedingd1_plot
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
#female_feedingd2_summary <- long_female_feedingd2 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#           se = sd/sqrt(n))
#---------------- Visualise the data of female feeding (experiment 1, day 2)
#female_feedingd2_plot <- female_feedingd2_summary %>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "#FF6863",
#          alpha = 0.6)+
#  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "#FF6863",
#               width = 0.2)+
#  geom_jitter(data = long_female_feedingd2,
#              aes(x = diet,
#                 y = fly_numbers),
#              fill = "skyblue",
#              colour = "black",
#              width = 0.2,
#              shape = 21)+
#  ylim(0.0, 4.0)+
#  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 2*", 
#       y = "Mean +/- SE number of flies on each patch")+ 
#  theme_minimal()
#female_feedingd2_plot + male_feedingd2_plot
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
# Making a linear model
#exp1ls1 <- lm(fly_numbers ~ diet + sex, data = exp1)
#performance::check_model(exp1ls1)
#broom::tidy(exp1ls1,  
#            exponentiate=T, 
#            conf.int=T)
# Same linear model but with sqrt 
#exp1ls1a <- lm(sqrt(fly_numbers) ~ diet + sex , data = exp1)
#performance::check_model(exp1ls1a)
#broom::tidy(exp1ls1a,  
#           exponentiate=T, 
#           conf.int=T)
# Making a second linear model (with an interaction effect)
#exp1ls2 <- lm(fly_numbers ~ diet * sex, data = exp1)
#performance::check_model(exp1ls2)
#broom::tidy(exp1ls2,  
# exponentiate=T, 
#           conf.int=T)
# Same linear model but with sqrt 
#exp1ls2a <- lm(sqrt(fly_numbers) ~ diet * sex , data = exp1)
#performance::check_model(exp1ls2a)
#broom::tidy(exp1ls2a,  
# exponentiate=T, 
#           conf.int=T)
# From performance::check, exp 2a looks better but probably use (2) 
# Day 2 
#exp1male2 <- long_male_feedingd2 %>% mutate(sex = "male")
#exp1female2 <- long_female_feedingd2 %>% mutate(sex = "female")
#exp1d2 <- rbind(exp1male2, exp1female2)
#anova(male_feedingd1_ls2)
#male_feedingd2_summary <- long_male_feedingd2 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(mfly2_numbers),
#           sd = sd(mfly2_numbers),
#            n = n(),
#            se = sd/sqrt(n))
#------------Visualise the data of male feeding behaviour (experiment 1, day 2)
#male_feedingd2_plot <- male_feedingd2_summary %>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#           fill = "skyblue",
#           colour = "#A8DBAF",
#        alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#         colour = "#A8DBAF",
#           width = 0.2)+
#geom_jitter(data = long_male_feedingd2,
#            aes(x = diet,
#                y = mfly2_numbers),
#              fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0.0, 2.0)+
# labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 2*", 
#      y = "")+
# theme_minimal()