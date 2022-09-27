
 #------------------------------Data from four experiments-------------------#

#-- Data analysis 
#includes only data analysis of diets within the separate parts of the 
#experiments 

#---Need to include:
#Data analysis comparing separate parts of the individual experiments 
#Comparing different experiments? 

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

#_________________________________ Installing appropriate packages__________

library(tidyverse)
library(readxl)
library(kableExtra)
library(performance)
library(see)
library(patchwork)
library(usethis)
library(devtools)
library(here)

#_________________________________ Experiment 1 _____________________________# 

#_________________________________ Egg counting

egg_counting_data <- read_csv("~/Downloads/project/eggcountingdata.csv", col_select = 1:5) %>% drop_na()

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
  geom_jitter(data = longdata,
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

female_feedingd1 <- read_excel("~/Desktop/MatedFemalesE1D1.xlsx")


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

female_feedingd1_summary <- long_female_feedingd1 %>%
  group_by(diet) %>%
  summarise(mean = mean(fly_numbers),
            sd=sd(fly_numbers))


female_feedingd1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

female_feedingd1_ls1 <- lm(fly_numbers ~ diet, data = long_female_feedingd1)

summary(female_feedingd1_ls1)

female_feedingd1_ls1
confint(female_feedingd1_ls1)
anova(female_feedingd1_ls1)


performance::check_model(female_feedingd1_ls1)

broom::tidy(female_feedingd1_ls1,  
            exponentiate=T, 
            conf.int=T)

female_feedingd1_table <- female_feedingd1_ls1 %>% broom::tidy(conf.int = T) %>% 
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








#---------------- Female feeding behaviour (Day 2) 


female_feedingd2 <- read_excel("~/Desktop/MatedFemalesE1D2.xlsx")

long_female_feedingd2 <- female_feedingd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly2_numbers")

female_feedingd2_summary <- long_female_feedingd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly2_numbers),
            sd = sd(fly2_numbers),
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
                  y = fly2_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 2*", 
       y = "")+
  theme_minimal()

#---------------Data analysis of female feeding behaviour (experiment 1, day 2)

female_feedingd2_summary <- long_female_feedingd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly2_numbers),
            sd = sd(fly2_numbers),
            n = n(),
            se = sd/sqrt(n))


female_feedingd2_ls1 <- lm(fly2_numbers ~ diet, data = long_female_feedingd2)

summary(female_feedingd2_ls1)

female_feedingd2_ls1
confint(female_feedingd2_ls1)
anova(female_feedingd2_ls1)


performance::check_model(female_feedingd1_ls1)

broom::tidy(female_feedingd2_ls1,  
            exponentiate=T, 
            conf.int=T)


#--------------------------- Male feeding behaviour (Day 1)

male_feedingd1 <- read_excel("~/Desktop/MatedMalesE1D1.xlsx")


long_male_feedingd1 <- male_feedingd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "mfly1_numbers")

male_feedingd1_summary <- long_male_feedingd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(mfly1_numbers),
            sd = sd(mfly1_numbers),
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
                  y = mfly1_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 1*", 
       y = "Mean (+/- S.E.) number of flies on each patch" )+
  theme_minimal()


#-------------- Data Analysis of male feeding behaviour (experiment 1, day 1)


male_feedingd1_summary <- long_male_feedingd1 %>%
  group_by(diet) %>%
  summarise(mean = mean(mfly1_numbers),
            sd=sd(mfly1_numbers))

male_feedingd1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

male_feedingd1_ls1 <- lm(mfly1_numbers ~ diet, data = long_male_feedingd1)

male_feedingd1_ls1
summary(male_feedingd1_ls1)

male_feedingd1_ls2 <- glm(formula = mfly1_numbers ~ diet,
                        family = quasipoisson(), data = long_male_feedingd1)

summary(male_feedingd1_ls2)
performance::check_model(male_feedingd1_ls2, check=c("homogeneity", "qq"))


performance::check_model(male_feedingd1_ls2)


broom::tidy(male_feedingd1_ls2)
anova(male_feedingd1_ls2)


#------------------------ Male feeding behaviour (Day 2)

male_feedingd2 <- read_excel("~/Desktop/MatedMalesE1D1.xlsx")



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

female_notfeedinge1 <- read_csv("~/Downloads/project/femalenf2.csv", col_select = 1:9)  %>% drop_na()

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
       y = "Mean (+/- S.E.) flies per plate not on a patch (females)")+
  theme_minimal()



#----------------- Data for male flies not feeding (exp 1)


male_notfeedinge1 <- read_csv("~/Downloads/project/malenf.csv", col_select = 1:9)  %>% drop_na()

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
       y = "Mean (+/- S.E.) flies per plate not on a patch (males)")+
  theme_minimal()



#----------------------------- Experiment 2-------------------------------------


#--------------------- Mated Females (Day 1, exp 2) 

mated_femalesd1 <- read_csv("~/Downloads/project/MatedFemalesD1.csv")  %>% drop_na()

long_mated_femalesd1 <- mated_femalesd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalesd1_summary <- long_mated_femalesd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers)
            ,
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------------- Visualising data for mated females (exp 2, day 1) 

mated_femalesd1_plot <- long_mated_femalesd1_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
geom_jitter(data = long_mated_femalesd1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()

long_mated_femalesd1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

#----------------------------- Mated Females Day 2 (exp 2)


mated_femalesd2 <- (read_excel(path = "~/Desktop/MatedFemalesD2.xls", na = "NA"))

long_mated_femalesd2 <- mated_femalesd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalesd2_summary <- long_mated_femalesd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers)
            ,
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------------------ Visualising the data for mated females (exp 2, day 2) 

mated_femalesd2_plot <- long_mated_femalesd2_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_mated_femalesd2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#------------------ Data analysis for mated females (exp 2, day 2)

mated_femalesd2_ls <- lm(fly_numbers ~ diet, data = long_mated_femalesd2)

mated_femalesd2_ls
anova(mated_femalesd2_ls)

performance::check_model(mated_femalesd2_ls)
performance::check_model(mated_femalesd2_ls, check=c("homogeneity", "qq"))

broom::tidy(mated_femalesd2_ls,  
            exponentiate=T, 
            conf.int=T)


#----------------------------- Mated Females Day 3

mated_femalesd3 <- (read_excel(path = "~/Desktop/MatedFemalesD3.xlsx", na = "NA"))

long_mated_femalesd3 <- mated_femalesd3 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalesd3_summary <- long_mated_femalesd3 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#-------------- Visualising the data for mated females (exp 2,day 3)

mated_femalesd3_plot <- long_mated_femalesd3_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_mated_femalesd2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()

#-------------- Data analysis for mated females (exp 2, day 3)

mated_femalesd3_ls <- lm(fly_numbers ~ diet, data = long_mated_femalesd3)

summary(mated_femalesd3_ls)



#----------------------------- Virgin Females Day 1 


virgin_femalesd1 <- read_excel("~/Downloads/project/VirginFemalesD1.xls") %>% drop_na()


long_virgin_femalesd1 <- virgin_femalesd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_femalesd1_summary <- long_virgin_femalesd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers)
            ,
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#-------------- Visualising data for virgin females (exp 2, day 1)


virgin_femalesd1_plot <- long_virgin_femalesd1_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_virgin_femalesd1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()

#-------------- Data analysis for virgin females (exp 2, day 1)

long_virgin_femalesd1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


virgin_femalesd1_ls <- lm(fly_numbers ~ diet, data = long_virgin_femalesd1)
summary(virgin_femalesd1_ls)
anova(virgin_femalesd1_ls)
performance::check_model(virgin_femalesd1_ls)
performance::check_model(virgin_femalesd1_ls, check=c("homogeneity", "qq"))
#-- Choose a different linear model !!!
broom::tidy(mated_femalesd2_ls,  
            exponentiate=T, 
            conf.int=T)
 
 #---------------------- Virgin Females Day 2 
 
 virgin_femalesd2 <- (read_excel(path = "~/Downloads/project/VirginFemalesD2.xlsx", na = "NA"))

long_virgin_femalesd2 <- virgin_femalesd2 %>% 
   pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_femalesd2_summary <- long_virgin_femalesd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers)
            ,
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------------------ Visualising the data for virgin females (exp 2, day 2)

virgin_femalesd2_plot <- long_virgin_femalesd2_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_virgin_femalesd2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#------------------ Data analysis for virgin females (exp 2, day 2)

long_virgin_femalesd2_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


virgin_femalesd2_ls <- lm(fly_numbers ~ diet, data = long_virgin_femalesd1)
summary(virgin_femalesd2_ls)
anova(virgin_femalesd2_ls)
performance::check_model(virgin_femalesd2_ls)
performance::check_model(virgin_femalesd2_ls, check=c("homogeneity", "qq"))
#-- Choose a different linear model !!!

broom::tidy(virgin_femalesd2_ls,  
            exponentiate=T, 
            conf.int=T)


#----------------------------- Virgin Females Day 3

virgin_femalesd3 <- (read_excel(path = "~/Desktop/VirginFemalesD3.xlsx", na = "NA"))

long_virgin_femalesd3 <- virgin_femalesd3 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_femalesd3_summary <- long_virgin_femalesd3 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#----------------- Visualising the data for virgin females (exp 3, day 3) 

virgin_femalesd3_plot <- long_virgin_femalesd3_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_virgin_femalesd3,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#----------------- Data analysis for virgin females (exp 3, day 3) 

long_virgin_femalesd3_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


virgin_femalesd3_ls <- lm(fly_numbers ~ diet, data = long_virgin_femalesd3)
summary(virgin_femalesd3_ls)
anova(virgin_femalesd3_ls)
performance::check_model(virgin_femalesd3_ls)
performance::check_model(virgin_femalesd3_ls, check=c("homogeneity", "qq"))
#-- Choose a different linear model !!!

broom::tidy(virgin_femalesd3_ls,  
            exponentiate=T, 
            conf.int=T)

#---------------------- Egg counts for experiment 2

#---------------- Mated female egg count (exp 2)

mated_females_e2_eggcount <- (read_excel(path = "~/Desktop/MatedFemalesE2EggCount.xlsx", na = "NA"))

long_mated_females_e2_eggcount <- mated_females_e2_eggcount %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")

mated_femalese2_eggcount_summary <- long_mated_females_e2_eggcount %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

#-------------- Visualising the data for mated female egg count (exp 2)

mated_females_e2_eggcount_plot <- long_mated_femalese2_eggcount_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_mated_females_e2_eggcount,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,200)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()

#----------- Data analysis for mated female egg count (exp 2)

mated_femalese2_eggcount_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


mated_femalese2_eggcount_ls <- lm(egg_numbers ~ diet, data = long_mated_females_e2_eggcount)
summary(mated_femalese2_eggcount_ls)

anova()
performance::check_model(mated_femalese2_eggcount_ls)
performance::check_model(mated_femalese2_eggcount_ls, check=c("homogeneity", "qq"))
#-- Choose a different linear model !!!

broom::tidy(mated_femalese2_eggcount_ls,  
            exponentiate=T, 
            conf.int=T)






#-------------- Virgin female egg count 

virgin_females_e2_eggcount <- (read_excel(path = "~/Desktop/VirginFemalesE2EggCount.xlsx", na = "NA"))

long_virgin_females_e2_eggcount <- virgin_females_e2_eggcount %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")

long_virgin_femalese2_eggcount_summary <- long_virgin_females_e2_eggcount %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

#-------------- Visualising the data for virgin female egg count 

virgin_female_e2_eggcount_plot <- long_virgin_femalese2_eggcount_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_virgin_females_e2_eggcount,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,200)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()



#------------------------- Data analysis for virgin egg count 

virgin_females_e2_eggcount <- (read_excel(path = "~/Desktop/VirginFemalesE2EggCount.xlsx", na = "NA"))

long_virgin_females_e2_eggcount <- virgin_females_e2_eggcount %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")

long_virgin_femalese2_eggcount_summary <- long_virgin_females_e2_eggcount %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

                               

#----------------------------------- Experiment 3 ----------------------------

#-----------------  Feeding behaviour analysis 

#--------------------- Mated females (exp 3, day 1)

mated_femalese3d1 <- (read_excel(path = "~/Desktop/MatedFemalesE3D1.xlsx", na = "NA"))


long_mated_femalese3d1 <- mated_femalese3d1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalese3d1_summary <- long_mated_femalese3d1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


#--------------------- Visualising the data for mated females (exp 3, day 1)

mated_femalese3d1_plot <- long_mated_femalese3d1_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_mated_femalese3d1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()
 
#--------- Data analysis for mated females (exp 3, day 1)



#--------------------- Mated females (exp 3, day 2)

mated_femalese3d2 <- (read_excel(path = "~/Desktop/MatedFemalesE3D2.xlsx", na = "NA"))

long_mated_femalese3d2 <- mated_femalese3d2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalese3d2_summary <- long_mated_femalese3d2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#--------------  Visualising the data for mated females (exp 3, day 2)

mated_femalese3d2_plot <- long_mated_femalese3d2_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_mated_femalese3d2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#-------- Data analysis for mated females (exp 3, day 2)



#---------------- Males + Females ----- -----------

#----------------------------- Males (m+f) (exp 3, day 1)

males_mf_e3_d1 <- (read_excel(path = "~/Desktop/MatedMalesE3D1(M+F).xlsx", na = "NA"))

long_males_mf_e3_d1 <- males_mf_e3_d1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

males_mf_e3_d1_summary <- long_males_mf_e3_d1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------------------- Visualising the data for males (exp 3, day 1)

males_mf_e3_d1_plot <- males_mf_e3_d1_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_males_mf_e3_d1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,2)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#------------------------- Females (m+f) (exp 3, day 1) 

females_mf_e3_d1 <- (read_excel(path = "~/Desktop/MatedFemalesE3D1(M+F).xlsx", na = "NA"))

long_females_mf_e3_d1 <- females_mf_e3_d1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

females_mf_e3_d1_summary <- long_females_mf_e3_d1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#---------------------- Visualising the data for females (exp 3, day 1)

females_mf_e3_d1_plot <- females_mf_e3_d1_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_females_mf_e3_d1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,2)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()

#-------- Plot showing males and females together (Day 1)

males_mf_e3_d1_plot + females_mf_e3_d1_plot

#-------- Males (m+f) (exp 3,Day 2)

males_mf_e3_d2 <- (read_excel(path = "~/Desktop/MatedMalesE3D2(M+F).xlsx", na = "NA"))

long_males_mf_e3_d2 <- males_mf_e3_d2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

males_mf_e3_d2_summary <- long_males_mf_e3_d2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#--------  Visualising the data for males (m+f) (exp 3, day 2)

males_mf_e3_d2_plot <- males_mf_e3_d2_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_males_mf_e3_d2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,2)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#-------- Females (Day 2)


females_mf_e3_d2 <- (read_excel(path = "~/Desktop/MatedFemalesE3D2(M+F).xlsx", na = "NA"))

long_females_mf_e3_d2 <- females_mf_e3_d2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

females_mf_e3_d2_summary <- long_females_mf_e3_d2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


females_mf_e3_d2_plot <- females_mf_e3_d2_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_females_mf_e3_d2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,2)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()







#------------------------------Experiment 4---------------------------------#


#-----------------------  Mated Females (exp 4, day 1)
 
mated_femalese4d1 <- (read_excel(path = "~/Desktop/MatedFemalesE4D1.xlsx", na = "NA"))


long_mated_femalese4d1 <- mated_femalese4d1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalese4d1_summary <- long_mated_femalese4d1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#---------------- Visualising the data for mated females (exp 4, day 1)

mated_femalese4d1_plot <- long_mated_femalese4d1_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_mated_femalese4d1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()

#------------------ Data analysis for mated females (exp 4, day 1)


#-----------------------  Mated Females (exp 4, day 2)

mated_femalese4d2 <- (read_excel(path = "~/Desktop/MatedFemalesE4D2.xlsx", na = "NA"))


long_mated_femalese4d2 <- mated_femalese4d2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalese4d2_summary <- long_mated_femalese4d2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#----------------- Visualising the data for mated females (exp 4, day 2)

mated_femalese4d2_plot <- long_mated_femalese4d2_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_mated_femalese4d2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()

#------------------ Data analysis for mated females (exp 4, day 2)


#----------------------- Virgin Females (exp 4, day 1)

virgin_femalese4d1 <- (read_excel(path = "~/Desktop/VirginFemalesE4D1.xlsx", na = "NA"))


long_virgin_femalese4d1 <- virgin_femalese4d1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_femalese4d1_summary <- long_virgin_femalese4d1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#--------------- Visualising the data for virgin females (exp 4, day 1)

virgin_femalese4d1_plot <- long_virgin_femalese4d1_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_virgin_femalese4d1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#---------------- Data analysis for virgin females (exp 4, day 1)




#----------------------- Virgin Females (exp 4, day 2)

virgin_femalese4d2 <- (read_excel(path = "~/Desktop/VirginFemalesE4D2.xlsx", na = "NA"))


long_virgin_femalese4d2 <- virgin_femalese4d2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_femalese4d2_summary <- long_virgin_femalese4d2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------------- Visualising the data for virgin females (exp 4, day 2)

virgin_femalese4d2_plot <- long_virgin_femalese4d2_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_virgin_femalese4d2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies")+
  theme_minimal()


#---------------- Data analysis for virgin females (exp 4, day 2)





