#----------------------------- Experiment 2a ------------------------------------


#--------------------- Mated Females (day 1, exp 2) 

mated_femalesd1 <- read_csv("~/Documents/drosophilaresearchproject/data/MatedFemalesE2aD1.csv")  %>% drop_na()

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
           colour = "#03fc7f",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#03fc7f",
                width = 0.2)+
  geom_jitter(data = long_mated_femalesd1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n Mated",
       y = "Mean (+/- S.E.) number of flies on a feeding patch")+
  theme_minimal()

long_mated_femalesd1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

#----------------------------- Mated Females Day 2 (exp 2)


mated_femalesd2 <- read.csv("~/Documents/drosophilaresearchproject/data/MatedFemalesE2aD2.csv")

long_mated_femalesd2 <- mated_femalesd2 %>% 
  pivot_longer(cols = ("X8.1":"X1.8"), names_to = "diet", values_to = "fly_numbers")

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
           colour = "#03fc7f",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#03fc7f",
                width = 0.2)+
  geom_jitter(data = long_mated_femalesd2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate) \n Mated",
       y = "Mean (+/- S.E.) number of flies on a feeding patch")+
  theme_minimal()


#------------------ Data analysis for mated females (exp 2, day 2)

#mated_femalesd2_ls <- lm(fly_numbers ~ diet, data = long_mated_femalesd2)

#mated_femalesd2_ls
#anova(mated_femalesd2_ls)

#performance::check_model(mated_femalesd2_ls)
#performance::check_model(mated_femalesd2_ls, check=c("homogeneity", "qq"))

#broom::tidy(mated_femalesd2_ls,  
#      exponentiate=T, 
#     conf.int=T)


#----------------------------- Mated Females Day 3

mated_femalesd3 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedFemalesE2aD3.xlsx", na = "NA"))

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
           colour = "#03fc7f",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#03fc7f",
                width = 0.2)+
  geom_jitter(data = long_mated_femalesd3,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)\ n Mated",
       y = "Mean (+/- S.E.) number of flies on a patch")+
  theme_minimal()

#-------------- Data analysis for mated females (exp 2, day 3)

#mated_femalesd3_ls <- lm(fly_numbers ~ diet, data = long_mated_femalesd3)

#summary(mated_femalesd3_ls)



#----------------------------- Virgin Females Day 1 


virgin_femalesd1 <- read_csv("data/VirginFemalesE2aD1.csv", col_select = 1:6) %>% drop_na()


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
           colour = "#c203fc",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#c203fc",
                width = 0.2)+
  geom_jitter(data = long_virgin_femalesd1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate) \nVirgin",
       y = "")+
  theme_minimal()

mated_femalesd1_plot + virgin_femalesd1_plot

#-------------- Data analysis for virgin females (exp 2, day 1)

long_virgin_femalesd1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


#virgin_femalesd1_ls <- lm(fly_numbers ~ diet, data = long_virgin_femalesd1)
#summary(virgin_femalesd1_ls)
#anova(virgin_femalesd1_ls)
#performance::check_model(virgin_femalesd1_ls)
#performance::check_model(virgin_femalesd1_ls, check=c("homogeneity", "qq"))
#-- Choose a different linear model !!!
#broom::tidy(mated_femalesd2_ls,  
#exponentiate=T, 
#conf.int=T)

#---------------------- Virgin Females Day 2 

virgin_femalesd2 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/VirginFemalesE2aD2.xlsx", na = "NA"))

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
           colour = "#c203fc",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#c203fc",
                width = 0.2)+
  geom_jitter(data = long_virgin_femalesd2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)\n Virgin",
       y = "")+
  theme_minimal()

mated_femalesd2_plot + virgin_femalesd2_plot

#------------------ Data analysis for virgin females (exp 2, day 2)
#
#long_virgin_femalesd2_summary %>%
#kbl(caption=" ") %>% 
#kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


#virgin_femalesd2_ls <- lm(fly_numbers ~ diet, data = long_virgin_femalesd1)
#summary(virgin_femalesd2_ls)
#anova(virgin_femalesd2_ls)
#performance::check_model(virgin_femalesd2_ls)
#performance::check_model(virgin_femalesd2_ls, check=c("homogeneity", "qq"))
#-- Choose a different linear model !!!

#broom::tidy(virgin_femalesd2_ls,  
#     exponentiate=T, 
#           conf.int=T)


#----------------------------- Virgin Females Day 3

virgin_femalesd3 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/VirginFemalesE2aD3.xlsx", na = "NA"))

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




#---------------------- Egg counts for experiment 2

#---------------- Mated female egg count (exp 2)

mated_females_e2_eggcount <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedEggCountE2a.xlsx", na = "NA"))

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








#-------------- Virgin female egg count 

virgin_females_e2_eggcount <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/VirginEggCountE2a.xlsx", na = "NA"))

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

virgin_females_e2_eggcount <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/VirginEggCountE2a.xlsx", na = "NA"))

long_virgin_females_e2_eggcount <- virgin_females_e2_eggcount %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")

long_virgin_femalese2_eggcount_summary <- long_virgin_females_e2_eggcount %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))


#------------------------ OVERALL DATA ANALYSIS FOR EXPERIMENT 2 --------------

exp2avirgin <- long_virgin_femalesd1 %>% mutate(variable = "virgin")
exp2amated <- long_mated_femalesd1 %>% mutate(variable = "mated")

# mated v virgin have different n numbers ? but males and females
# had different n numbers and that worked 



# Error in rbind(deparse.level, ...) : 
# numbers of columns of arguments do not match - confused because they don't match 

exp2a <- rbind(exp2amated, exp2avirgin)


exp2als1 <- lm(fly_numbers ~ diet + variable, data = exp2a)

performance::check_model(exp2als1)
# hmmm 

exp2als1a <- lm(sqrt(fly_numbers) ~ diet + variable, data = exp2a)

performance::check_model(exp2als1a)
# without sqrt looks a lot better 



exp2als2 <- lm(fly_numbers ~ diet * variable, data = exp2a)
# hmmm 

performance::check_model(exp2als2)

broom::tidy(exp2als2,  
            exponentiate=T, 
            conf.int=T)


exp2als2a <- lm(sqrt(fly_numbers) ~ diet * variable, data = exp2a)

performance::check_model(exp2als2a)

# without sqrt looks better 

broom::tidy(exp2als2a,  
            exponentiate=T, 
            conf.int=T)
