#----------------------------- 🪰 Experiment 2a ------------------------------------
#--------------------- ♀️ Mated Females -----
#------  Day 1 
#--- Reading the data in 
mated_femalesd1 <- read_excel("data/MatedFemalesE2aD1.xlsx")  %>% drop_na()
#------------- Making the data long 
long_mated_femalesd1 <- mated_femalesd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#---------- Day 2 
#--- Reading the data in
mated_femalesd2 <- (read_excel(path = "data/MatedFemalesE2aD2.xlsx", na = "NA" ))
#---------- Making the data long 
long_mated_femalesd2 <- mated_femalesd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#---------- Day 3 
#--- Reading the data in
mated_femalesd3 <- (read_excel(path = "data/MatedFemalesE2aD3.xlsx", na = "NA"))
#---------- Making the data long 
long_mated_femalesd3 <- mated_femalesd3 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#------- Mutating variables 
exp2amated1 <- long_mated_femalesd1 %>% mutate(type = "mated") %>% mutate(day = "1")
exp2amated2 <- long_mated_femalesd2 %>% mutate(type = "mated") %>% mutate(day = "2")
exp2amated3 <- long_mated_femalesd3 %>% mutate(type = "mated") %>% mutate(day = "3")
#-------- Binding mated days 1 - 3 
exp2matedall <- rbind(exp2amated1, exp2amated2, exp2amated3)
# ------ Summarising the data 
exp2matedall_summary <- exp2matedall %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#--------  Visualising the data for mated females experiment 2a 
exp2matedall_plot <- exp2matedall_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = exp2matedall,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of mated female flies")+
  theme_minimal()
#-------- 👰 Virgin Females ----  
#------  Day 1 
#--- Reading the data in 
virgin_femalesd1 <- read_excel("data/VirginFemalesE2aD1.xlsx") %>% drop_na()
#---------- Making the data long 
long_virgin_femalesd1 <- virgin_femalesd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#---------- Day 2 
#--- Reading the data in
virgin_femalesd2 <- (read_excel(path = "data/VirginFemalesE2aD2.xlsx", na = "NA"))
#---------- Making the data long 
long_virgin_femalesd2 <- virgin_femalesd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#---------- Day 3 
#--- Reading the data in
virgin_femalesd3 <- (read_excel(path = "data/VirginFemalesE2aD3.xlsx", na = "NA"))
#------- Making the data long 
long_virgin_femalesd3 <- virgin_femalesd3 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
# -------  Mutating variables 
exp2avirgin1 <- long_virgin_femalesd1 %>% mutate(type = "virgin") %>% mutate(day = "1")
exp2avirgin2 <- long_virgin_femalesd2 %>% mutate(type = "virgin") %>% mutate(day = "2")
exp2avirgin3 <- long_virgin_femalesd3 %>% mutate(type = "virgin") %>% mutate(day = "3")
#------ Binding virgin days 1 - 3 
exp2avirginall <- rbind(exp2avirgin1, exp2avirgin2, exp2avirgin3)
#------ Summarising the data 
exp2avirginall_summary <- exp2avirginall %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------ Visualising the data 
exp2avirginall_plot <- exp2avirginall_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#eb34c3",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#eb34c3",
                width = 0.2)+
  geom_jitter(data = exp2avirginall,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,6)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of virgin female flies")+
  theme_minimal()
#------- Using patchwork to combine the two parts of data 
exp2matedall_plot + exp2avirginall_plot

#------ Egg counting data -----------------------------------------------------#
# ---- ♀️🥚 Mated Egg count ----
# Reading the data in 
mated_females_e2_eggcount <- (read_excel(path = "data/MatedEggCountE2a.xlsx", na = "NA"))
# Making the data long 
long_mated_females_e2_eggcount <- mated_females_e2_eggcount %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")
# Summarising the data 
mated_femalese2_eggcount_summary <- long_mated_females_e2_eggcount %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#-------------- Visualising the data for mated female egg count 
mated_females_e2_eggcount_plot <- mated_femalese2_eggcount_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = long_mated_females_e2_eggcount,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,150)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number eggs layed by mated flies")+
  theme_minimal()



#-------------- 👰🥚Virgin Egg ----
# Reading the data in
virgin_females_e2_eggcount <- (read_excel(path = "data/VirginEggCountE2a.xlsx", na = "NA"))
# Making the data long 
long_virgin_females_e2_eggcount <- virgin_females_e2_eggcount %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")
# Summarising the data 
virgin_females_e2_eggcount_summary <- long_virgin_females_e2_eggcount %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#-------------- Visualising the data for virgin female egg count 
virgin_female_e2_eggcount_plot <- virgin_females_e2_eggcount_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#eb34c3",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#eb34c3",
                width = 0.2)+
  geom_jitter(data = long_virgin_females_e2_eggcount,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,150)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs layed by virgin flies")+
  theme_minimal()

#---------- Using patchwork to combine the mated and virgin egg counting plots 
mated_females_e2_eggcount_plot + virgin_female_e2_eggcount_plot

#------------------------- Data analysis for egg count ----


#-----------
exp2avirginegg <- long_virgin_females_e2_eggcount %>% mutate(type = "virgin") 
exp2amatedegg <- long_mated_females_e2_eggcount %>% mutate(type = "mated") 

exp2a_all_egg <- rbind(exp2avirginegg, exp2amatedegg)

exp2a_egg_lm <- lm(egg_numbers ~ diet * type, data = exp2a_all_egg)

performance::check_model(exp2a_egg_lm)

summary(exp2a_egg_lm)


virgin_females_e2_eggcount <- (read_excel(path = "data/VirginEggCountE2a.xlsx", na = "NA"))
long_virgin_females_e2_eggcount <- virgin_females_e2_eggcount %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")

long_virgin_females_e2_eggcount_summary <- long_virgin_females_e2_eggcount %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#--------------- 📊Data Analysis of Feeding Behaviour   --------------
# Binding mated and virgin days 1 - 3 
exp2a_all <- rbind(exp2matedall, exp2avirginall)
# making a linear model 
exp2a_all_lm <- lm(fly_numbers ~ diet + type + day, data = exp2a_all)
# Checking the data 
performance::check_model(exp2a_all_lm)



# linear model with interaction effect
exp2a_all_lm_2 <- lm(fly_numbers ~ diet * type + day, data = exp2a_all)
# Checking the model 
performance::check_model(exp2a_all_lm_2)
# Using the summary function
summary(exp2a_all_lm_2)
# Only day 3 is significant but this has changed data 

# Using broom::tidy 
broom::tidy(exp2a_all_lm,  
            exponentiate=T, 
            conf.int=T)


# trying glm 
exp2aglm <- glm(fly_numbers ~ diet * type + day,
                  data = exp2a_all, family = poisson(link = "log"))

summary(exp2aglm)



#long_mated_femalesd1_summary <- long_mated_femalesd1 %>% 
#  group_by(diet) %>% 
#  summarise(mean = mean(fly_numbers)
#           ,
#            sd = sd(fly_numbers),
#           n = n(),
#          se = sd/sqrt(n))
#------------- Visualising data for mated females (exp 2, day 1) 
#mated_femalesd1_plot <- long_mated_femalesd1_summary%>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "#03fc7f",
#          alpha = 0.6)+
#  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "#03fc7f",
#              width = 0.2)+
# geom_jitter(data = long_mated_femalesd1,
#            aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#colour = "black",
#            width = 0.2,
#            shape = 21)+
#  ylim(0,6)+
# labs(x = "Diet \n(Protein; Carbohydrate)\n Mated",
#      y = "Mean (+/- S.E.) number of flies on a feeding patch")+
# theme_minimal()

#long_mated_femalesd1_summary %>%
# kbl(caption=" ") %>% 
# kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
#long_mated_femalesd2_summary <- long_mated_femalesd2 %>% 
#  group_by(diet) %>% 
#  summarise(mean = mean(fly_numbers)
#         ,
#        sd = sd(fly_numbers),
#         n = n(),
#        se = sd/sqrt(n))
#------------------ Visualising the data for mated females (exp 2, day 2) 
#mated_femalesd2_plot <- long_mated_femalesd2_summary%>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#           fill = "skyblue",
#           colour = "#03fc7f",
#           alpha = 0.6)+
#  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "#03fc7f",
#                width = 0.2)+
# geom_jitter(data = long_mated_femalesd2,
#              aes(x = diet,
#               y = fly_numbers),
#            fill = "skyblue",
#            colour = "black",
#            width = 0.2,
#             shape = 21)+
# ylim(0,6)+
# labs(x = "Diet \n(Protein; Carbohydrate) \n Mated",
#      y = "Mean (+/- S.E.) number of flies on a feeding patch")+
# theme_minimal()
#------------------ Data analysis for mated females (exp 2, day 2)
#mated_femalesd2_ls <- lm(fly_numbers ~ diet, data = long_mated_femalesd2)
#mated_femalesd2_ls
#anova(mated_femalesd2_ls)
#performance::check_model(mated_femalesd2_ls)
#performance::check_model(mated_femalesd2_ls, check=c("homogeneity", "qq"))
#broom::tidy(mated_femalesd2_ls,  
#      exponentiate=T, 
#     conf.int=T)
#long_mated_femalesd3_summary <- long_mated_femalesd3 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#     sd = sd(fly_numbers),
#     n = n(),
#    se = sd/sqrt(n))
#-------------- Visualising the data for mated females (exp 2,day 3)
#mated_femalesd3_plot <- long_mated_femalesd3_summary%>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "#03fc7f",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#                colour = "#03fc7f",
#                width = 0.2)+
#  geom_jitter(data = long_mated_femalesd3,
#              aes(x = diet,
#                  y = fly_numbers),
#              fill = "skyblue",
#             colour = "black",
#              width = 0.2,
#              shape = 21)+
#  ylim(0,6)+
# labs(x = "Diet \n(Protein; Carbohydrate) \n Mated",
#      y = "Mean (+/- S.E.) number of flies on a feeding patch")+
# theme_minimal()
#-------------- Data analysis for mated females (exp 2, day 3)
#mated_femalesd3_ls <- lm(fly_numbers ~ diet, data = long_mated_femalesd3)
#summary(mated_femalesd3_ls)

#long_virgin_femalesd1_summary <- long_virgin_femalesd1 %>% 
#  group_by(diet) %>% 
#  summarise(mean = mean(fly_numbers)
#          ,
#          sd = sd(fly_numbers),
#          n = n(),
#          se = sd/sqrt(n))
#-------------- Visualising data for virgin females (exp 2, day 1)
#virgin_femalesd1_plot <- long_virgin_femalesd1_summary%>% 
#  ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "#c203fc",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "#c203fc",
#               width = 0.2)+
# geom_jitter(data = long_virgin_femalesd1,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
#  ylim(0,6)+
#  labs(x = "Diet \n(Protein; Carbohydrate) \nVirgin",
#       y = "")+
# theme_minimal()
#mated_femalesd1_plot + virgin_femalesd1_plot
#-------------- Data analysis for virgin females (exp 2, day 1)
#long_virgin_femalesd1_summary %>%
#- kbl(caption=" ") %>% 
#- kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
#virgin_femalesd1_ls <- lm(fly_numbers ~ diet, data = long_virgin_femalesd1)
#summary(virgin_femalesd1_ls)
#anova(virgin_femalesd1_ls)
#performance::check_model(virgin_femalesd1_ls)
#performance::check_model(virgin_femalesd1_ls, check=c("homogeneity", "qq"))
#-- Choose a different linear model !!!
#broom::tidy(mated_femalesd2_ls,  
#exponentiate=T, 
#conf.int=T)
#long_virgin_femalesd2_summary <- long_virgin_femalesd2 %>% 
#group_by(diet) %>% 
# summarise(mean = mean(fly_numbers)
#           ,
#           sd = sd(fly_numbers),
#           n = n(),
#           se = sd/sqrt(n))
#------------------ Visualising the data for virgin females (exp 2, day 2)
#virgin_femalesd2_plot <- long_virgin_femalesd2_summary%>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "skyblue",
#           colour = "#c203fc",
#           alpha = 0.6)+
#  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#                colour = "#c203fc",
#                width = 0.2)+
#  geom_jitter(data = long_virgin_femalesd2,
#              aes(x = diet,
#                 y = fly_numbers),
#              fill = "skyblue",
#              colour = "black",
#              width = 0.2,
#             shape = 21)+
# ylim(0,6)+ 
# labs(x = "Diet \n(Protein; Carbohydrate)\n Virgin",
#      y = "")+
# theme_minimal()
#mated_femalesd2_plot + virgin_femalesd2_plot
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
#virgin_femalesd3_summary <- long_virgin_femalesd3 %>% 
#group_by(diet) %>% 
#summarise(mean = mean(fly_numbers),
#         sd = sd(fly_numbers),
#         n = n(),
#         se = sd/sqrt(n))
#----------------- Visualising the data for virgin females (exp 3, day 3) 
#virgin_femalesd3_plot <- virgin_femalesd3_summary%>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "#c203fc",
#          alpha = 0.6)+
#  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#                colour = "#c203fc",
#               width = 0.2)+
# geom_jitter(data = long_virgin_femalesd3,
#             aes(x = diet,
#                  y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#              shape = 21)+
# ylim(0,6)+ 
# labs(x = "Diet \n(Protein; Carbohydrate) \n Virgin",
#      y = "")+
# theme_minimal()
#mated_femalesd3_plot + virgin_femalesd3_plot
#---------------------- Egg counts for experiment 2
#---------------- Mated female egg count (exp 2)
#exp3both1 <- long_females_mf_e3_d1 %>% mutate(variable = "both") %>% mutate(day = "1")
#exp3both2 <- long_females_mf_e3_d2 %>% mutate(variable = "both") %>% mutate(day = "2")

# exp2avirgin <- long_virgin_femalesd1 %>% mutate(variable = "virgin")
# exp2amated <- long_mated_femalesd1 %>% mutate(variable = "mated")
# mated v virgin have different n numbers ? but males and females
# had different n numbers and that worked 
# Error in rbind(deparse.level, ...) : 
# numbers of columns of arguments do not match - confused because they don't match 
# exp2a <- rbind(exp2amated, exp2avirgin)
# exp2als1 <- lm(fly_numbers ~ diet + variable, data = exp2a)
# performance::check_model(exp2als1)
# hmmm 
# exp2als1a <- lm(sqrt(fly_numbers) ~ diet + variable, data = exp2a)
# performance::check_model(exp2als1a)
# without sqrt looks a lot better 
# exp2als2 <- lm(fly_numbers ~ diet * variable, data = exp2a)
# hmmm 
# performance::check_model(exp2als2)
# broom::tidy(exp2als2,  
#            exponentiate=T, 
#           conf.int=T)
# exp2als2a <- lm(sqrt(fly_numbers) ~ diet * variable, data = exp2a)
# performance::check_model(exp2als2a)
# without sqrt looks better 
# broom::tidy(exp2als2a,  
#  exponentiate=T, 
#           conf.int=T)
