#------------------------------Experiment 2b---------------------------------#


#-----------------------  Mated Females (exp 4, day 1)

mated_females_e2bd1 <- (read_excel(path = "data/MatedFemalesE2bD1.xlsx", na = "NA"))


long_mated_females_e2bd1 <- mated_females_e2bd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_females_e2bd1_summary <- long_mated_females_e2bd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#---------------- Visualising the data for mated females (exp 4, day 1)

mated_females_e2bd1_plot <- long_mated_females_e2bd1_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_mated_females_e2bd1,
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

mated_females_e2bd2 <- (read_excel(path = "data/MatedFemalesE2bD2.xlsx", na = "NA"))


long_mated_females_e2bd2 <- mated_females_e2bd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_females_e2bd2_summary <- long_mated_females_e2bd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#----------------- Visualising the data for mated females (exp 4, day 2)

mated_females_e2bd2_plot <- long_mated_females_e2bd2_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_mated_females_e2bd2,
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

virgin_females_e2bd1 <- (read_excel(path = "data/VirginFemalesE2bD1.xlsx", na = "NA"))


long_virgin_females_e2bd1 <- virgin_females_e2bd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_females_e2bd1_summary <- long_virgin_females_e2bd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#--------------- Visualising the data for virgin females (exp 4, day 1)

virgin_females_e2bd1_plot <- long_virgin_females_e2bd1_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_virgin_females_e2bd1,
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

#virgin_females_e4_d1ls1 <- lm(fly_numbers ~ diet, data = long_virgin_females_e4d1)
#performance::check_model(virgin_females_e4d1_ls1)

#----------------------- Virgin Females (exp 4, day 2)

virgin_females_e2bd2 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/VirginFemalesE2bD2.xlsx", na = "NA"))


long_virgin_females_e2bd2 <- virgin_females_e2bd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

virgin_females_e2bd2_summary <- long_virgin_females_e2bd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------------- Visualising the data for virgin females (exp 4, day 2)

virgin_femalese2bd2_plot <- long_virgin_females_e2bd2_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "red",
           colour = "blue",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "blue",
                width = 0.2)+
  geom_jitter(data = long_virgin_females_e2bd2,
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

#virgin_femalese4d2_summary %>%
#kbl(caption=" ") %>% 
# kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

  #virgin_femalese4d2ls1 <- lm(fly_numbers ~ diet, data = long_virgin_femalese4d2)

  #virgin_femalese4d2ls1
  #summary(virgin_femalese4d2ls1)
  #anova(virgin_femalese4d2ls1)
  #confint(virgin_femalese4d2ls1)
  #broom::tidy(virgin_femalese4d2ls1,  
  #     exponentiate=T, 
  #         conf.int=T)
#performance::check_model(virgin_femalese4d2ls1)

#virgin_femalese4d2ls1_table <- eggcountingls1 %>% broom::tidy(conf.int = T) %>% 
# select(-`std.error`) %>% 
# mutate_if(is.numeric, round, 2) %>% 
# kbl(col.names = c("Predictors",
#                   "Estimates",
#                   "Z-value",
#                   "P",
#                   "Lower 95% CI",
#                   "Upper 95% CI"),
#     #  caption = "", 
#     booktabs = TRUE) %>% 
# kable_styling(full_width = FALSE, font_size=16, latex_options = c("striped", "hold_position"))




  
  #----------------------OVERALL DATA ANALYSIS FOR EXPERIMENT 2b ----------------#
  
  # Mutating a sex variable 
  exp2bvirgin <- long_virgin_females_e2bd1 %>% mutate(status = "virgin")
  
  
  exp2bmated <- long_mated_females_e2bd1 %>% mutate(status = "mated")
  
  
  #using r bind to make two data sets one data set 
  
  
  exp2b <- rbind(exp2bvirgin, exp2bmated)
  
  # Making a linear model
  exp2bls1 <- lm(fly_numbers ~ diet + status, data = exp2b)
  
  performance::check_model(exp2bls1)
  
  
  
  broom::tidy(exp1ls1,  
              exponentiate=T, 
              conf.int=T)
  
  # Same linear model but with sqrt 
  exp2bls1a <- lm(sqrt(fly_numbers) ~ diet + status , data = exp2b)
  
  # Looks a lot better with sqrt 
  
  
  performance::check_model(exp2bls1a)
  

  
  broom::tidy(exp1ls1a,  
              exponentiate=T, 
              conf.int=T)
  
  
  # Making a second linear model (with an interaction effect)
  
  exp2bls2 <- lm(fly_numbers ~ diet * status, data = exp2b)
  
  performance::check_model(exp2bls2)
  
  #almost perfect
  
  broom::tidy(exp1ls2,  
              exponentiate=T, 
              conf.int=T)
  
  # Same linear model but with sqrt 
  
  exp2bls2a <- lm(sqrt(fly_numbers) ~ diet * status , data = exp2b)
  
  performance::check_model(exp2bls2a)
  
  # maybe better with sqrt? 
  
  broom::tidy(exp1ls2a,  
              exponentiate=T, 
              conf.int=T)

