#----------------------------------- Experiment 3 ----------------------------

#-----------------  Feeding behaviour analysis 

#--------------------- Mated females (exp 3, day 1)

mated_femalese3d1 <- (read_excel(path = "data/MatedFemalesE3D1.xlsx", na = "NA"))


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

mated_femalese3d2 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedFemalesE3D2.xlsx", na = "NA"))

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

males_mf_e3_d1 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedMalesE3D1(M+F).xlsx", na = "NA"))

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

females_mf_e3_d1 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedFemalesE3D1(M+F).xlsx", na = "NA"))

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

males_mf_e3_d2 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedMalesE3D2(M+F).xlsx", na = "NA"))

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


females_mf_e3_d2 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedFemalesE3D2(M+F).xlsx", na = "NA"))

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




#--------------------OVERALL DATA ANALYSIS FOR EXPERIMENT 3 ----------------#


# Using mutate to add a variable 
exp3alone <- long_females_mf_e3_d1 %>% mutate(variable = "together")
exp3together <- long_mated_femalese3d1 %>% mutate(variable = "alone")

# Using rbind to bind the two data sets from the experiment 
exp3 <- rbind(exp3alone, exp3together)

# Making a linear model of fly numbers and diet  
exp3ls1 <- lm(fly_numbers ~ diet + variable, data = exp3)

# Using performance 
performance::check_model(exp3ls1)

exp3ls1a <- lm(sqrt(fly_numbers) ~ diet + variable, data = exp3)

performance::check_model(exp3ls1a)

exp3ls2 <- lm(fly_numbers ~ diet * variable, data = exp3)

performance::check_model(exp3ls2)

exp3ls2a <- lm(sqrt(fly_numbers) ~ diet * variable, data = exp3)

performance::check_model(exp3ls2a)



#without sqrt - works better

broom::tidy(exp3ls2,  
            exponentiate=T, 
            conf.int=T)
# Day 2 
# Using mutate to add a variable 
exp3alone2 <- long_females_mf_e3_d2 %>% mutate(variable = "together")
exp3together2 <- long_mated_femalese3d2 %>% mutate(variable = "alone")

# Using rbind to bind the two data sets from the experiment 
exp3d2 <- rbind(exp3alone2, exp3together2)

# Making a linear model of fly numbers and diet  
exp3d2ls1 <- lm(fly_numbers ~ diet + variable, data = exp3d2)

performance::check_model(exp3d2ls1)

# Same model but with sqrt 
exp3d2ls1a <- lm(sqrt(fly_numbers) ~ diet + variable, data = exp3d2)

performance::check_model(exp3d2ls1a)
# better without sqrt 

# Testing with an interaction effect 

exp3d2ls2 <- lm(fly_numbers ~ diet * variable, data = exp3d2)

performance::check_model(exp3d2ls2)

exp3d2ls2a <- lm(sqrt(fly_numbers) ~ diet * variable, data = exp3d2)

performance::check_model(exp3d2ls2a)

# better without sqrt 


# -------------------- Collating days 

# Just females 
exp3females1 <- long_mated_femalese3d1 %>% mutate(variable = "females") %>% mutate(day = "1")
exp3females2 <- long_mated_femalese3d2 %>% mutate(variable = "females") %>% mutate(day = "2")

# Binding days 1 and 2 
exp3femalesall <- rbind(exp3females1, exp3females2)

# Females in a plate with males 
exp3both1 <- long_females_mf_e3_d1 %>% mutate(variable = "both") %>% mutate(day = "1")
exp3both2 <- long_females_mf_e3_d2 %>% mutate(variable = "both") %>% mutate(day = "2")

# Binding days 1 and 2 
exp3bothall <- rbind(exp3both1, exp3both2)

# Binding mated and virgin days 1 - 3 
exp3all <- rbind(exp3femalesall, exp3bothall)

exp3all <- exp3all %>% mutate(fly_prop = if_else(variable =="females", 
                                     fly_numbers/10,
                                     fly_numbers/5))


# linear model with interaction effect
exp3allls <- glm(fly_numbers ~ diet * variable + day, data = exp3all, family = poisson())
# use quasi likelihood as null/df >1 quasipoisson()
performance::check_model(exp3allls)

