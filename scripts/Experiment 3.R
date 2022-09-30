#----------------------------------- Experiment 3 ----------------------------

#-----------------  Feeding behaviour analysis 

#--------------------- Mated females (exp 3, day 1)

mated_femalese3d1 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedFemalesE3D1.xlsx", na = "NA"))


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
