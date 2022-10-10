#------------------------------Experiment 2b----------------------------------------#
#-----------------------  Mated Females
#--------- Day 1 
#---------- Reading the data in 
mated_females_e2bd1 <- (read_excel(path = "data/MatedFemalesE2bD1.xlsx", na = "NA"))
#----- Making the data long 
long_mated_females_e2bd1 <- mated_females_e2bd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#---------Day 2 
#---------- Reading the data in 
mated_females_e2bd2 <- (read_excel(path = "data/MatedFemalesE2bD2.xlsx", na = "NA"))
#----- Making the data long 
long_mated_females_e2bd2 <- mated_females_e2bd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#--------- Mutating variables
exp2bmated1 <- long_mated_females_e2bd1 %>% mutate(variable = "mated") %>% mutate(day = "1")
exp2bmated2 <- long_mated_females_e2bd2 %>% mutate(variable = "mated") %>% mutate(day = "2")
#----- Binding mated days 1 - 2 
exp2bmatedall <- rbind(exp2bmated1, exp2bmated2)
#----- Summarising the data 
exp2bmatedall_summary <- exp2bmatedall %>%
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#---- Visualising the data 
exp2bmatedall_plot <- exp2bmatedall_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = exp2bmatedall,
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
#----------------------------- Virgin Females
#--------- Day 1 
#---------- Reading the data in 
virgin_females_e2bd1 <- (read_excel(path = "data/VirginFemalesE2bD1.xlsx", na = "NA"))
#----- Making the data long 
long_virgin_females_e2bd1 <- virgin_females_e2bd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#--------- Day 2 
#---------- Reading the data in 
virgin_females_e2bd2 <- (read_excel(path = "data/VirginFemalesE2bD2.xlsx", na = "NA"))
#----- Making the data long 
long_virgin_females_e2bd2 <- virgin_females_e2bd2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#------ Mutating variables 
exp2bvirgin1 <- long_virgin_females_e2bd1 %>% mutate(variable = "virgin") %>% mutate(day = "1")
exp2bvirgin2 <- long_virgin_females_e2bd2 %>% mutate(variable = "virgin") %>% mutate(day = "2")
#------- Binding virgin days 1 - 2 
exp2bvirginall <- rbind(exp2bvirgin1, exp2bvirgin2)
#----- Summarising the data 
exp2bvirginall_summary <- exp2bvirginall %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#---- Visualising the data
exp2bvirginall_plot <- exp2bvirginall_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#eb34c3",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#eb34c3",
                width = 0.2)+
  geom_jitter(data = exp2bvirginall,
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
#------------------ Using patchwork to combine the plots -----------------------
exp2bmatedall_plot + exp2bvirginall_plot
#-------------------------------------------------------------------------------

#------------------------------ Offspring counts -------------------------------



#------------------ Overall data analysis for experiment 2b--------------------#

#-- Binding mated and virgin days 1 - 2 
exp2ball <- rbind(exp2bmatedall, exp2bvirginall)
# linear model without interaction effect 
exp2blm0 <- lm(fly_numbers ~ diet + variable + day, data = exp2ball)
# Checking the model 
performance::check_model(exp2blm0)
# linear model with interaction effect
exp2blm <- lm(fly_numbers ~ diet * variable + day, data = exp2ball)
# Checking the model 
performance::check_model(exp2blm)
# trying glm with poisson
exp2bglm <- glm(fly_numbers ~ diet * variable + day, 
                data = exp2ball, family = poisson(link = "log"))
# trying glm with quasi poisson as is overdispersion 
exp2bglm <- glm(fly_numbers ~ diet * variable + day,
                data = exp2ball, family = quasipoisson(link = "log"))

# Checking the model
performance::check_model(exp2bglm)
# 
summary(exp2bglm)
# can drop day as is not significant
exp2bglm2 <- glm(fly_numbers ~ diet * variable,
                data = exp2ball, family = quasipoisson(link = "log"))
#
performance::check_model(exp2bglm2)
#
summary(exp2bglm2)
#
broom::tidy(exp2bglm2)
#
emmeans::emmeans(exp2bglm, specs = pairwise ~ diet + variable + day)
#
tab_model(exp2bglm2)
#
tab_model(exp2bglm)
#
anova(exp2bglm2)
#
tbl_regression(exp2bglm2)






#------------------------------------------------------------------------------- 
#-------------------- IGNORE THIS HASHTAGGED OUT CODE -------------------------
#virgin_females_e2bd2_summary <- long_virgin_females_e2bd2 %>% 
#  group_by(diet) %>% 
#  summarise(mean = mean(fly_numbers),
#          sd = sd(fly_numbers),
#           n = n(),
#    se = sd/sqrt(n))
#------------- Visualising the data for virgin females (exp 4, day 2)
#virgin_femalese2bd2_plot <- long_virgin_females_e2bd2 %>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#           fill = "red",
#           colour = "blue",
#           alpha = 0.6)+
#  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#                colour = "blue",
#              width = 0.2)+
#geom_jitter(data = long_virgin_females_e2bd2,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0,6)+ 
#  labs(x = "Diet \n(Protein; Carbohydrate)",
#       y = "Mean (+/- S.E.) number of flies")+
#  theme_minimal()
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

# Day 1 

# Mutating a sex variable 
#exp2bvirgin <- long_virgin_females_e2bd1 %>% mutate(status = "virgin")
#exp2bmated <- long_mated_females_e2bd1 %>% mutate(status = "mated")
#using r bind to make two data sets one data set 
#exp2b <- rbind(exp2bvirgin, exp2bmated)
# Making a linear model
#exp2bls1 <- lm(fly_numbers ~ diet + status, data = exp2b)
# Checking the model 
#performance::check_model(exp2bls1)
# 
#broom::tidy(exp1ls1,  
#    exponentiate=T, 
#    conf.int=T)
# Same linear model but with sqrt 
#exp2bls1a <- lm(sqrt(fly_numbers) ~ diet + status , data = exp2b)
# Looks a lot better with sqrt 
#performance::check_model(exp2bls1a)
##
#broom::tidy(exp1ls1a,  
#             exponentiate=T, 
#             conf.int=T)
# Making a second linear model (with an interaction effect)
#exp2bls2 <- lm(fly_numbers ~ diet * status, data = exp2b)
# Checking the model 
#performance::check_model(exp2bls2) #almost perfect
# 
#broom::tidy(exp1ls2,  
#              exponentiate=T, 
#              conf.int=T)
# Same linear model but with sqrt 
#exp2bls2a <- lm(sqrt(fly_numbers) ~ diet * status , data = exp2b)
# Checking the model 
#performance::check_model(exp2bls2a) # maybe better with sqrt? 
#
#broom::tidy(exp1ls2a,  
#             exponentiate=T, 
#             conf.int=T)
# ------------  
# Day 2 
# Mutating a sex variable 
#exp2bvirgin2 <- long_virgin_females_e2bd2 %>% mutate(status = "virgin")
#exp2bmated2 <- long_mated_females_e2bd2 %>% mutate(status = "mated")
# Using r bind to make two data sets one data set 
#exp2bd2 <- rbind(exp2bvirgin2, exp2bmated2)
# Model 
#exp2bd2ls1 <- lm(fly_numbers ~ diet + status, data = exp2bd2)
# Checking the model 
#performance::check_model(exp2bd2ls1)
# Using sqrt 
#exp2bd2ls1a <- lm(sqrt(fly_numbers) ~ diet + status, data = exp2bd2)
# Checking the model 
#performance::check_model(exp2bd2ls1a) # Better with sqrt 
# Same model but with interaction effect 
#exp2bd2ls2 <- lm(fly_numbers ~ diet * status, data = exp2bd2)
# Checking the model 
#performance::check_model(exp2bd2ls2)
# Model with sqrt 
#exp2bd2ls2a <- lm(sqrt(fly_numbers) ~ diet * status, data = exp2bd2)
# Checking the model 
#performance::check_model(exp2bd2ls2a) # Better with sqrt 
#---------
#exp2bd1ls3 <- glm(formula = fly_numbers ~ diet * status,
#family = quasipoisson(), data = exp2b)
#performance::check_model(exp2bd1ls3)
#long_mated_females_e2bd1_summary <- long_mated_females_e2bd1 %>% 
#  group_by(diet) %>% 
#  summarise(mean = mean(fly_numbers),
#            sd = sd(fly_numbers),
#            n = n(),
#            se = sd/sqrt(n))
#---------------- Visualising the data for mated females (exp 4, day 1)
#mated_females_e2bd1_plot <- long_mated_females_e2bd1_summary%>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "red",
#          colour = "blue",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "blue",
#               width = 0.2)+
# geom_jitter(data = long_mated_females_e2bd1,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#              shape = 21)+
#  ylim(0,6)+ 
#  labs(x = "Diet \n(Protein; Carbohydrate)",
#       y = "Mean (+/- S.E.) number of flies")+
#  theme_minimal()
#------------------ Data analysis for mated females (exp 4, day 1)
#-----------------------  Mated Females (exp 4, day 2)
#long_mated_females_e2bd2_summary <- long_mated_females_e2bd2 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#           se = sd/sqrt(n))
#----------------- Visualising the data for mated females (exp 4, day 2)
#mated_females_e2bd2_plot <- long_mated_females_e2bd2_summary%>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#          fill = "red",
#          colour = "blue",
#           alpha = 0.6)+
#  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#                colour = "blue",
#                width = 0.2)+
#  geom_jitter(data = long_mated_females_e2bd2,
#             aes(x = diet,
#                  y = fly_numbers),
#              fill = "skyblue",
#              colour = "black",
#             width = 0.2,
#             shape = 21)+
#  ylim(0,6)+ 
#  labs(x = "Diet \n(Protein; Carbohydrate)",
#       y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
#------------------ Data analysis for mated females (exp 4, day 2)
#----------------------- Virgin Females (exp 4, day 1)
# ------------  
#long_virgin_females_e2bd1_summary <- long_virgin_females_e2bd1 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#            n = n(),
#            se = sd/sqrt(n))
#--------------- Visualising the data for virgin females (exp 4, day 1)
#virgin_females_e2bd1_plot <- long_virgin_females_e2bd1_summary%>% 
#  ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "red",
#          colour = "blue",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#                colour = "blue",
#               width = 0.2)+
# geom_jitter(data = long_virgin_females_e2bd1,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#  colour = "black",
#          width = 0.2,
#           shape = 21)+
#ylim(0,6)+ 
#labs(x = "Diet \n(Protein; Carbohydrate)",
#     y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
#---------------- Data analysis for virgin females (exp 4, day 1)
#virgin_females_e4_d1ls1 <- lm(fly_numbers ~ diet, data = long_virgin_females_e4d1)
#performance::check_model(virgin_females_e4d1_ls1)
#----------------------- Virgin Females (exp 4, day 2)
