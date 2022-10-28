#----------------------------------- 🪰 Experiment 3 🪰 ----------------------------



#----- 👶Offspring counts -----

#------- Just females on a plate 
offspring_alone_exp3 <- read_excel("data/Exp3OffspringAlone.xlsx")
#------- Making the data long 
long_offspring_alone_exp3 <- offspring_alone_exp3 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "offspring_numbers")
#--- Summarising the data 
offspring_alone_exp3_summary <- long_offspring_alone_exp3 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(offspring_numbers),
            sd = sd(offspring_numbers),
            n = n(),
            se = sd/sqrt(n))
#----- Visualising the data 
offspring_alone_exp3_plot <- offspring_alone_exp3_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = long_offspring_alone_exp3,
              aes(x = diet,
                  y = offspring_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,150)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) offspring from alone plates")+
  theme_minimal()

offspring_alone_exp3_summary2  <- exp3offspringalone %>%  
  group_by(diet) %>% 
  summarise(mean = mean(offspring_prop),
            sd = sd(offspring_prop),
            n = n(),
            se = sd/sqrt(n))

offspring_alone_exp3_plot2 <- offspring_alone_exp3_summary2 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = exp3offspringalone,
              aes(x = diet,
                  y = offspring_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,150)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) offspring from alone plates")+
  theme_minimal()

offspring_alone_exp3_plot2 + offspring_both_exp3_plot2





#---- 📊 Offspring Data analysis  ---- 

exp3offspring_alone_lm <- lm(offspring_numbers ~ diet, data = long_offspring_alone_exp3)

summary(exp3offspring_alone_lm)


#------- Female count ( with males on a plate) 
offspring_both_exp3 <- read_excel("data/Exp3OffspringBoth.xlsx")
#------- Making the data long 
long_offspring_both_exp3 <- offspring_both_exp3 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "offspring_numbers")
#--- Summarising the data 
offspring_both_exp3_summary <- long_offspring_both_exp3 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(offspring_numbers),
            sd = sd(offspring_numbers),
            n = n(),
            se = sd/sqrt(n))
#----- Visualising the data 
offspring_both_exp3_plot <- offspring_both_exp3_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = long_offspring_both_exp3,
              aes(x = diet,
                  y = offspring_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,200)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) offspring from both plates")+
  theme_minimal()

offspring_both_exp3_summary2  <- exp3offspringboth %>%  
  group_by(diet) %>% 
  summarise(mean = mean(offspring_prop),
            sd = sd(offspring_prop),
            n = n(),
            se = sd/sqrt(n))

offspring_both_exp3_plot2 <- offspring_both_exp3_summary2 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = exp3offspringboth,
              aes(x = diet,
                  y = offspring_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,150)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) offspring from both plates")+
  theme_minimal()

exp3offspringboth 

#----- Data analysis 

exp3offspring_both_lm <- lm(offspring_numbers ~ diet, data = long_offspring_both_exp3)

summary(exp3offspring_both_lm)

#----- patchworking the data 

offspring_alone_exp3_plot + offspring_both_exp3_plot

#----- Data analysis for offspring counts 

#- adding varibles 
exp3offspringboth <- long_offspring_alone_exp3 %>% mutate(variable = "alone") %>% mutate(offspring_prop = if_else(variable =="both", 
                                                                                                            offspring_numbers/5,
                                                                                                            offspring_numbers/10))
exp3offspringalone <- long_offspring_both_exp3 %>% mutate(variable = "both") %>% mutate(offspring_prop = if_else(variable =="both", 
                                                                                                          offspring_numbers/5,
                                                                                                           offspring_numbers/10))
#- Binding the two data sets 
exp3offspring <- rbind(exp3offspringboth, exp3offspringalone)

exp3offspring_lm <- lm(offspring_prop ~ diet * variable, data = exp3offspring)

summary(exp3offspring_lm)





#-----------------  Feeding behaviour analysis 

#----------------- ♀️ Female feeding behaviour ----
#----------- Females alone on a plate 
#-------Day 1
#---------Reading the data in 
mated_femalese3d1 <- (read_excel(path = "data/MatedFemalesE3D1.xlsx", na = "NA"))
#-- Making data "long"
long_mated_femalese3d1 <- mated_femalese3d1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#-------Day 2 
#---------Reading the data in 
mated_femalese3d2 <- (read_excel(path = "data/MatedFemalesE3D2.xlsx", na = "NA"))
#-- Making data "long"
long_mated_femalese3d2 <- mated_femalese3d2 %>% 
pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
# data without fly proportion variable
exp3females01 <- long_mated_femalese3d1 %>% mutate(status = "females") %>% mutate(day = "1")
exp3females02 <- long_mated_femalese3d2 %>% mutate(status = "females") %>% mutate(day = "2")
exp3females0all <- rbind(exp3females01, exp3females02)
# mutating status, day and proporiton variables 
exp3females1 <- long_mated_femalese3d1 %>% mutate(status = "females") %>% mutate(day = "1") %>% mutate(fly_prop = if_else(status =="females", 
                                                                                                                      fly_numbers/10,
                                                                                                                      fly_numbers/5))
exp3females2 <- long_mated_femalese3d2 %>% mutate(status = "females") %>% mutate(day = "2")  %>% mutate(fly_prop = if_else(status =="females", 
                                                                                                                           fly_numbers/10,
                                                                                                                           fly_numbers/5))
#-------Binding days 1 and 2 
exp3femalesall <- rbind(exp3females1, exp3females2)

#----------------- Making data "long"
#long_exp3femalesall <- exp3femalesall %>% 
#pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#----------------- Summarising the data 
exp3femalesall_summary <- exp3femalesall %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_prop),
            sd = sd(fly_prop),
            n = n(),
            se = sd/sqrt(n))
#----------------- Visualising the data 
exp3femalesall_plot <- exp3femalesall_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "red",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "red",
                width = 0.2)+
  geom_jitter(data = exp3femalesall,
              aes(x = diet,
                  y = fly_prop),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,1)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) female flies/patch on a plate alone")+
  theme_minimal()





#---- ⚤ Females on a plate with males  ---- 
#----- Day 1 
#-------- Reading the data in 
bothplate_e3d1 <- (read_excel(path = "data/MatedFemalesE3D1(M+F).xlsx", na = "NA"))
#---- Making the data long
long_bothplate_e3d1 <- bothplate_e3d1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#----- Day 2 
#-------- Reading the data in
bothplate_e3d2 <- (read_excel(path = "data/MatedFemalesE3D2(M+F).xlsx", na = "NA"))
#---- Making the data long
long_bothplate_e3d2 <- bothplate_e3d2 %>% 
pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#------ Combining the days 
#------  Data without fly proportion variable 
exp3both01 <- long_bothplate_e3d1 %>% mutate(status = "both") %>% mutate(day = "1")
exp3both02 <- long_bothplate_e3d2 %>% mutate(status = "both") %>% mutate(day = "2")
exp3both0all <- rbind(exp3both01, exp3both02)

#------------------------------- Mutating a status variable and a day variable 
exp3both1 <- long_bothplate_e3d1 %>% mutate(status = "both") %>% mutate(day = "1")%>% mutate(fly_prop = if_else(status =="females", 
                                                                                                                  fly_numbers/10,
                                                                                                                  fly_numbers/5))
exp3both2 <- long_bothplate_e3d2 %>% mutate(status = "both") %>% mutate(day = "2") %>% mutate(fly_prop = if_else(status =="females", 
                                                                                                                   fly_numbers/10,
                                                                                                                   fly_numbers/5))
#----- Binding days 1 and 2 
exp3bothall <- rbind(exp3both1, exp3both2)

#----------------- Making data "long"
#long_exp3bothall <- exp3bothall %>% 
#pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#----- Summarising the data 
exp3bothall_summary <- exp3bothall %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_prop),
            sd = sd(fly_prop),
            n = n(),
            se = sd/sqrt(n))
#----- Visualising the data 
exp3both_plot <- exp3bothall_summary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "purple",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "purple",
                width = 0.2)+
  geom_jitter(data = exp3bothall,
              aes(x = diet,
                  y = fly_prop),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,1)+ 
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) female flies/patch on a plate with males")+
  theme_minimal()

#--------------------------- Using patchwork to combine the two data plots 
exp3femalesall_plot + exp3both_plot

#-------------------- 📊 Feeding behaviour data analysis  -------------------
# fly_numbers: number of flies on a flies on a food patch
# status: whether or not there were just females on a plate or females with males
# diet: one of the four P:C ratios 


#----- binding the original data (WITHOUT proportions)
exp3all01 <- rbind(exp3both0all, exp3females01)
# model with original data 
exp3allglm <- glm(fly_numbers ~ diet * status + day, data = exp3all01, family = quasipoisson())




#-----  Fly proportion model  
# Binding the combined days data of alone on a plate and with males on a plate
exp3all <- rbind(exp3femalesall, exp3bothall)
# Adding a fly proportion variable 
#  exp3allz <- exp3all %>% mutate(fly_prop = if_else(status =="females", 
#   fly_numbers/10,
#   fly_numbers/5))
# linear model with interaction effect
exp3alllm <- lm(fly_numbers ~ diet * status + day, data = exp3all)
# summary of lm 
summary(exp3alllm)
# testing for significance of day 
drop1(exp3allglm, test = "F")
# day is dropped as not significant 
# use quasi likelihood as null/df >1 quasipoisson()
exp3allglm2 <- glm(fly_prop ~ diet * status, data = exp3all, family = quasipoisson())
# Checking the data 
performance::check_model(exp3allglm2)
# Using the summary function
summary(exp3allglm2)
# information summary
broom::tidy(exp3allglm2)
# information about the model but irrelevant 
broom::glance(exp3allglm2)
# inividual observations 
broom::augment(exp3allglm2)
















#------------------- PROBALY IGNORE ALL THIS HASHTAGGED CODE
#--------------------- Mated females (exp 3, day 1)
#long_mated_femalese3d1_summary <- long_mated_femalese3d1 %>% 
#group_by(diet) %>% 
#summarise(mean = mean(fly_numbers),
#sd = sd(fly_numbers),
#n = n(),
# se = sd/sqrt(n))
#--------------------- Visualising the data for mated females (exp 3, day 1)
#mated_femalese3d1_plot <- long_mated_femalese3d1_summary%>% 
#ggplot(aes(x = diet, y = mean))+
#geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "orange",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "orange",
#               width = 0.2)+
# geom_jitter(data = long_mated_femalese3d1,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0,6)+ 
# labs(x = "Diet \n(Protein; Carbohydrate)",
#      y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
#--------- Data analysis for mated females (exp 3, day 1)
#--------------------- Mated females (exp 3, day 2)

#long_mated_femalese3d2_summary <- long_mated_femalese3d2 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#  se = sd/sqrt(n))

#--------------  Visualising the data for mated females (exp 3, day 2)

#mated_femalese3d2_plot <- long_mated_femalese3d2_summary%>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "skyblue",
#          colour = "orange",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "orange",
#               width = 0.2)+
# geom_jitter(data = long_mated_femalese3d2,
#             aes(x = diet,
#   y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0,6)+ 
# labs(x = "Diet \n(Protein; Carbohydrate)",
#      y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
#-------- Data analysis for mated females (exp 3, day 2)
#---------------- Males + Females
#----------------------------- Males (m+f) (exp 3, day 1)
#males_mf_e3_d1 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedMalesE3D1(M+F).xlsx", na = "NA"))
#long_males_mf_e3_d1 <- males_mf_e3_d1 %>% 
#pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#males_mf_e3_d1_summary <- long_males_mf_e3_d1 %>% 
#group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#           se = sd/sqrt(n))
#------------------- Visualising the data for males (exp 3, day 1)
#males_mf_e3_d1_plot <- males_mf_e3_d1_summary  %>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#  fill = "red",
#          colour = "blue",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "blue",
#               width = 0.2)+
# geom_jitter(data = long_males_mf_e3_d1,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0,2)+ 
# labs(x = "Diet \n(Protein; Carbohydrate)",
#      y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
#------------------------- Females (m+f) (exp 3, day 1) 
#females_mf_e3_d1 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedFemalesE3D1(M+F).xlsx", na = "NA"))
#long_females_mf_e3_d1 <- females_mf_e3_d1 %>% 
# pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

#females_mf_e3_d1_summary <- long_females_mf_e3_d1 %>% 
#  group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#           se = sd/sqrt(n))
#---------------------- Visualising the data for females (exp 3, day 1)
#females_mf_e3_d1_plot <- females_mf_e3_d1_summary  %>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "red",
#          colour = "blue",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "blue",
#               width = 0.2)+
# geom_jitter(data = long_females_mf_e3_d1,
#             aes(x = diet,
#   y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
# ylim(0,2)+ 
# labs(x = "Diet \n(Protein; Carbohydrate)",
#      y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
#-------- Plot showing males and females together (Day 1)
#males_mf_e3_d1_plot + females_mf_e3_d1_plot
#-------- Males (m+f) (exp 3,Day 2)
#-males_mf_e3_d2 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedMalesE3D2(M+F).xlsx", na = "NA"))

#long_males_mf_e3_d2 <- males_mf_e3_d2 %>% 
#pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#males_mf_e3_d2_summary <- long_males_mf_e3_d2 %>% 
#group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#           se = sd/sqrt(n))
#--------  Visualising the data for males (m+f) (exp 3, day 2)
#males_mf_e3_d2_plot <- males_mf_e3_d2_summary  %>% 
# ggplot(aes(x = diet, y = mean))+
# geom_bar(stat = "identity",
#          fill = "red",
#          colour = "blue",
#          alpha = 0.6)+
# geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#               colour = "blue",
#           width = 0.2)+
# geom_jitter(data = long_males_mf_e3_d2,
#             aes(x = diet,
#                 y = fly_numbers),
#             fill = "skyblue",
#             colour = "black",
#             width = 0.2,
#             shape = 21)+
#  ylim(0,2)+ 
# labs(x = "Diet \n(Protein; Carbohydrate)",
#      y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
#-------- Females (Day 2)
#females_mf_e3_d2 <- (read_excel(path = "~/Documents/drosophilaresearchproject/data/MatedFemalesE3D2(M+F).xlsx", na = "NA"))
#long_females_mf_e3_d2 <- females_mf_e3_d2 %>% 
# pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")
#females_mf_e3_d2_summary <- long_females_mf_e3_d2 %>% 
# group_by(diet) %>% 
# summarise(mean = mean(fly_numbers),
#           sd = sd(fly_numbers),
#           n = n(),
#            se = sd/sqrt(n))
#females_mf_e3_d2_plot <- females_mf_e3_d2_summary  %>% 
#  ggplot(aes(x = diet, y = mean))+
#  geom_bar(stat = "identity",
#          fill = "red",
#           colour = "blue",
#           alpha = 0.6)+
#geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
#                colour = "blue",
#               width = 0.2)+
#geom_jitter(data = long_females_mf_e3_d2,
#             aes(x = diet,
#            y = fly_numbers),
#             fill = "skyblue",
#            colour = "black",
#            width = 0.2,
#             shape = 21)+
# ylim(0,2)+ 
# labs(x = "Diet \n(Protein; Carbohydrate)",
#      y = "Mean (+/- S.E.) number of flies")+
# theme_minimal()
# ------- Overall data analysis 
# Using mutate to add a variable 
#exp3alone <- long_females_mf_e3_d1 %>% mutate(variable = "together")
#exp3together <- long_mated_femalese3d1 %>% mutate(variable = "alone")
# Using rbind to bind the two data sets from the experiment 
#exp3 <- rbind(exp3alone, exp3together)
# Making a linear model of fly numbers and diet  
#exp3ls1 <- lm(fly_numbers ~ diet + variable, data = exp3)
# Using performance 
#performance::check_model(exp3ls1)
#exp3ls1a <- lm(sqrt(fly_numbers) ~ diet + variable, data = exp3)
#performance::check_model(exp3ls1a)
#exp3ls2 <- lm(fly_numbers ~ diet * variable, data = exp3)
#performance::check_model(exp3ls2)
#exp3ls2a <- lm(sqrt(fly_numbers) ~ diet * variable, data = exp3)
#performance::check_model(exp3ls2a)
#without sqrt - works better
#broom::tidy(exp3ls2,  
#       exponentiate=T, 
#            conf.int=T)
# Day 2 
# Using mutate to add a variable 
#exp3alone2 <- long_females_mf_e3_d2 %>% mutate(variable = "together")
#exp3together2 <- long_mated_femalese3d2 %>% mutate(variable = "alone")
# Using rbind to bind the two data sets from the experiment 
#exp3d2 <- rbind(exp3alone2, exp3together2)
# Making a linear model of fly numbers and diet  
#exp3d2ls1 <- lm(fly_numbers ~ diet + variable, data = exp3d2)
#performance::check_model(exp3d2ls1)
# Same model but with sqrt 
#exp3d2ls1a <- lm(sqrt(fly_numbers) ~ diet + variable, data = exp3d2)
#performance::check_model(exp3d2ls1a)
# better without sqrt 
# Testing with an interaction effect 
#exp3d2ls2 <- lm(fly_numbers ~ diet * variable, data = exp3d2)
#performance::check_model(exp3d2ls2)
#exp3d2ls2a <- lm(sqrt(fly_numbers) ~ diet * variable, data = exp3d2)
#performance::check_model(exp3d2ls2a)
# better without sqrt 







