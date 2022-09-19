library(tidyverse)
library(readxl)
library(kableExtra)
library(performance)
library(see)
library(patchwork)
library(usethis)
library(devtools)
library(here)




#Fly_data_ <- Fly_data_ %>% 
  # mutate(RATIO = fct_relevel(RATIO, "1;8", "1;2", "2;1", "8;1" ), 
#  DAY = fct_relevel(DAY))

 
#ggplot(data = Fly_data_, aes(x = DAY, y = FLIES, fill = RATIO)) +
   #geom_col(position = "dodge")+
#geom_point(colour = "black", 
#            position = position_dodge(width = 0.9),
#            shape = 21,
#            alpha = 0.6)+
# theme_minimal()+
#labs(y = "Mean number of flies per patch", 
#      x = "Day")


#Malefliez <- Maleflies %>% 
# mutate(Ratio = fct_relevel(Ratio, "1;8", "1;2", "2;1", "8;1" ))

#ggplot(data = Malefliez, aes(x = Day, y = Flies, fill = Ratio))+
#  geom_col(position = "dodge")+ 
# geom_point(colour = "black", 
#            position = position_dodge(width = 0.9),
#            shape = 21,
#    alpha = 0.6)+
  # theme_minimal()+
  #  labs(y = "Mean number of flies per plate", 
  #    x = "Day")


  #rlang::last_error()

  #ggplot(data = eggdata, aes(x = ))

#_________________________________ Egg counting 1 (Figure)



egg_counting_data <- read_csv("~/Downloads/project/eggcountingdata.csv", col_select = 1:5) %>% drop_na()

longdata <- egg_counting_data %>% 
pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "egg_numbers")


summary1 <- longdata %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

realegg <- summary1%>% 
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

  


#---------------------------------------- Female feeding behaviour figure 

# Day 1 feeding behaviour 

femaleflyday1 <- read_csv("~/Downloads/project/femaleflyday1.csv")  %>% drop_na()



femaleflyday1long <- femaleflyday1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

summary2 <- femaleflyday1long %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

fly2 <- summary2%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = femaleflyday1long,
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


fly2





# Day 2 feeding behaviour 

femaleflyday2 <- read_csv("~/Downloads/project/femaleflyday2.csv", col_select = 1:5 )  %>% drop_na()

femaleflyday2long <- femaleflyday2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly2_numbers")

summary3 <- femaleflyday2long %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly2_numbers),
            sd = sd(fly2_numbers),
            n = n(),
            se = sd/sqrt(n))

fly3 <- summary3 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = femaleflyday2long,
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



# Putting plots together 
fly2 + fly3



#---------------------------------------- Male feeding behaviour figure 

#------------------------ day 1 male feeding 


maleflyday1 <- read_csv("~/Downloads/project/maleflyday1.csv" )  %>% drop_na()

maleflyday1long <- maleflyday1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "mfly1_numbers")

summary4 <- maleflyday1long %>% 
  group_by(diet) %>% 
  summarise(mean = mean(mfly1_numbers),
            sd = sd(mfly1_numbers),
            n = n(),
            se = sd/sqrt(n))

fly4 <- summary4 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#A8DBAF",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#A8DBAF",
                width = 0.2)+
  geom_jitter(data = maleflyday1long,
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

fly4


#------------------------ day 2 male feeding 

maleflyday2 <- read_csv("~/Downloads/project/maleflyday2.csv", col_select = 1:5 )  %>% drop_na()

maleflyday2long <- maleflyday2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "mfly2_numbers")

summary6 <- maleflyday2long %>% 
  group_by(diet) %>% 
  summarise(mean = mean(mfly2_numbers),
            sd = sd(mfly2_numbers),
            n = n(),
            se = sd/sqrt(n))

fly5 <- summary6 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#A8DBAF",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#A8DBAF",
                width = 0.2)+
  geom_jitter(data = maleflyday2long,
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

fly5

# patchwork plots together 



fly4 + fly5


eggtry <- read_csv("~/Downloads/project/flytry.csv" )  %>% drop_na()

eggtrylong <- eggtry %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "flytry_numbers")

summary7 <- eggtrylong %>% 
  group_by(diet) %>% 
  summarise(mean = mean(flytry_numbers),
            sd = sd(flytry_numbers),
            n = n(),
            se = sd/sqrt(n))

fly5 <- summary6 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#A8DBAF",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#A8DBAF",
                width = 0.2)+
  geom_jitter(data = maleflyday2long,
              aes(x = diet,
                  y = mfly2_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Diet \n(Protein; Carbohydrate)\n*Day 2*", 
       y = "")+
  theme_minimal()

fly4


#---------------------



femalenf2 <- read_csv("~/Downloads/project/femalenf2.csv", col_select = 1:9)  %>% drop_na()

femalenflong <- femalenf2 %>% 
  pivot_longer(cols = ("1":"8"), names_to = "plate", values_to = "fnf")

femalenflong

summary8 <- femalenflong %>% 
  group_by(plate) %>% 
  summarise(mean = mean(fnf),
            sd = sd(fnf),
            n = n(),
            se = sd/sqrt(n))

fly6 <- summary8 %>% 
  ggplot(aes(x = plate, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = femalenflong,
              aes(x = plate,
                  y = fnf),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Plate", 
       y = "Mean (+/- S.E.) flies per plate not on a patch (females)")+
  theme_minimal()

fly6




malenf <- read_csv("~/Downloads/project/malenf.csv", col_select = 1:9)  %>% drop_na()

malenflong <- malenf %>% 
  pivot_longer(cols = ("1":"8"), names_to = "plate", values_to = "mnf")

malenflong

summary9 <- malenflong %>% 
  group_by(plate) %>% 
  summarise(mean = mean(mnf),
            sd = sd(mnf),
            n = n(),
            se = sd/sqrt(n))

fly7 <- summary9 %>% 
  ggplot(aes(x = plate, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#A8DBAF",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#A8DBAF",
                width = 0.2)+
  geom_jitter(data = malenflong,
              aes(x = plate,
                  y = mnf),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Plate", 
       y = "Mean (+/- S.E.) flies per plate not on a patch (males)")+
  theme_minimal()

fly7

fly6 + fly7



#-------------------------- Data Analysis --------------------------------------



#-------- Egg counting 

eggcountingsummary  <- longdata %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

eggcountingsummary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

eggcountingls1 <- lm(egg_numbers ~ diet, data = longdata)

eggcountingls1
summary(eggcountingls1)
anova(eggcountingls1)
confint(eggcountingls1)

broom::tidy(eggcountingls1,  
            exponentiate=T, 
            conf.int=T)


performance::check_model(eggcountingls1)





eggcountingls1table <- eggcountingls1 %>% broom::tidy(conf.int = T) %>% 
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

#------------- Male feeding behaviour day 1 

maleday1summary <- maleflyday1long %>%
  group_by(diet) %>%
  summarise(mean = mean(mfly1_numbers),
            sd=sd(mfly1_numbers))

maleday1summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

maleflyday1summary <- lm(mfly1_numbers ~ diet, data = maleflyday1long)

maleday1summary
summary(maleday1summary)

maleday1summary2 <- glm(formula = mfly1_numbers ~ diet,
                    family = quasipoisson(), data = maleflyday1long)

summary(maleday1summary2)
performance::check_model(maleday1summary2, check=c("homogeneity", "qq"))


performance::check_model(maleday1summary2)


broom::tidy(maleday1summary2)
anova(maleday1summary2)


broom::tidy(eggcountingls1,  
            exponentiate=T, 
            conf.int=T)


#---------------- Female feeding behaviour day 1 

femaleday1summary <- femaleflyday1long %>%
  group_by(diet) %>%
  summarise(mean = mean(fly_numbers),
            sd=sd(fly_numbers))


femaleday1summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

femaleday1summary <- lm(fly_numbers ~ diet, data = femaleflyday1long)

summary(femaleday1summary)

femaleday1summary
confint(femaleday1summary)
anova(femaleday1summary)


performance::check_model(femaleday1summary)

broom::tidy(femaleday1summary,  
            exponentiate=T, 
            conf.int=T)

femaleday1table <- femaleday1summary %>% broom::tidy(conf.int = T) %>% 
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

      
      
     

#---------------- Repeatability chart 

repeatp <- read_csv("~/Downloads/project/RepeatabilityPractise.csv" ) %>% drop_na()

#-----------------------------------------------------

repeaty <- read_csv("~/Downloads/project/repeatdata.csv")  %>% drop_na()

longrepeaty <- repeaty %>% 
  pivot_longer(cols = ("20":"180"), names_to = "one", values_to = "two")

longrepeaty

ggplot(data = longrepeaty, aes(x = one , y = two))+
  geom_line()+
  geom_point()+
  ylim(0,200)

#-----------------------------------------------------

eggpractise<- read_csv("~/Downloads/project/eggpractise.csv", col_select = 1:5) %>% drop_na()

eggpractiselong <- eggpractise %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "practiseegg")

practisesummary <- eggpractiselong %>% 
  group_by(diet) %>% 
  summarise(mean = mean(practiseegg),
            sd = sd(practiseegg),
            n = n(),
            se = sd/sqrt(n))



practiseegg <- practisesummary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "pink",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = eggpractiselong,
              aes(x = diet,
                  y = practiseegg),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs")+
  theme_minimal()+
  ylim(0, 200)



practiseegg
realegg

realegg + practiseegg 



 
#----------------------------------------------------- Egg practise 2


eggpractise2<- read_csv("~/Downloads/project/eggpractise2.csv", col_select = 1:5) %>% drop_na()

eggpractiselong2 <- eggpractise2 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "practiseegg2")

practisesummary2 <- eggpractiselong2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(practiseegg2),
            sd = sd(practiseegg2),
            n = n(),
            se = sd/sqrt(n))



practiseeggplot <- practisesummary2%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "pink",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = eggpractiselong2,
              aes(x = diet,
                  y = practiseegg2),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs")+
  theme_minimal()+
  ylim(0, 200)






comparingegg <- read_csv("~/Downloads/project/comparingegg.csv", col_select = 1:5) %>% drop_na()

comparingegglong<- comparingegg %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "comparingeggn")

comparingeggsummary <- comparingegglong %>% 
  group_by(diet) %>% 
  summarise(mean = mean(comparingeggn),
            sd = sd(comparingeggn),
            n = n(),
            se = sd/sqrt(n))

comparingeggs<- comparingeggsummary%>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "pink",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = comparingegglong,
              aes(x = diet,
                  y = comparingeggn),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs")+
  theme_minimal()+
  ylim(0, 200)



comapringeggsum <- glm(TRY ~  comparingeggn, data = comparingegglong)

summary(comapringeggsum)
anova(comapringeggsum)


problems()

#----------------------------- Feeding behaviour analysis, experiment 2----------------------------




#-----------------------------  Mated Females Day 1 

mated_femalesd1 <- read_csv("~/Desktop/MatedFemalesD1.csv", col_select = 3:6)  %>% drop_na()

long_mated_femalesd1 <- mated_femalesd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_mated_femalesd1_summary <- long_mated_femalesd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers)
            ,
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

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







#----------------------------- Virgin Females Day 1 
virgin_femalesd1 <- read_excel("~/Desktop/VirginFemalesD1.xls") %>% drop_na()


long_virgin_femalesd1 <- virgin_femalesd1 %>% 
  pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_femalesd1_summary <- long_virgin_femalesd1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers)
            ,
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

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





long_virgin_femalesd1_summary %>%
  kbl(caption=" ") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")






#----------------------------- Mated Females Day 2 


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




 mated_femalesd2_ls <- lm(fly_numbers ~ diet, data = long_mated_femalesd2)

 mated_femalesd2_ls
 anova(mated_femalesd2_ls)
 
 performance::check_model(mated_femalesd2_ls)
 performance::check_model(mated_femalesd2_ls, check=c("homogeneity", "qq"))
 
 broom::tidy(mated_femalesd2_ls,  
             exponentiate=T, 
             conf.int=T)
 
 
 #----------------------------- Virgin Females Day 2 
 
 virgin_femalesd2 <- (read_excel(path = "~/Desktop/VirginFemalesD2.xlsx", na = "NA"))

long_virgin_femalesd2 <- virgin_femalesd2 %>% 
   pivot_longer(cols = ("8;1":"1;8"), names_to = "diet", values_to = "fly_numbers")

long_virgin_femalesd2_summary <- long_virgin_femalesd2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers)
            ,
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

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

mated_femalesd2_plot + virgin_femalesd2_plot 


