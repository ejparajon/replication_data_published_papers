#Epistemic Communities and Public Support for the Paris Agreement on Climate Change

# Preamble ----------------------------------------------------------------

#Clear workspace and setting seed for any randomization
rm(list=ls())

set.seed(101)


#The pacman package is required to load/install the additional packages.

if (!require("pacman")) install.packages("pacman")

#Installing and loading packages for use
pacman::p_load(readstata13,
               MatchIt,
               "wesanderson",
               tidyverse,
               qwraps2,stargazer)

#Creating a ggplot theme for use in several figures throughout the code.
plot_theme = theme(text = element_text(size=11),  
                   panel.grid.major.y = element_blank(), 
                   panel.grid.minor.y = element_blank(), 
                   panel.grid.minor.x = element_blank(), 
                   panel.background = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   axis.text = element_text(color="black"),
                   axis.line = element_line(colour = "black"),
                   strip.text.y = element_text(angle = 0), axis.title.y = element_text(angle=0, hjust=.5, vjust=.5),
                   axis.title = element_text(lineheight=1.1), 
                   panel.grid.minor = element_blank(),
) 


#Reading in datafile for analysis
data <- read.dta13("final_data_both_samples.dta")


#table 1 and 2
#demographics - gender, age, region, income
demo_df_vars <- c("gender", "famincom", "race", "edu_new3", "region", "age", "mturk")
demo_df <- data[demo_df_vars]
demo_df$region <- factor(demo_df$region, levels=c("Northeast", "Midwest", "South","West"))
#edu 
demo_df$edu_new3 <- factor(demo_df$edu_new3, levels=c(1,2, 3, 4), labels=c("Up to high school diploma", "Some college", "Associate degree/Bachelor's degree", "Advanced degree"))
#income
demo_df$famincom <- as.numeric(demo_df$famincom)
demo_df$famincom[ demo_df$famincom <= 3 ] <- 1
demo_df$famincom[ demo_df$famincom >= 4 & demo_df$famincom <=6 ] <- 2
demo_df$famincom[ demo_df$famincom >= 7 & demo_df$famincom <=9 ] <- 3
demo_df$famincom[ demo_df$famincom >= 10 & demo_df$famincom <=11 ] <- 4
demo_df$famincom[ demo_df$famincom >= 12 ] <- 5
demo_df$famincom <- factor(demo_df$famincom, levels=c(1, 2, 3, 4, 5), labels=c("Up to $29,999", "$30,0000-$59,999", "$60,000-$99,999","$100,000-$149,999", "More than $150,000"))
#age 
demo_df <- demo_df %>% mutate(age_range = case_when(
  age >= 18 & age <= 35 ~ "18--35",
  age >= 36 & age <= 50 ~ "36--50",
  age >= 51 & age <= 75 ~ "51--75",
  age >= 76  ~ "75+",
  TRUE ~ NA_character_), age_range = factor(age_range, levels= c("18--35", "36--50", "51--75", "75+")))


demos_summary<- list("Gender" =  
                       list("Male"=  ~ n_perc(.data$gender == "~~~Male", na_rm=TRUE),
                            "Female" = ~ n_perc(.data$gender == "~~~Female", na_rm=TRUE),
                            "Other" = ~ n_perc(.data$gender == "~~~Not listed", na_rm=TRUE)), 
                     "Income" = 
                       list("Up to \\$29,999"=  ~ n_perc(.data$famincom == "Up to $29,999", na_rm=TRUE),
                            "\\$30,0000-\\$59,999" = ~ n_perc(.data$famincom == "$30,0000-$59,999", na_rm=TRUE),
                            "\\$60,000-\\$99,999"= ~ n_perc(.data$famincom == "$60,000-$99,999", na_rm=TRUE),
                            "\\$100,000-\\$149,999" = ~ n_perc(.data$famincom == "$100,000-$149,999", na_rm=TRUE), 
                            "More than \\$150,000" = ~ n_perc(.data$famincom == "More than $150,000", na_rm=TRUE)),
                     "Education" =
                       list("Up to high school diploma"=  ~ n_perc(.data$edu_new3 =="Up to high school diploma", na_rm=TRUE),
                            "Some college"=  ~ n_perc(.data$edu_new3 == "Some college", na_rm=TRUE),
                            "Associate degree/Bachelor's degree" =  ~ n_perc(.data$edu_new3 == "Associate degree/Bachelor's degree", na_rm=TRUE),
                            "Advanced degree"=  ~ n_perc(.data$edu_new3 ==  "Advanced degree", na_rm=TRUE)), 
                     "Age" =
                       list("18--35"=  ~ n_perc(.data$age_range == "18--35", na_rm=TRUE),
                            "36--50"=  ~ n_perc(.data$age_range == "36--50", na_rm=TRUE),
                            "51--75" =  ~ n_perc(.data$age_range == "51--75", na_rm=TRUE),
                            "75+"=  ~ n_perc(.data$age_range == "75+", na_rm=TRUE)), 
                     "Region" = 
                       list("Northeast"=  ~ n_perc(.data$region == "Northeast", na_rm=TRUE),
                            "Midwest"=  ~ n_perc(.data$region == "Midwest", na_rm=TRUE),
                            "South and Central"=  ~ n_perc(.data$region == "South", na_rm=TRUE),
                            "West"=  ~ n_perc(.data$region == "West", na_rm=TRUE)), 
                     "Race/Ethnicity" = 
                       list("White"=  ~ n_perc(.data$race == "~~~White", na_rm=TRUE),
                            "Black"=  ~ n_perc(.data$race == "~~~Black", na_rm=TRUE),
                            "American Indian or Alaskan Native"=  ~ n_perc(.data$race == "~~~American Indian or Alaskan Native", na_rm=TRUE),
                            "Asian "=  ~ n_perc(.data$race == "~~~Asian", na_rm=TRUE), 
                            "Native Hawaiian or Pacific Islander"=  ~ n_perc(.data$race == "~~~Native Hawaiian or Pacific Islander", na_rm=TRUE), 
                            "Other"=  ~ n_perc(.data$race == "~~~Other", na_rm=TRUE), 
                            "Hispanic/Latino"=  ~ n_perc(.data$race == "~~~Hispanic/Latino", na_rm=TRUE)))



#census distributions from 2016 American Community Survey - based on tables released by census 
# Income: https://www.census.gov/data/tables/time-series/demo/income-poverty/cps-hinc/hinc-06.2016.html
# Age and Gender: https://www.census.gov/data/tables/2016/demo/age-and-sex/2016-age-sex-composition.html
# Edu: https://www.census.gov/data/tables/2016/demo/education-attainment/cps-detailed-tables.html
# REgion: https://www.census.gov/library/publications/2017/demo/p60-259.html
# Race: https://data.census.gov/cedsci/table?d=ACS%205-Year%20Estimates%20Data%20Profiles&table=DP05&tid=ACSDP5Y2016.DP05
census <- tibble(value = c("49.20\\%", "50.80\\%", "0\\%", "25.48\\%", "25.07\\%", "21.76\\%", "14.13\\%", "13.57\\%", "40.66\\%", "19.10\\%", "30.96\\%", "11.19\\%", "28.52\\%", "23.53\\%", "35.27\\%", "7.60\\%", "17.69\\%", "21.68\\%", "38.08\\%", "22.56\\%", "62.00\\%", "12.30\\%", "0.70\\%", "5.20\\%", "0.20\\%", "2.30\\%", "17.30\\%"),
                 names = c("Male", "Female" , "Other" , "Up to $29,999" , "$30,0000-$59,999" , "$60,000-$99,999" , "$100,000-$149,999" , "More than $150,000" , "Up to high school diploma" , "Some college" , "Associate degree/Bachelor's degree" , "Advanced degree" , "18--34" , "35--59" , "55--75" , "75+" , "Northeast" , "Midwest" , "South and Central", "West", "White","Black","American.Indian.or.Alaskan.Native","Asian.","Native.Hawaiian.or.Pacific.Islander","Other.1","Hispanic.Latino"))
census <- column_to_rownames(census, var="names")


ssi_demos <- summary_table(filter(demo_df, mturk==0), demos_summary)
mturk_demos <- summary_table(filter(demo_df, mturk==1), demos_summary)
#this gives us a qwraps object into which we can copy our census figures. 
demos_template_for_census <- summary_table(filter(demo_df, mturk==1), demos_summary)
#map census into qwraps object
for (i in seq_along(census$value)) {
  demos_template_for_census[i] = census$value[i] 
}

#table 1
sink('Tables/table1.tex')
print(cbind(ssi_demos,demos_template_for_census),  cnames =c("SSI sample", "2016 ACS"), booktabs=TRUE)
sink()
#table 2
sink('Tables/table2.tex')
print(cbind(mturk_demos,demos_template_for_census),  cnames =c("mTurk sample", "2016 ACS"), booktabs = TRUE)
sink()

#Figure 3: Manipulation Check 
#Estimated effect of treatment on perceptions of support for treaty approval in each EC relative to control condition.

#3a. CS treatment
#ssi manipulation
support_cs <- lm(cs_support~scholars_treatments_2, data=data[data$expert_type_2=="climate science" & data$mturk==0,])
support_cs_df <- as.data.frame(coef(summary(support_cs)))
support_cs_df$iv <- rownames(support_cs_df)
support_cs_df$upper95 <- support_cs_df$Estimate + 1.96*support_cs_df$"Std. Error"
support_cs_df$lower95 <- support_cs_df$Estimate - 1.96*support_cs_df$"Std. Error"
support_cs_df$upper90 <- support_cs_df$Estimate + 1.645*support_cs_df$"Std. Error"
support_cs_df$lower90 <- support_cs_df$Estimate - 1.645*support_cs_df$"Std. Error"
support_cs_df$scholars <- "Climate Science"
support_cs_df$level <- ""
support_cs_df$Sample <- "SSI"
support_cs_df[support_cs_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_cs_df[support_cs_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_cs_df[support_cs_df$iv == "scholars_treatments_2support",]$level <- "Support"
#mturk manipulation
support_cs <- lm(cs_support~scholars_treatments_2, data=data[data$expert_type_2=="climate science" & data$mturk==1,])
support_cs_df2 <- as.data.frame(coef(summary(support_cs)))
support_cs_df2$iv <- rownames(support_cs_df2)
support_cs_df2$upper95 <- support_cs_df2$Estimate + 1.96*support_cs_df2$"Std. Error"
support_cs_df2$lower95 <- support_cs_df2$Estimate - 1.96*support_cs_df2$"Std. Error"
support_cs_df2$upper90 <- support_cs_df2$Estimate + 1.645*support_cs_df2$"Std. Error"
support_cs_df2$lower90 <- support_cs_df2$Estimate - 1.645*support_cs_df2$"Std. Error"
support_cs_df2$scholars <- "Climate Science"
support_cs_df2$level <- ""
support_cs_df2$Sample <- "MTurk"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2support",]$level <- "Support"

#combined 
support_cs <- lm(cs_support~scholars_treatments_2, data=data[data$expert_type_2=="climate science",])
support_cs_df3 <- as.data.frame(coef(summary(support_cs)))
support_cs_df3$iv <- rownames(support_cs_df3)
support_cs_df3$upper95 <- support_cs_df3$Estimate + 1.96*support_cs_df3$"Std. Error"
support_cs_df3$lower95 <- support_cs_df3$Estimate - 1.96*support_cs_df3$"Std. Error"
support_cs_df3$upper90 <- support_cs_df3$Estimate + 1.645*support_cs_df3$"Std. Error"
support_cs_df3$lower90 <- support_cs_df3$Estimate - 1.645*support_cs_df3$"Std. Error"
support_cs_df3$scholars <- "Climate Science"
support_cs_df3$level <- ""
support_cs_df3$Sample <- "Combined"
support_cs_df3[support_cs_df3$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_cs_df3[support_cs_df3$iv == "scholars_treatments_2split",]$level <- "Split"
support_cs_df3[support_cs_df3$iv == "scholars_treatments_2support",]$level <- "Support"

support_cs <- rbind(support_cs_df, support_cs_df2, support_cs_df3)


#3b. IR treatment

#SSI
support_ir <- lm(ir_support~scholars_treatments_2, data=data[data$expert_type_2=="international relations" & data$mturk==0,])
support_ir_df <- as.data.frame(coef(summary(support_ir)))
support_ir_df$iv <- rownames(support_ir_df)
support_ir_df$upper95 <- support_ir_df$Estimate + 1.96*support_ir_df$"Std. Error"
support_ir_df$lower95 <- support_ir_df$Estimate - 1.96*support_ir_df$"Std. Error"
support_ir_df$upper90 <- support_ir_df$Estimate + 1.645*support_ir_df$"Std. Error"
support_ir_df$lower90 <- support_ir_df$Estimate - 1.645*support_ir_df$"Std. Error"
support_ir_df$scholars <- "International Relations"
support_ir_df$level <- ""
support_ir_df$Sample <- "SSI"
support_ir_df[support_ir_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ir_df[support_ir_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_ir_df[support_ir_df$iv == "scholars_treatments_2support",]$level <- "Support"
#mturk 
support_ir <- lm(ir_support~scholars_treatments_2, data=data[data$expert_type_2=="international relations" & data$mturk==1,])
support_ir_df2 <- as.data.frame(coef(summary(support_ir)))
support_ir_df2$iv <- rownames(support_ir_df)
support_ir_df2$upper95 <- support_ir_df2$Estimate + 1.96*support_ir_df2$"Std. Error"
support_ir_df2$lower95 <- support_ir_df2$Estimate - 1.96*support_ir_df2$"Std. Error"
support_ir_df2$upper90 <- support_ir_df2$Estimate + 1.645*support_ir_df2$"Std. Error"
support_ir_df2$lower90 <- support_ir_df2$Estimate - 1.645*support_ir_df2$"Std. Error"
support_ir_df2$scholars <- "International Relations"
support_ir_df2$level <- ""
support_ir_df2$Sample <- "MTurk"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2support",]$level <- "Support"

#combined 
support_ir <- lm(ir_support~scholars_treatments_2, data=data[data$expert_type_2=="international relations",])
support_ir_df3 <- as.data.frame(coef(summary(support_ir)))
support_ir_df3$iv <- rownames(support_ir_df)
support_ir_df3$upper95 <- support_ir_df3$Estimate + 1.96*support_ir_df3$"Std. Error"
support_ir_df3$lower95 <- support_ir_df3$Estimate - 1.96*support_ir_df3$"Std. Error"
support_ir_df3$upper90 <- support_ir_df3$Estimate + 1.645*support_ir_df3$"Std. Error"
support_ir_df3$lower90 <- support_ir_df3$Estimate - 1.645*support_ir_df3$"Std. Error"
support_ir_df3$scholars <- "International Relations"
support_ir_df3$level <- ""
support_ir_df3$Sample <- "Combined"
support_ir_df3[support_ir_df3$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ir_df3[support_ir_df3$iv == "scholars_treatments_2split",]$level <- "Split"
support_ir_df3[support_ir_df3$iv == "scholars_treatments_2support",]$level <- "Support"


support_ir <- rbind(support_ir_df, support_ir_df2, support_ir_df3)

#3c. IE treatment

support_ie <- lm(econ_support~scholars_treatments_2, data=data[data$expert_type_2=="international economics" & data$mturk==0,])
#ssi
support_ie_df <- as.data.frame(coef(summary(support_ie)))
support_ie_df$iv <- rownames(support_ie_df)
support_ie_df$upper95 <- support_ie_df$Estimate + 1.96*support_ie_df$"Std. Error"
support_ie_df$lower95 <- support_ie_df$Estimate - 1.96*support_ie_df$"Std. Error"
support_ie_df$upper90 <- support_ie_df$Estimate + 1.645*support_ie_df$"Std. Error"
support_ie_df$lower90 <- support_ie_df$Estimate - 1.645*support_ie_df$"Std. Error"
support_ie_df$scholars <- "International Economics"
support_ie_df$level <- ""
support_ie_df$Sample <- "SSI"
support_ie_df[support_ie_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ie_df[support_ie_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_ie_df[support_ie_df$iv == "scholars_treatments_2support",]$level <- "Support"


#mturk
support_ie <- lm(econ_support~scholars_treatments_2, data=data[data$expert_type_2=="international economics" & data$mturk==1,])
support_ie_df2 <- as.data.frame(coef(summary(support_ie)))
support_ie_df2$iv <- rownames(support_ie_df2)
support_ie_df2$upper95 <- support_ie_df2$Estimate + 1.96*support_ie_df2$"Std. Error"
support_ie_df2$lower95 <- support_ie_df2$Estimate - 1.96*support_ie_df2$"Std. Error"
support_ie_df2$upper90 <- support_ie_df2$Estimate + 1.645*support_ie_df2$"Std. Error"
support_ie_df2$lower90 <- support_ie_df2$Estimate - 1.645*support_ie_df2$"Std. Error"
support_ie_df2$scholars <- "International Economics"
support_ie_df2$level <- ""
support_ie_df2$Sample <- "MTurk"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2support",]$level <- "Support"


#combined
support_ie <- lm(econ_support~scholars_treatments_2, data=data[data$expert_type_2=="international economics",])
support_ie_df3 <- as.data.frame(coef(summary(support_ie)))
support_ie_df3$iv <- rownames(support_ie_df3)
support_ie_df3$upper95 <- support_ie_df3$Estimate + 1.96*support_ie_df3$"Std. Error"
support_ie_df3$lower95 <- support_ie_df3$Estimate - 1.96*support_ie_df3$"Std. Error"
support_ie_df3$upper90 <- support_ie_df3$Estimate + 1.645*support_ie_df3$"Std. Error"
support_ie_df3$lower90 <- support_ie_df3$Estimate - 1.645*support_ie_df3$"Std. Error"
support_ie_df3$scholars <- "International Economics"
support_ie_df3$level <- ""
support_ie_df3$Sample <- "Combined"
support_ie_df3[support_ie_df3$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ie_df3[support_ie_df3$iv == "scholars_treatments_2split",]$level <- "Split"
support_ie_df3[support_ie_df3$iv == "scholars_treatments_2support",]$level <- "Support"


support_ie <- rbind(support_ie_df, support_ie_df2, support_ie_df3)

support_epis <- rbind(support_cs, support_ir, support_ie)
support_epis <- subset(support_epis, iv!="(Intercept)" & Sample!="Combined")

#3d. Graph

ec_support_plot <- ggplot(data=support_epis, aes(x=scholars, y=Estimate, shape=Sample)) + 
  geom_hline(yintercept=0, color="#DDDDDD", size=.5) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95, width=0, color=Sample), size=.5, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0, color=Sample), size=.8, position=position_dodge(width=0.5)) + 
  geom_point(aes(y=Estimate, color=Sample), size=1.75, position=position_dodge(width=0.5)) + 
  xlab("Treatment\n condition") +
  ylab("Estimated treatment effect") +
  scale_color_manual(values =c("#E1AF00", "#3B9AB2")) +
  coord_flip() +
  facet_grid(level~.) +
  plot_theme + theme(legend.position = "bottom", legend.key = element_rect(colour = "transparent", fill = "transparent"))
ec_support_plot

ggsave("Figures/Figure3.eps", ec_support_plot, width=6.5, height=4,dpi=300)

#average manipulation check
#``Averaging across the three ECs, the oppose treatment reduced perceived EC support for the climate agreement by 29.06 (t =12.53, p < .000) percentage points among MTurk respondents and by about 11.06  percentage points (t =7.55, p < .000) among SSI respondents.''
# FN 11: In the control group, we find that our respondents, on average, believe that about 70% of climate scientists support COP21.
mturk_manipulation_oppose <-t.test(data$ec_support[data$scholars_treatments_2=="oppose"&data$mturk==1],data$ec_support[data$scholars_treatments_2=="control"&data$mturk==1])
mturk_manipulation_oppose
mturk_manipulation_oppose$estimate[1] - mturk_manipulation_oppose$estimate[2]

SSI_manipulation_oppose <-t.test(data$ec_support[data$scholars_treatments_2=="oppose"&data$mturk==0],data$ec_support[data$scholars_treatments_2=="control"&data$mturk==0])
SSI_manipulation_oppose
SSI_manipulation_oppose$estimate[1] - SSI_manipulation_oppose$estimate[2]

#``Again averaging across the three EC treatments, the split treatment reduced perceptions of EC support for the climate agreement by 8.21 percentage points among MTurk respondents (t = 4.07, p < .000) and by just 2.23 (t =2.36, p = .105) percentage points among SSI respondents. ''
mturk_manipulation_split <-t.test(data$ec_support[data$scholars_treatments_2=="split"&data$mturk==1],data$ec_support[data$scholars_treatments_2=="control"&data$mturk==1])
mturk_manipulation_split
mturk_manipulation_split$estimate[1] - mturk_manipulation_split$estimate[2]

SSI_manipulation_split <-t.test(data$ec_support[data$scholars_treatments_2=="split"&data$mturk==0],data$ec_support[data$scholars_treatments_2=="control"&data$mturk==0])
SSI_manipulation_split
SSI_manipulation_split$estimate[1] - SSI_manipulation_split$estimate[2]

#``In the MTurk control group, for example, the mean response to our question about the proportion of climate scholars who support COP21 was about 87 percent. The same figure in our SSI sample was about 74 percent.''
mean(data[data$scholars_treatments_2=="control" & data$mturk==0,]$cs_support, na.rm=TRUE)
mean(data[data$scholars_treatments_2=="control" & data$mturk==1,]$cs_support, na.rm=TRUE)

#``In the MTurk sample, respondents’ mean level of perceived support for COP21 among IR scholars was 66 percent, while the same figure for scholars of international economics was about 53 percent.''
mean(data[data$scholars_treatments_2=="control" & data$mturk==1,]$ir_support, na.rm=TRUE)
mean(data[data$scholars_treatments_2=="control" & data$mturk==1,]$econ_support, na.rm=TRUE)

#``In the SSI sample, these figures were about 61 and 57 percent respectively.''
mean(data[data$scholars_treatments_2=="control" & data$mturk==0,]$ir_support, na.rm=TRUE)
mean(data[data$scholars_treatments_2=="control" & data$mturk==0,]$econ_support, na.rm=TRUE)

#Figure 4: Main Results 
#Estimated treatment effect of scholar views on support for COP21 Climate Agreement.
#4a. MTurk Sample
results<-lm(cop21_dv~scholars_treatments_2, data=data[data$mturk==1,])

#``Among those in the oppose treatment for the MTurk sample, support for COP21 was about .98 (95% CI: 1.29, .675) points lower than among those assigned to the control group.''
#``Support for COP21 among those assigned to the “scholars split” treatment for the MTurk sample was about .55 (95% CI: .86, .24)''
summary(results)

temp_df <- as.data.frame(coef(summary(results)))
temp_df$iv <- rownames(temp_df)
temp_df$upper95 <- temp_df$Estimate + 1.96*temp_df$"Std. Error"
temp_df$lower95 <- temp_df$Estimate - 1.96*temp_df$"Std. Error"
temp_df$upper90 <- temp_df$Estimate + 1.645*temp_df$"Std. Error"
temp_df$lower90 <- temp_df$Estimate - 1.645*temp_df$"Std. Error"
#formatting
temp_df <- subset(temp_df, iv!="(Intercept)")
temp_df$iv_formatted <- ""
temp_df$Sample <- "MTurk"
temp_df[temp_df$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df[temp_df$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df[temp_df$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df$ivfactor<-as.factor(temp_df$iv_formatted) #reordering 


#4b. SSI Sample
results<-lm(cop21_dv~scholars_treatments_2, data=data[data$mturk==0,])

#``In the SSI sample, support for COP21 was about .56 (95% CI: .76, .37) points lower on our seven point scale among those assigned to our oppose group than among those assigned to the control group.''
#``Support for COP21 among those assigned to the “scholars split” treatment for the SSI sample was about .18 (95% CI: 0.38, 0.01) points lower on our seven point scale than among those in the control''
summary(results)

temp_df_2 <- as.data.frame(coef(summary(results)))
temp_df_2$iv <- rownames(temp_df_2)
temp_df_2$upper95 <- temp_df_2$Estimate + 1.96*temp_df_2$"Std. Error"
temp_df_2$lower95 <- temp_df_2$Estimate - 1.96*temp_df_2$"Std. Error"
temp_df_2$upper90 <- temp_df_2$Estimate + 1.645*temp_df_2$"Std. Error"
temp_df_2$lower90 <- temp_df_2$Estimate - 1.645*temp_df_2$"Std. Error"

temp_df_2 <- subset(temp_df_2, iv!="(Intercept)")
temp_df_2$iv_formatted <- ""
temp_df_2$Sample <- "SSI"
temp_df_2[temp_df_2$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_2[temp_df_2$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_2[temp_df_2$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_2$ivfactor<-as.factor(temp_df_2$iv_formatted) #reordering 

#combined estimate 
results<-lm(cop21_dv~scholars_treatments_2, data=data)
temp_df_3 <- as.data.frame(coef(summary(results)))
temp_df_3$iv <- rownames(temp_df_3)
temp_df_3$upper95 <- temp_df_3$Estimate + 1.96*temp_df_3$"Std. Error"
temp_df_3$lower95 <- temp_df_3$Estimate - 1.96*temp_df_3$"Std. Error"
temp_df_3$upper90 <- temp_df_3$Estimate + 1.645*temp_df_3$"Std. Error"
temp_df_3$lower90 <- temp_df_3$Estimate - 1.645*temp_df_3$"Std. Error"

temp_df_3 <- subset(temp_df_3, iv!="(Intercept)")
temp_df_3$iv_formatted <- ""
temp_df_3$Sample <- "Combined"
temp_df_3[temp_df_3$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_3[temp_df_3$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_3[temp_df_3$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_3$ivfactor<-as.factor(temp_df_3$iv_formatted) #reordering 



#combined df
combined_df<-rbind(temp_df,temp_df_2)
combined_df$ivfactor<- fct_relevel(combined_df$ivfactor, "Scholars support", "Scholars split", "Scholars oppose")

main_results_both_surveys <- ggplot(data=combined_df, aes(x=ivfactor, shape=Sample)) + 
  geom_hline(yintercept=0, color="#DDDDDD", size=.5) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95, width=0, color=Sample), size=.5,position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0,  color=Sample), size=.9,position=position_dodge(width=0.5)) + 
  geom_point(aes(y=Estimate, color=Sample), size=2.2, position=position_dodge(width=0.5)) + 
  # facet_grid(Sample~.) +
  xlab("Treatment\ncondition") +
  ylab("Estimated treatment effect") +
  coord_flip() +
  scale_color_manual(values =c("#E1AF00","#3B9AB2")) +
  plot_theme + theme(legend.position = 'bottom', legend.key = element_rect(colour = "transparent", fill = "transparent"))

main_results_both_surveys

ggsave("Figures/Figure4.eps", main_results_both_surveys, width=6, height=3.5,dpi=300)

#Main results t-tests
#binary support_cop21_dv is 100 if the registered some support and 0 otherwise. 

#``This represents about an 8.6 percentage point reduction in the number of respondents reporting that they support the COP21 agreement a great deal, a moderate amount, or a little (t =3.74, p < .000). ''
SSI_main_oppose<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="oppose"&data$mturk==0],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==0])
SSI_main_oppose
SSI_main_oppose$estimate[1] - SSI_main_oppose$estimate[2]


#``In terms of the overall effect on support for COP21, this represents a much more modest 1.6 percentage point reduction in the number of respondents reporting that they support the COP21 agreement a great deal, a moderate amount, or a little (t = .745, p < .460).''
SSI_main_split<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="split"&data$mturk==0],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==0])
SSI_main_split
SSI_main_split$estimate[1] - SSI_main_split$estimate[2]

#``For the MTurk sample, the results were stronger and again in the expected direction. ... #This represents a 12.5 percentage point reduction in support, (t = 3.47, p < .000). ''
mturk_main_oppose<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="oppose"&data$mturk==1],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==1])
mturk_main_oppose
mturk_main_oppose$estimate[1] - mturk_main_oppose$estimate[2]

#``Support for COP21 among those assigned to the "scholars split" treatment for the MTurk sample was about .55 (95% CI: .86, .24) points lower than among those in the control group, representing roughly a 4.57 percentage point decrease in support (t =1.36, p <.1741).''
mturk_main_split<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="split"&data$mturk==1],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==1])
mturk_main_split
mturk_main_split$estimate[1] - mturk_main_split$estimate[2]

#5. Figure 5
#Effecs by ideology and level of consensus
data$Personal_Ideology<- ifelse(data$ideology == 4, "moderate",
                                ifelse(data$ideology <= 3, "liberal",
                                       ifelse(data$ideology >= 5, "conservative", NA)))

data$Personal_Ideology<- fct_relevel(data$Personal_Ideology, "moderate", "liberal", "conservative")

#Appendix Table 8
backlash_SSI<-lm(cop21_dv~Personal_Ideology*scholars_treatments_2+edu_new3+age+gender+race+quarter_income,data=data[data$mturk==0,]) 
backlash_Mturk<-lm(cop21_dv~Personal_Ideology*scholars_treatments_2+edu_new3+age+gender+race+quarter_income,data=data[data$mturk==1,])  

stargazer(backlash_Mturk,backlash_SSI,type="latex",column.labels = c("mTurk","SSI"), omit = c("quarter_income", "race", "gender", "edu_new3", "age"), 
              covariate.labels = c("Liberals", "Conservatives", "Scholars oppose", "Scholars split", "Scholars support", "Liberals: Scholars oppose", "Conservatives: Scholars oppose", "Liberals: Scholars split", "Conservatives: Scholars split", "Liberals: Scholars support", "Conservatives: Scholars support"), 
          notes = "Demographic controls included in model (age, gender, income, and, race), but excluded from table.", dep.var.labels = c("Support for COP21"), out="Tables/table8.tex")


#Appendix Table 9
backlash_SSI_2<-lm(cop21_dv~scholars_treatments_2+edu_new3+age+gender+race+quarter_income,data=data[data$mturk==0&data$Personal_Ideology=="conservative"&data$expert_type_2=="climate science",]) 
backlash_Mturk_2<-lm(cop21_dv~scholars_treatments_2+edu_new3+age+gender+race+quarter_income,data=data[data$mturk==1&data$Personal_Ideology=="conservative"&data$expert_type_2=="climate science",])  

stargazer(backlash_Mturk_2,backlash_SSI_2,column.labels = c("mTurk","SSI"),type="latex", out="Tables/table9.tex", omit = c("quarter_income", "race", "gender", "edu_new3", "age"), 
          covariate.labels = c("Climate scholars oppose", "Climate scholars favor", "Climate scholars support"), 
          notes = "Demographic controls included in model (age, gender, income, and, race), but excluded from table.",  dep.var.labels = c("Support for COP21"))


#5a. mturk results 
results<-lm(cop21_dv~scholars_treatments_2, data=data[data$Personal_Ideology=="moderate"&data$mturk==1,])
temp_df_1_mod <- as.data.frame(coef(summary(results)))
temp_df_1_mod$iv <- rownames(temp_df_1_mod)
temp_df_1_mod$upper95 <- temp_df_1_mod$Estimate + 1.96*temp_df_1_mod$"Std. Error"
temp_df_1_mod$lower95 <- temp_df_1_mod$Estimate - 1.96*temp_df_1_mod$"Std. Error"
temp_df_1_mod$upper90 <- temp_df_1_mod$Estimate + 1.645*temp_df_1_mod$"Std. Error"
temp_df_1_mod$lower90 <- temp_df_1_mod$Estimate - 1.645*temp_df_1_mod$"Std. Error"
#formatting
temp_df_1_mod <- subset(temp_df_1_mod, iv!="(Intercept)")
temp_df_1_mod$iv_formatted <- ""
temp_df_1_mod$Sample <- "MTurk"
temp_df_1_mod$ideology <- "Moderate"
temp_df_1_mod[temp_df_1_mod$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_1_mod[temp_df_1_mod$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_1_mod[temp_df_1_mod$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_1_mod$ivfactor<-as.factor(temp_df_1_mod$iv_formatted) #reordering 

results<-lm(cop21_dv~scholars_treatments_2, data=data[data$Personal_Ideology=="liberal"&data$mturk==1,])
temp_df_1_lib <- as.data.frame(coef(summary(results)))
temp_df_1_lib$iv <- rownames(temp_df_1_lib)
temp_df_1_lib$upper95 <- temp_df_1_lib$Estimate + 1.96*temp_df_1_lib$"Std. Error"
temp_df_1_lib$lower95 <- temp_df_1_lib$Estimate - 1.96*temp_df_1_lib$"Std. Error"
temp_df_1_lib$upper90 <- temp_df_1_lib$Estimate + 1.645*temp_df_1_lib$"Std. Error"
temp_df_1_lib$lower90 <- temp_df_1_lib$Estimate - 1.645*temp_df_1_lib$"Std. Error"
#formatting
temp_df_1_lib <- subset(temp_df_1_lib, iv!="(Intercept)")
temp_df_1_lib$iv_formatted <- ""
temp_df_1_lib$Sample <- "MTurk"
temp_df_1_lib$ideology <- "Liberal"
temp_df_1_lib[temp_df_1_lib$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_1_lib[temp_df_1_lib$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_1_lib[temp_df_1_lib$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_1_lib$ivfactor<-as.factor(temp_df_1_lib$iv_formatted) #reordering 

results<-lm(cop21_dv~scholars_treatments_2, data=data[data$Personal_Ideology=="conservative"&data$mturk==1,])
temp_df_1_con <- as.data.frame(coef(summary(results)))
temp_df_1_con$iv <- rownames(temp_df_1_con)
temp_df_1_con$upper95 <- temp_df_1_con$Estimate + 1.96*temp_df_1_con$"Std. Error"
temp_df_1_con$lower95 <- temp_df_1_con$Estimate - 1.96*temp_df_1_con$"Std. Error"
temp_df_1_con$upper90 <- temp_df_1_con$Estimate + 1.645*temp_df_1_con$"Std. Error"
temp_df_1_con$lower90 <- temp_df_1_con$Estimate - 1.645*temp_df_1_con$"Std. Error"
#formatting
temp_df_1_con <- subset(temp_df_1_con, iv!="(Intercept)")
temp_df_1_con$iv_formatted <- ""
temp_df_1_con$Sample <- "MTurk"
temp_df_1_con$ideology <- "Conservative"
temp_df_1_con[temp_df_1_con$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_1_con[temp_df_1_con$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_1_con[temp_df_1_con$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_1_con$ivfactor<-as.factor(temp_df_1_con$iv_formatted) #reordering 

#5b. SSI results 
results<-lm(cop21_dv~scholars_treatments_2, data=data[data$Personal_Ideology=="moderate"&data$mturk==0,])
temp_df_2_mod <- as.data.frame(coef(summary(results)))
temp_df_2_mod$iv <- rownames(temp_df_2_mod)
temp_df_2_mod$upper95 <- temp_df_2_mod$Estimate + 1.96*temp_df_2_mod$"Std. Error"
temp_df_2_mod$lower95 <- temp_df_2_mod$Estimate - 1.96*temp_df_2_mod$"Std. Error"
temp_df_2_mod$upper90 <- temp_df_2_mod$Estimate + 1.645*temp_df_2_mod$"Std. Error"
temp_df_2_mod$lower90 <- temp_df_2_mod$Estimate - 1.645*temp_df_2_mod$"Std. Error"
#formatting
temp_df_2_mod <- subset(temp_df_2_mod, iv!="(Intercept)")
temp_df_2_mod$iv_formatted <- ""
temp_df_2_mod$Sample <- "SSI"
temp_df_2_mod$ideology <- "Moderate"
temp_df_2_mod[temp_df_2_mod$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_2_mod[temp_df_2_mod$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_2_mod[temp_df_2_mod$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_2_mod$ivfactor<-as.factor(temp_df_2_mod$iv_formatted) #reordering 

results<-lm(cop21_dv~scholars_treatments_2, data=data[data$Personal_Ideology=="liberal"&data$mturk==0,])
temp_df_2_lib <- as.data.frame(coef(summary(results)))
temp_df_2_lib$iv <- rownames(temp_df_2_lib)
temp_df_2_lib$upper95 <- temp_df_2_lib$Estimate + 1.96*temp_df_2_lib$"Std. Error"
temp_df_2_lib$lower95 <- temp_df_2_lib$Estimate - 1.96*temp_df_2_lib$"Std. Error"
temp_df_2_lib$upper90 <- temp_df_2_lib$Estimate + 1.645*temp_df_2_lib$"Std. Error"
temp_df_2_lib$lower90 <- temp_df_2_lib$Estimate - 1.645*temp_df_2_lib$"Std. Error"
#formatting
temp_df_2_lib <- subset(temp_df_2_lib, iv!="(Intercept)")
temp_df_2_lib$iv_formatted <- ""
temp_df_2_lib$Sample <- "SSI"
temp_df_2_lib$ideology <- "Liberal"
temp_df_2_lib[temp_df_2_lib$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_2_lib[temp_df_2_lib$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_2_lib[temp_df_2_lib$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_2_lib$ivfactor<-as.factor(temp_df_2_lib$iv_formatted) #reordering 

results<-lm(cop21_dv~scholars_treatments_2, data=data[data$Personal_Ideology=="conservative"&data$mturk==0,])
temp_df_2_con <- as.data.frame(coef(summary(results)))
temp_df_2_con$iv <- rownames(temp_df_2_con)
temp_df_2_con$upper95 <- temp_df_2_con$Estimate + 1.96*temp_df_2_con$"Std. Error"
temp_df_2_con$lower95 <- temp_df_2_con$Estimate - 1.96*temp_df_2_con$"Std. Error"
temp_df_2_con$upper90 <- temp_df_2_con$Estimate + 1.645*temp_df_2_con$"Std. Error"
temp_df_2_con$lower90 <- temp_df_2_con$Estimate - 1.645*temp_df_2_con$"Std. Error"
#formatting
temp_df_2_con <- subset(temp_df_2_con, iv!="(Intercept)")
temp_df_2_con$iv_formatted <- ""
temp_df_2_con$Sample <- "SSI"
temp_df_2_con$ideology <- "Conservative"
temp_df_2_con[temp_df_2_con$iv == "scholars_treatments_2support",]$iv_formatted <- "Scholars support"
temp_df_2_con[temp_df_2_con$iv == "scholars_treatments_2split",]$iv_formatted <- "Scholars split"
temp_df_2_con[temp_df_2_con$iv == "scholars_treatments_2oppose",]$iv_formatted <- "Scholars oppose"

temp_df_2_con$ivfactor<-as.factor(temp_df_2_con$iv_formatted) #reordering 

#combined df
combined_df<-rbind(temp_df_1_mod,temp_df_1_lib,temp_df_1_con,temp_df_2_mod,temp_df_2_lib,temp_df_2_con)


combined_df$ideology<- fct_relevel(combined_df$ideology, "Conservative", "Moderate", "Liberal")
combined_df$iv_formatted<- fct_relevel(combined_df$iv_formatted, "Scholars oppose", "Scholars split", "Scholars support")


combined_df<-mutate(combined_df,iv_formatted=recode(iv_formatted,"Scholars oppose" = "Oppose",
                                                    "Scholars split"= "Split",
                                                    "Scholars support" = "Support"))

Fig_5_backlash <- ggplot(data=combined_df, aes(x=ideology, shape=Sample)) + 
  geom_hline(yintercept=0,color="#DDDDDD", size=.5) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95, width=0, color=Sample), size=.5,position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0,  color=Sample), size=.9,position=position_dodge(width=0.5)) + 
  geom_point(aes(y=Estimate, color=Sample), size=2.2, position=position_dodge(width=0.5)) + 
  facet_grid(~iv_formatted) +
  xlab("Treatment\ncondition") +
  ylab("Estimated treatment effect") +
  coord_flip() +
  scale_color_manual(values =c("#E1AF00","#3B9AB2")) +
  plot_theme + theme(legend.position = 'bottom', legend.key = element_rect(colour = "transparent", fill = "transparent"))
Fig_5_backlash

ggsave("Figures/Figure5.eps", Fig_5_backlash,dpi=300, width=6.5, height=4)

#``In Figure 5, we see that support for COP21 among liberals in the MTurk sample declined in the face of the support treatment. The average level of perceived support among scholars of climate science for the COP21 agreement among our liberal MTurk respondents was about 90 percent, suggesting that the support treatment—which put support for COP21 among climate scholars at around 85 percent—may have caused a significant portion of ideological liberals to update their perceptions about support for COP21 among climate scholars in a negative direction.''
mean(data[data$scholars_treatments_2=="control" & data$mturk==1 & data$ideology <= 3,]$cs_support, na.rm=TRUE)

#``The average level of support for COP21 among conservatives assigned to the control condition was 3.78 in our SSI sample and 3.67 in our Mturk sample on our seven-point scale. This is about a point lower than moderates and more than 2.5 points lower than liberals in both samples.''
mean(data[data$scholars_treatments_2=="control" & data$mturk==0 & data$ideology >= 5,]$cop21_dv, na.rm=TRUE)
mean(data[data$scholars_treatments_2=="control" & data$mturk==1 & data$ideology >= 5,]$cop21_dv, na.rm=TRUE)

#``This is about a point lower than moderates and more than two points lower than liberals in both samples.''
#moderates
mean(data[data$scholars_treatments_2=="control" & data$mturk==0 & data$ideology == 4,]$cop21_dv, na.rm=TRUE)
mean(data[data$scholars_treatments_2=="control" & data$mturk==1 & data$ideology == 4,]$cop21_dv, na.rm=TRUE)
#liberals
mean(data[data$scholars_treatments_2=="control" & data$mturk==0 & data$ideology <= 3,]$cop21_dv, na.rm=TRUE)
mean(data[data$scholars_treatments_2=="control" & data$mturk==1 & data$ideology <= 3,]$cop21_dv, na.rm=TRUE)

#6. Figure 6
#Estimated effect of scholar type and level of support on COP21 approval among respondents.

#6a. CS treatment
support_cs <- lm(cop21_dv~scholars_treatments_2, data=data[data$expert_type_2=="climate science" & data$mturk==0,])
# ``For the SSI sample, respondents in the climate scholars oppose condition were about 0.66 points less supportive of the COP21 climate agreement than those in the control condition, while those in the climate scholars support condition were about 0.37 points more supportive of the COP21 climate agreement compared to those in the control condition.''
summary(support_cs)
support_cs_df <- as.data.frame(coef(summary(support_cs)))
support_cs_df$iv <- rownames(support_cs_df)
support_cs_df$upper95 <- support_cs_df$Estimate + 1.96*support_cs_df$"Std. Error"
support_cs_df$lower95 <- support_cs_df$Estimate - 1.96*support_cs_df$"Std. Error"
support_cs_df$upper90 <- support_cs_df$Estimate + 1.645*support_cs_df$"Std. Error"
support_cs_df$lower90 <- support_cs_df$Estimate - 1.645*support_cs_df$"Std. Error"
support_cs_df$scholars <- "Climate Science"
support_cs_df$level <- ""
support_cs_df$Sample <- "SSI"
support_cs_df[support_cs_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_cs_df[support_cs_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_cs_df[support_cs_df$iv == "scholars_treatments_2support",]$level <- "Support"
#mturk
support_cs <- lm(cop21_dv~scholars_treatments_2, data=data[data$expert_type_2=="climate science" & data$mturk==1,])
#``In the MTurk sample, respondents in the climate scholars oppose condition were about 1.17 points less supportive of the agreement than those in the control, while those in the climate scholars support condition were about 0.43 points more supportive. ''
summary(support_cs)

support_cs_df2 <- as.data.frame(coef(summary(support_cs)))
support_cs_df2$iv <- rownames(support_cs_df2)
support_cs_df2$upper95 <- support_cs_df2$Estimate + 1.96*support_cs_df2$"Std. Error"
support_cs_df2$lower95 <- support_cs_df2$Estimate - 1.96*support_cs_df2$"Std. Error"
support_cs_df2$upper90 <- support_cs_df2$Estimate + 1.645*support_cs_df2$"Std. Error"
support_cs_df2$lower90 <- support_cs_df2$Estimate - 1.645*support_cs_df2$"Std. Error"
support_cs_df2$scholars <- "Climate Science"
support_cs_df2$level <- ""
support_cs_df2$Sample <- "MTurk"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2support",]$level <- "Support"


support_cs <- rbind(support_cs_df, support_cs_df2)

#6b. IR treatment
support_ir <- lm(cop21_dv~scholars_treatments_2, data=data[data$expert_type_2=="international relations" & data$mturk==0,])
support_ir_df <- as.data.frame(coef(summary(support_ir)))
support_ir_df$iv <- rownames(support_ir_df)
support_ir_df$upper95 <- support_ir_df$Estimate + 1.96*support_ir_df$"Std. Error"
support_ir_df$lower95 <- support_ir_df$Estimate - 1.96*support_ir_df$"Std. Error"
support_ir_df$upper90 <- support_ir_df$Estimate + 1.645*support_ir_df$"Std. Error"
support_ir_df$lower90 <- support_ir_df$Estimate - 1.645*support_ir_df$"Std. Error"
support_ir_df$scholars <- "International Relations"
support_ir_df$level <- ""
support_ir_df$Sample <- "SSI"
support_ir_df[support_ir_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ir_df[support_ir_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_ir_df[support_ir_df$iv == "scholars_treatments_2support",]$level <- "Support"
#mturk 
support_ir <- lm(cop21_dv~scholars_treatments_2, data=data[data$expert_type_2=="international relations" & data$mturk==1,])
support_ir_df2 <- as.data.frame(coef(summary(support_ir)))
support_ir_df2$iv <- rownames(support_ir_df)
support_ir_df2$upper95 <- support_ir_df2$Estimate + 1.96*support_ir_df2$"Std. Error"
support_ir_df2$lower95 <- support_ir_df2$Estimate - 1.96*support_ir_df2$"Std. Error"
support_ir_df2$upper90 <- support_ir_df2$Estimate + 1.645*support_ir_df2$"Std. Error"
support_ir_df2$lower90 <- support_ir_df2$Estimate - 1.645*support_ir_df2$"Std. Error"
support_ir_df2$scholars <- "International Relations"
support_ir_df2$level <- ""
support_ir_df2$Sample <- "MTurk"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2support",]$level <- "Support"

support_ir <- rbind(support_ir_df, support_ir_df2)

#6c. IE treatment
support_ie <- lm(cop21_dv~scholars_treatments_2, data=data[data$expert_type_2=="international economics" & data$mturk==0,])
support_ie_df <- as.data.frame(coef(summary(support_ie)))
support_ie_df$iv <- rownames(support_ie_df)
support_ie_df$upper95 <- support_ie_df$Estimate + 1.96*support_ie_df$"Std. Error"
support_ie_df$lower95 <- support_ie_df$Estimate - 1.96*support_ie_df$"Std. Error"
support_ie_df$upper90 <- support_ie_df$Estimate + 1.645*support_ie_df$"Std. Error"
support_ie_df$lower90 <- support_ie_df$Estimate - 1.645*support_ie_df$"Std. Error"
support_ie_df$scholars <- "International Economics"
support_ie_df$level <- ""
support_ie_df$Sample <- "SSI"
support_ie_df[support_ie_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ie_df[support_ie_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_ie_df[support_ie_df$iv == "scholars_treatments_2support",]$level <- "Support"
#mturk
support_ie <- lm(cop21_dv~scholars_treatments_2, data=data[data$expert_type_2=="international economics" & data$mturk==1,])
support_ie_df2 <- as.data.frame(coef(summary(support_ie)))
support_ie_df2$iv <- rownames(support_ie_df2)
support_ie_df2$upper95 <- support_ie_df2$Estimate + 1.96*support_ie_df2$"Std. Error"
support_ie_df2$lower95 <- support_ie_df2$Estimate - 1.96*support_ie_df2$"Std. Error"
support_ie_df2$upper90 <- support_ie_df2$Estimate + 1.645*support_ie_df2$"Std. Error"
support_ie_df2$lower90 <- support_ie_df2$Estimate - 1.645*support_ie_df2$"Std. Error"
support_ie_df2$scholars <- "International Economics"
support_ie_df2$level <- ""
support_ie_df2$Sample <- "MTurk"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2support",]$level <- "Support"

support_ie <- rbind(support_ie_df, support_ie_df2)

support_scholar_type <- rbind(support_cs, support_ir, support_ie)
support_scholar_type <- subset(support_scholar_type, iv!="(Intercept)")

#6d. Figure
scholar_type_results_both_plot <- ggplot(data=support_scholar_type, aes(x=scholars, y=Estimate, shape=Sample)) + 
  geom_hline(yintercept=0, color="#DDDDDD", size=.5) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95, width=0, color=Sample), size=.5, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0, color=Sample), size=.8, position=position_dodge(width=0.5)) + 
  geom_point(aes(y=Estimate, color=Sample), size=1.75, position=position_dodge(width=0.5)) + 
  xlab("Treatment\n condition") +
  ylab("Estimated treatment effect") +
  scale_color_manual(values =c("#E1AF00", "#3B9AB2")) +
  coord_flip() +
  facet_grid(level~.) +
  plot_theme + theme(legend.position = "bottom", legend.key = element_rect(colour = "transparent", fill = "transparent"))
scholar_type_results_both_plot

ggsave("Figures/Figure6.eps", scholar_type_results_both_plot, width=7, height=4,dpi=300)


#T-tests

#``For the SSI sample, ...  Respectively, this is about a 12.55 percentage point decline (t = 2.96, p < .003) and an 8 percentage point increase (t = 2.226, p = .0265) in respondents reporting any support for joining COP21. "

SSI_scholar_type_oppose<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="oppose"&data$mturk==0&data$expert_type_2=="climate science"],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==0&data$expert_type_2=="climate science"])
SSI_scholar_type_oppose
SSI_scholar_type_oppose$estimate[1] - SSI_scholar_type_oppose$estimate[2]
SSI_scholar_type_support<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="support"&data$mturk==0&data$expert_type_2=="climate science"],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==0&data$expert_type_2=="climate science"])
SSI_scholar_type_support
SSI_scholar_type_support$estimate[1] - SSI_scholar_type_support$estimate[2]

#``In the MTurk sample, respondents in the climate scholars oppose condition were about 1.17 points less supportive of the agreement than those in the control, while those in the climate scholars support condition were about 0.43 points more supportive. This results in a 15.7 percentage point decrease (t =2.41, p < .0169) and a 9.96 percentage increase (t =1.883, p < .0614) in respondents who support the COP21 agreement, respectively. ''

mturk_scholar_type_oppose<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="oppose"&data$mturk==1&data$expert_type_2=="climate science"],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==1&data$expert_type_2=="climate science"])
mturk_scholar_type_oppose
mturk_scholar_type_oppose$estimate[1] - mturk_scholar_type_oppose$estimate[2]


mturk_scholar_type_support<-t.test(data$support_cop21_dv[data$scholars_treatments_2=="support"&data$mturk==1&data$expert_type_2=="climate science"],data$support_cop21_dv[data$scholars_treatments_2=="control"&data$mturk==1&data$expert_type_2=="climate science"])
mturk_scholar_type_support
mturk_scholar_type_support$estimate[1] - mturk_scholar_type_support$estimate[2]


#Figure 7
#Perceived knowledge of climate change by knowledge elite grouping
# We use SSI because it is closer to represenative sample of US. Mturk shows even more skew in favor of climate scholars. 
data_for_analysis_ssi <- data[data$mturk==0,]
temp_df <- data_for_analysis_ssi[,c("knowledge_cs", "knowledge_j", "knowledge_ir", "knowledge_mc", "knowledge_ie")]
temp_df <- na.omit(temp_df)
temp_df$id <-  as.numeric(rownames(temp_df))
temp_df <- reshape2::melt(temp_df, id="id")

temp_df$facet[temp_df$variable == "knowledge_cs"] <- "Climate Scholars" 
temp_df$facet[temp_df$variable == "knowledge_ir"] <- "IR Scholars" 
temp_df$facet[temp_df$variable == "knowledge_ie"] <- "IE Scholars" 
temp_df$facet[temp_df$variable == "knowledge_mc"] <- "Members of Congress" 
temp_df$facet[temp_df$variable == "knowledge_j"] <- "Journalists" 
temp_df$value <- as.numeric(temp_df$value)

temp_df <- temp_df %>% group_by(facet)

knowledge_plot_SSI <- ggplot(data=temp_df, aes(x=value)) +geom_bar(aes(y = ..prop..),  fill="#003366",stat="count") + 
  facet_wrap(~facet) + 
  plot_theme + 
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(limits=c(1,2,3,4,5),breaks=c(1,2,3,4,5),labels=rev(c("A lot\nmore","","About\nas much","","A lot\nless"))) + 
  xlab("Perceived knowledge of the COP21 Climate Agreement relative to average Americans") + 
  ylab("Percent")
knowledge_plot_SSI

ggsave("Figures/Figure7.eps", knowledge_plot_SSI, width=7, height=3.5,dpi=300)

#``The average level of perceived scholarly knowledge is 3.60 on our five point scale (95% CI: 3.57, 3.64). This is over a point higher than would be expected if respondents perceived scholars as having the same level of knowledge about COP21 as the average American. Further, average perceptions of scholarly knowledge of COP21 are .43 points higher (t = 21.005, p < .000) than perceptions of the level of knowledge of members of Congress and .41 points higher (t =23.153, p < .000) than perceptions of the level of knowledge of journalists''

#average scholarly knowledge
avg_scholarly_know <- data %>%
  filter(mturk == 0) %>%
  summarise(mean = mean(avg_scholar_knowledge, na.rm = TRUE),
            sd = sd(avg_scholar_knowledge, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
avg_scholarly_know

#avg compared to mcs
knowledge_avg_know<-t.test(data[data$mturk == 0,]$avg_scholar_knowledge,data[data$mturk == 0,]$knowledge_mc, paired=TRUE)
knowledge_avg_know

#avg compared to journalists 
knowledge_avg_know<-t.test(data[data$mturk == 0,]$avg_scholar_knowledge,data[data$mturk == 0,]$knowledge_j, paired=TRUE)
knowledge_avg_know



#``On our five point scale, climate scholars received an average score of 3.92 (95% CI: 3.88, 3.98). This was .53 points higher than the average score for scholars of International Economics (t =16.93, p < .000) and .46 points higher than scholars of IR (t = 14.8, p < .000). ''


# ``On our five point scale, climate scholars received an average score of 3.92 (95% CI: 3.88, 3.98).''
cs_know <- data %>%
  filter(mturk == 0) %>%
  summarise(mean = mean(knowledge_cs, na.rm = TRUE),
            sd = sd(knowledge_cs, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
cs_know

#cs compared to econ
knowledge_t_test_3<-t.test(data[data$mturk == 0,]$knowledge_cs,data[data$mturk == 0,]$knowledge_ie, paired=TRUE)
knowledge_t_test_3

#cs compared to IR
knowledge_t_test_4<-t.test(data[data$mturk == 0,]$knowledge_cs,data[data$mturk == 0,]$knowledge_ir, paired=TRUE)
knowledge_t_test_4

#Figure 8
#Estimated effect of treatment on expectations that the COP21 agreement will benefit the U.S. by scholar type

#8a. CS results
#ssi
support_cs <- lm(agreement_fx~scholars_treatments_2, data=data[data$expert_type_2=="climate science" & data$mturk==0,])
support_cs_df <- as.data.frame(coef(summary(support_cs)))
support_cs_df$iv <- rownames(support_cs_df)
support_cs_df$upper95 <- support_cs_df$Estimate + 1.96*support_cs_df$"Std. Error"
support_cs_df$lower95 <- support_cs_df$Estimate - 1.96*support_cs_df$"Std. Error"
support_cs_df$upper90 <- support_cs_df$Estimate + 1.645*support_cs_df$"Std. Error"
support_cs_df$lower90 <- support_cs_df$Estimate - 1.645*support_cs_df$"Std. Error"
support_cs_df$scholars <- "Climate Science"
support_cs_df$level <- ""
support_cs_df$Sample <- "SSI"
support_cs_df[support_cs_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_cs_df[support_cs_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_cs_df[support_cs_df$iv == "scholars_treatments_2support",]$level <- "Support"
#mturk
support_cs <- lm(agreement_fx~scholars_treatments_2, data=data[data$expert_type_2=="climate science" & data$mturk==1,])
support_cs_df2 <- as.data.frame(coef(summary(support_cs)))
support_cs_df2$iv <- rownames(support_cs_df2)
support_cs_df2$upper95 <- support_cs_df2$Estimate + 1.96*support_cs_df2$"Std. Error"
support_cs_df2$lower95 <- support_cs_df2$Estimate - 1.96*support_cs_df2$"Std. Error"
support_cs_df2$upper90 <- support_cs_df2$Estimate + 1.645*support_cs_df2$"Std. Error"
support_cs_df2$lower90 <- support_cs_df2$Estimate - 1.645*support_cs_df2$"Std. Error"
support_cs_df2$scholars <- "Climate Science"
support_cs_df2$level <- ""
support_cs_df2$Sample <- "MTurk"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_cs_df2[support_cs_df2$iv == "scholars_treatments_2support",]$level <- "Support"

support_cs <- rbind(support_cs_df, support_cs_df2)

#8b. IR results
#SSI
support_ir <- lm(agreement_fx~scholars_treatments_2, data=data[data$expert_type_2=="international relations" & data$mturk==0,])
support_ir_df <- as.data.frame(coef(summary(support_ir)))
support_ir_df$iv <- rownames(support_ir_df)
support_ir_df$upper95 <- support_ir_df$Estimate + 1.96*support_ir_df$"Std. Error"
support_ir_df$lower95 <- support_ir_df$Estimate - 1.96*support_ir_df$"Std. Error"
support_ir_df$upper90 <- support_ir_df$Estimate + 1.645*support_ir_df$"Std. Error"
support_ir_df$lower90 <- support_ir_df$Estimate - 1.645*support_ir_df$"Std. Error"
support_ir_df$scholars <- "International Relations"
support_ir_df$level <- ""
support_ir_df$Sample <- "SSI"
support_ir_df[support_ir_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ir_df[support_ir_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_ir_df[support_ir_df$iv == "scholars_treatments_2support",]$level <- "Support"
#mturk 
support_ir <- lm(agreement_fx~scholars_treatments_2, data=data[data$expert_type_2=="international relations" & data$mturk==1,])
support_ir_df2 <- as.data.frame(coef(summary(support_ir)))
support_ir_df2$iv <- rownames(support_ir_df)
support_ir_df2$upper95 <- support_ir_df2$Estimate + 1.96*support_ir_df2$"Std. Error"
support_ir_df2$lower95 <- support_ir_df2$Estimate - 1.96*support_ir_df2$"Std. Error"
support_ir_df2$upper90 <- support_ir_df2$Estimate + 1.645*support_ir_df2$"Std. Error"
support_ir_df2$lower90 <- support_ir_df2$Estimate - 1.645*support_ir_df2$"Std. Error"
support_ir_df2$scholars <- "International Relations"
support_ir_df2$level <- ""
support_ir_df2$Sample <- "MTurk"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_ir_df2[support_ir_df2$iv == "scholars_treatments_2support",]$level <- "Support"

support_ir <- rbind(support_ir_df, support_ir_df2)

#8c. IE results
#ssi
support_ie <- lm(agreement_fx~scholars_treatments_2, data=data[data$expert_type_2=="international economics" & data$mturk==0,])
support_ie_df <- as.data.frame(coef(summary(support_ie)))
support_ie_df$iv <- rownames(support_ie_df)
support_ie_df$upper95 <- support_ie_df$Estimate + 1.96*support_ie_df$"Std. Error"
support_ie_df$lower95 <- support_ie_df$Estimate - 1.96*support_ie_df$"Std. Error"
support_ie_df$upper90 <- support_ie_df$Estimate + 1.645*support_ie_df$"Std. Error"
support_ie_df$lower90 <- support_ie_df$Estimate - 1.645*support_ie_df$"Std. Error"
support_ie_df$scholars <- "International Economics"
support_ie_df$level <- ""
support_ie_df$Sample <- "SSI"
support_ie_df[support_ie_df$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ie_df[support_ie_df$iv == "scholars_treatments_2split",]$level <- "Split"
support_ie_df[support_ie_df$iv == "scholars_treatments_2support",]$level <- "Support"

#mturk
support_ie <- lm(agreement_fx~scholars_treatments_2, data=data[data$expert_type_2=="international economics" & data$mturk==1,])
support_ie_df2 <- as.data.frame(coef(summary(support_ie)))
support_ie_df2$iv <- rownames(support_ie_df2)
support_ie_df2$upper95 <- support_ie_df2$Estimate + 1.96*support_ie_df2$"Std. Error"
support_ie_df2$lower95 <- support_ie_df2$Estimate - 1.96*support_ie_df2$"Std. Error"
support_ie_df2$upper90 <- support_ie_df2$Estimate + 1.645*support_ie_df2$"Std. Error"
support_ie_df2$lower90 <- support_ie_df2$Estimate - 1.645*support_ie_df2$"Std. Error"
support_ie_df2$scholars <- "International Economics"
support_ie_df2$level <- ""
support_ie_df2$Sample <- "MTurk"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2oppose",]$level <- "Oppose"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2split",]$level <- "Split"
support_ie_df2[support_ie_df2$iv == "scholars_treatments_2support",]$level <- "Support"

support_ie <- rbind(support_ie_df, support_ie_df2)

support_scholar_type <- rbind(support_cs, support_ir, support_ie)
support_scholar_type <- subset(support_scholar_type, iv!="(Intercept)")


scholar_type_results_both_plot_help_hurt <- ggplot(data=support_scholar_type, aes(x=scholars, y=Estimate, shape=Sample)) + 
  geom_hline(yintercept=0, color="#DDDDDD", size=.5) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95, width=0, color=Sample), size=.5, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0, color=Sample), size=.8, position=position_dodge(width=0.5)) + 
  geom_point(aes(y=Estimate, color=Sample), size=1.75, position=position_dodge(width=0.5)) + 
  xlab("Treatment\n condition") +
  ylab("Estimated treatment effect") +
  scale_color_manual(values =c("#E1AF00", "#3B9AB2")) +
  coord_flip() +
  facet_grid(level~.) +
  plot_theme + theme(legend.position = "bottom", legend.key = element_rect(colour = "transparent", fill = "transparent"))
scholar_type_results_both_plot_help_hurt

ggsave("Figures/Figure8.eps", scholar_type_results_both_plot_help_hurt, width=7, height=4.5,dpi=300)




#Appendix Table 3
#Matching Analysis

df_for_match <- na.omit(dplyr::select(data, mturk, edu_new3, gender, famincom, age, ideology, cop21_dv, en_scholars_treatments_2))

#match on: gender, ideo, age, edu. 
set.seed(1985)
match.it <- matchit(mturk ~ edu_new3 + gender + famincom + age , data = df_for_match, method="nearest", ratio=1)
summary(match.it)
m.data <- match.data(match.it)


mturk_trimmed <- lm(cop21_dv~en_scholars_treatments_2, data=m.data[m.data$mturk==1,])
ssi_trimmed <- lm(cop21_dv~en_scholars_treatments_2, data=m.data[m.data$mturk==0,])
#Table 3:
summary(mturk_trimmed)
summary(ssi_trimmed)


stargazer(mturk_trimmed,ssi_trimmed,type="latex",column.labels = c("mTurk","SSI"), 
          covariate.labels = c("Scholars oppose", "Scholars split", "Scholars support"), dep.var.labels = c("Support for COP21"), out="Tables/table3.tex")


