######################
#Replication data for "Can Increasing Awareness of Gender Gaps in International Relations Help Close Them? Evidence from a Scholar Ranking Experiment"


######################
#Preamble
######################

#Clear workspace
rm(list=ls())

#The pacman package is required to load/install the additional packages. Uncomment the line below to install the package.
#install.packages("pacman")

#Installing and loading packages for use throughout the code.
pacman::p_load("wesanderson",tidyverse,xtable,stargazer,modelsummary)

#Creating functions for use throughout the code. 

#This function enables quick data summarizing and grouping
pctgroup <- function(data,...,rd = 1) {
  select(data,...) %>%
    filter(complete.cases(.)) %>%
    group_by(...,.drop=FALSE) %>%
    summarise(n = n()) %>%
    mutate(Percentage = round(100*n/sum(n),rd)) %>%
    ungroup %>%
    complete(...,fill=list(n=0,Percentage=0))
}

#This function enables quick latex exporting of tables

printtable <- function(data,caption,digits = NULL) {
  print(xtable(data,auto=TRUE,digits = 2, caption =caption),caption.placement = 'bottom',scalebox = ".8",digits=digits)
}


#Creating a ggplot theme for use in several figures throughout the code.
plot_theme = theme(text = element_text(size=12),  
                   panel.grid.major.y = element_blank(), 
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.y = element_blank(), 
                   panel.grid.minor.x = element_blank(), 
                   panel.background = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   axis.text = element_text(color="black"),
                   axis.line = element_line(colour = "black"),
                   strip.text.y = element_text(angle = 0), axis.title.y = element_text(angle=0, hjust=.5, vjust=.5),
                   axis.title = element_text(lineheight=1.1), 
                   legend.position = 'none',
                   panel.grid.minor = element_blank()
) 


#Loading in US scholar data
scholar_ranks<-read.csv("scholar_ranks_USA.csv",na.strings=c("", "NA"), header = TRUE)
FS2017<-read.csv("FS2017.csv",na.strings=c("", "NA"), header = TRUE)

#Loading in intl scholar data
scholar_ranks_intl<-read.csv("scholar_ranks_intl.csv",na.strings=c("", "NA"), header = TRUE)
FS2017_intl<-read.csv("FS2017_intl.csv",na.strings=c("", "NA"), header = TRUE)

# 1. Attitudes Towards Gender Balance Analysis ----------------------------------------------------------------

#Figure 1: Do you approve of this citation policy?

genderpolicy_1_all<-select(FS2017,ResponseId,genderpolicy_Response,gender) %>%
 dplyr::distinct(ResponseId,.keep_all=TRUE) %>% 
  gather(Question,Response,genderpolicy_Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  pctgroup(Question,Response)%>%
  arrange(desc(Percentage))
genderpolicy_1_all$Response <- factor(genderpolicy_1_all$Response, c("Don't know","Disapprove","Approve"))
genderpolicy_1_all$gender<-"All"
genderpolicy_1_all$group<-"US"


genderpolicy_1_all_intl<-select(FS2017_intl,ResponseId,genderpolicy_Response,gender) %>%
 dplyr::distinct(ResponseId,.keep_all=TRUE) %>% 
  gather(Question,Response,genderpolicy_Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  pctgroup(Question,Response)%>%
  arrange(desc(Percentage))

genderpolicy_1_all_intl$Response <- factor(genderpolicy_1_all_intl$Response, c("Don't know","Disapprove","Approve"))
genderpolicy_1_all_intl$gender<-"All"
genderpolicy_1_all_intl$group<-"Non-US"


#Combining US and International Results into one graphic with a facet for region
Fig1_data<-rbind(genderpolicy_1_all_intl,genderpolicy_1_all)

#Plotting Figure 1
Fig1 <- ggplot(Fig1_data, aes(x=Response, y=Percentage, fill=Response)) +
  geom_bar(stat="identity")+
  facet_grid(group ~ .) + plot_theme + coord_flip() + xlab("") +  scale_fill_manual(values=(wes_palette("Zissou1"))) +theme(axis.ticks.y=element_blank(),legend.position="none", axis.text.y = element_text(angle=0,size=7,margin=margin(0)))+
  scale_y_continuous(limits=c(-1,70),expand=c(0,0),breaks=seq(0,70,10)) +
  guides(fill = guide_legend(reverse=TRUE))+
  geom_text(aes(label = Percentage),position = position_dodge(width = 1),hjust =-.05,size = 3.5) 

Fig1

#ggsave("Fig1.png", Fig1,width=6,height=2,dpi=600)

#Figure 2: Do you approve of this gender citation policy? Broken down by gender
#US response by gender
genderpolicy_1_gender<-select(FS2017,genderpolicy_Response,gender) %>%
  gather(Question,Response,genderpolicy_Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(gender = recode(gender,"m" = "Male",
                         "f" = "Female")) %>%
  pctgroup(Question,gender,Response)%>%
  arrange(desc(Percentage))

genderpolicy_1_gender$Response <- factor(genderpolicy_1_gender$Response, c("Don't know","Disapprove","Approve"))

genderpolicy_1_gender$group<-"US"
#International response by gender

genderpolicy_1_intl_gender<-select(FS2017_intl,genderpolicy_Response,gender) %>%
  gather(Question,Response,genderpolicy_Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(gender = recode(gender,"m" = "Male",
                         "f" = "Female")) %>%
  pctgroup(Question,gender,Response)%>%
  arrange(desc(Percentage))

genderpolicy_1_intl_gender$Response <- factor(genderpolicy_1_intl_gender$Response, c("Don't know","Disapprove","Approve"))

genderpolicy_1_intl_gender$group<-"Non-US"

#Combining US and International Results into one graphic with a facet for region and gender
fig2_data<-rbind(genderpolicy_1_gender,genderpolicy_1_intl_gender)

#Figure 2: Do you approve of this gender citation policy? Broken down by gender
Fig2 <- ggplot(fig2_data, aes(x=Response, y=Percentage, fill=Response)) +
  geom_bar(stat="identity")+
  facet_grid(gender ~ group) + plot_theme + coord_flip() + xlab("") +  scale_fill_manual(values=(wes_palette("Zissou1"))) +theme(axis.ticks.y=element_blank(),legend.position="none", axis.text.y = element_text(angle=0,size=7,margin=margin(0)))+
  scale_y_continuous(limits=c(-1,90),expand=c(0,0),breaks=seq(0,80,10)) +
  guides(fill = guide_legend(reverse=TRUE))+
  geom_text(aes(label = Percentage),position = position_dodge(width = 1),hjust =-.05,size = 3.5) 

Fig2

#ggsave("Fig2.png", Fig2,width=8.5,height=4,dpi=600)

#Figure 3: If you received such a reminder from a journal editor in the review process this would. 

genderpolicy_2_all<-select(FS2017,genderpolicy_2__Response,gender) %>%
  gather(Question,Response,genderpolicy_2__Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(Response = recode(Response,"Change my behavior by causing me to cite fewer women"="Change my behavior by causing me to cite fewer women",
                           "Change my behavior by causing me to cite more women"="Change my behavior by causing me to cite more women",
                           "Have no impact on my behavior because I already make a special effort to cite underrepresented individuals"="Have no impact, I already\nmake a special effort to cite\nunderrepresented individuals",
                           "Have no impact on my behavior even though I currently do not make a special effort to cite underrepresented individuals"="Have no impact, even though I\ncurrently do not make a special effort\nto cite underrepresented individuals")) %>%
  pctgroup(Question,Response)%>%
  arrange(desc(Percentage))
genderpolicy_2_all$gender<-"All"


genderpolicy_2_gender<-select(FS2017,genderpolicy_2__Response,gender) %>%
  gather(Question,Response,genderpolicy_2__Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(Response = recode(Response,"Change my behavior by causing me to cite fewer women"="Change my behavior by causing me to cite fewer women",
                           "Change my behavior by causing me to cite more women"="Change my behavior by causing me to cite more women",
                           "Have no impact on my behavior because I already make a special effort to cite underrepresented individuals"="Have no impact, I already\nmake a special effort to cite\nunderrepresented individuals",
                           "Have no impact on my behavior even though I currently do not make a special effort to cite underrepresented individuals"="Have no impact, even though I\ncurrently do not make a special effort\nto cite underrepresented individuals")) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(gender = recode(gender,"m" = "Male",
                         "f" = "Female")) %>%
  pctgroup(Question,gender,Response)%>%
  arrange(desc(Percentage))

facet_gender_2<-rbind(genderpolicy_2_all,genderpolicy_2_gender)
facet_gender_2$group<-"US"

genderpolicy_2_all_intl<-select(FS2017_intl,genderpolicy_2__Response,gender) %>%
  gather(Question,Response,genderpolicy_2__Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(Response = recode(Response,"Change my behavior causing me to cite fewer women"="Change my behavior by causing me to cite fewer women",
                           "Change my behavior causing me to cite more women"="Change my behavior by causing me to cite more women",
                           "Have no impact on my behavior because I already make a special effort to cite underrepresented individuals"="Have no impact, I already\nmake a special effort to cite\nunderrepresented individuals",
                           "Have no impact on my behavior even though I currently do not make a special effort to cite underrepresented individuals"="Have no impact, even though I\ncurrently do not make a special effort\nto cite underrepresented individuals")) %>%
  pctgroup(Question,Response)%>%
  arrange(desc(Percentage))

genderpolicy_2_all_intl$gender<-"All"


genderpolicy_2_gender_intl<-select(FS2017_intl,genderpolicy_2__Response,gender) %>%
  gather(Question,Response,genderpolicy_2__Response) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(Response = recode(Response,"Change my behavior causing me to cite fewer women"="Change my behavior by causing me to cite fewer women",
                           "Change my behavior causing me to cite more women"="Change my behavior by causing me to cite more women",
                           "Have no impact on my behavior because I already make a special effort to cite underrepresented individuals"="Have no impact, I already\nmake a special effort to cite\nunderrepresented individuals",
                           "Have no impact on my behavior even though I currently do not make a special effort to cite underrepresented individuals"="Have no impact, even though I\ncurrently do not make a special effort\nto cite underrepresented individuals")) %>%
  filter(gender!="Prefer not to answer") %>% 
  mutate(gender = recode(gender,"m" = "Male",
                         "f" = "Female")) %>%
  pctgroup(Question,gender,Response)%>%
  arrange(desc(Percentage))

#Binding together
facet_gender_2_intl<-rbind(genderpolicy_2_all_intl,genderpolicy_2_gender_intl)
facet_gender_2_intl$group<-"Non-US"


#Figure 3: If you received such a reminder from a journal editor in the review process this would. 

#Combining US and International Results into one graphic with a facet for region and gender

Fig3_data<-rbind(facet_gender_2_intl,facet_gender_2)

Fig3<-ggplot(Fig3_data, aes(x=Response, y=Percentage, fill=Response)) +
  geom_bar(stat="identity") + plot_theme + coord_flip() + xlab("") +
  facet_grid(gender ~ group)+
  scale_fill_manual(values=(wes_palette("Zissou1"))) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),legend.position="none", axis.text.y = element_text(angle=0,size=7,margin=margin(0)))+
  scale_y_continuous(limits=c(-.5,60),expand=c(0,0)) +
  guides(fill = guide_legend(reverse=TRUE))+
  geom_text(aes(label = Percentage),position = position_dodge(width = 1),hjust =-.15,size = 3)
Fig3


#ggsave("Fig3.png",Fig3,width=8,height=6,dpi=600)


# 2. Gender experiment Analysis ----------------------------------------------------------------
  #2a. US scholar results

#Checking demographic balance between treatments (US):
scholar_ranks_demos_us<-select(scholar_ranks,treatment,Gender=resp_gender,Subfield,paradigm,Rank) %>% 
  filter(treatment!="rank_outsideUS") %>% 
  mutate(treatment = recode(treatment,"rank_control" ="Control",
                            "rank_gender" ="Treatment")) %>% 
  mutate(Subfield = ifelse(Subfield %in% c("Human Rights", "International Organization(s)","International/Global Political Economy","International/Global Security", "U.S. Foreign Policy"),
                           Subfield,"Other")) %>%
  mutate(Gender = recode(Gender,"f" ="Female",
                         "m" ="Male")) %>% 
  filter(Gender!="Prefer not to answer") %>% 
  filter(Rank!="") %>% 
  filter(paradigm!="NA")

datasummary_balance_us<-datasummary_balance(~ treatment,data = select(scholar_ranks_demos_us,treatment,Gender,Subfield,Paradigm=paradigm,Rank),output="latex",title="Demographic Balance Table: U.S.")
datasummary_balance_us



#Table 1: Question Ordering Experiment Descriptive Results


#US 
d1_table <- separate(scholar_ranks,standardized,c("Choice1","Choice2","Choice3","Choice4"),sep = ",") %>%
  select(ResponseId,Choice1:Choice4,treatment,gender) %>%
  filter(gender!="")  %>% 
  gather(Question,Response,Choice1:Choice4) %>%
  group_by(treatment) %>%
  filter(!is.na(Response)) 

#n respondents per treatment
us_treatment_ns <- d1_table %>% group_by(ResponseId, treatment) %>% summarize(n=n()) %>% group_by(treatment) %>% summarize(n=n())

#Number of male/female names out of total among those assigned to the treatment condition
us_treatment_gender <- table(d1_table$gender[d1_table$treatment=="rank_gender"])
us_treatment_names <- length(d1_table$gender[d1_table$treatment=="rank_gender"])

#Unique female names in the treatment condition
us_treatment_unique_female <- length(unique(d1_table$Response[d1_table$treatment=="rank_gender"&d1_table$gender=="f"]))

#Number of male/female names out of total among those assigned to the control condition
us_control_gender <- table(d1_table$gender[d1_table$treatment=="rank_control"])
us_control_names <- length(d1_table$gender[d1_table$treatment=="rank_control"])

#Unique names in the control
us_control_unique_female <- length(unique(d1_table$Response[d1_table$treatment=="rank_control"&d1_table$gender=="f"]))


#Table 1: Question Ordering Experiment Descriptive Results (Non-US)

#Scholar ranks table -intl

d1_table_intl <- separate(scholar_ranks_intl,standardized,c("Choice1","Choice2","Choice3","Choice4"),sep = ",") %>%
  select(ResponseId,Choice1:Choice4,treatment,gender) %>%
  gather(Question,Response,Choice1:Choice4) %>%
  group_by(treatment) %>%
  filter(!is.na(Response)) 

#number of female and male responses and unique female responses 

#n respondents per treatment
intl_treatment_ns <- d1_table_intl %>% group_by(ResponseId, treatment) %>% summarize(n=n()) %>% group_by(treatment) %>% summarize(n=n())

#treatment
intl_treatment_gender <- table(d1_table_intl$gender[d1_table_intl$treatment=="rank_gender"])
intl_treatment_names <- length(d1_table_intl$gender[d1_table_intl$treatment=="rank_gender"])

#unique female names in treatment 
intl_treatment_unique_female <- length(unique(d1_table_intl$Response[d1_table_intl$treatment=="rank_gender"&d1_table_intl$gender=="f"]))

#control
intl_control_gender <- table(d1_table_intl$gender[d1_table_intl$treatment=="rank_control"])
intl_control_names <- length(d1_table_intl$gender[d1_table_intl$treatment=="rank_control"])

#unique female names in control
intl_control_unique_female <- length(unique(d1_table_intl$Response[d1_table_intl$treatment=="rank_control"&d1_table_intl$gender=="f"]))

#Building Table 1

#generate label vectors
sample <- c("US", "US", "Intl", "Intl")
manip_labels <- c("Treatment", "Control", "Treatment", "Control")
#get respondents per treatment
resp_by_treat <- rbind(us_treatment_ns[2,2], us_treatment_ns[1,2], intl_treatment_ns[2,2], intl_treatment_ns[1,2] )
#number of names by treatment 
names <- rbind(us_treatment_names[1],  us_control_names[1], intl_treatment_names[1],  intl_control_names[1])
#percent of names that are female
female_n <- rbind(us_treatment_gender[1], us_control_gender[1],intl_treatment_gender[1], intl_control_gender[1])  
#unique female names
unique_f <- rbind(us_treatment_unique_female, us_control_unique_female, intl_treatment_unique_female, intl_control_unique_female)

#build table 1
table1 <- cbind(sample, manip_labels, resp_by_treat, names, female_n)
table1 <- table1 %>% mutate(pct_female = round((female_n/names)*100, digits=1))
table1 <- cbind(table1, unique_f)  

table1

#Table 2: Overall Gender Experiment Results 

#1a. All US respondents 
gender_results<-scholar_ranks %>% 
  filter(treatment!="rank_outsideUS") %>% 
  group_by(ResponseId,gender) %>% 
  summarise(n=n()) %>% 
  group_by(ResponseId) %>% 
  mutate(sum=sum(n),per=n/sum*100) %>% 
  filter(gender!="")

gender_results$per[gender_results$gender=="m" & gender_results$per==100]<-0
gender_results$gender[gender_results$gender=="m" & gender_results$per==0]<-"f"

gender_results<-gender_results %>% filter(gender=="f") %>% 
  select(-n,-sum)

gender_results<-left_join(gender_results,scholar_ranks,"ResponseId") %>% 
  distinct(ResponseId,.keep_all=TRUE) 

all_respondents<-t.test(gender_results$per[gender_results$treatment=="rank_gender"],gender_results$per[gender_results$treatment=="rank_control"])


#1b. All intl respondents
gender_results_intl<-scholar_ranks_intl %>% group_by(ResponseId,gender) %>% 
  summarise(n=n()) %>% 
  group_by(ResponseId) %>% 
  mutate(sum=sum(n),per=n/sum*100)

gender_results_intl$per[gender_results_intl$gender=="m" & gender_results_intl$per==100]<-0
gender_results_intl$gender[gender_results_intl$gender=="m" & gender_results_intl$per==0]<-"f"
gender_results_intl<-gender_results_intl %>% filter(gender=="f") %>% select(-n,-sum)
gender_results_intl<-left_join(gender_results_intl,scholar_ranks_intl,"ResponseId") %>% 
  distinct(ResponseId,.keep_all=TRUE) 

all_respondents_intl<-t.test(gender_results_intl$per[gender_results_intl$treatment=="rank_gender"],gender_results_intl$per[gender_results_intl$treatment=="rank_control"])


#2a. Female respondents US
scholar_ranks_female<-scholar_ranks %>% 
  filter(resp_gender=="f")

gender_results_female<-scholar_ranks_female %>% group_by(ResponseId,gender) %>% 
  filter(treatment!="rank_outsideUS") %>% 
  summarise(n=n()) %>% 
  group_by(ResponseId) %>% 
  mutate(sum=sum(n),per=n/sum*100)%>% 
  filter(gender!="")

gender_results_female$per[gender_results_female$gender=="m" & gender_results_female$per==100]<-0
gender_results_female$gender[gender_results_female$gender=="m" & gender_results_female$per==0]<-"f"

gender_results_female<-gender_results_female %>% filter(gender=="f") %>% 
  select(-n,-sum)

gender_results_female<-left_join(gender_results_female,scholar_ranks,"ResponseId") %>% 
  distinct(ResponseId,.keep_all=TRUE) 

female_respondents<-t.test(gender_results_female$per[gender_results_female$treatment=="rank_gender"],gender_results_female$per[gender_results_female$treatment=="rank_control"])

#Confidence interval for female results
CI_female_US<-select(gender_results_female, per, treatment) %>%
  group_by(treatment) %>% 
  summarize(average=mean(per), sd=sd(per), n=n(), se=sd/sqrt(n)) %>% 
  mutate(upper = average+1.96*se, lower=average-1.96*se)

#2b. Female intl respondents

scholar_ranks_intl_female<-scholar_ranks_intl %>% 
  filter(resp_gender=="f")

gender_results_intl_FEMALE<-scholar_ranks_intl_female %>% group_by(ResponseId,gender) %>% 
  summarise(n=n()) %>% 
  group_by(ResponseId) %>% 
  mutate(sum=sum(n),per=n/sum*100)

gender_results_intl_FEMALE$per[gender_results_intl_FEMALE$gender=="m" & gender_results_intl_FEMALE$per==100]<-0
gender_results_intl_FEMALE$gender[gender_results_intl_FEMALE$gender=="m" & gender_results_intl_FEMALE$per==0]<-"f"
gender_results_intl_FEMALE<-gender_results_intl_FEMALE %>% filter(gender=="f") %>% select(-n,-sum)
gender_results_intl_FEMALE<-left_join(gender_results_intl_FEMALE,scholar_ranks_intl_female,"ResponseId") %>% 
  distinct(ResponseId,.keep_all=TRUE) 

Female_respondents_intl<-t.test(gender_results_intl_FEMALE$per[gender_results_intl_FEMALE$treatment=="rank_gender"],gender_results_intl_FEMALE$per[gender_results_intl_FEMALE$treatment=="rank_control"])


CI_female_intl<-dplyr::select(gender_results_intl_FEMALE, per, treatment) %>%
  filter(treatment!="rank_outsideUS") %>%
  group_by(treatment) %>% 
  summarize(average=mean(per), sd=sd(per), n=n(), se=sd/sqrt(n)) %>% 
  mutate(upper = average+1.96*se, lower=average-1.96*se)



#3a Male respondents for US
scholar_ranks_male<-scholar_ranks %>% 
  filter(resp_gender=="m")

gender_results_male<-scholar_ranks_male %>% group_by(ResponseId,gender) %>% 
  filter(treatment!="rank_outsideUS") %>% 
  summarise(n=n()) %>% 
  group_by(ResponseId) %>% 
  mutate(sum=sum(n),per=n/sum*100)%>% 
  filter(gender!="")

gender_results_male$per[gender_results_male$gender=="m" & gender_results_male$per==100]<-0
gender_results_male$gender[gender_results_male$gender=="m" & gender_results_male$per==0]<-"f"
gender_results_male<-gender_results_male %>% filter(gender=="f") %>% select(-n,-sum)
gender_results_male<-left_join(gender_results_male,scholar_ranks,"ResponseId") %>% distinct(ResponseId,.keep_all=TRUE) 

male_respondents<-t.test(gender_results_male$per[gender_results_male$treatment=="rank_gender"],gender_results_male$per[gender_results_male$treatment=="rank_control"])


#Experiment results t-test (Non_US)


#3b. Male intl respondents

scholar_ranks_intl_male<-scholar_ranks_intl %>% 
  filter(resp_gender=="m")

gender_results_intl_MALE<-scholar_ranks_intl_male %>% group_by(ResponseId,gender) %>% 
  summarise(n=n()) %>% 
  group_by(ResponseId) %>% 
  mutate(sum=sum(n),per=n/sum*100)

gender_results_intl_MALE$per[gender_results_intl_MALE$gender=="m" & gender_results_intl_MALE$per==100]<-0
gender_results_intl_MALE$gender[gender_results_intl_MALE$gender=="m" & gender_results_intl_MALE$per==0]<-"f"

gender_results_intl_MALE<-gender_results_intl_MALE %>% filter(gender=="f") %>% 
  select(-n,-sum)

gender_results_intl_MALE<-left_join(gender_results_intl_MALE,scholar_ranks_intl_male,"ResponseId") %>%
  distinct(ResponseId,.keep_all=TRUE) 

Male_respondents_intl<-t.test(gender_results_intl_MALE$per[gender_results_intl_MALE$treatment=="rank_gender"],gender_results_intl_MALE$per[gender_results_intl_MALE$treatment=="rank_control"])




#build table 2
t_row_labels <- c("Treatment","Control", "Difference of means", "P-value")

us_only <- c(sprintf("%.2f",all_respondents$estimate[1]), 
             sprintf("%.2f",all_respondents$estimate[2]), 
             paste0(sprintf("%.2f",all_respondents$estimate[1] - all_respondents$estimate[2]), " (95% CI:", sprintf("%.2f",all_respondents$conf.int[1]), ", ", sprintf("%.2f",all_respondents$conf.int[2]), ")"), 
             sprintf("%.3f",all_respondents$p.value))

intl_only <- c(sprintf("%.2f",all_respondents_intl$estimate[1]), 
               sprintf("%.2f",all_respondents_intl$estimate[2]), 
             paste0(sprintf("%.2f",all_respondents_intl$estimate[1] - all_respondents_intl$estimate[2]), " (95% CI:", sprintf("%.2f",all_respondents_intl$conf.int[1]), ", ", sprintf("%.2f",all_respondents_intl$conf.int[2]), ")"), 
             sprintf("%.3f",all_respondents_intl$p.value))

table2 <- cbind(t_row_labels, us_only, intl_only)
rownames(table2) <- NULL
table2


#build table 3
female_us_only <- c(sprintf("%.2f",female_respondents$estimate[1]), 
                    sprintf("%.2f",female_respondents$estimate[2]), 
             paste0(sprintf("%.2f",female_respondents$estimate[1] - female_respondents$estimate[2]), " (95% CI:", sprintf("%.2f",female_respondents$conf.int[1]), ", ", sprintf("%.2f",female_respondents$conf.int[2]), ")"), 
             sprintf("%.3f",female_respondents$p.value))


female_intl_only <- c(sprintf("%.2f",Female_respondents_intl$estimate[1]), 
                      sprintf("%.2f",Female_respondents_intl$estimate[2]), 
               paste0(sprintf("%.2f",Female_respondents_intl$estimate[1] - Female_respondents_intl$estimate[2]), " (95% CI:", sprintf("%.2f",Female_respondents_intl$conf.int[1]), ", ", sprintf("%.2f",Female_respondents_intl$conf.int[2]), ")"), 
               sprintf("%.3f",Female_respondents_intl$p.value))

table3 <- cbind(t_row_labels, female_us_only, female_intl_only)
rownames(table3) <- NULL
table3


#build table 4

male_us_only <- c(sprintf("%.2f",male_respondents$estimate[1]), 
                    sprintf("%.2f",male_respondents$estimate[2]), 
                    paste0(sprintf("%.2f",male_respondents$estimate[1] - male_respondents$estimate[2]), " (95% CI:", sprintf("%.2f",male_respondents$conf.int[1]), ", ", sprintf("%.2f",male_respondents$conf.int[2]), ")"), 
                    sprintf("%.3f",male_respondents$p.value))


male_intl_only <- c(sprintf("%.2f",Male_respondents_intl$estimate[1]), 
                      sprintf("%.2f",Male_respondents_intl$estimate[2]), 
                      paste0(sprintf("%.2f",Male_respondents_intl$estimate[1] - Male_respondents_intl$estimate[2]), " (95% CI:", sprintf("%.2f",Male_respondents_intl$conf.int[1]), ", ", sprintf("%.2f",Male_respondents_intl$conf.int[2]), ")"), 
                      sprintf("%.3f",Male_respondents_intl$p.value))

table4 <- cbind(t_row_labels, male_us_only, male_intl_only)
rownames(table4) <- NULL
table4


###Appendix

#Table B.1: Using computer-generated responses to the survey shows that randomization was successfully implemented

test_df <- read.csv("test_df.csv",na.strings=c("", "NA"), header = TRUE) #Read in test data

# Test for randomization in the test dataframe
ttest_rand_test<-t.test(test_df$Gender.Dummy[test_df$treatment=="Control"],test_df$Gender.Dummy[test_df$treatment=="Treatment"])
ttest_rand_test

#Table B.2: Assignment to treatment does not correlate with likelihood to skip the name generator question

#Loading in US attrition_df

attrition_df<-read.csv("attrition_df.csv",na.strings=c("", "NA"), header = TRUE)


# T TEST AND REGRESION. Conditional dummy (if they skipped the ranking questions) - treatment 
ttest_attrition<-t.test(attrition_df$conditional.Dummy[attrition_df$treatment=="Control"],attrition_df$conditional.Dummy[attrition_df$treatment=="Treatment"])

reg_model_attrition<-lm(conditional.Dummy ~treatment, data=attrition_df)
reg_model_attrition_2<-lm(conditional.Dummy ~treatment*Gender.Dummy, data=attrition_df)

#Latex version
stargazer(reg_model_attrition,reg_model_attrition_2,title="",digits=2,keep.stat=c("n"),
          covariate.labels = c("\\textbf{Treatment} (reference= Control) &   \\\\
      \\hspace{1cm}Gender treatment",
      "\\textbf{Gender} (reference= Male) &   \\\\
                            \\hspace{1cm}Female",
      "\\textbf{Treatment Interacted with Gender} (reference= Male) &   \\\\
                            \\hspace{1cm}Gender Treatment: Female"),
      dep.var.labels = ("Non-Response (1=no answer)"),   
      dep.var.labels.include = T,
      style="apsr",type="latex")

#Word version
stargazer(reg_model_attrition,reg_model_attrition_2,title="",digits=2,keep.stat=c("n"),
          covariate.labels = c("Treatment (reference=Control)",
                               "Gender (reference=Male)",
                               "Treatment Interacted with Gender (Reference=Male)"),
      dep.var.labels = ("Non-Response (1=no answer)"),   
      dep.var.labels.include = T,
      style="apsr",type="text",out="TableB.2.txt")


#Table B.3: Assignment to treatment does not have an effect on the number of names listed
# Analysis using sum of total names listed

#Regression results

# sum.response.rank - number represents the number of blanks. 0 represents they listed all 4 names. 

reg_model1<-lm(sum.response.rank ~ treatment*response.qpri, data=attrition_df)

reg_model2<-lm(sum.response.rank ~ treatment*Gender.Dummy+response.qpri, data=attrition_df)

#Displaying in table
stargazer(reg_model1,reg_model2,title="",digits=2,keep.stat=c("n"),
          omit=c("response.qpri"),
          covariate.labels = c("\\textbf{Treatment} (reference= Control) &   \\\\
      \\hspace{1cm}Gender treatment",
      "\\textbf{Gender} (reference= Male) &   \\\\
                            \\hspace{1cm}Female",
      "\\textbf{Treatment Interacted with Gender} (reference= Male) &   \\\\
                            \\hspace{1cm}Gender Treatment: Female"),
      dep.var.labels = ("Sum of names listed"),   
      dep.var.labels.include = T,
      style="apsr",type="latex")


#Table C1: Gender Treatment Including Demographic Controls: U.S.
#creating df for models including recoding subfield

gender_results_model<-gender_results %>% 
  filter(resp_gender!="Prefer not to answer") %>% 
  filter(treatment!="rank_outsideUS") %>% 
  mutate(Subfield_2 = ifelse(Subfield %in% c("Human Rights","International Organization(s)","International/Global Political Economy","International/Global Security","U.S. Foreign Policy"),Subfield,"Other"))

  #Re-leveling subfield
gender_results_model$Subfield_2 <- factor(gender_results_model$Subfield_2, c("Other","Human Rights","International Organization(s)","International/Global Political Economy","International/Global Security","U.S. Foreign Policy"))
#To create the reference category Subfield: Other we collapsed the following relatively smaller subfields: Comparative Foreign Policy, Development Studies, European Studies/European Integration, Global Civil Society, History of the international relations discipline, Human Security, International Relations of a particular region/country, International Relations Theory, International/Global Ethics, International/Global Health, International/Global History, International/Global Environmental Politics, Gender in IR, Religion in IR, International Law, Other, and respondents whose subfield does not explicitly fall within IR.


#Model 1: Includes the gender treatment, and several demographic variables: the respondents gender, subfield, paradigm, and rank.

model_1<-lm(per ~ treatment+resp_gender+ Rank+Subfield_2+paradigm, data=gender_results_model)

#Model 2: Includes the treatment, the same demographics variables, and an interaction term between the respondents gender and the treatment.

model_2<-lm(per~treatment*resp_gender+Rank+Subfield_2+paradigm, data=gender_results_model)

#Displaying in table
stargazer(model_1,model_2,title="Gender Treatment Including Demographic Controls",digits=2,keep.stat=c("n"),
          covariate.labels = c("Treatment (reference= Control)",
      "Gender (reference= Female)", 
      "Rank: Associate Professor (reference= Assistant Professor)",
      "Rank: Emeritus",
      "Rank: Full Professor",
      "Rank: Other",
      "Subfield: Human Rights (reference= Other)",   
      "Subfield: International Organization(s)",
      "Subfield: International/Global Political Economy",
      "Subfield: International/Global Security",
      "Subfield: U.S. Foreign Policy",
      "Paradigm: Liberalism (reference= Constructivism)",
      "Paradigm: Non-paradigmatic",
      "Paradigm: Other",
      "Paradigm: Realism",
      "Treatment Interacted with Gender (reference= Female)") ,
dep.var.caption = c("Percentage of responses that are female:"),   
dep.var.labels.include = FALSE,
font.size = "footnotesize", 
column.sep.width = "1pt",
style="apsr",
no.space = TRUE,
type="html",
out="TableC1.html")

#2b. Intl scholar results

#Checking demographic balance between treatments: Non-U.S
scholar_ranks_demos_international<-select(scholar_ranks_intl,treatment,Gender=resp_gender,Subfield,paradigm,Rank) %>% 
  filter(treatment!="rank_outsideUS") %>% 
  mutate(treatment = recode(treatment,"rank_control" ="Control",
                            "rank_gender" ="Treatment")) %>% 
  mutate(Subfield = ifelse(Subfield %in% c("Human Rights", "International Organization(s)","International/Global Political Economy","International/Global Security", "International Relations of a particular region/country"),
                           Subfield,"Other")) %>%
  mutate(Gender = recode(Gender,"f" ="Female",
                         "m" ="Male")) %>% 
  filter(Gender!="Prefer not to answer") %>% 
  filter(Rank!="")

datasummary_balance_non_us<-datasummary_balance(~ treatment,data = select(scholar_ranks_demos_international,treatment,Gender,Subfield,Paradigm=paradigm,Rank),output="latex",title="Demographic Balance Table: Non-U.S")


#Table C2: Gender Treatment Including Demographic Controls: Non-U.S
#creating models
gender_results_model_intl<-gender_results_intl %>% 
  filter(resp_gender!="Prefer not to answer") %>% 
  filter(treatment!="rank_outsideUS")

#recoding subfield
gender_results_model_intl<-gender_results_model_intl %>% 
  mutate(Subfield_2 = ifelse(Subfield %in% c("Human Rights", "International Organization(s)","International/Global Political Economy","International/Global Security", "International Relations of a particular region/country"),Subfield,"Other"))


gender_results_model_intl$Subfield_2 <- factor(gender_results_model_intl$Subfield_2, c("Other","Human Rights","International Organization(s)","International/Global Political Economy","International/Global Security","International Relations of a particular region/country"))


#Regression models
model_1_intl<-lm(per ~ treatment+resp_gender+ Rank+Subfield_2+paradigm, data=gender_results_model_intl)

model_2_intl<-lm(per~treatment*resp_gender+Rank+Subfield_2+paradigm, data=gender_results_model_intl)

#Model 1: Includes the gender treatment, and several demographic variables: the respondents gender, subfield, paradigm, and rank.
#Model 2: Includes the treatment, the same demographics variables, and an interaction term between the respondents gender and the treatment.


stargazer(model_1_intl,model_2_intl,title="Gender Treatment Including Demographic Controls- NonU.S.",digits=2,keep.stat=c("n"),
          covariate.labels = c("Treatment (reference= Control)",
                               "Gender (reference= Female)", 
                               "Rank: Associate Professor (reference= Assistant Professor)",
                               "Rank: Emeritus",
                               "Rank: Full Professor",
                               "Rank: Other",
                               "Subfield: Human Rights (reference= Other)",   
                               "Subfield: International Organization(s)",
                               "Subfield: International/Global Political Economy",
                               "Subfield: International/Global Security",
                               "Subfield: U.S. Foreign Policy",
                               "Paradigm: Liberalism (reference= Constructivism)",
                               "Paradigm: Non-paradigmatic",
                               "Paradigm: Other",
                               "Paradigm: Realism",
                               "Treatment Interacted with Gender (reference= Female)") ,
          dep.var.caption = ("Percentage of responses that are female:"),   
dep.var.labels.include = FALSE,
font.size = "footnotesize", 
column.sep.width = "1pt",
style="apsr",
no.space = TRUE,
type="html",
out="TableC2.html")


