#Assessing the Renaissance of Individuals in International Relations Theory

# Preamble ----------------------------------------------------------------

#Clear workspace and setting seed for any randomization
rm(list=ls())

set.seed(101)


#The pacman package is required to load/install the additional packages.

if (!require("pacman")) install.packages("pacman")

#Installing and loading packages for use
pacman::p_load(tidyverse,
               xtable,
               stargazer)

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


#Loading TRIP Journal Article Data 1980-2018
JAD<-read.csv("TRIP_JAD_1980_2018.csv",na.strings=c("", "NA"), header = TRUE)

##############################################
#1. Data wrangling
JAD_model<-JAD
#recoding variables
JAD_model$FirstImage[JAD_model$Level_1=="No"]<-0
JAD_model$FirstImage[JAD_model$Level_1=="Yes"]<-1
JAD_model$imagenone[JAD_model$Level_None=="No"]<-0
JAD_model$imagenone[JAD_model$Level_None=="Yes"]<-1
JAD_model$epistemology[JAD_model$Epistemology=="Non-Positivist/Post-Positivist"]<-0
JAD_model$epistemology[JAD_model$Epistemology=="Positivist"]<-1
JAD_model$qual[JAD_model$Methodology_Qualitative=="No"]<-0
JAD_model$qual[JAD_model$Methodology_Qualitative=="Yes"]<-1
JAD_model$quant[JAD_model$Methodology_Quantitative=="No"]<-0
JAD_model$quant[JAD_model$Methodology_Quantitative=="Yes"]<-1
JAD_model$formal_modeling[JAD_model$Methodology_FormalModeling=="No"]<-0
JAD_model$formal_modeling[JAD_model$Methodology_FormalModeling=="Yes"]<-1

JAD_model$Atheoretic<-ifelse(JAD_model$Paradigm=="Atheoretic/Non",1,0)
JAD_model$Constructivist<-ifelse(JAD_model$Paradigm=="Constructivist",1,0)
JAD_model$Liberal<-ifelse(JAD_model$Paradigm=="Liberal",1,0)
JAD_model$Marxist<-ifelse(JAD_model$Paradigm=="Marxist",1,0)
JAD_model$NonPara<-ifelse(JAD_model$Paradigm=="Non-paradigmatic",1,0)
JAD_model$Realist<-ifelse(JAD_model$Paradigm=="Realist",1,0)

JAD_model$SeriouslyAtheoretic<-ifelse(JAD_model$Seriously_Atheoretic=="Yes",1,0)
JAD_model$SeriouslyConstructivism<-ifelse(JAD_model$Seriously_Constructivism=="Yes",1,0)
JAD_model$SeriouslyLiberalism<-ifelse(JAD_model$Seriously_Liberalism=="Yes",1,0)
JAD_model$SeriouslyMarxism<-ifelse(JAD_model$Seriously_Marxism=="Yes",1,0)
JAD_model$SeriouslyNonPara<-ifelse(JAD_model$Seriously_NonParadigmatic=="Yes",1,0)
JAD_model$SeriouslyRealism<-ifelse(JAD_model$Seriously_Realism=="Yes",1,0)

JAD_model$AP<-ifelse(JAD_model$IssueArea=="American Politics",1,0)
JAD_model$CFP<-ifelse(JAD_model$IssueArea=="Comparative Foreign Policy",1,0)
JAD_model$CP<-ifelse(JAD_model$IssueArea=="Comparative Politics",1,0)
JAD_model$ENV<-ifelse(JAD_model$IssueArea=="Environment",1,0)
JAD_model$GEN<-ifelse(JAD_model$IssueArea=="General (or non-specific)",1,0)
JAD_model$HEALTH<-ifelse(JAD_model$IssueArea=="Health",1,0)
JAD_model$HIST<-ifelse(JAD_model$IssueArea=="History of the IR Discipline",1,0)
JAD_model$HR<-ifelse(JAD_model$IssueArea=="Human Rights",1,0)
JAD_model$IL<-ifelse(JAD_model$IssueArea=="International Law",1,0)
JAD_model$IO<-ifelse(JAD_model$IssueArea=="International Organization",1,0)
JAD_model$IPE<-ifelse(JAD_model$IssueArea=="International Political Economy",1,0)
JAD_model$IS<-ifelse(JAD_model$IssueArea=="International Security",1,0)
JAD_model$IRTHEORY<-ifelse(JAD_model$IssueArea=="IR theory",1,0)
JAD_model$METHODS<-ifelse(JAD_model$IssueArea=="Methodology",1,0)
JAD_model$OTHER<-ifelse(JAD_model$IssueArea=="Other",1,0)
JAD_model$PHIL<-ifelse(JAD_model$IssueArea=="Philosophy of Science",1,0)
JAD_model$PTHEORY<-ifelse(JAD_model$IssueArea=="Political Theory",1,0)
JAD_model$USFP<-ifelse(JAD_model$IssueArea=="US Foreign Policy",1,0)

#removing irrelevant (Non_IR) articles
JAD_model_filtered<-JAD_model %>% 
  filter(AP==0) %>% 
  filter(PHIL==0) %>% 
  filter(PTHEORY==0) %>% 
  filter(METHODS==0) %>% 
  filter(CP==0) %>% 
  filter(imagenone==0)


#2. #Table 1: Estimate of Probit Regressions for Use of First Image

#Formatting the data
JAD_model_formatted<-JAD_model_filtered %>% 
  mutate(IssueArea=recode(IssueArea,"Environment" = "Other",
                          "General (or non-specific)"= "Other",
                          "History of the IR Discipline"= "Other",
                          "IR theory"= "Other",
                          "Comparative Foreign Policy"= "Other",
                          "Human Rights"= "Other",
                          "Health"= "Other",
                          "Other"= "Other")) %>% 
  mutate(IssueArea=fct_relevel(IssueArea, "Other","International Organization","International Political Economy","International Law","US Foreign Policy","International Security"))


#Model one 
JAD_GLM1<-glm(FirstImage ~ epistemology + quant + qual + formal_modeling, family = binomial(link = "probit"), JAD_model_formatted)

#Model two
JAD_GLM2<-glm(FirstImage ~ epistemology + quant + qual + formal_modeling+ Constructivist + Liberal + Marxist  + Realist+ NonPara + Atheoretic, family=binomial(link="probit"), JAD_model_formatted)

# Model three
JAD_GLM3<-glm(FirstImage ~ epistemology + quant + qual + formal_modeling+ Constructivist + Liberal + Marxist  + Realist+ NonPara + Atheoretic + SeriouslyLiberalism + SeriouslyConstructivism + SeriouslyRealism + SeriouslyMarxism + SeriouslyNonPara + SeriouslyAtheoretic + IssueArea, family=binomial(link="probit"), JAD_model_formatted)

# Model four (IO only)
JAD_GLM4<-glm(FirstImage ~ epistemology + quant + qual + formal_modeling+ Constructivist + Liberal + Marxist  + Realist+ NonPara + Atheoretic + SeriouslyLiberalism + SeriouslyConstructivism + SeriouslyRealism + SeriouslyMarxism + SeriouslyNonPara + SeriouslyAtheoretic, family=binomial(link="probit"),data=JAD_model_formatted[JAD_model_formatted$IssueArea=="International Organization",])

# Model 5 (Time check)
JAD_GLM5<-glm(FirstImage ~ epistemology + quant + qual + formal_modeling+ Constructivist + Liberal + Marxist  + Realist+ NonPara + Atheoretic+ SeriouslyLiberalism + SeriouslyConstructivism + SeriouslyRealism + SeriouslyMarxism + SeriouslyNonPara + SeriouslyAtheoretic + IssueArea, family=binomial(link="probit"),data=JAD_model_formatted[JAD_model_formatted$year>=2015,])


#outputting models to html for word formatting 
stargazer(JAD_GLM1,JAD_GLM2,JAD_GLM3,JAD_GLM4,JAD_GLM5,column.labels = c("Model 1","Model 2","Model 3","Model 4","Model 5"),title="Estimate of Probit Regressions for Use of First Image", dep.var.labels = c("First Image Approach"),type="html",out="Table1.html")


#3.Appendix Tables 
#Table A1: Level of Analysis 1980-2018

Table_A1<-select(JAD_model_filtered,Level_1,Level_2,Level_3,Level_None,Paradigm) %>%
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  pctgroup(Image,Value)%>%
  filter(Value=="Yes") %>% 
  select(-Value)

printtable(Table_A1,"Level of Analysis 1980-2018")


#Table A2: Use of First Image by Journal 1980-2018

Table_A2<-select(JAD_model_filtered,Level_1,journal,year) %>% 
  pctgroup(journal,Level_1) %>% 
  filter(Level_1=="Yes") %>% 
  select(-Level_1) %>% 
  rename('Percentage First Image'=Percentage)

printtable(Table_A2,"Use of First Image by Journal 1980-2018")

#Table A3: Use of First Image over Time
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=1980 & JAD_model_filtered$year <=1984]<-"1980-1984"
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=1985 & JAD_model_filtered$year <=1989]<-"1985-1989"
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=1990 & JAD_model_filtered$year <=1994]<-"1990-1994"
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=1995 & JAD_model_filtered$year <=1999]<-"1995-1999"
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=2000 & JAD_model_filtered$year <=2004]<-"2000-2004"
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=2005 & JAD_model_filtered$year <=2009]<-"2005-2009"
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=2010 & JAD_model_filtered$year <=2014]<-"2010-2014"
JAD_model_filtered$Time_Period[JAD_model_filtered$year >=2015 & JAD_model_filtered$year <=2018]<-"2015-2018"

Table_A3<-select(JAD_model_filtered,Time_Period,Level_1) %>% 
  pctgroup(Time_Period,Level_1) %>% 
  filter(Level_1=="Yes") %>% 
  select(-Level_1) %>% 
  rename('Percentage First Image'=Percentage)

printtable(Table_A3,"Use of First Image over Time")

#Figure 1: Proportion of IR articles by image 1980-2018

Figure_1<-select(JAD_model_filtered,Level_1,Level_2,Level_3,Level_None,year) %>%
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  pctgroup(year,Image,Value)%>%
  filter(Value=="Yes") 
Figure_1$Year<-Figure_1$year

  #Creating a custom theme for visualizations
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
                   panel.grid.minor = element_blank()
) 



  #Color version
Figure_1_plot<-ggplot(data = Figure_1, aes(x = Year, y = Percentage, group = Image, color = Image)) + 
  geom_line(size=1.2)+
  scale_y_continuous(limits = c(0, 100),breaks = c(0,20,40,60,80,100))+
  scale_x_continuous(expand = c(0, 0),breaks = c(1980,1990,2000,2010,2018))+
  plot_theme

#ggsave("Figure_1.eps", Figure_1_plot, dpi=300, width=9.5, height=5) 

  #black and white version of Figure 1
Figure_1_plot_bw<-ggplot(data = Figure_1, aes(x = Year, y = Percentage, group = Image)) +
  geom_point(aes(shape=Image),size=1.5) +
  geom_line(size=1)+
  theme_minimal()+
  xlab(element_blank())+
  guides(fill=guide_legend(nrow=nrow,byrow=TRUE))+
  scale_y_continuous(limits = c(0, 100),breaks = c(0,20,40,60,80,100))+
  scale_x_continuous(expand = c(0, 0),breaks = c(1980,1990,2000,2010,2018))+
  plot_theme

#ggsave("Figure_1_bw.eps", Figure_1_plot_bw, dpi=300, width=9.5, height=5) 


#Table A4: Use of First Image by Paradigm 1980-2018

Table_A4<-select(JAD_model_filtered,Paradigm,Level_1) %>% 
  pctgroup(Paradigm,Level_1) %>% 
  filter(Level_1=="Yes") %>% 
  select(-Level_1) %>% 
  rename('Percentage First Image'=Percentage)

printtable(Table_A4,"Use of First Image by Paradigm 1980-2018")


#Table A5: Use of First Image by Methodology 1980-2018

Table_A5<-select(JAD_model_filtered,Level_1,Methodology_AnalyticNonFormal:Methodology_Quantitative) %>% 
  gather(Method,Value,Methodology_AnalyticNonFormal:Methodology_Quantitative) %>%
  mutate(Method = recode(Method,"Methodology_AnalyticNonFormal" = "Analytic NonFormal",
                        "Methodology_Counterfactual" = "Counterfactual",
                        "Methodology_Descriptive" = "Descriptive",
                        "Methodology_Experimental" = "Experimental",
                        "Methodology_FormalModeling" = "Formal Modeling",
                        "Methodology_PolicyAnalysis" = "Policy Analysis",
                        "Methodology_Qualitative" = "Qualitative",
                        "Methodology_Quantitative" = "Quantitative"))%>%
  filter(Value=="Yes") %>% 
  pctgroup(Method,Level_1) %>% 
  filter(Level_1=="Yes") %>% 
  select(-Level_1) %>% 
  rename('Percentage First Image'=Percentage)

printtable(Table_A5,"Use of First Image by Methodology 1980-2018")


#Table A6: Use of First Image by Epistemology 1980-2018

Table_A6<-select(JAD_model_filtered,Level_1,Epistemology) %>% 
  pctgroup(Epistemology,Level_1) %>% 
  filter(Level_1=="Yes") %>% 
  select(-Level_1)%>% 
  rename('Percentage First Image'=Percentage)

printtable(Table_A6,"Use of First Image by Epistemology 1980-2018")

#Table A7: Use of First Image by Issue Area 1980-2018

Table_A7<-select(JAD_model_filtered,Level_1,IssueArea) %>% 
  pctgroup(IssueArea,Level_1) %>% 
  filter(Level_1=="Yes") %>% 
  select(-Level_1)%>% 
  rename('Percentage First Image'=Percentage)

printtable(Table_A7,"Use of First Image by Issue Area 1980-2018")

#Table A8: Level of Analysis TRIP Book Data 2000-2014

trip_books<-read.csv("TRIP_BookDatabase_2000_2014.csv")

#Data wrangling
Book_model<-trip_books
#recode variables
Book_model$FirstImage[Book_model$Level_1=="No"]<-0
Book_model$FirstImage[Book_model$Level_1=="Yes"]<-1
Book_model$imagenone[Book_model$Level_None=="No"]<-0
Book_model$imagenone[Book_model$Level_None=="Yes"]<-1
Book_model$epistemology[Book_model$Epistemology=="Non-Positivist/Post-Positivist"]<-0
Book_model$epistemology[Book_model$Epistemology=="Positivist"]<-1
Book_model$qual[Book_model$Methodology_Qualitative=="No"]<-0
Book_model$qual[Book_model$Methodology_Qualitative=="Yes"]<-1
Book_model$quant[Book_model$Methodology_Quantitative=="No"]<-0
Book_model$quant[Book_model$Methodology_Quantitative=="Yes"]<-1
Book_model$formal_modeling[Book_model$Methodology_FormalModeling=="No"]<-0
Book_model$formal_modeling[Book_model$Methodology_FormalModeling=="Yes"]<-1

Book_model$Atheoretic<-ifelse(Book_model$Paradigm=="Atheoretic/Non",1,0)
Book_model$Constructivist<-ifelse(Book_model$Paradigm=="Constructivist",1,0)
Book_model$Liberal<-ifelse(Book_model$Paradigm=="Liberal",1,0)
Book_model$Marxist<-ifelse(Book_model$Paradigm=="Marxist",1,0)
Book_model$NonPara<-ifelse(Book_model$Paradigm=="Non-paradigmatic",1,0)
Book_model$Realist<-ifelse(Book_model$Paradigm=="Realist",1,0)

Book_model$SeriouslyAtheoretic<-ifelse(Book_model$Seriously_Atheoretic=="Yes",1,0)
Book_model$SeriouslyConstructivism<-ifelse(Book_model$Seriously_Constructivism=="Yes",1,0)
Book_model$SeriouslyLiberalism<-ifelse(Book_model$Seriously_Liberalism=="Yes",1,0)
Book_model$SeriouslyMarxism<-ifelse(Book_model$Seriously_Marxism=="Yes",1,0)
Book_model$SeriouslyNonPara<-ifelse(Book_model$Seriously_NonParadigmatic=="Yes",1,0)
Book_model$SeriouslyRealism<-ifelse(Book_model$Seriously_Realism=="Yes",1,0)

Book_model$AP<-ifelse(Book_model$IssueArea=="American Politics",1,0)
Book_model$CFP<-ifelse(Book_model$IssueArea=="Comparative Foreign Policy",1,0)
Book_model$CP<-ifelse(Book_model$IssueArea=="Comparative Politics",1,0)
Book_model$ENV<-ifelse(Book_model$IssueArea=="Environment",1,0)
Book_model$GEN<-ifelse(Book_model$IssueArea=="General (or non-specific)",1,0)
Book_model$HEALTH<-ifelse(Book_model$IssueArea=="Health",1,0)
Book_model$HIST<-ifelse(Book_model$IssueArea=="History of the IR Discipline",1,0)
Book_model$HR<-ifelse(Book_model$IssueArea=="Human Rights",1,0)
Book_model$IL<-ifelse(Book_model$IssueArea=="International Law",1,0)
Book_model$IO<-ifelse(Book_model$IssueArea=="International Organization",1,0)
Book_model$IPE<-ifelse(Book_model$IssueArea=="International Political Economy",1,0)
Book_model$IS<-ifelse(Book_model$IssueArea=="International Security",1,0)
Book_model$IRTHEORY<-ifelse(Book_model$IssueArea=="IR theory",1,0)
Book_model$METHODS<-ifelse(Book_model$IssueArea=="Methodology",1,0)
Book_model$OTHER<-ifelse(Book_model$IssueArea=="Other",1,0)
Book_model$PHIL<-ifelse(Book_model$IssueArea=="Philosophy of Science",1,0)
Book_model$PTHEORY<-ifelse(Book_model$IssueArea=="Political Theory",1,0)
Book_model$USFP<-ifelse(Book_model$IssueArea=="US Foreign Policy",1,0)

#removing irrelevant (Non_IR) articles
book_filtered<-Book_model %>% 
  filter(AP==0) %>% 
  filter(PHIL==0) %>% 
  filter(PTHEORY==0) %>% 
  filter(METHODS==0) %>% 
  filter(CP==0) %>% 
  filter(imagenone==0)

#Appendix Table 8
Table_A8<-select(book_filtered,Level_1,Level_2,Level_3,Level_None) %>%
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  pctgroup(Image,Value)%>%
  filter(Value=="Yes")

printtable(Table_A8,"Level of Analysis TRIP Book Data 2000-2014")


#4. Additional analysis

#"Using the Teaching, Research, and International Policy (TRIP) journal article database, we show that only 13.7% of IR articles in twelve leading journals employ the first image"

level_of_analysis_overall<-select(JAD_model_filtered,Level_1,Level_2,Level_3,Level_None,Paradigm) %>%
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  pctgroup(Image,Value)%>%
  filter(Value=="Yes")
printtable(level_of_analysis_overall,"Level of Analysis 1980-2018")

#"There is no meaningful difference in the number of first-image articles before and after 2001, either in the field as a whole or in International Security, where the proportion dropped from 13.6% to 11.8%"

International_Security_pre_2001<-select(JAD_model_filtered,Level_1,journal,year) %>% 
  filter(year<2001) %>% 
  filter(journal=="IS") %>% 
  pctgroup(journal,Level_1)

International_Security_post_2001<-select(JAD_model_filtered,Level_1,journal,year) %>% 
  filter(year>2001) %>% 
  filter(journal=="IS") %>% 
  pctgroup(journal,Level_1)

#"(20% of constructivist articles study the first image and 73% study the third.)"

level_of_analysis_Constructivist<-select(JAD_model_filtered,Level_1,Level_2,Level_3,Level_None,Paradigm) %>%
  filter(Paradigm=="Constructivist") %>% 
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  pctgroup(Image,Value)%>%
  filter(Value=="Yes")
printtable(level_of_analysis_Constructivist,"Level of Analysis 1980-2018 Constructivist Articles")

#"from 1980-2004, when the paradigms dominated IR, 15.5% of articles used the first image; from 2015-2018, 14.8% did"

level_of_analysis_1980_2004<-select(JAD_model_filtered,Level_1,Level_2,Level_3,Level_None,year) %>%
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  filter(year<=2004) %>% 
  pctgroup(Image,Value)%>%
  filter(Value=="Yes")

level_of_analysis_2015_2018<-select(JAD_model_filtered,Level_1,Level_2,Level_3,Level_None,year) %>%
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  filter(year>=2015) %>% 
  pctgroup(Image,Value)%>%
  filter(Value=="Yes")

#"The current renaissance improves on 2005-2014 (9.9%), but it is roughly the same as the overall proportion for the past 40 years." 

level_of_analysis_2005_2014<-select(JAD_model_filtered,Level_1,Level_2,Level_3,Level_None,year) %>%
  gather(Image,Value,Level_1,Level_1,Level_2,Level_3,Level_None) %>%
  mutate(Image = recode(Image,"Level_1" = "1st image",
                        "Level_2" = "2nd image",
                        "Level_3" = "3rd image",
                        "Level_None" = "NA"))%>%
  filter(year>=2005&year<=2014) %>% 
  pctgroup(Image,Value)%>%
  filter(Value=="Yes")


