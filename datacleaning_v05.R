#title: "Data Cleaning"
#author: "Ziwei Crystal Zang"
#date: "7/6/2018"


##    ********************************    ##
##              QUESTIONS
##    1. Are Physical and Mental Health Mean Imputed for those missing?

library(SASxport)
library(tidyverse)
library(survey)
library(foreign)


#Import Data
    
    #2016 Landline Multiple Version Questionnaire Data Sets
    #setwd("~/Google Drive File Stream/My Drive/Research/BRFSS_Veterans_CogFx/RMD_data/data")
    setwd("~/Desktop/PopulationHealth/Final Version") #Crystal's directory

    #brfss2016 <- read.xport("~/Dropbox/0_temp/LLCP2016.XPT")  #Ben's diractory
    brfss2016 <- read.xport("~/Desktop/PopulationHealth/data/LLCP2016.XPT ")  #Crystal's directory
    
    brfss2016[] <- lapply(brfss2016, unclass)
    ##2016V1
    #brfss2016_v1 <- read.xport("LLCP16V1.XPT") #Ben
    brfss2016_v1 <- read.xport("~/Desktop/PopulationHealth/data/LLCP16V1.XPT") #Crystal
    brfss2016_v1[] <- lapply(brfss2016_v1, unclass)
    ##2016V2
    #brfss2016_v2 <- read.xport("LLCP16V2.XPT") #Ben
    brfss2016_v2 <- read.xport("~/Desktop/PopulationHealth/data/LLCP16V2.XPT") #Crystal
    brfss2016_v2[] <- lapply(brfss2016_v2, unclass)
    ##2016V3
    #brfss2016_v3 <- read.xport("LLCP16V3.XPT") #Ben
    brfss2016_v3 <- read.xport("~/Desktop/PopulationHealth/data/LLCP16V3.XPT") #Crystal
    brfss2016_v3[] <- lapply(brfss2016_v3, unclass)

    #2015data
    #brfss2015 <- read.xport("~/Dropbox/0_temp/LLCP2015.XPT") #Ben 
    brfss2015 <- read.xport("~/Desktop/PopulationHealth/data/LLCP2015.XPT ")  #Crystal

    brfss2015[] <- lapply(brfss2015, unclass)
    ##2015V1
    #brfss2015_v1 <- read.xport("LLCP15V1.XPT") #Ben
    brfss2015_v1 <- read.xport("~/Desktop/PopulationHealth/data/LLCP15V1.XPT") #Crystal
    brfss2015_v1[] <- lapply(brfss2015_v1, unclass)
    ##2015V2
    #brfss2015_v2 <- read.xport("LLCP15V2.XPT") #Ben
    brfss2015_v2 <- read.xport("~/Desktop/PopulationHealth/data/LLCP15V2.XPT") #Crystal
    brfss2015_v2[] <- lapply(brfss2015_v2, unclass)

    #Select variables
    ###2015
    brfss2015_0<-brfss2015%>%
      select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5, X_AGE80,X_AGE_G, X_AGEG5YR, 
             INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
             X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LLCPWT)
    brfss2015_1<-brfss2015_v1%>%
      select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5, X_AGE80, X_AGE_G, X_AGEG5YR,
             INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
             X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LCPWTV1) %>%
      rename(X_LLCPWT=X_LCPWTV1) 

    brfss2015_2<-brfss2015_v2%>%
      select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5, X_AGE80, X_AGE_G, X_AGEG5YR,
             INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
             X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LCPWTV2) %>%
      rename(X_LLCPWT=X_LCPWTV2)
    
    
    
    
    ###2016
    brfss2016_0<-brfss2016%>%
      select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5,X_AGE80, X_AGE_G ,X_AGEG5YR, 
             INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
             X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LLCPWT)
  
    brfss2016_1<-brfss2016_v1%>%
      select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5, X_AGE80, X_AGE_G, X_AGEG5YR, 
             INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
             X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LCPWTV1)  %>%
      rename(X_LLCPWT=X_LCPWTV1)
    
    brfss2016_2<-brfss2016_v2%>%
      select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5, X_AGE80, X_AGE_G, X_AGEG5YR, 
             INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
             X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LCPWTV2)  %>%
      rename(X_LLCPWT=X_LCPWTV2)
    
    brfss2016_3<-brfss2016_v3%>%
      select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5, X_AGE80, X_AGE_G, X_AGEG5YR, 
             INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
             X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LCPWTV3)  %>%
      rename(X_LLCPWT=X_LCPWTV3)
    
    
    ##Combine dataset into one
    brfss_1516 <- rbind(brfss2015_0,brfss2015_1,brfss2015_2,brfss2016_0, brfss2016_1, brfss2016_2, brfss2016_3)
    #Set as numeric 
    brfss_1516$PHYSHLTH <- as.numeric(brfss_1516$PHYSHLTH)
    brfss_1516$X_MRACE1 <- as.numeric(brfss_1516$X_MRACE1)

#Data Cleaning 2015 and 2016
    
      #Data Cleaning
      
        ## Missing for Response or Explanatory Variables
        brfss_baseelig <- brfss_1516 %>%
          
          ###Missing for Response variable Cognitive Loss
          drop_na(CIMEMLOS) %>%
          filter(CIMEMLOS!=7)%>%
          filter(CIMEMLOS != 9) %>%
          mutate(CIMEMLOS=if_else(CIMEMLOS ==2,0,1)) %>%
        
          ###Missing for Explanatary variable Veteran Status
          drop_na(VETERAN3) %>%
          filter(VETERAN3!=7)%>%
          filter(VETERAN3 != 9) %>%
          mutate(VETERAN3=if_else(VETERAN3 ==2,0,1))
      
          # Baseline Number Eligible Participants - not missing Response or Explanatory Variables
          base_elig_n<-nrow(brfss_baseelig)
      
     ## Missing for Covariates Variables
     brfss_cogfcn <-brfss_baseelig %>%
            
            ### Severity of Cogntive Decline            
            # mutate(CDHOUSE = as.numeric(if_else(is.na(CDHOUSE),10, CDHOUSE))) %>%
              
            ##Edit(Crystal)
            ### Severity of Cogntive Decline            
              mutate(CDHOUSE = as.numeric(CDHOUSE)) %>%
              mutate(CDHOUSE = replace(CDHOUSE, is.na(CDHOUSE), 10)) %>%
              
              #mutate( = factor(CDHOUSE, levels = c(0,1,2,3,4,9,10), 
              #                  labels = c("Never", "Always", "Usually", 
              #                            "Sometimes", "Rarely", "Don't know/Not sure/Refused", "Not asked or missing"))) %>%
              
            ###Sex  
              filter(SEX != 9) %>%
              mutate(SEX = as.factor(ifelse(SEX == 1, "Male", "Female"))) %>%
            ###Race
              drop_na(X_MRACE1) %>%
              mutate(X_MRACE1=if_else(X_MRACE1==77,8, X_MRACE1))%>%
              mutate(X_MRACE1=if_else(X_MRACE1==99,8, X_MRACE1)) %>%
              mutate(X_MRACE1=factor(X_MRACE1, levels = c(1:8), 
                      labels = c("White only", "Black or African American only", 
                                 "American Indian or Alaskan Native only", "Asian only", 
                                 "Native Hawaiian or other Pacific Islander only", "Other race only", 
                                 "Multiracial", "Don't know or refused")) ) %>%
          
            ###Income
              mutate(INCOME2 = ifelse(INCOME2 == 99, 9, INCOME2)) %>%
              mutate(INCOME2 = ifelse(INCOME2 == 77, 9, INCOME2)) %>%
              drop_na(INCOME2) %>%
              mutate(INCOME2 = factor(INCOME2, levels = c(1:9), 
                    labels = c("Less than $10,000", "Less than $15,000", 
                               "Less than $20,000", "Less than $25,000", 
                               "Less than $35,000", "Less than $50,000", 
                               "Less than $75,000", "$75,000 or more", 
                               "Don't know or refused")) ) %>%
 
            ###Education
              drop_na(EDUCA) %>%
              mutate(EDUCA = ifelse(EDUCA == 9, 7, EDUCA)) %>%
                mutate(EDUCA = factor(EDUCA, levels = c(1:7), 
                      labels = c("Never attended school or only kindergarten", 
                                 "Grades 1 through 8", "Grades 9 through 11", 
                                 "High school graduate", "College 1 year to 3 years", 
                                 "College graduate", "Refused")) ) %>%
            ###Age
              filter(X_AGE80 > 44) %>%
              mutate(AGE_category = if_else(X_AGE80 < 60, "Age 45-59",
                                    if_else(X_AGE80 > 59 & X_AGE80 < 70, "Age 60-69", "70 or older"))) %>%
              mutate(AGE_cat = as.factor(if_else(X_AGE80 < 50, "45-49",
                                               if_else(X_AGE80 >= 50 & X_AGE80 < 55, "50-54", 
                                               if_else(X_AGE80 >= 55 & X_AGE80 < 60, "55-59", 
                                               if_else(X_AGE80 >= 60 & X_AGE80 < 65, "60-64", 
                                               if_else(X_AGE80 >= 65 & X_AGE80 < 70, "65-69", 
                                               if_else(X_AGE80 >= 70 & X_AGE80 < 75, "70-74", "75+")))))))) %>%
           
            ### AGE & Veterans Interaction
              #mutate(Vet_Age = VETERAN3*AGE_cat) %>%
            ###Marital Status
              drop_na(MARITAL)  %>%
                mutate(MARITAL = factor(MARITAL, levels = c(1,2,3,4,5,6,9), 
                       labels = c("Married", "Divorced","Widowed", "Seperated", 
                                  "Never married", "A member of an unmarried couple", "Refused"))) %>%
           ###Number of Children
               drop_na(CHILDREN) %>%
               filter(CHILDREN != 99) %>%
               mutate(CHILDREN = ifelse(CHILDREN == 88, 0, CHILDREN)) %>%
           
           ###Employment Status
               drop_na(EMPLOY1) %>%
               mutate(EMPLOY1 = ifelse(EMPLOY1 == 3, 0,
                                       ifelse(EMPLOY1 == 4, 0, EMPLOY1))) %>%
               mutate(EMPLOY1 = factor(EMPLOY1, levels = c(0,1,2,5,6,7,8,9), 
                                       labels = c("Out of work", "Employed for wages", "Self-employed", 
                                                  "A homemaker", "A student", "Retired", "Unable to work", "Refused")) ) %>%
            #Physical activity (Mean)
              drop_na(PHYSHLTH) %>%
              mutate(PHYSHLTH = ifelse(PHYSHLTH == 88, 0, 
                                ifelse(PHYSHLTH == 77, 5.066,
                                ifelse(PHYSHLTH == 99, 5.066, PHYSHLTH)))) %>%
            #Mental health  
              drop_na(MENTHLTH) %>%
              mutate(MENTHLTH = ifelse(MENTHLTH == 88, 0,
                                ifelse(MENTHLTH == 77, 3.137,
                                ifelse(MENTHLTH == 99, 3.137, MENTHLTH)))) %>%
            ###Depression Diagnosis
              drop_na(ADDEPEV2) %>%
              mutate(ADDEPEV2 = ifelse(ADDEPEV2 == 2, 0, ADDEPEV2)) %>%
              mutate(ADDEPEV2 = ifelse(ADDEPEV2 == 7, 9, ADDEPEV2)) %>%
              mutate(ADDEPEV2 = factor(ADDEPEV2, levels= c(0,1,9), 
                     labels = c("No", "Yes", "Don't know/Not sure/Refused"))) %>%
          
            ###Smoker status
              mutate(X_SMOKER3 = ifelse(X_SMOKER3 == 4, 0, 
                                 ifelse(X_SMOKER3 == 2, 1, X_SMOKER3))) %>%
              mutate(X_SMOKER3 = factor(X_SMOKER3, levels = c(0,1,3,9), 
                                         labels = c("Never smoked", "Current smoker", 
                                                    "Former smoker", "Don't know/Refused/Missing"))) %>%
            ###Overweight (BMI >25)
              mutate(X_RFBMI5 = ifelse(X_RFBMI5 == 1, 0,
                                  ifelse(X_RFBMI5 == 2, 1, X_RFBMI5))) %>%
              mutate(X_RFBMI5 = factor(X_RFBMI5, levels = c(0,1,9), 
                                       labels = c("Not overweight or obese", "Overweight or obese", 
                                                  "Don't know/Refused/Missing"))) %>%
            ###Physical Activity
              mutate(X_TOTINDA = ifelse(X_TOTINDA == 2, 0, X_TOTINDA)) %>%
              mutate(X_TOTINDA = factor(X_TOTINDA, levels = c(0,1,9), 
                                        labels = c("No physical activity or exercise in last 30 days", 
                                                   "Had physical activity or exercise", "Don't know/Refused/Missing"))) %>%
           ### Binge Drinking
               filter(X_RFBING5 != 9)%>%
               mutate(X_RFBING5 = as.factor(if_else(X_RFBING5 == 1,"No","Yes"))) %>%
     
     ## Keeping only necessary Variables
       select(VETERAN3, CIMEMLOS, CDHOUSE, SEX, X_MRACE1, X_RFBING5, AGE_cat,
              INCOME2, EDUCA, MARITAL, PHYSHLTH, MENTHLTH, ADDEPEV2,CHILDREN, EMPLOY1, X_SMOKER3,
              X_RFBMI5, X_TOTINDA, X_PSU, X_STSTR, X_LLCPWT)
       
      # Relevel Reference Groups
          brfss_cogfcn$AGE_cat <- relevel(brfss_cogfcn$AGE_cat, ref = "45-49")
          brfss_cogfcn$X_TOTINDA <- relevel(brfss_cogfcn$X_TOTINDA, ref = "No physical activity or exercise in last 30 days")
          brfss_cogfcn$X_RFBMI5 <- relevel(brfss_cogfcn$X_RFBMI5, ref = "Not overweight or obese")
          brfss_cogfcn$X_SMOKER3 <- relevel(brfss_cogfcn$X_SMOKER3, ref = "Never smoked")
          brfss_cogfcn$EMPLOY1 <- relevel(brfss_cogfcn$EMPLOY1, ref = "Employed for wages")
          brfss_cogfcn$ADDEPEV2 <- relevel(brfss_cogfcn$ADDEPEV2, ref = "No")
          brfss_cogfcn$MARITAL <- relevel(brfss_cogfcn$MARITAL, ref = "Married")
          brfss_cogfcn$EDUCA <- relevel(brfss_cogfcn$EDUCA, ref = "High school graduate")
          brfss_cogfcn$X_MRACE1 <- relevel(brfss_cogfcn$X_MRACE1, ref = "White only")
          brfss_cogfcn$SEX <- relevel(brfss_cogfcn$SEX, ref = "Male")
          brfss_cogfcn$INCOME2 <- relevel(brfss_cogfcn$INCOME2, ref = "$75,000 or more")
          
      # Final Number Eligible Participants - not missing Response or Explanatory Variables
          final_elig_n<-nrow(brfss_cogfcn)
          diff_elig_n<-(base_elig_n-final_elig_n)
          diff_elig_pct<-((diff_elig_n/base_elig_n)*100)
          
vets_n<-brfss_cogfcn %>%
    count(VETERAN3) %>%
    mutate(pct=(n/sum(n))*100) %>%
    filter(VETERAN3==1)

vets_cog<-brfss_cogfcn %>%
  group_by(VETERAN3) %>%
  count(CIMEMLOS) %>%
  mutate(pct=(n/sum(n))*100) %>%
  filter(VETERAN3==1 & CIMEMLOS==1)
      
  ###Secondary Hypothesis, use a different dataframe
      ###Cognitive Decline, response variable for level of cognitive decline
          brfss_cogfcn$CDHOUSE <- as.numeric(brfss_cogfcn$CDHOUSE)
      
      brfss_cogsev <- brfss_cogfcn %>%
        drop_na(CDHOUSE) %>%
        filter(CDHOUSE<5) %>%
        #Frequent Cognitive IADL issues defined as Always, Usually, or Sometimes. Not frequent as Rarely or Never. 
        mutate(CDHOUSE = if_else(CDHOUSE %in% c(1:3), 1, 0))
      
      table(brfss_cogsev$CDHOUSE)
      
          # Final Number Eligible Participants for Cognitive Levels- not missing Response or Explanatory Variables
          final_elig_n_CogFreq<-nrow(brfss_cogsev)
          diff_elig_n_CogFreq<-(base_elig_n-final_elig_n_CogFreq)
          diff_elig_pct_CogFreq<-((diff_elig_n_CogFreq/base_elig_n)*100)
