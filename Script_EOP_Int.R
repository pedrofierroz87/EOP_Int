rm(list=ls())

#Load libraries
library(haven)
library(knitr)
library(lattice)
library(tidyverse)
library(here)
library(flextable)
library(devtools)
library(lavaan)
library(ggplot2)
library(plm)
library(naniar)
library(purrr)


#**************************************************************************************************************************/
##########  EOP 2018  ##########################################################
#**************************************************************************************************************************/

#Import Data

EOP2018 <- read_dta("EOP2018.dta")
View(EOP2018)

# Year
EOP2018$year <- 2018

# Weight

EOP2018$weight <- EOP2018$PONDERADOR_2018

######## 2018 - SOCIODEMOGRAPHIC VARIABLES ########

# Sex (NAs and create dummy =1 if women)

EOP2018 <- EOP2018%>%
  mutate(sex = ifelse(A6 == 2, 1,
                      ifelse(A6 == 1, 0, NA)))
table(EOP2018$A6)
table(EOP2018$sex)

# Age

EOP2018$age <- EOP2018$A5
summary(EOP2018$A5)
summary(EOP2018$age)

# Socioeconomic Status
EOP2018$SES <- EOP2018$GSEX
table(EOP2018$GSEX)
table(EOP2018$SES)

# recoding

EOP2018 <- EOP2018 %>%
  mutate(SES = case_when(
    SES == 1 ~ 5,
    SES == 2 ~ 4,
    SES == 4 ~ 2,
    SES == 5 ~ 1,
    TRUE ~ SES  # This line keeps all other values as they are
  ))

table(EOP2018$SES)

# Education Z2 (NAs)

EOP2018$education <- EOP2018$Z2

table(EOP2018$Z2)
table(EOP2018$education)

EOP2018$householdeduc <- ifelse(EOP2018$Z3 == 99, NA, EOP2018$Z3)
table(EOP2018$Z3)  
table(EOP2018$householdeduc)

# Living in the periphery

EOP2018$periphery <- ifelse(EOP2018$S1 == 1 | EOP2018$S1 == 2 | EOP2018$S1 == 3 | EOP2018$S1 == 4 | EOP2018$S1 == 5 | EOP2018$S1 == 9 | EOP2018$S1 == 10, 1, 0)
table(EOP2018$periphery)
table(EOP2018$S1)

# Internet Access

table(EOP2018$Z9_1)
table(EOP2018$Z9_2)
table(EOP2018$Z9_3)

EOP2018 <- EOP2018%>%
  mutate(inthome = ifelse(Z9_1 == 1, 1,
                      ifelse(Z9_1 == 2, 0, NA)))
EOP2018 <- EOP2018%>%
  mutate(intmobile = ifelse(Z9_2 == 1, 1,
                          ifelse(Z9_2 == 2, 0, NA)))
EOP2018 <- EOP2018%>%
  mutate(intwork = ifelse(Z9_3 == 1, 1,
                            ifelse(Z9_3 == 2, 0, NA)))

table(EOP2018$inthome)
table(EOP2018$intmobile)
table(EOP2018$intwork)

######## 2018 - EVALUATION OF AUTHORITIES ########

## Presidential Approval

table(EOP2018$B1)
EOP2018 <- EOP2018%>%
  mutate(presapp = ifelse(EOP2018$B1 == 1, 1,
                          ifelse(EOP2018$B1 == 99, NA, 0)))
table (EOP2018$presapp)

## Government Approval

table(EOP2018$B2)
EOP2018 <- EOP2018%>%
  mutate(govapp = ifelse(EOP2018$B2 == 1, 1,
                         ifelse(EOP2018$B2 == 99, NA, 0)))
table (EOP2018$govapp)

## Country Present

table(EOP2018$B3)
EOP2018 <- EOP2018%>%
  mutate(progress = ifelse(EOP2018$B3 == 1, 1,
                           ifelse(EOP2018$B3 == 99, NA, 0)))
EOP2018 <- EOP2018%>%
  mutate(stagnant = ifelse(EOP2018$B3 == 2, 1,
                           ifelse(EOP2018$B3 == 99, NA, 0)))
EOP2018 <- EOP2018%>%
  mutate(decline = ifelse(EOP2018$B3 == 3, 1,
                          ifelse(EOP2018$B3 == 99, NA, 0)))
table (EOP2018$progress)
table (EOP2018$stagnant)
table (EOP2018$decline)


## Country Future
table(EOP2018$B4)
EOP2018 <- EOP2018%>%
  mutate(better = ifelse(EOP2018$B4 == 1, 1,
                         ifelse(EOP2018$B4 == 99, NA, 0)))
EOP2018 <- EOP2018%>%
  mutate(equal = ifelse(EOP2018$B4 == 2, 1,
                        ifelse(EOP2018$B4 == 99, NA, 0)))
EOP2018 <- EOP2018%>%
  mutate(worse = ifelse(EOP2018$B4 == 3, 1,
                        ifelse(EOP2018$B4 == 99, NA, 0)))
table (EOP2018$better)
table (EOP2018$equal)
table (EOP2018$worse)

## Quality of life

table(EOP2018$B7_1)
table(EOP2018$B7_2)
EOP2018$mebetter <- ifelse(EOP2018$B7_1 == 99, NA, EOP2018$B7_1)
EOP2018$childrenbetter <- ifelse(EOP2018$B7_2 == 99, NA, EOP2018$B7_2)
table(EOP2018$mebetter)
table(EOP2018$childrenbetter)

######## 2018 - POLITICAL ENGAGEMENT ########

## Trust
lapply(EOP2018 %>% select(starts_with("D2_")), table)

EOP2018 <- EOP2018 %>%
  mutate(across(starts_with("D2_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "trust{str_remove(.col, 'D2_')}"))

lapply(EOP2018 %>% select(starts_with("trust")), table)

## Corruption 

lapply(EOP2018 %>% select(starts_with("D5_")), table)

EOP2018 <- EOP2018 %>%
  mutate(across(starts_with("D5_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "corruption{str_remove(.col, 'D5_')}"))

lapply(EOP2018 %>% select(starts_with("corruption")), table)

# recoding, so 5 is more corrupted

EOP2018 <- EOP2018 %>%
  mutate(across(starts_with("corruption"), ~ 6 - .x))
lapply(EOP2018 %>% select(starts_with("corruption")), table)


## Rational Approval of Demcoracy

table(EOP2018$B5)
EOP2018 <- EOP2018%>%
  mutate(democracy = ifelse(EOP2018$B5 == 1, 1,
                            ifelse(EOP2018$B5 == 99, NA, 0)))
EOP2018 <- EOP2018%>%
  mutate(authoritarianism = ifelse(EOP2018$B5 == 2, 1,
                                   ifelse(EOP2018$B5 == 99, NA, 0)))
EOP2018 <- EOP2018%>%
  mutate(nocare = ifelse(EOP2018$B5 == 3, 1,
                         ifelse(EOP2018$B5 == 99, NA, 0)))


table(EOP2018$democracy)
table(EOP2018$authoritarianism)
table(EOP2018$nocare)

## Satisfaction with democracy

table(EOP2018$B6)
EOP2018$satisdemoc <- ifelse(EOP2018$B6 == 99, NA, EOP2018$B6)
table(EOP2018$satisdemoc)

## Internal Efficacy (NAs)

table(EOP2018$E17_1)
table(EOP2018$E17_2)
table(EOP2018$E17_3)
table(EOP2018$E17_4)
table(EOP2018$E17_5)

EOP2018$intef1 <- ifelse(EOP2018$E17_1 == 99, NA, EOP2018$E17_1)
table(EOP2018$intef1)

EOP2018$intef2 <- ifelse(EOP2018$E17_2 == 99, NA, EOP2018$E17_2)
table(EOP2018$intef2)

EOP2018$intef3 <- ifelse(EOP2018$E17_3 == 99, NA, EOP2018$E17_3)
table(EOP2018$intef3)

EOP2018$intef4 <- ifelse(EOP2018$E17_4 == 99, NA, EOP2018$E17_4)
table(EOP2018$intef4)

EOP2018$intef5 <- ifelse(EOP2018$E17_5 == 99, NA, EOP2018$E17_5)
table(EOP2018$intef5)
        
## External Efficacy (NAs)

table(EOP2018$E18_1)
table(EOP2018$E18_2)
table(EOP2018$E18_3)
table(EOP2018$E18_4)

EOP2018$extef1 <- ifelse(EOP2018$E18_1 == 99, NA, EOP2018$E18_1)
table(EOP2018$extef1)

EOP2018$extef2 <- ifelse(EOP2018$E18_2 == 99, NA, EOP2018$E18_2)
table(EOP2018$extef2)

EOP2018$extef3 <- ifelse(EOP2018$E18_3 == 99, NA, EOP2018$E18_3)
table(EOP2018$extef3)

EOP2018$extef4 <- ifelse(EOP2018$E18_4 == 99, NA, EOP2018$E18_4)
table(EOP2018$extef4)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
EOP2018 <- EOP2018 %>%
  mutate(across(c(intef1, intef3, extef1, extef3, extef4), ~ 6 - .x))

table(EOP2018$extef4)


## Online Political Efficacy

table(EOP2018$E19_1)
table(EOP2018$E19_2)
table(EOP2018$E19_3)
table(EOP2018$E19_4)

EOP2018$ope1 <- ifelse(EOP2018$E19_1 == 99, NA, EOP2018$E19_1)
EOP2018$ope2 <- ifelse(EOP2018$E19_2 == 99, NA, EOP2018$E19_2)
EOP2018$ope3 <- ifelse(EOP2018$E19_3 == 99, NA, EOP2018$E19_3)
EOP2018$ope4 <- ifelse(EOP2018$E19_4 == 99, NA, EOP2018$E19_4)

table(EOP2018$ope1)
table(EOP2018$ope2)
table(EOP2018$ope3)
table(EOP2018$ope4)


## Political Interest

table(EOP2018$E7_1)
table(EOP2018$E7_2)
table(EOP2018$E7_3)
table(EOP2018$E7_4)
table(EOP2018$E7_5)

EOP2018$polint1 <- ifelse(EOP2018$E7_1 == 99, NA, EOP2018$E7_1)
EOP2018$polint2 <- ifelse(EOP2018$E7_2 == 99, NA, EOP2018$E7_2)
EOP2018$polint3 <- ifelse(EOP2018$E7_3 == 99, NA, EOP2018$E7_3)
EOP2018$polint4 <- ifelse(EOP2018$E7_4 == 99, NA, EOP2018$E7_4)
EOP2018$polint5 <- ifelse(EOP2018$E7_5 == 99, NA, EOP2018$E7_5)

table(EOP2018$polint1)
table(EOP2018$polint2)
table(EOP2018$polint3)
table(EOP2018$polint4)
table(EOP2018$polint5)


## Political Knowledge

table(EOP2018$B8_34)
table(EOP2018$B8_35)
table(EOP2018$B8_36)
table(EOP2018$B8_37)
table(EOP2018$B8_38)
table(EOP2018$B8_39)


EOP2018 <- EOP2018 %>%
  mutate(B8_34 = replace_na(B8_34, 0)) %>%
  mutate(know_sen1 = ifelse(B8_34 == 34, 1, 0))

EOP2018 <- EOP2018 %>%
  mutate(B8_35 = replace_na(B8_35, 0)) %>%
  mutate(know_sen2 = ifelse(B8_35 == 35, 1, 0))

EOP2018 <- EOP2018 %>%
  mutate(B8_36 = replace_na(B8_36, 0)) %>%
  mutate(know_sen3 = ifelse(B8_36 == 36, 1, 0))

EOP2018 <- EOP2018 %>%
  mutate(B8_37 = replace_na(B8_37, 0)) %>%
  mutate(know_sen4 = ifelse(B8_37 == 37, 1, 0))

EOP2018 <- EOP2018 %>%
  mutate(B8_38 = replace_na(B8_38, 0)) %>%
  mutate(know_sen5 = ifelse(B8_38 == 38, 1, 0))

EOP2018 <- EOP2018 %>%
  mutate(B8_39 = replace_na(B8_39, 0)) %>%
  mutate(know_int = ifelse(B8_39 == 39, 1, 0))

table(EOP2018$know_sen1)
table(EOP2018$know_sen2)
table(EOP2018$know_sen3)
table(EOP2018$know_sen4)
table(EOP2018$know_sen5)
table(EOP2018$know_int)

EOP2018$polknowledge <- EOP2018$know_sen1 + EOP2018$know_sen2 + EOP2018$know_sen3 + EOP2018$know_sen4 + EOP2018$know_sen5 + EOP2018$know_int
summary(EOP2018$polknowledge)


## Real Political Knowledge

table(EOP2018$E9)
table(EOP2018$E10)
table(EOP2018$E11)
table(EOP2018$E12)
table(EOP2018$E13)
table(EOP2018$E14)
table(EOP2018$E15)
table(EOP2018$E16)

EOP2018$realknow1 <- ifelse(EOP2018$E9 == 1, 1, 0)
EOP2018$realknow2 <- ifelse(EOP2018$E10 == 1, 1, 0)
EOP2018$realknow3 <- ifelse(EOP2018$E11 == 1, 1, 0)
EOP2018$realknow4 <- ifelse(EOP2018$E12 == 1, 1, 0)
EOP2018$realknow5 <- ifelse(EOP2018$E13 == 1, 1, 0)
EOP2018$realknow6 <- ifelse(EOP2018$E14 == 1, 1, 0)
EOP2018$realknow7 <- ifelse(EOP2018$E15 == 1, 1, 0)
EOP2018$realknow8 <- ifelse(EOP2018$E16 == 1, 1, 0)

table(EOP2018$realknow1)
table(EOP2018$realknow2)
table(EOP2018$realknow3)
table(EOP2018$realknow4)
table(EOP2018$realknow5)
table(EOP2018$realknow6)
table(EOP2018$realknow7)
table(EOP2018$realknow8)

## Ideology

table(EOP2018$Z10)

EOP2018$righ <- NULL

EOP2018$right <- ifelse(EOP2018$Z10 == 1 | EOP2018$Z10 == 2, 1 , 0)
EOP2018$center <- ifelse(EOP2018$Z10 == 3, 1 , 0)
EOP2018$left <- ifelse(EOP2018$Z10 == 4 | EOP2018$Z10 == 5, 1 , 0)

table(EOP2018$right)
table(EOP2018$left)
table(EOP2018$center)


## Participation

table(EOP2018$E25_1)
table(EOP2018$E25_2)
table(EOP2018$E25_3)
table(EOP2018$E25_4)
table(EOP2018$E25_5)
table(EOP2018$E25_6)
table(EOP2018$E25_7)
table(EOP2018$E25_8)
table(EOP2018$E25_9)
table(EOP2018$E25_10)
table(EOP2018$E25_11)
table(EOP2018$E25_12)

EOP2018 <- EOP2018 %>%
  mutate(across(starts_with("E25_"), 
                ~ ifelse(.x == 99, NA, ifelse(.x == 1, 1, 0)), 
                .names = "part{str_remove(.col, 'E25_')}"))

EOP2018$participation <- EOP2018$part1 + EOP2018$part2 + EOP2018$part3 + EOP2018$part4 + EOP2018$part5 + EOP2018$part6 + EOP2018$part7 + EOP2018$part8 + EOP2018$part9 + EOP2018$part10 + EOP2018$part11 + EOP2018$part12
summary(EOP2018$participation)

## Last year participation

table(EOP2018$E26_1)
table(EOP2018$E26_2)
table(EOP2018$E26_3)
table(EOP2018$E26_4)
table(EOP2018$E26_5)
table(EOP2018$E26_6)
table(EOP2018$E26_7)
table(EOP2018$E26_8)
table(EOP2018$E26_9)
table(EOP2018$E26_10)
table(EOP2018$E26_11)
table(EOP2018$E26_12)

EOP2018 <- EOP2018 %>%
  mutate(across(starts_with("E26_"), 
                ~ ifelse(.x == 99, NA, ifelse(.x == 1, 1, 0)), 
                .names = "lyearpart{str_remove(.col, 'E26_')}"))

EOP2018$lyearpart <- EOP2018$lyearpart1 + EOP2018$lyearpart2 + EOP2018$lyearpart3 + EOP2018$lyearpart4 + EOP2018$lyearpart5 + EOP2018$lyearpart6 + EOP2018$lyearpart7 + EOP2018$lyearpart8 + EOP2018$lyearpart9 + EOP2018$lyearpart10 + EOP2018$lyearpart11 + EOP2018$lyearpart12
summary(EOP2018$lyearpart)
table(EOP2018$lyearpart)

# Ideational Cleavage

table(EOP2018$C8_1)
table(EOP2018$C8_2)
table(EOP2018$C8_3)
table(EOP2018$C8_4)
table(EOP2018$C8_5)
table(EOP2018$C8_6)

EOP2018$idea1 <- EOP2018$C8_1
EOP2018$idea2 <- EOP2018$C8_2
EOP2018$idea3 <- EOP2018$C8_3
EOP2018$idea4 <- EOP2018$C8_4
EOP2018$idea5 <- EOP2018$C8_5
EOP2018$idea6 <- EOP2018$C8_6


# Social Media Use

table(EOP2018$E4_1)
table(EOP2018$E4_2)
table(EOP2018$E4_3)
table(EOP2018$E4_4)
table(EOP2018$E4_5)
table(EOP2018$E4_6)
table(EOP2018$E4_7)
table(EOP2018$E4_8)
table(EOP2018$E4_9)

EOP2018 <- EOP2018 %>%
  mutate(across(starts_with("E4_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "use{str_remove(.col, 'E4_')}"))

lapply(EOP2018 %>% select(starts_with("use")), table)


#**************************************************************************************************************************/
##########  EOP 2019  ##########################################################
#**************************************************************************************************************************/

#Import Data

EOP2019 <- read_dta("EOP2019.dta")
View(EOP2019)

# Year
EOP2019$year <- 2019

# Weight

EOP2019$weight <- EOP2019$PONDERADOR_2019

######## 2019 - SOCIODEMOGRAPHIC VARIABLES ########

# Sex (NAs and create dummy =1 if women)

EOP2019 <- EOP2019%>%
  mutate(sex = ifelse(A5 == 2, 1,
                      ifelse(A5 == 1, 0, NA)))
table(EOP2019$A5)
table(EOP2019$sex)

# Age

EOP2019$age <- EOP2019$A4
summary(EOP2019$A4)
summary(EOP2019$age)

## Socioeconomic Status (1 highest)
EOP2019$SES <- ifelse(EOP2019$GSEX == 99, NA, EOP2019$GSEX)
table(EOP2019$GSEX)
table(EOP2019$SES)

# recoding

EOP2019 <- EOP2019 %>%
  mutate(SES = case_when(
    SES == 1 ~ 5,
    SES == 2 ~ 4,
    SES == 4 ~ 2,
    SES == 5 ~ 1,
    TRUE ~ SES  # This line keeps all other values as they are
  ))

table(EOP2019$SES)


# Education Z2 (NAs)

EOP2019$education <- EOP2019$Z3

table(EOP2019$Z3)
table(EOP2019$education)

EOP2019$householdeduc <- ifelse(EOP2019$Z4 == 11, NA, EOP2019$Z4)
table(EOP2019$Z4)  
table(EOP2019$householdeduc)

# Living in the periphery

EOP2019$periphery <- ifelse(EOP2019$S1 == 1 | EOP2019$S1 == 2 | EOP2019$S1 == 3 | EOP2019$S1 == 4 | EOP2019$S1 == 5 | EOP2019$S1 == 9 | EOP2019$S1 == 10, 1, 0)
table(EOP2019$periphery)
table(EOP2019$S1)

# Internet Access

table(EOP2019$Z10_1)
table(EOP2019$Z10_2)
table(EOP2019$Z10_3)

EOP2019 <- EOP2019%>%
  mutate(inthome = ifelse(Z10_1 == 1, 1,
                          ifelse(Z10_1 == 2, 0, NA)))
EOP2019 <- EOP2019%>%
  mutate(intmobile = ifelse(Z10_2 == 1, 1,
                            ifelse(Z10_2 == 2, 0, NA)))
EOP2019 <- EOP2019%>%
  mutate(intwork = ifelse(Z10_3 == 1, 1,
                          ifelse(Z10_3 == 2, 0, NA)))

table(EOP2019$inthome)
table(EOP2019$intmobile)
table(EOP2019$intwork)

######## 2019 - EVALUATION OF AUTHORITIES ########

## Presidential Approval

table(EOP2019$B1)
EOP2019 <- EOP2019%>%
  mutate(presapp = ifelse(EOP2019$B1 == 1, 1,
                          ifelse(EOP2019$B1 == 99, NA, 0)))
table (EOP2019$presapp)

## Government Approval

table(EOP2019$B2)
EOP2019 <- EOP2019%>%
  mutate(govapp = ifelse(EOP2019$B2 == 1, 1,
                          ifelse(EOP2019$B2 == 99, NA, 0)))
table (EOP2019$govapp)

## Country Present

table(EOP2019$B3)
EOP2019 <- EOP2019%>%
  mutate(progress = ifelse(EOP2019$B3 == 1, 1,
                         ifelse(EOP2019$B3 == 99, NA, 0)))
EOP2019 <- EOP2019%>%
  mutate(stagnant = ifelse(EOP2019$B3 == 2, 1,
                         ifelse(EOP2019$B3 == 99, NA, 0)))
EOP2019 <- EOP2019%>%
  mutate(decline = ifelse(EOP2019$B3 == 3, 1,
                         ifelse(EOP2019$B3 == 99, NA, 0)))

table (EOP2019$progress)
table (EOP2019$stagnant)
table (EOP2019$decline)


## Country Future
table(EOP2019$B4)
EOP2019 <- EOP2019%>%
  mutate(better = ifelse(EOP2019$B4 == 1, 1,
                           ifelse(EOP2019$B4 == 99, NA, 0)))
EOP2019 <- EOP2019%>%
  mutate(equal = ifelse(EOP2019$B4 == 2, 1,
                           ifelse(EOP2019$B4 == 99, NA, 0)))
EOP2019 <- EOP2019%>%
  mutate(worse = ifelse(EOP2019$B4 == 3, 1,
                          ifelse(EOP2019$B4 == 99, NA, 0)))
table (EOP2019$better)
table (EOP2019$equal)
table (EOP2019$worse)

## Quality of life

table(EOP2019$B7_1)
table(EOP2019$B7_2)
EOP2019$mebetter <- ifelse(EOP2019$B7_1 == 99, NA, EOP2019$B7_1)
EOP2019$childrenbetter <- ifelse(EOP2019$B7_2 == 99, NA, EOP2019$B7_2)
table(EOP2019$mebetter)
table(EOP2019$childrenbetter)

######## 2019 - POLITICAL ENGAGEMENT ########

## Trust
lapply(EOP2019 %>% select(starts_with("D2_")), table)

EOP2019 <- EOP2019 %>%
  mutate(across(starts_with("D2_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "trust{str_remove(.col, 'D2_')}"))

# Trust in Media was not considered before
EOP2019$trust18 <- NULL

lapply(EOP2019 %>% select(starts_with("trust")), table)

## Corruption 

lapply(EOP2019 %>% select(starts_with("D13_")), table)

EOP2019 <- EOP2019 %>%
  mutate(across(starts_with("D13_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "corruption{str_remove(.col, 'D13_')}"))

lapply(EOP2019 %>% select(starts_with("corruption")), table)

# recoding, so 5 is more corrupted

EOP2019 <- EOP2019 %>%
  mutate(across(starts_with("corruption"), ~ 6 - .x))
lapply(EOP2019 %>% select(starts_with("corruption")), table)



## Rational Approval of Demcoracy

table(EOP2019$B5)

EOP2019 <- EOP2019%>%
  mutate(democracy = ifelse(EOP2019$B5 == 1, 1,
                         ifelse(EOP2019$B5 == 99, NA, 0)))
EOP2019 <- EOP2019%>%
  mutate(authoritarianism = ifelse(EOP2019$B5 == 2, 1,
                            ifelse(EOP2019$B5 == 99, NA, 0)))
EOP2019 <- EOP2019%>%
  mutate(nocare = ifelse(EOP2019$B5 == 3, 1,
                            ifelse(EOP2019$B5 == 99, NA, 0)))

table (EOP2019$democracy)
table (EOP2019$authoritarianism)
table (EOP2019$nocare)

## Satisfaction with democracy

table(EOP2019$B6)
EOP2019$satisdemoc <- ifelse(EOP2019$B6 == 99, NA, EOP2019$B6)
table(EOP2019$satisdemoc)

## Internal Efficacy (NAs)

table(EOP2019$E3_1)
table(EOP2019$E3_2)
table(EOP2019$E3_3)
table(EOP2019$E3_4)
table(EOP2019$E3_5)

EOP2019$intef1 <- ifelse(EOP2019$E3_1 == 99, NA, EOP2019$E3_1)
table(EOP2019$intef1)

EOP2019$intef2 <- ifelse(EOP2019$E3_2 == 99, NA, EOP2019$E3_2)
table(EOP2019$intef2)

EOP2019$intef3 <- ifelse(EOP2019$E3_3 == 99, NA, EOP2019$E3_3)
table(EOP2019$intef3)

EOP2019$intef4 <- ifelse(EOP2019$E3_4 == 99, NA, EOP2019$E3_4)
table(EOP2019$intef4)

EOP2019$intef5 <- ifelse(EOP2019$E3_5 == 99, NA, EOP2019$E3_5)
table(EOP2019$intef5)

## External Efficacy (NAs)

table(EOP2019$E4_1)
table(EOP2019$E4_2)
table(EOP2019$E4_3)
table(EOP2019$E4_4)

EOP2019$extef1 <- ifelse(EOP2019$E4_1 == 99, NA, EOP2019$E4_1)
table(EOP2019$extef1)

EOP2019$extef2 <- ifelse(EOP2019$E4_2 == 99, NA, EOP2019$E4_2)
table(EOP2019$extef2)

EOP2019$extef3 <- ifelse(EOP2019$E4_3 == 99, NA, EOP2019$E4_3)
table(EOP2019$extef3)

EOP2019$extef4 <- ifelse(EOP2019$E4_4 == 99, NA, EOP2019$E4_4)
table(EOP2019$extef4)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
EOP2019 <- EOP2019 %>%
  mutate(across(c(intef1, intef3, extef1, extef3, extef4), ~ 6 - .x))

table(EOP2019$extef4)


## Online Political Efficacy

table(EOP2019$F5_1)
table(EOP2019$F5_2)
table(EOP2019$F5_3)
table(EOP2019$F5_4)

EOP2019$ope1 <- ifelse(EOP2019$F5_1 == 99, NA, EOP2019$F5_1)
EOP2019$ope2 <- ifelse(EOP2019$F5_2 == 99, NA, EOP2019$F5_2)
EOP2019$ope3 <- ifelse(EOP2019$F5_3 == 99, NA, EOP2019$F5_3)
EOP2019$ope4 <- ifelse(EOP2019$F5_4 == 99, NA, EOP2019$F5_4)

table(EOP2019$ope1)
table(EOP2019$ope2)
table(EOP2019$ope3)
table(EOP2019$ope4)


## Political Interest

table(EOP2019$E1_1)
table(EOP2019$E1_2)
table(EOP2019$E1_3)
table(EOP2019$E1_4)
table(EOP2019$E1_5)

EOP2019$polint1 <- ifelse(EOP2019$E1_1 == 99, NA, EOP2019$E1_1)
EOP2019$polint2 <- ifelse(EOP2019$E1_2 == 99, NA, EOP2019$E1_2)
EOP2019$polint3 <- ifelse(EOP2019$E1_3 == 99, NA, EOP2019$E1_3)
EOP2019$polint4 <- ifelse(EOP2019$E1_4 == 99, NA, EOP2019$E1_4)
EOP2019$polint5 <- ifelse(EOP2019$E1_5 == 99, NA, EOP2019$E1_5)

table(EOP2019$polint1)
table(EOP2019$polint2)
table(EOP2019$polint3)
table(EOP2019$polint4)
table(EOP2019$polint5)


## Political Knowledge

table(EOP2019$B8_34)
table(EOP2019$B8_35)
table(EOP2019$B8_36)
table(EOP2019$B8_37)
table(EOP2019$B8_38)
table(EOP2019$B8_39)


EOP2019 <- EOP2019 %>%
  mutate(B8_34 = replace_na(B8_34, 0)) %>%
  mutate(know_sen1 = ifelse(B8_34 == 34, 1, 0))

EOP2019 <- EOP2019 %>%
  mutate(B8_35 = replace_na(B8_35, 0)) %>%
  mutate(know_sen2 = ifelse(B8_35 == 35, 1, 0))

EOP2019 <- EOP2019 %>%
  mutate(B8_36 = replace_na(B8_36, 0)) %>%
  mutate(know_sen3 = ifelse(B8_36 == 36, 1, 0))

EOP2019 <- EOP2019 %>%
  mutate(B8_37 = replace_na(B8_37, 0)) %>%
  mutate(know_sen4 = ifelse(B8_37 == 37, 1, 0))

EOP2019 <- EOP2019 %>%
  mutate(B8_38 = replace_na(B8_38, 0)) %>%
  mutate(know_sen5 = ifelse(B8_38 == 38, 1, 0))

EOP2019 <- EOP2019 %>%
  mutate(B8_39 = replace_na(B8_39, 0)) %>%
  mutate(know_int = ifelse(B8_39 == 39, 1, 0))

table(EOP2019$know_sen1)
table(EOP2019$know_sen2)
table(EOP2019$know_sen3)
table(EOP2019$know_sen4)
table(EOP2019$know_sen5)
table(EOP2019$know_int)

EOP2019$polknowledge <- EOP2019$know_sen1 + EOP2019$know_sen2 + EOP2019$know_sen3 + EOP2019$know_sen4 + EOP2019$know_sen5 + EOP2019$know_int
summary(EOP2019$polknowledge)



## Ideology

table(EOP2019$Z11)

EOP2019$right <- ifelse(EOP2019$Z11 == 1 | EOP2019$Z11 == 2, 1 , 0)
EOP2019$center <- ifelse(EOP2019$Z11 == 3, 1 , 0)
EOP2019$left <- ifelse(EOP2019$Z11 == 4 | EOP2019$Z11 == 5, 1 , 0)

table(EOP2019$right)
table(EOP2019$left)
table(EOP2019$center)


## Participation

table(EOP2019$E9_1)
table(EOP2019$E9_2)
table(EOP2019$E9_3)
table(EOP2019$E9_4)
table(EOP2019$E9_5)
table(EOP2019$E9_6)
table(EOP2019$E9_7)
table(EOP2019$E9_8)
table(EOP2019$E9_9)
table(EOP2019$E9_10)
table(EOP2019$E9_11)
table(EOP2019$E9_12)

EOP2019 <- EOP2019 %>%
  mutate(across(starts_with("E9_"), 
                ~ ifelse(.x == 99, NA, ifelse(.x == 1, 1, 0)), 
                .names = "part{str_remove(.col, 'E9_')}"))

lapply(EOP2019 %>% select(starts_with("part")), table)


EOP2019$participation <- EOP2019$part1 + EOP2019$part2 + EOP2019$part3 + EOP2019$part4 + EOP2019$part5 + EOP2019$part6 + EOP2019$part7 + EOP2019$part8 + EOP2019$part9 + EOP2019$part10 + EOP2019$part11 + EOP2019$part12
summary(EOP2019$participation)


## Ideational Cleavage / In 2019 data there is a problem with this variables, values are 2-6, and not 1-5.

table(EOP2019$C4_1)
table(EOP2019$C4_2)
table(EOP2019$C4_3)
table(EOP2019$C4_4)
table(EOP2019$C4_5)
table(EOP2019$C4_6)

EOP2019$idea1 <- EOP2019$C4_1
EOP2019$idea2 <- EOP2019$C4_2
EOP2019$idea3 <- EOP2019$C4_3
EOP2019$idea4 <- EOP2019$C4_4
EOP2019$idea5 <- EOP2019$C4_5
EOP2019$idea6 <- EOP2019$C4_6

# Solving the problem

EOP2019 <- EOP2019 %>%
  mutate(across(starts_with("idea"), ~ .x - 1))

lapply(EOP2019 %>% select(starts_with("idea")), table)



## Social Media Use

table(EOP2019$F3_1)
table(EOP2019$F3_2)
table(EOP2019$F3_3)
table(EOP2019$F3_4)
table(EOP2019$F3_5)
table(EOP2019$F3_6)
table(EOP2019$F3_7)
table(EOP2019$F3_8)
table(EOP2019$F3_9)

EOP2019 <- EOP2019 %>%
  mutate(across(starts_with("F3_"), 
                ~ ifelse(.x == 99 | .x == 97, NA, .x), 
                .names = "use{str_remove(.col, 'F3_')}"))

lapply(EOP2019 %>% select(starts_with("use")), table)





#**************************************************************************************************************************/
##########  EOP 2020  ##########################################################
#**************************************************************************************************************************/

#Import Data

EOP2020 <- read_dta("EOP2020.dta")
View(EOP2020)

# Year
EOP2020$year <- 2020

# Weight

EOP2020$weight <- EOP2020$PONDERADOR_2020

######## 2020 - SOCIODEMOGRAPHIC VARIABLES ########

# Sex (NAs and create dummy =1 if women)

EOP2020 <- EOP2020%>%
  mutate(sex = ifelse(A5 == 2, 1,
                      ifelse(A5 == 1, 0, NA)))
table(EOP2020$A5)
table(EOP2020$sex)

# Age

EOP2020$age <- EOP2020$A4
summary(EOP2020$A4)
summary(EOP2020$age)

## Socioeconomic Status (1 highest)
EOP2020$SES <- ifelse(EOP2020$GSEX == 99, NA, EOP2020$GSEX)
table(EOP2020$GSEX)
table(EOP2020$SES)

# recoding

EOP2020 <- EOP2020 %>%
  mutate(SES = case_when(
    SES == 1 ~ 5,
    SES == 2 ~ 4,
    SES == 4 ~ 2,
    SES == 5 ~ 1,
    TRUE ~ SES  # This line keeps all other values as they are
  ))

table(EOP2020$SES)


# Education Z2 (NAs)

EOP2020$education <- ifelse(EOP2020$Z3 == 99, NA, EOP2020$Z3)

table(EOP2020$Z3)
table(EOP2020$education)

EOP2020$householdeduc <- ifelse(EOP2020$Z4 == 99, NA, EOP2020$Z4)
table(EOP2020$Z4)  
table(EOP2020$householdeduc)

# Living in the periphery

EOP2020$periphery <- ifelse(EOP2020$S1 == 1 | EOP2020$S1 == 2 | EOP2020$S1 == 3 | EOP2020$S1 == 4 | EOP2020$S1 == 5 | EOP2020$S1 == 9 | EOP2020$S1 == 10, 1, 0)
table(EOP2020$periphery)
table(EOP2020$S1)

# Internet Access

table(EOP2020$Z10_1)
table(EOP2020$Z10_2)
table(EOP2020$Z10_3)

EOP2020 <- EOP2020%>%
  mutate(inthome = ifelse(Z10_1 == 1, 1,
                          ifelse(Z10_1 == 2, 0, NA)))
EOP2020 <- EOP2020%>%
  mutate(intmobile = ifelse(Z10_2 == 1, 1,
                            ifelse(Z10_2 == 2, 0, NA)))
EOP2020 <- EOP2020%>%
  mutate(intwork = ifelse(Z10_3 == 1, 1,
                          ifelse(Z10_3 == 2, 0, NA)))

table(EOP2020$inthome)
table(EOP2020$intmobile)
table(EOP2020$intwork)

######## 2020 - EVALUATION OF AUTHORITIES ########

## Presidential Approval

table(EOP2020$B1)
EOP2020 <- EOP2020%>%
  mutate(presapp = ifelse(EOP2020$B1 == 1, 1,
                          ifelse(EOP2020$B1 == 99, NA, 0)))
table (EOP2020$presapp)

## Government Approval

table(EOP2020$B2)
EOP2020 <- EOP2020%>%
  mutate(govapp = ifelse(EOP2020$B2 == 1, 1,
                         ifelse(EOP2020$B2 == 99, NA, 0)))
table (EOP2020$govapp)

## Country Present

table(EOP2020$B3)
EOP2020 <- EOP2020%>%
  mutate(progress = ifelse(EOP2020$B3 == 1, 1,
                           ifelse(EOP2020$B3 == 99, NA, 0)))
EOP2020 <- EOP2020%>%
  mutate(stagnant = ifelse(EOP2020$B3 == 2, 1,
                           ifelse(EOP2020$B3 == 99, NA, 0)))
EOP2020 <- EOP2020%>%
  mutate(decline = ifelse(EOP2020$B3 == 3, 1,
                          ifelse(EOP2020$B3 == 99, NA, 0)))

table (EOP2020$progress)
table (EOP2020$stagnant)
table (EOP2020$decline)


## Country Future
table(EOP2020$B4)
EOP2020 <- EOP2020%>%
  mutate(better = ifelse(EOP2020$B4 == 1, 1,
                         ifelse(EOP2020$B4 == 99, NA, 0)))
EOP2020 <- EOP2020%>%
  mutate(equal = ifelse(EOP2020$B4 == 2, 1,
                        ifelse(EOP2020$B4 == 99, NA, 0)))
EOP2020 <- EOP2020%>%
  mutate(worse = ifelse(EOP2020$B4 == 3, 1,
                        ifelse(EOP2020$B4 == 99, NA, 0)))
table (EOP2020$better)
table (EOP2020$equal)
table (EOP2020$worse)

## Quality of life

table(EOP2020$B7_1)
table(EOP2020$B7_2)
EOP2020$mebetter <- ifelse(EOP2020$B7_1 == 99, NA, EOP2020$B7_1)
EOP2020$childrenbetter <- ifelse(EOP2020$B7_2 == 99, NA, EOP2020$B7_2)
table(EOP2020$mebetter)
table(EOP2020$childrenbetter)

######## 2020 - POLITICAL ENGAGEMENT ########

## Trust
lapply(EOP2020 %>% select(starts_with("D2_")), table)

EOP2020 <- EOP2020 %>%
  mutate(across(starts_with("D2_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "trust{str_remove(.col, 'D2_')}"))

# Trust in Media was not considered before
EOP2020$trust18 <- NULL

lapply(EOP2020 %>% select(starts_with("trust")), table)

## Corruption 

lapply(EOP2020 %>% select(starts_with("D13_")), table)

EOP2020 <- EOP2020 %>%
  mutate(across(starts_with("D13_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "corruption{str_remove(.col, 'D13_')}"))

lapply(EOP2020 %>% select(starts_with("corruption")), table)

# recoding, so 5 is more corrupted

EOP2020 <- EOP2020 %>%
  mutate(across(starts_with("corruption"), ~ 6 - .x))
lapply(EOP2020 %>% select(starts_with("corruption")), table)



## Rational Approval of Demcoracy

table(EOP2020$B5)

EOP2020 <- EOP2020%>%
  mutate(democracy = ifelse(EOP2020$B5 == 1, 1,
                            ifelse(EOP2020$B5 == 99, NA, 0)))
EOP2020 <- EOP2020%>%
  mutate(authoritarianism = ifelse(EOP2020$B5 == 2, 1,
                                   ifelse(EOP2020$B5 == 99, NA, 0)))
EOP2020 <- EOP2020%>%
  mutate(nocare = ifelse(EOP2020$B5 == 3, 1,
                         ifelse(EOP2020$B5 == 99, NA, 0)))

table(EOP2020$democracy)
table(EOP2020$authoritarianism)
table(EOP2020$nocare)

## Satisfaction with democracy

table(EOP2020$B6)
EOP2020$satisdemoc <- ifelse(EOP2020$B6 == 99, NA, EOP2020$B6)
table(EOP2020$satisdemoc)

## Internal Efficacy (NAs)

table(EOP2020$E3_1)
table(EOP2020$E3_2)
table(EOP2020$E3_3)
table(EOP2020$E3_4)
table(EOP2020$E3_5)

EOP2020$intef1 <- ifelse(EOP2020$E3_1 == 99, NA, EOP2020$E3_1)
table(EOP2020$intef1)

EOP2020$intef2 <- ifelse(EOP2020$E3_2 == 99, NA, EOP2020$E3_2)
table(EOP2020$intef2)

EOP2020$intef3 <- ifelse(EOP2020$E3_3 == 99, NA, EOP2020$E3_3)
table(EOP2020$intef3)

EOP2020$intef4 <- ifelse(EOP2020$E3_4 == 99, NA, EOP2020$E3_4)
table(EOP2020$intef4)

EOP2020$intef5 <- ifelse(EOP2020$E3_5 == 99, NA, EOP2020$E3_5)
table(EOP2020$intef5)

## External Efficacy (NAs)

table(EOP2020$E4_1)
table(EOP2020$E4_2)
table(EOP2020$E4_3)
table(EOP2020$E4_4)

EOP2020$extef1 <- ifelse(EOP2020$E4_1 == 99, NA, EOP2020$E4_1)
table(EOP2020$extef1)

EOP2020$extef2 <- ifelse(EOP2020$E4_2 == 99, NA, EOP2020$E4_2)
table(EOP2020$extef2)

EOP2020$extef3 <- ifelse(EOP2020$E4_3 == 99, NA, EOP2020$E4_3)
table(EOP2020$extef3)

EOP2020$extef4 <- ifelse(EOP2020$E4_4 == 99, NA, EOP2020$E4_4)
table(EOP2020$extef4)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
EOP2020 <- EOP2020 %>%
  mutate(across(c(intef1, intef3, extef1, extef3, extef4), ~ 6 - .x))

table(EOP2020$extef4)


## Online Political Efficacy

table(EOP2020$F5_1)
table(EOP2020$F5_2)
table(EOP2020$F5_3)
table(EOP2020$F5_4)

EOP2020$ope1 <- ifelse(EOP2020$F5_1 == 99, NA, EOP2020$F5_1)
EOP2020$ope2 <- ifelse(EOP2020$F5_2 == 99, NA, EOP2020$F5_2)
EOP2020$ope3 <- ifelse(EOP2020$F5_3 == 99, NA, EOP2020$F5_3)
EOP2020$ope4 <- ifelse(EOP2020$F5_4 == 99, NA, EOP2020$F5_4)

table(EOP2020$ope1)
table(EOP2020$ope2)
table(EOP2020$ope3)
table(EOP2020$ope4)


## Political Interest

table(EOP2020$E1_1)
table(EOP2020$E1_2)
table(EOP2020$E1_3)
table(EOP2020$E1_4)
table(EOP2020$E1_5)

EOP2020$polint1 <- ifelse(EOP2020$E1_1 == 99, NA, EOP2020$E1_1)
EOP2020$polint2 <- ifelse(EOP2020$E1_2 == 99, NA, EOP2020$E1_2)
EOP2020$polint3 <- ifelse(EOP2020$E1_3 == 99, NA, EOP2020$E1_3)
EOP2020$polint4 <- ifelse(EOP2020$E1_4 == 99, NA, EOP2020$E1_4)
EOP2020$polint5 <- ifelse(EOP2020$E1_5 == 99, NA, EOP2020$E1_5)

table(EOP2020$polint1)
table(EOP2020$polint2)
table(EOP2020$polint3)
table(EOP2020$polint4)
table(EOP2020$polint5)


## Political Knowledge

table(EOP2020$B8_34)
table(EOP2020$B8_35)
table(EOP2020$B8_36)
table(EOP2020$B8_37)
table(EOP2020$B8_38)
table(EOP2020$B8_39)


EOP2020 <- EOP2020 %>%
  mutate(B8_34 = replace_na(B8_34, 0)) %>%
  mutate(know_sen1 = ifelse(B8_34 == 34, 1, 0))

EOP2020 <- EOP2020 %>%
  mutate(B8_35 = replace_na(B8_35, 0)) %>%
  mutate(know_sen2 = ifelse(B8_35 == 35, 1, 0))

EOP2020 <- EOP2020 %>%
  mutate(B8_36 = replace_na(B8_36, 0)) %>%
  mutate(know_sen3 = ifelse(B8_36 == 36, 1, 0))

EOP2020 <- EOP2020 %>%
  mutate(B8_37 = replace_na(B8_37, 0)) %>%
  mutate(know_sen4 = ifelse(B8_37 == 37, 1, 0))

EOP2020 <- EOP2020 %>%
  mutate(B8_38 = replace_na(B8_38, 0)) %>%
  mutate(know_sen5 = ifelse(B8_38 == 38, 1, 0))

EOP2020 <- EOP2020 %>%
  mutate(B8_39 = replace_na(B8_39, 0)) %>%
  mutate(know_int = ifelse(B8_39 == 39, 1, 0))

table(EOP2020$know_sen1)
table(EOP2020$know_sen2)
table(EOP2020$know_sen3)
table(EOP2020$know_sen4)
table(EOP2020$know_sen5)
table(EOP2020$know_int)

EOP2020$polknowledge <- EOP2020$know_sen1 + EOP2020$know_sen2 + EOP2020$know_sen3 + EOP2020$know_sen4 + EOP2020$know_sen5 + EOP2020$know_int
summary(EOP2020$polknowledge)



## Ideology

table(EOP2020$Z11)

EOP2020$right <- ifelse(EOP2020$Z11 == 1 | EOP2020$Z11 == 2, 1 , 0)
EOP2020$center <- ifelse(EOP2020$Z11 == 3, 1 , 0)
EOP2020$left <- ifelse(EOP2020$Z11 == 4 | EOP2020$Z11 == 5, 1 , 0)

table(EOP2020$right)
table(EOP2020$left)
table(EOP2020$center)


## Participation

table(EOP2020$E9_1)
table(EOP2020$E9_2)
table(EOP2020$E9_3)
table(EOP2020$E9_4)
table(EOP2020$E9_5)
table(EOP2020$E9_6)
table(EOP2020$E9_7)
table(EOP2020$E9_8)
table(EOP2020$E9_9)
table(EOP2020$E9_10)
table(EOP2020$E9_11)
table(EOP2020$E9_12)
table(EOP2020$E9_13)
table(EOP2020$E9_14)

EOP2020 <- EOP2020 %>%
  mutate(across(starts_with("E9_"), 
                ~ ifelse(.x == 99, NA, ifelse(.x == 1, 1, 0)), 
                .names = "part{str_remove(.col, 'E9_')}"))

lapply(EOP2020 %>% select(starts_with("part")), table)

# Cacerolazos and cabildos were not considered before
EOP2020$part13 <- NULL
EOP2020$part14 <- NULL

EOP2020$participation <- EOP2020$part1 + EOP2020$part2 + EOP2020$part3 + EOP2020$part4 + EOP2020$part5 + EOP2020$part6 + EOP2020$part7 + EOP2020$part8 + EOP2020$part9 + EOP2020$part10 + EOP2020$part11 + EOP2020$part12
summary(EOP2020$participation)


## Ideational Cleavage / In 2019 data there is a problem with this variables, values are 2-6, and not 1-5.

table(EOP2020$C4_1)
table(EOP2020$C4_2)
table(EOP2020$C4_3)
table(EOP2020$C4_4)
table(EOP2020$C4_5)
table(EOP2020$C4_6)

EOP2020$idea1 <- EOP2020$C4_1
EOP2020$idea2 <- EOP2020$C4_2
EOP2020$idea3 <- EOP2020$C4_3
EOP2020$idea4 <- EOP2020$C4_4
EOP2020$idea5 <- EOP2020$C4_5
EOP2020$idea6 <- EOP2020$C4_6

# Solving the problem

EOP2020 <- EOP2020 %>%
  mutate(across(starts_with("idea"), ~ .x - 1))

lapply(EOP2020 %>% select(starts_with("idea")), table)


## Social Media Use

table(EOP2020$F3_1)
table(EOP2020$F3_2)
table(EOP2020$F3_3)
table(EOP2020$F3_4)
table(EOP2020$F3_5)
table(EOP2020$F3_6)
table(EOP2020$F3_7)
table(EOP2020$F3_8)
table(EOP2020$F3_9)

EOP2020 <- EOP2020 %>%
  mutate(across(starts_with("F3_"), 
                ~ ifelse(.x == 99 | .x == 97, NA, .x), 
                .names = "use{str_remove(.col, 'F3_')}"))

lapply(EOP2020 %>% select(starts_with("use")), table)


#**************************************************************************************************************************/
##########  EOP 2021  ##########################################################
#**************************************************************************************************************************/

#Import Data

EOP2021 <- read_dta("EOP2021.dta")
View(EOP2021)

# Year
EOP2021$year <- 2021

# Weight

EOP2021$weight <- EOP2021$PONDERAD

######## 2021 - SOCIODEMOGRAPHIC VARIABLES ########

# Sex (NAs and create dummy =1 if women)

EOP2021 <- EOP2021%>%
  mutate(sex = ifelse(A5 == 2, 1,
                      ifelse(A5 == 1, 0, NA)))
table(EOP2021$A5)
table(EOP2021$sex)

# Age

EOP2021$age <- EOP2021$A4
summary(EOP2021$A4)
summary(EOP2021$age)

## Socioeconomic Status (1 highest)
EOP2021$SES <- ifelse(EOP2021$GSEX == 99, NA, EOP2021$GSEX)
table(EOP2021$GSEX)
table(EOP2021$SES)

# recoding

EOP2021 <- EOP2021 %>%
  mutate(SES = case_when(
    SES == 1 ~ 5,
    SES == 2 ~ 4,
    SES == 4 ~ 2,
    SES == 5 ~ 1,
    TRUE ~ SES  # This line keeps all other values as they are
  ))

table(EOP2021$SES)


# Education (NAs)

EOP2021$education <- ifelse(EOP2021$Z3 == 11, NA, EOP2021$Z3)

table(EOP2021$Z3)
table(EOP2021$education)

EOP2021$householdeduc <- ifelse(EOP2021$Z4 == 11, NA, EOP2021$Z4)
table(EOP2021$Z4)  
table(EOP2021$householdeduc)

# Living in the periphery

EOP2021$periphery <- ifelse(EOP2021$S1 == 1 | EOP2021$S1 == 2 | EOP2021$S1 == 3 | EOP2021$S1 == 4 | EOP2021$S1 == 5 | EOP2021$S1 == 9 | EOP2021$S1 == 10, 1, 0)
table(EOP2021$periphery)
table(EOP2021$S1)

# Internet Access

table(EOP2021$Z10_1)
table(EOP2021$Z10_2)
table(EOP2021$Z10_3)

EOP2021 <- EOP2021%>%
  mutate(inthome = ifelse(Z10_1 == 1, 1,
                          ifelse(Z10_1 == 2, 0, NA)))
EOP2021 <- EOP2021%>%
  mutate(intmobile = ifelse(Z10_2 == 1, 1,
                            ifelse(Z10_2 == 2, 0, NA)))
EOP2021 <- EOP2021%>%
  mutate(intwork = ifelse(Z10_3 == 1, 1,
                          ifelse(Z10_3 == 2, 0, NA)))

table(EOP2021$inthome)
table(EOP2021$intmobile)
table(EOP2021$intwork)

######## 2021 - EVALUATION OF AUTHORITIES ########

## Presidential Approval

table(EOP2021$B1)
EOP2021 <- EOP2021%>%
  mutate(presapp = ifelse(EOP2021$B1 == 1, 1,
                          ifelse(EOP2021$B1 == 99, NA, 0)))
table(EOP2021$presapp)

## Government Approval

table(EOP2021$B2)
EOP2021 <- EOP2021%>%
  mutate(govapp = ifelse(EOP2021$B2 == 1, 1,
                         ifelse(EOP2021$B2 == 99, NA, 0)))
table (EOP2021$govapp)

## Country Present

table(EOP2021$B3)
EOP2021 <- EOP2021%>%
  mutate(progress = ifelse(EOP2021$B3 == 1, 1,
                           ifelse(EOP2021$B3 == 99, NA, 0)))
EOP2021 <- EOP2021%>%
  mutate(stagnant = ifelse(EOP2021$B3 == 2, 1,
                           ifelse(EOP2021$B3 == 99, NA, 0)))
EOP2021 <- EOP2021%>%
  mutate(decline = ifelse(EOP2021$B3 == 3, 1,
                          ifelse(EOP2021$B3 == 99, NA, 0)))

table(EOP2021$progress)
table(EOP2021$stagnant)
table(EOP2021$decline)


## Country Future
table(EOP2021$B4)
EOP2021 <- EOP2021%>%
  mutate(better = ifelse(EOP2021$B4 == 1, 1,
                         ifelse(EOP2021$B4 == 99, NA, 0)))
EOP2021 <- EOP2021%>%
  mutate(equal = ifelse(EOP2021$B4 == 2, 1,
                        ifelse(EOP2021$B4 == 99, NA, 0)))
EOP2021 <- EOP2021%>%
  mutate(worse = ifelse(EOP2021$B4 == 3, 1,
                        ifelse(EOP2021$B4 == 99, NA, 0)))
table (EOP2021$better)
table (EOP2021$equal)
table (EOP2021$worse)

## Quality of life

table(EOP2021$B7_1)
table(EOP2021$B7_2)
EOP2021$mebetter <- ifelse(EOP2021$B7_1 == 99, NA, EOP2021$B7_1)
EOP2021$childrenbetter <- ifelse(EOP2021$B7_2 == 99, NA, EOP2021$B7_2)
table(EOP2021$mebetter)
table(EOP2021$childrenbetter)

######## 2021 - POLITICAL ENGAGEMENT ########

## Trust
lapply(EOP2021 %>% select(starts_with("D2_")), table)

EOP2021 <- EOP2021 %>%
  mutate(across(starts_with("D2_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "trust{str_remove(.col, 'D2_')}"))

# Trust in Media was not considered before
EOP2021$trust18 <- NULL

lapply(EOP2021 %>% select(starts_with("trust")), table)

## Corruption 

lapply(EOP2021 %>% select(starts_with("D13_")), table)

EOP2021 <- EOP2021 %>%
  mutate(across(starts_with("D13_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "corruption{str_remove(.col, 'D13_')}"))

lapply(EOP2021 %>% select(starts_with("corruption")), table)

# recoding, so 5 is more corrupted

EOP2021 <- EOP2021 %>%
  mutate(across(starts_with("corruption"), ~ 6 - .x))
lapply(EOP2021 %>% select(starts_with("corruption")), table)



## Rational Approval of Demcoracy

table(EOP2021$B5)

EOP2021 <- EOP2021%>%
  mutate(democracy = ifelse(EOP2021$B5 == 1, 1,
                            ifelse(EOP2021$B5 == 99, NA, 0)))
EOP2021 <- EOP2021%>%
  mutate(authoritarianism = ifelse(EOP2021$B5 == 2, 1,
                                   ifelse(EOP2021$B5 == 99, NA, 0)))
EOP2021 <- EOP2021%>%
  mutate(nocare = ifelse(EOP2021$B5 == 3, 1,
                         ifelse(EOP2021$B5 == 99, NA, 0)))

table(EOP2021$democracy)
table(EOP2021$authoritarianism)
table(EOP2021$nocare)

## Satisfaction with democracy

table(EOP2021$B6)
EOP2021$satisdemoc <- ifelse(EOP2021$B6 == 99, NA, EOP2021$B6)
table(EOP2021$satisdemoc)

## Internal Efficacy (NAs)

table(EOP2021$E2_1)
table(EOP2021$E2_2)
table(EOP2021$E2_3)
table(EOP2021$E2_4)
table(EOP2021$E2_5)

EOP2021$intef1 <- ifelse(EOP2021$E2_1 == 99, NA, EOP2021$E2_1)
table(EOP2021$intef1)

EOP2021$intef2 <- ifelse(EOP2021$E2_2 == 99, NA, EOP2021$E2_2)
table(EOP2021$intef2)

EOP2021$intef3 <- ifelse(EOP2021$E2_3 == 99, NA, EOP2021$E2_3)
table(EOP2021$intef3)

EOP2021$intef4 <- ifelse(EOP2021$E2_4 == 99, NA, EOP2021$E2_4)
table(EOP2021$intef4)

EOP2021$intef5 <- ifelse(EOP2021$E2_5 == 99, NA, EOP2021$E2_5)
table(EOP2021$intef5)

## External Efficacy (NAs)

table(EOP2021$E3_1)
table(EOP2021$E3_2)
table(EOP2021$E3_3)
table(EOP2021$E3_4)

EOP2021$extef1 <- ifelse(EOP2021$E3_1 == 99, NA, EOP2021$E3_1)
table(EOP2021$extef1)

EOP2021$extef2 <- ifelse(EOP2021$E3_2 == 99, NA, EOP2021$E3_2)
table(EOP2021$extef2)

EOP2021$extef3 <- ifelse(EOP2021$E3_3 == 99, NA, EOP2021$E3_3)
table(EOP2021$extef3)

EOP2021$extef4 <- ifelse(EOP2021$E3_4 == 99, NA, EOP2021$E3_4)
table(EOP2021$extef4)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
EOP2021 <- EOP2021 %>%
  mutate(across(c(intef1, intef3, extef1, extef3, extef4), ~ 6 - .x))

table(EOP2021$extef4)


## Online Political Efficacy

table(EOP2021$F5_1)
table(EOP2021$F5_2)
table(EOP2021$F5_3)
table(EOP2021$F5_4)

EOP2021$ope1 <- ifelse(EOP2021$F5_1 == 99, NA, EOP2021$F5_1)
EOP2021$ope2 <- ifelse(EOP2021$F5_2 == 99, NA, EOP2021$F5_2)
EOP2021$ope3 <- ifelse(EOP2021$F5_3 == 99, NA, EOP2021$F5_3)
EOP2021$ope4 <- ifelse(EOP2021$F5_4 == 99, NA, EOP2021$F5_4)

table(EOP2021$ope1)
table(EOP2021$ope2)
table(EOP2021$ope3)
table(EOP2021$ope4)


## Political Interest

table(EOP2021$E1_1)
table(EOP2021$E1_2)
table(EOP2021$E1_3)
table(EOP2021$E1_4)
table(EOP2021$E1_5)

EOP2021$polint1 <- ifelse(EOP2021$E1_1 == 99, NA, EOP2021$E1_1)
EOP2021$polint2 <- ifelse(EOP2021$E1_2 == 99, NA, EOP2021$E1_2)
EOP2021$polint3 <- ifelse(EOP2021$E1_3 == 99, NA, EOP2021$E1_3)
EOP2021$polint4 <- ifelse(EOP2021$E1_4 == 99, NA, EOP2021$E1_4)
EOP2021$polint5 <- ifelse(EOP2021$E1_5 == 99, NA, EOP2021$E1_5)

table(EOP2021$polint1)
table(EOP2021$polint2)
table(EOP2021$polint3)
table(EOP2021$polint4)
table(EOP2021$polint5)


## Political Knowledge

table(EOP2021$B8_34)
table(EOP2021$B8_35)
table(EOP2021$B8_36)
table(EOP2021$B8_37)
table(EOP2021$B8_38)
table(EOP2021$B8_39)


EOP2021 <- EOP2021 %>%
  mutate(B8_34 = replace_na(B8_34, 0)) %>%
  mutate(know_sen1 = ifelse(B8_34 == 34, 1, 0))

EOP2021 <- EOP2021 %>%
  mutate(B8_35 = replace_na(B8_35, 0)) %>%
  mutate(know_sen2 = ifelse(B8_35 == 35, 1, 0))

EOP2021 <- EOP2021 %>%
  mutate(B8_36 = replace_na(B8_36, 0)) %>%
  mutate(know_sen3 = ifelse(B8_36 == 36, 1, 0))

EOP2021 <- EOP2021 %>%
  mutate(B8_37 = replace_na(B8_37, 0)) %>%
  mutate(know_sen4 = ifelse(B8_37 == 37, 1, 0))

EOP2021 <- EOP2021 %>%
  mutate(B8_38 = replace_na(B8_38, 0)) %>%
  mutate(know_sen5 = ifelse(B8_38 == 38, 1, 0))

EOP2021 <- EOP2021 %>%
  mutate(B8_39 = replace_na(B8_39, 0)) %>%
  mutate(know_int = ifelse(B8_39 == 39, 1, 0))

table(EOP2021$know_sen1)
table(EOP2021$know_sen2)
table(EOP2021$know_sen3)
table(EOP2021$know_sen4)
table(EOP2021$know_sen5)
table(EOP2021$know_int)

EOP2021$polknowledge <- EOP2021$know_sen1 + EOP2021$know_sen2 + EOP2021$know_sen3 + EOP2021$know_sen4 + EOP2021$know_sen5 + EOP2021$know_int
summary(EOP2021$polknowledge)



## Ideology

table(EOP2021$Z11)

EOP2021$right <- ifelse(EOP2021$Z11 == 1 | EOP2021$Z11 == 2, 1 , 0)
EOP2021$center <- ifelse(EOP2021$Z11 == 3, 1 , 0)
EOP2021$left <- ifelse(EOP2021$Z11 == 4 | EOP2021$Z11 == 5, 1 , 0)

table(EOP2021$right)
table(EOP2021$left)
table(EOP2021$center)


## Participation

table(EOP2021$E9_1)
table(EOP2021$E9_2)
table(EOP2021$E9_3)
table(EOP2021$E9_4)
table(EOP2021$E9_5)
table(EOP2021$E9_6)
table(EOP2021$E9_7)
table(EOP2021$E9_8)
table(EOP2021$E9_9)
table(EOP2021$E9_10)
table(EOP2021$E9_11)
table(EOP2021$E9_12)
table(EOP2021$E9_13)
table(EOP2021$E9_14)

EOP2021 <- EOP2021 %>%
  mutate(across(starts_with("E9_"), 
                ~ ifelse(.x == 99, NA, ifelse(.x == 1, 1, 0)), 
                .names = "part{str_remove(.col, 'E9_')}"))

lapply(EOP2021 %>% select(starts_with("part")), table)

# Cacerolazos and cabildos were not considered before
EOP2021$part13 <- NULL
EOP2021$part14 <- NULL

EOP2021$participation <- EOP2021$part1 + EOP2021$part2 + EOP2021$part3 + EOP2021$part4 + EOP2021$part5 + EOP2021$part6 + EOP2021$part7 + EOP2021$part8 + EOP2021$part9 + EOP2021$part10 + EOP2021$part11 + EOP2021$part12
summary(EOP2021$participation)


## Ideational Cleavage / In 2019 data there is a problem with this variables, values are 2-6, and not 1-5.

table(EOP2021$C4_1)
table(EOP2021$C4_2)
table(EOP2021$C4_3)
table(EOP2021$C4_4)
table(EOP2021$C4_5)
table(EOP2021$C4_6)

EOP2021$idea1 <- EOP2021$C4_1
EOP2021$idea2 <- EOP2021$C4_2
EOP2021$idea3 <- EOP2021$C4_3
EOP2021$idea4 <- EOP2021$C4_4
EOP2021$idea5 <- EOP2021$C4_5
EOP2021$idea6 <- EOP2021$C4_6

# Solving the problem

EOP2021 <- EOP2021 %>%
  mutate(across(starts_with("idea"), ~ .x - 1))

lapply(EOP2021 %>% select(starts_with("idea")), table)


## Social Media Use

table(EOP2021$F3_1)
table(EOP2021$F3_2)
table(EOP2021$F3_3)
table(EOP2021$F3_4)
table(EOP2021$F3_5)
table(EOP2021$F3_6)
table(EOP2021$F3_7)
table(EOP2021$F3_8)
table(EOP2021$F3_9)

EOP2021 <- EOP2021 %>%
  mutate(across(starts_with("F3_"), 
                ~ ifelse(.x == 99 | .x == 97, NA, .x), 
                .names = "use{str_remove(.col, 'F3_')}"))

lapply(EOP2021 %>% select(starts_with("use")), table)

#**************************************************************************************************************************/
##########  EOP 2022  ##########################################################
#**************************************************************************************************************************/

#Import Data

EOP2022 <- read_dta("EOP2022.dta")
View(EOP2022)

# Year
EOP2022$year <- 2022

# Weight

EOP2022$weight <- EOP2022$PONDERAD

######## 2022 - SOCIODEMOGRAPHIC VARIABLES ########

# Sex (NAs and create dummy =1 if women)

EOP2022 <- EOP2022%>%
  mutate(sex = ifelse(A5 == 2, 1,
                      ifelse(A5 == 1, 0, NA)))
table(EOP2022$A5)
table(EOP2022$sex)

# Age

EOP2022$age <- EOP2022$A4
summary(EOP2022$A4)
summary(EOP2022$age)

## Socioeconomic Status (1 highest)
EOP2022$SES <- ifelse(EOP2022$GSEX == 99, NA, EOP2022$GSEX)
table(EOP2022$GSEX)
table(EOP2022$SES)

# recoding

EOP2022 <- EOP2022 %>%
  mutate(SES = case_when(
    SES == 1 ~ 5,
    SES == 2 ~ 4,
    SES == 4 ~ 2,
    SES == 5 ~ 1,
    TRUE ~ SES  # This line keeps all other values as they are
  ))

table(EOP2022$SES)


# Education (NAs)

EOP2022$education <- ifelse(EOP2022$Z3 == 11, NA, EOP2022$Z3)

table(EOP2022$Z3)
table(EOP2022$education)

EOP2022$householdeduc <- ifelse(EOP2022$Z4 == 11, NA, EOP2022$Z4)
table(EOP2022$Z4)  
table(EOP2022$householdeduc)

# Living in the periphery

EOP2022$periphery <- ifelse(EOP2022$S1 == 1 | EOP2022$S1 == 2 | EOP2022$S1 == 3 | EOP2022$S1 == 4 | EOP2022$S1 == 5 | EOP2022$S1 == 9 | EOP2022$S1 == 10, 1, 0)
table(EOP2022$periphery)
table(EOP2022$S1)

# Internet Access

table(EOP2022$Z10_1)
table(EOP2022$Z10_2)
table(EOP2022$Z10_3)

EOP2022 <- EOP2022%>%
  mutate(inthome = ifelse(Z10_1 == 1, 1,
                          ifelse(Z10_1 == 2, 0, NA)))
EOP2022 <- EOP2022%>%
  mutate(intmobile = ifelse(Z10_2 == 1, 1,
                            ifelse(Z10_2 == 2, 0, NA)))
EOP2022 <- EOP2022%>%
  mutate(intwork = ifelse(Z10_3 == 1, 1,
                          ifelse(Z10_3 == 2, 0, NA)))

table(EOP2022$inthome)
table(EOP2022$intmobile)
table(EOP2022$intwork)

######## 2022 - EVALUATION OF AUTHORITIES ########

## Presidential Approval

table(EOP2022$B1)
EOP2022 <- EOP2022%>%
  mutate(presapp = ifelse(EOP2022$B1 == 1, 1,
                          ifelse(EOP2022$B1 == 99, NA, 0)))
table(EOP2022$presapp)

## Government Approval

table(EOP2022$B2)
EOP2022 <- EOP2022%>%
  mutate(govapp = ifelse(EOP2022$B2 == 1, 1,
                         ifelse(EOP2022$B2 == 99, NA, 0)))
table (EOP2022$govapp)

## Country Present

table(EOP2022$B3)
EOP2022 <- EOP2022%>%
  mutate(progress = ifelse(EOP2022$B3 == 1, 1,
                           ifelse(EOP2022$B3 == 99, NA, 0)))
EOP2022 <- EOP2022%>%
  mutate(stagnant = ifelse(EOP2022$B3 == 2, 1,
                           ifelse(EOP2022$B3 == 99, NA, 0)))
EOP2022 <- EOP2022%>%
  mutate(decline = ifelse(EOP2022$B3 == 3, 1,
                          ifelse(EOP2022$B3 == 99, NA, 0)))

table(EOP2022$progress)
table(EOP2022$stagnant)
table(EOP2022$decline)


## Country Future
table(EOP2022$B4)
EOP2022 <- EOP2022%>%
  mutate(better = ifelse(EOP2022$B4 == 1, 1,
                         ifelse(EOP2022$B4 == 99, NA, 0)))
EOP2022 <- EOP2022%>%
  mutate(equal = ifelse(EOP2022$B4 == 2, 1,
                        ifelse(EOP2022$B4 == 99, NA, 0)))
EOP2022 <- EOP2022%>%
  mutate(worse = ifelse(EOP2022$B4 == 3, 1,
                        ifelse(EOP2022$B4 == 99, NA, 0)))
table(EOP2022$better)
table(EOP2022$equal)
table(EOP2022$worse)

## Quality of life

table(EOP2022$B7_1)
table(EOP2022$B7_2)
EOP2022$mebetter <- ifelse(EOP2022$B7_1 == 99, NA, EOP2022$B7_1)
EOP2022$childrenbetter <- ifelse(EOP2022$B7_2 == 99, NA, EOP2022$B7_2)
table(EOP2022$mebetter)
table(EOP2022$childrenbetter)

######## 2022 - POLITICAL ENGAGEMENT ########

## Trust
lapply(EOP2022 %>% select(starts_with("D2_")), table)

EOP2022 <- EOP2022 %>%
  mutate(across(starts_with("D2_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "trust{str_remove(.col, 'D2_')}"))

# Trust in Media was not considered before
EOP2022$trust18 <- NULL

lapply(EOP2022 %>% select(starts_with("trust")), table)

## Corruption 

lapply(EOP2022 %>% select(starts_with("D13_")), table)

EOP2022 <- EOP2022 %>%
  mutate(across(starts_with("D13_"), 
                ~ ifelse(.x == 99, NA, .x), 
                .names = "corruption{str_remove(.col, 'D13_')}"))

lapply(EOP2022 %>% select(starts_with("corruption")), table)

# recoding, so 5 is more corrupted

EOP2022 <- EOP2022 %>%
  mutate(across(starts_with("corruption"), ~ 6 - .x))
lapply(EOP2022 %>% select(starts_with("corruption")), table)



## Rational Approval of Demcoracy

table(EOP2022$B5)

EOP2022 <- EOP2022%>%
  mutate(democracy = ifelse(EOP2022$B5 == 1, 1,
                            ifelse(EOP2022$B5 == 99, NA, 0)))
EOP2022 <- EOP2022%>%
  mutate(authoritarianism = ifelse(EOP2022$B5 == 2, 1,
                                   ifelse(EOP2022$B5 == 99, NA, 0)))
EOP2022 <- EOP2022%>%
  mutate(nocare = ifelse(EOP2022$B5 == 3, 1,
                         ifelse(EOP2022$B5 == 99, NA, 0)))

table(EOP2022$democracy)
table(EOP2022$authoritarianism)
table(EOP2022$nocare)

## Satisfaction with democracy

table(EOP2022$B6)
EOP2022$satisdemoc <- ifelse(EOP2022$B6 == 99, NA, EOP2022$B6)
table(EOP2022$satisdemoc)

## Internal Efficacy (NAs)

table(EOP2022$E2_1)
table(EOP2022$E2_2)
table(EOP2022$E2_3)
table(EOP2022$E2_4)
table(EOP2022$E2_5)

EOP2022$intef1 <- ifelse(EOP2022$E2_1 == 99, NA, EOP2022$E2_1)
table(EOP2022$intef1)

EOP2022$intef2 <- ifelse(EOP2022$E2_2 == 99, NA, EOP2022$E2_2)
table(EOP2022$intef2)

EOP2022$intef3 <- ifelse(EOP2022$E2_3 == 99, NA, EOP2022$E2_3)
table(EOP2022$intef3)

EOP2022$intef4 <- ifelse(EOP2022$E2_4 == 99, NA, EOP2022$E2_4)
table(EOP2022$intef4)

EOP2022$intef5 <- ifelse(EOP2022$E2_5 == 99, NA, EOP2022$E2_5)
table(EOP2022$intef5)

## External Efficacy (NAs)

table(EOP2022$E3_1)
table(EOP2022$E3_2)
table(EOP2022$E3_3)
table(EOP2022$E3_4)

EOP2022$extef1 <- ifelse(EOP2022$E3_1 == 99, NA, EOP2022$E3_1)
table(EOP2022$extef1)

EOP2022$extef2 <- ifelse(EOP2022$E3_2 == 99, NA, EOP2022$E3_2)
table(EOP2022$extef2)

EOP2022$extef3 <- ifelse(EOP2022$E3_3 == 99, NA, EOP2022$E3_3)
table(EOP2022$extef3)

EOP2022$extef4 <- ifelse(EOP2022$E3_4 == 99, NA, EOP2022$E3_4)
table(EOP2022$extef4)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
EOP2022 <- EOP2022 %>%
  mutate(across(c(intef1, intef3, extef1, extef3, extef4), ~ 6 - .x))

table(EOP2022$extef4)


## Online Political Efficacy

table(EOP2022$F5_1)
table(EOP2022$F5_2)
table(EOP2022$F5_3)
table(EOP2022$F5_4)

EOP2022$ope1 <- ifelse(EOP2022$F5_1 == 99, NA, EOP2022$F5_1)
EOP2022$ope2 <- ifelse(EOP2022$F5_2 == 99, NA, EOP2022$F5_2)
EOP2022$ope3 <- ifelse(EOP2022$F5_3 == 99, NA, EOP2022$F5_3)
EOP2022$ope4 <- ifelse(EOP2022$F5_4 == 99, NA, EOP2022$F5_4)

table(EOP2022$ope1)
table(EOP2022$ope2)
table(EOP2022$ope3)
table(EOP2022$ope4)


## Political Interest

table(EOP2022$E1_1)
table(EOP2022$E1_2)
table(EOP2022$E1_3)
table(EOP2022$E1_4)
table(EOP2022$E1_5)

EOP2022$polint1 <- ifelse(EOP2022$E1_1 == 99, NA, EOP2022$E1_1)
EOP2022$polint2 <- ifelse(EOP2022$E1_2 == 99, NA, EOP2022$E1_2)
EOP2022$polint3 <- ifelse(EOP2022$E1_3 == 99, NA, EOP2022$E1_3)
EOP2022$polint4 <- ifelse(EOP2022$E1_4 == 99, NA, EOP2022$E1_4)
EOP2022$polint5 <- ifelse(EOP2022$E1_5 == 99, NA, EOP2022$E1_5)

table(EOP2022$polint1)
table(EOP2022$polint2)
table(EOP2022$polint3)
table(EOP2022$polint4)
table(EOP2022$polint5)


## Political Knowledge

table(EOP2022$B8_34)
table(EOP2022$B8_35)
table(EOP2022$B8_36)
table(EOP2022$B8_37)
table(EOP2022$B8_38)
table(EOP2022$b8_40)


EOP2022 <- EOP2022 %>%
  mutate(B8_34 = replace_na(B8_34, 0)) %>%
  mutate(know_sen1 = ifelse(B8_34 == 34, 1, 0))

EOP2022 <- EOP2022 %>%
  mutate(B8_35 = replace_na(B8_35, 0)) %>%
  mutate(know_sen2 = ifelse(B8_35 == 35, 1, 0))

EOP2022 <- EOP2022 %>%
  mutate(B8_36 = replace_na(B8_36, 0)) %>%
  mutate(know_sen3 = ifelse(B8_36 == 36, 1, 0))

EOP2022 <- EOP2022 %>%
  mutate(B8_37 = replace_na(B8_37, 0)) %>%
  mutate(know_sen4 = ifelse(B8_37 == 37, 1, 0))

EOP2022 <- EOP2022 %>%
  mutate(B8_38 = replace_na(B8_38, 0)) %>%
  mutate(know_sen5 = ifelse(B8_38 == 38, 1, 0))

EOP2022 <- EOP2022 %>%
  mutate(b8_40 = replace_na(b8_40, 0)) %>%
  mutate(know_int = ifelse(b8_40 == 40, 1, 0))

table(EOP2022$know_sen1)
table(EOP2022$know_sen2)
table(EOP2022$know_sen3)
table(EOP2022$know_sen4)
table(EOP2022$know_sen5)
table(EOP2022$know_int)

EOP2022$polknowledge <- EOP2022$know_sen1 + EOP2022$know_sen2 + EOP2022$know_sen3 + EOP2022$know_sen4 + EOP2022$know_sen5 + EOP2022$know_int
summary(EOP2022$polknowledge)



## Ideology

table(EOP2022$Z11)

EOP2022$right <- ifelse(EOP2022$Z11 == 1 | EOP2022$Z11 == 2, 1 , 0)
EOP2022$center <- ifelse(EOP2022$Z11 == 3, 1 , 0)
EOP2022$left <- ifelse(EOP2022$Z11 == 4 | EOP2022$Z11 == 5, 1 , 0)

table(EOP2022$right)
table(EOP2022$left)
table(EOP2022$center)


## Participation

table(EOP2022$E9_1)
table(EOP2022$E9_2)
table(EOP2022$E9_3)
table(EOP2022$E9_4)
table(EOP2022$E9_5)
table(EOP2022$E9_6)
table(EOP2022$E9_7)
table(EOP2022$E9_8)
table(EOP2022$E9_9)
table(EOP2022$E9_10)
table(EOP2022$E9_11)
table(EOP2022$E9_12)
table(EOP2022$E9_13)
table(EOP2022$E9_14)

EOP2022 <- EOP2022 %>%
  mutate(across(starts_with("E9_"), 
                ~ ifelse(.x == 99, NA, ifelse(.x == 1, 1, 0)), 
                .names = "part{str_remove(.col, 'E9_')}"))

lapply(EOP2022 %>% select(starts_with("part")), table)

# Cacerolazos and cabildos were not considered before
EOP2022$part13 <- NULL
EOP2022$part14 <- NULL

EOP2022$participation <- EOP2022$part1 + EOP2022$part2 + EOP2022$part3 + EOP2022$part4 + EOP2022$part5 + EOP2022$part6 + EOP2022$part7 + EOP2022$part8 + EOP2022$part9 + EOP2022$part10 + EOP2022$part11 + EOP2022$part12
summary(EOP2022$participation)


## Ideational Cleavage / In 2019 data there is a problem with this variables, values are 2-6, and not 1-5.

table(EOP2022$C4_1)
table(EOP2022$C4_2)
table(EOP2022$C4_3)
table(EOP2022$C4_4)
table(EOP2022$C4_5)
table(EOP2022$C4_6)

EOP2022$idea1 <- EOP2022$C4_1
EOP2022$idea2 <- EOP2022$C4_2
EOP2022$idea3 <- EOP2022$C4_3
EOP2022$idea4 <- EOP2022$C4_4
EOP2022$idea5 <- EOP2022$C4_5
EOP2022$idea6 <- EOP2022$C4_6

# Solving the problem

EOP2022 <- EOP2022 %>%
  mutate(across(starts_with("idea"), ~ .x - 1))

lapply(EOP2022 %>% select(starts_with("idea")), table)


## Social Media Use

table(EOP2022$F3_1)
table(EOP2022$F3_2)
table(EOP2022$F3_3)
table(EOP2022$F3_4)
table(EOP2022$F3_5)
table(EOP2022$F3_6)
table(EOP2022$F3_7)
table(EOP2022$F3_8)
table(EOP2022$F3_9)

EOP2022 <- EOP2022 %>%
  mutate(across(starts_with("F3_"), 
                ~ ifelse(.x == 99 | .x == 97, NA, .x), 
                .names = "use{str_remove(.col, 'F3_')}"))

lapply(EOP2022 %>% select(starts_with("use")), table)




#**************************************************************************************************************************/
##########  EOP 2023  ##########################################################
#**************************************************************************************************************************/

#Import Data

EOP2023 <- read_dta("EOP2023.dta")
View(EOP2023)

# Year
EOP2023$year <- 2023

# Weight

EOP2023$weight <- EOP2023$ponderador

######## 2023 - SOCIODEMOGRAPHIC VARIABLES ########

# Sex (NAs and create dummy =1 if women)

EOP2023 <- EOP2023%>%
  mutate(sex = ifelse(A7 == 2, 1,
                      ifelse(A7 == 1, 0, NA)))
table(EOP2023$A7)
table(EOP2023$sex)

# Age

EOP2023$age <- EOP2023$A6
summary(EOP2023$A6)
summary(EOP2023$age)

## Socioeconomic Status (1 highest)

EOP2023$SES <- ifelse(EOP2023$GSE == 99, NA, EOP2022$GSEX)
table(EOP2023$GSE)

EOP2023 <- EOP2023%>%
  mutate(SES = ifelse(GSE == 1 | GSE == 2 | GSE ==3, 1,
                          ifelse(GSE == 4, 2,
                                 ifelse(GSE == 5, 3, 
                                        ifelse(GSE == 6, 4,
                                               ifelse(GSE == 7, 5, NA))))))
table(EOP2023$SES)

# recoding

EOP2023 <- EOP2023 %>%
  mutate(SES = case_when(
    SES == 1 ~ 5,
    SES == 2 ~ 4,
    SES == 4 ~ 2,
    SES == 5 ~ 1,
    TRUE ~ SES  # This line keeps all other values as they are
  ))

table(EOP2023$SES)


# Education (NAs)

EOP2023 <- EOP2023%>%
  mutate(education = ifelse(Z2 == 88, NA,
                          ifelse(Z2 == 99, NA, Z2)))
table(EOP2023$Z2)
table(EOP2023$education)

EOP2023 <- EOP2023%>%
  mutate(householdeduc = ifelse(Z3 == 88, NA,
                            ifelse(Z3 == 99, NA, Z3)))
table(EOP2023$Z3)  
table(EOP2023$householdeduc)

# Living in the periphery

EOP2023$periphery <- ifelse(EOP2023$s1 == 1 | EOP2023$s1 == 2 | EOP2023$s1 == 3 | EOP2023$s1 == 4 | EOP2023$s1 == 5 | EOP2023$s1 == 9 | EOP2023$s1 == 10, 1, 0)
table(EOP2023$periphery)
table(EOP2023$s1)

# Internet Access

table(EOP2023$Z9_1)
table(EOP2023$Z9_2)
table(EOP2023$Z9_3)

EOP2023 <- EOP2023%>%
  mutate(inthome = ifelse(Z9_1 == 1, 1,
                          ifelse(Z9_1 == 2, 0, NA)))
EOP2023 <- EOP2023%>%
  mutate(intmobile = ifelse(Z9_2 == 1, 1,
                            ifelse(Z9_2 == 2, 0, NA)))
EOP2023 <- EOP2023%>%
  mutate(intwork = ifelse(Z9_3 == 1, 1,
                          ifelse(Z9_3 == 2, 0, NA)))

table(EOP2023$inthome)
table(EOP2023$intmobile)
table(EOP2023$intwork)

######## 2023 - EVALUATION OF AUTHORITIES ########

## Presidential Approval

table(EOP2023$B1)
EOP2023 <- EOP2023%>%
  mutate(presapp = ifelse(EOP2023$B1 == 1, 1,
                          ifelse(EOP2023$B1 == 88 | EOP2023$B1 == 99 , NA, 0)))
table(EOP2023$presapp)

## Government Approval

table(EOP2023$B2)
EOP2023 <- EOP2023%>%
  mutate(govapp = ifelse(EOP2023$B2 == 1, 1,
                         ifelse(EOP2023$B2 == 88 | EOP2023$B2 == 99, NA, 0)))
table (EOP2023$govapp)

## Country Present

table(EOP2023$B3)
EOP2023 <- EOP2023%>%
  mutate(progress = ifelse(EOP2023$B3 == 1, 1,
                           ifelse(EOP2023$B3 == 88 | EOP2023$B3 == 99, NA, 0)))
EOP2023 <- EOP2023%>%
  mutate(stagnant = ifelse(EOP2023$B3 == 2, 1,
                           ifelse(EOP2023$B3 == 88 | EOP2023$B3 == 99, NA, 0)))
EOP2023 <- EOP2023%>%
  mutate(decline = ifelse(EOP2023$B3 == 3, 1,
                           ifelse(EOP2023$B3 == 88 | EOP2023$B3 == 99, NA, 0)))

table(EOP2023$progress)
table(EOP2023$stagnant)
table(EOP2023$decline)


## Country Future
table(EOP2023$B4)
EOP2023 <- EOP2023%>%
  mutate(better = ifelse(EOP2023$B4 == 1, 1,
                         ifelse(EOP2023$B4 == 88 | EOP2023$B4 == 99, NA, 0)))
EOP2023 <- EOP2023%>%
  mutate(equal = ifelse(EOP2023$B4 == 2, 1,
                        ifelse(EOP2023$B4 == 88 | EOP2023$B4 == 99, NA, 0)))
EOP2023 <- EOP2023%>%
  mutate(worse = ifelse(EOP2023$B4 == 3, 1,
                        ifelse(EOP2023$B4 == 88 | EOP2023$B4 == 99, NA, 0)))
table(EOP2023$better)
table(EOP2023$equal)
table(EOP2023$worse)

## Quality of life

table(EOP2023$B7_1)
table(EOP2023$B7_2)

EOP2023 <- EOP2023%>%
  mutate(mebetter = ifelse(B7_1 == 88, NA,
                                ifelse(B7_1 == 99, NA, B7_1)))
EOP2023 <- EOP2023%>%
  mutate(childrenbetter = ifelse(B7_2 == 88, NA,
                                ifelse(B7_2 == 99, NA, B7_2)))


table(EOP2023$mebetter)
table(EOP2023$childrenbetter)

######## 2023 - POLITICAL ENGAGEMENT ########

## Trust
lapply(EOP2023 %>% select(starts_with("D2_")), table)

EOP2023 <- EOP2023 %>%
  mutate(across(starts_with("D2_"), 
                ~ ifelse(.x == 88 | .x == 99, NA, .x), 
                .names = "trust{str_remove(.col, 'D2_')}"))

# Trust in Media was not considered before
EOP2023$trust18 <- NULL

lapply(EOP2023 %>% select(starts_with("trust")), table)

## Corruption 

lapply(EOP2023 %>% select(starts_with("D6_")), table)

EOP2023 <- EOP2023 %>%
  mutate(across(starts_with("D6_"), 
                ~ ifelse(.x == 88 | .x == 99, NA, .x), 
                .names = "corruption{str_remove(.col, 'D6_')}"))

lapply(EOP2023 %>% select(starts_with("corruption")), table)

# recoding, so 5 is more corrupted

EOP2023 <- EOP2023 %>%
  mutate(across(starts_with("corruption"), ~ 6 - .x))
lapply(EOP2023 %>% select(starts_with("corruption")), table)



## Rational Approval of Demcoracy

table(EOP2023$B5)

EOP2023 <- EOP2023%>%
  mutate(democracy = ifelse(EOP2023$B5 == 1, 1,
                            ifelse(EOP2023$B5 == 88 | EOP2023$B5 == 99, NA, 0)))
EOP2023 <- EOP2023%>%
  mutate(authoritarianism = ifelse(EOP2023$B5 == 2, 1,
                                   ifelse(EOP2023$B5 == 88 | EOP2023$B5 == 99, NA, 0)))
EOP2023 <- EOP2023%>%
  mutate(nocare = ifelse(EOP2023$B5 == 3, 1,
                         ifelse(EOP2023$B5 == 88 | EOP2023$B5 == 99, NA, 0)))

table(EOP2023$democracy)
table(EOP2023$authoritarianism)
table(EOP2023$nocare)

## Satisfaction with democracy

table(EOP2023$B6)
EOP2023$satisdemoc <- ifelse(EOP2023$B6 == 88 | EOP2023$B6 == 99, NA, EOP2023$B6)
table(EOP2023$satisdemoc)

## Internal Efficacy (NAs)

table(EOP2023$E2_1)
table(EOP2023$E2_2)
table(EOP2023$E2_3)
table(EOP2023$E2_4)
table(EOP2023$E2_5)

EOP2023$intef1 <- ifelse(EOP2023$E2_1 == 88 | EOP2023$E2_1 == 99, NA, EOP2023$E2_1)
table(EOP2023$intef1)

EOP2023$intef2 <- ifelse(EOP2023$E2_2 == 88 | EOP2023$E2_2 == 99, NA, EOP2023$E2_2)
table(EOP2023$intef2)

EOP2023$intef3 <- ifelse(EOP2023$E2_3 == 88 | EOP2023$E2_3 == 99, NA, EOP2023$E2_3)
table(EOP2023$intef3)

EOP2023$intef4 <- ifelse(EOP2023$E2_4 == 88 | EOP2023$E2_4 == 99, NA, EOP2023$E2_4)
table(EOP2023$intef4)

EOP2023$intef5 <- ifelse(EOP2023$E2_5 == 88 | EOP2023$E2_5 == 99, NA, EOP2023$E2_5)
table(EOP2023$intef5)

## External Efficacy (NAs)

table(EOP2023$E3_1)
table(EOP2023$E3_2)
table(EOP2023$E3_3)
table(EOP2023$E3_4)

EOP2023$extef1 <- ifelse(EOP2023$E3_1 == 88 | EOP2023$E3_1 == 99, NA, EOP2023$E3_1)
table(EOP2023$extef1)

EOP2023$extef2 <- ifelse(EOP2023$E3_2 == 88 | EOP2023$E3_2 == 99, NA, EOP2023$E3_2)
table(EOP2023$extef2)

EOP2023$extef3 <- ifelse(EOP2023$E3_3 == 88 | EOP2023$E3_3 == 99, NA, EOP2023$E3_3)
table(EOP2023$extef3)

EOP2023$extef4 <- ifelse(EOP2023$E3_4 == 88 | EOP2023$E3_4 == 99, NA, EOP2023$E3_4)
table(EOP2023$extef4)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
EOP2023 <- EOP2023 %>%
  mutate(across(c(intef1, intef3, extef1, extef3, extef4), ~ 6 - .x))

table(EOP2023$extef4)


## Online Political Efficacy

table(EOP2023$F3_1)
table(EOP2023$F3_2)
table(EOP2023$F3_3)
table(EOP2023$F3_4)

EOP2023$ope1 <- ifelse(EOP2023$F3_1 == 88 | EOP2023$F3_1 == 99, NA, EOP2023$F3_1)
EOP2023$ope2 <- ifelse(EOP2023$F3_2 == 88 | EOP2023$F3_2 == 99, NA, EOP2023$F3_2)
EOP2023$ope3 <- ifelse(EOP2023$F3_3 == 88 | EOP2023$F3_3 == 99, NA, EOP2023$F3_3)
EOP2023$ope4 <- ifelse(EOP2023$F3_4 == 88 | EOP2023$F3_4 == 99, NA, EOP2023$F3_4)

table(EOP2023$ope1)
table(EOP2023$ope2)
table(EOP2023$ope3)
table(EOP2023$ope4)


## Political Interest

table(EOP2023$E1_1)
table(EOP2023$E1_2)
table(EOP2023$E1_3)
table(EOP2023$E1_4)
table(EOP2023$E1_5)

EOP2023$polint1 <- ifelse(EOP2023$E1_1 == 88 | EOP2023$E1_1 == 99, NA, EOP2023$E1_1)
EOP2023$polint2 <- ifelse(EOP2023$E1_2 == 88 | EOP2023$E1_2 == 99, NA, EOP2023$E1_2)
EOP2023$polint3 <- ifelse(EOP2023$E1_3 == 88 | EOP2023$E1_3 == 99, NA, EOP2023$E1_3)
EOP2023$polint4 <- ifelse(EOP2023$E1_4 == 88 | EOP2023$E1_4 == 99, NA, EOP2023$E1_4)
EOP2023$polint5 <- ifelse(EOP2023$E1_5 == 88 | EOP2023$E1_5 == 99, NA, EOP2023$E1_5)

table(EOP2023$polint1)
table(EOP2023$polint2)
table(EOP2023$polint3)
table(EOP2023$polint4)
table(EOP2023$polint5)


## Political Knowledge

table(EOP2023$B8_34)
table(EOP2023$B8_35)
table(EOP2023$B8_36)
table(EOP2023$B8_37)
table(EOP2023$B8_38)
table(EOP2023$B8_40)


EOP2023$know_sen1 <- ifelse(EOP2023$B8_34 == 1, 1, 0)
EOP2023$know_sen2 <- ifelse(EOP2023$B8_35 == 1, 1, 0)
EOP2023$know_sen3 <- ifelse(EOP2023$B8_36 == 1, 1, 0)
EOP2023$know_sen4 <- ifelse(EOP2023$B8_37 == 1, 1, 0)
EOP2023$know_sen5 <- ifelse(EOP2023$B8_38 == 1, 1, 0)
EOP2023$know_int <- ifelse(EOP2023$B8_40 == 1, 1, 0)

table(EOP2023$know_sen1)
table(EOP2023$know_sen2)
table(EOP2023$know_sen3)
table(EOP2023$know_sen4)
table(EOP2023$know_sen5)
table(EOP2023$know_int)

EOP2023$polknowledge <- EOP2023$know_sen1 + EOP2023$know_sen2 + EOP2023$know_sen3 + EOP2023$know_sen4 + EOP2023$know_sen5 + EOP2023$know_int
summary(EOP2023$polknowledge)


## Ideology

table(EOP2023$Z10)

EOP2023$right <- ifelse(EOP2023$Z10 == 1 | EOP2023$Z10 == 2, 1 , 0)
EOP2023$center <- ifelse(EOP2023$Z10 == 3, 1 , 0)
EOP2023$left <- ifelse(EOP2023$Z10 == 4 | EOP2023$Z10 == 5, 1 , 0)

table(EOP2023$right)
table(EOP2023$left)
table(EOP2023$center)


## Participation

table(EOP2023$E6_1)
table(EOP2023$E6_2)
table(EOP2023$E6_3)
table(EOP2023$E6_4)
table(EOP2023$E6_5)
table(EOP2023$E6_6)
table(EOP2023$E6_7)
table(EOP2023$E6_8)
table(EOP2023$E6_9)
table(EOP2023$E6_10)
table(EOP2023$E6_11)
table(EOP2023$E6_12)
table(EOP2023$E6_13)
table(EOP2023$E6_14)

EOP2023 <- EOP2023 %>%
  mutate(across(starts_with("E6_"), 
                ~ ifelse(.x == 88 | .x == 99, NA, ifelse(.x == 1, 1, 0)), 
                .names = "part{str_remove(.col, 'E6_')}"))

lapply(EOP2023 %>% select(starts_with("part")), table)

# Cacerolazos and cabildos were not considered before
EOP2023$part13 <- NULL
EOP2023$part14 <- NULL

EOP2023$participation <- EOP2023$part1 + EOP2023$part2 + EOP2023$part3 + EOP2023$part4 + EOP2023$part5 + EOP2023$part6 + EOP2023$part7 + EOP2023$part8 + EOP2023$part9 + EOP2023$part10 + EOP2023$part11 + EOP2023$part12
summary(EOP2023$participation)


## Ideational Cleavage / In 2019 data there is a problem with this variables, values are 2-6, and not 1-5.

table(EOP2023$C2_1)
table(EOP2023$C2_2)
table(EOP2023$C2_3)
table(EOP2023$C2_4)
table(EOP2023$C2_5)
table(EOP2023$C2_6)

EOP2023$idea1 <- EOP2023$C2_1
EOP2023$idea2 <- EOP2023$C2_2
EOP2023$idea3 <- EOP2023$C2_3
EOP2023$idea4 <- EOP2023$C2_4
EOP2023$idea5 <- EOP2023$C2_5
EOP2023$idea6 <- EOP2023$C2_6


lapply(EOP2023 %>% select(starts_with("idea")), table)


## Social Media Use

table(EOP2023$F2_1)
table(EOP2023$F2_2)
table(EOP2023$F2_3)
table(EOP2023$F2_4)
table(EOP2023$F2_5)
table(EOP2023$F2_6)
table(EOP2023$F2_7)
table(EOP2023$F2_8)
table(EOP2023$F2_9)

EOP2023 <- EOP2023 %>%
  mutate(across(starts_with("F2_"), 
                ~ ifelse(.x == 88 | .x == 99 | .x == 97, NA, .x), 
                .names = "use{str_remove(.col, 'F2_')}"))

lapply(EOP2023 %>% select(starts_with("use")), table)






#**************************************************************************************************************************/
##########  MERGING  ##########################################################
#**************************************************************************************************************************/

######### Paper Knowledge #########

# List of columns to keep
know_cols <- c("sex", "age", "SES", "education", "year", 
                  "intef1", "intef2", "intef3", "intef4", "intef5", 
                  "polint1", "polint2", "polint3", "polint4", "polint5", 
                  "know_sen1", "know_sen2", "know_sen3", "know_sen4", "know_sen5", 
                  "know_int", "right", "center", "left", "periphery")

# Select columns and merge datasets
know_data <- bind_rows(
  EOP2018[know_cols],
  EOP2019[know_cols],
  EOP2020[know_cols],
  EOP2021[know_cols],
  EOP2022[know_cols],
  EOP2023[know_cols]
)


write_dta(know_data, "know_data.dta")

######### Paper Ideology #########

# List of columns to keep
ideo_cols <- c("sex", "age", "SES", "education", "year", 
               "intef1", "intef2", "intef3", "intef4", "intef5",
               "extef1", "extef2", "extef3", "extef4",
               "polint1", "polint2", "polint3", "polint4", "polint5", 
               "know_sen1", "know_sen2", "know_sen3", "know_sen4", "know_sen5", 
               "know_int", "right", "center", "left", "periphery", "idea1", 
               "idea2", "idea3", "idea4", "idea5", "idea6", "presapp", "govapp")

# Select columns and merge datasets
ideo_data <- bind_rows(
  EOP2018[ideo_cols],
  EOP2019[ideo_cols],
  EOP2020[ideo_cols],
  EOP2021[ideo_cols],
  EOP2022[ideo_cols],
  EOP2023[ideo_cols]
)


write_dta(ideo_data, "ideo_data.dta")
