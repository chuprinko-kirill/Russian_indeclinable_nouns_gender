setwd("C:/Users/Kirill/Desktop")
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(optimx)
library(multcomp)
library(blme)

### Data extraction and preparation ###

raw_data <- read_excel("000000__Нарицательные_неодушевленные_единая таблица.xlsx", sheet = "Data")
raw_data <- raw_data %>% dplyr::filter(Case == "В"| Case == "Им" | Case == "Им/Вин") %>% dplyr::select(Meaning, gender) # we'll investigate only instances with nominative and accusative cases
raw_data$fem=as.factor(ifelse(raw_data$gender=="f", 1,0)) #one-hot encoding
raw_data$masc=as.factor(ifelse(raw_data$gender=="m", 1,0))
raw_data$neut=as.factor(ifelse(raw_data$gender=="n", 1,0))
raw_data$Meaning <- str_to_lower(raw_data$Meaning)
raw_data$Meaning <- str_trim(raw_data$Meaning)

lookup <- read_excel("000000__Нарицательные_неодушевленные_единая таблица.xlsx", sheet = "Lookup")
lookup <- lookup %>% dplyr::select(Meaning, Finale, Stress_from_final_syl, include_sem_transparent_set, final_assumed_hypernym_gender)

names(lookup) = c("Meaning","final","stress","transperent_sem","sem_gender")

full_data <- left_join(raw_data,lookup)

full_data$no_sem=as.factor(ifelse(as.character(full_data$gender)==as.character(full_data$sem_gender),0,1))
full_data$final=relevel(as.factor(full_data$final), ref="i") # "-i" as a reference level because it is the most frequent in our dataset (and has high type frequencey, close to "-o" ending subclass (see Murphy 2000))  
full_data$gender=as.factor(full_data$gender)
full_data$sem_gender=as.factor(full_data$sem_gender)

full_data$gender=relevel(full_data$gender,ref="n")
full_data$sem_gender=relevel(full_data$sem_gender,ref="n")

full_data$Meaning <- str_remove_all(full_data$Meaning, pattern = "\\s*\\(.*\\)\\s*$") # delete some semantics comments to nouns in brackets
full_data$syls=str_count(full_data$Meaning,"[уыаеоэяиюё]")
full_data <- full_data %>% filter(Meaning != "соль" & Meaning != "до" & Meaning != "") # delete these tokens because they are a way too frequent in other grammatical contexts 

full_data <- full_data %>% filter(Meaning != "спагетти" & Meaning != "мюсли"& Meaning != "пенсне") # we are not interested in the pluralia tantum patterns yet, we fetched some corpus data though
full_data <- full_data %>% filter(gender != "pl")

full_data_multisyl=droplevels(subset(full_data, syls>1))
full_data_multisyl$stress <- as.character(full_data_multisyl$stress)
full_data_multisyl= full_data_multisyl %>% filter(!str_detect(stress, "/"))
full_data_multisyl$stress <- as.factor(full_data_multisyl$stress)

Full_data_sure=droplevels(subset(full_data, transperent_sem == 1))


### Data exploration ###
str(full_data)
summary(full_data)
length(unique(full_data$Meaning)) #N of nouns in the study's scope
full_data %>% distinct(Meaning, final) %>% summary(.)
full_data %>% group_by(gender, final)  %>% count(.)


summary(Full_data_sure)
length(unique(Full_data_sure$Meaning)) #N of nouns for an investigation of semantic influence
Full_data_sure %>% distinct(Meaning, sem_gender) %>% summary(.)
Full_data_sure %>% group_by(gender, sem_gender)  %>% count(.)

str(full_data_multisyl)
summary(full_data_multisyl)
length(unique(full_data_multisyl$Meaning))

full_data_multisyl %>% distinct(Meaning, stress) %>% summary(.)
full_data_multisyl %>% group_by(gender, stress)  %>% count(.)
###

### Regression analysis ###
sink ("indeclinable_nouns_corpus_data_regressions.txt")
print ("by semantic predictors")
print ("MASC")
m_by_sem= glmer(masc ~ sem_gender + (1|Meaning) , Full_data_sure, family="binomial",
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(m_by_sem)
summary(glht(m_by_sem, linfct = mcp(sem_gender= "Tukey")), test = adjusted("holm"))


print ("FEM")
f_by_final= glmer( fem ~ sem_gender + (1|Meaning) , Full_data_sure, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(f_by_final)
summary(glht(f_by_final, linfct = mcp(sem_gender= "Tukey")), test = adjusted("holm"))

print ("NEUT")
n_by_final= glmer( neut ~ sem_gender + (1|Meaning) , Full_data_sure, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(n_by_final)
summary(glht(n_by_final, linfct = mcp(sem_gender= "Tukey")), test = adjusted("holm"))


print ("by final")
print ("MASC")
m_by_final= glmer( masc ~ final + (1|Meaning) , full_data, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(m_by_final)
summary(glht(m_by_final, linfct = mcp(final= "Tukey")), test = adjusted("holm"))

print ("FEM")
f_by_final= glmer( fem ~ final + (1|Meaning) , full_data, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(f_by_final)
summary(glht(f_by_final, linfct = mcp(final= "Tukey")), test = adjusted("holm"))

print ("NEUT")
n_by_final= glmer( neut ~ final + (1|Meaning) , full_data, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(n_by_final)
summary(glht(n_by_final, linfct = mcp(final= "Tukey")), test = adjusted("holm"))

print ("by stress")
print ("MASC")
m_by_stress= glmer( masc ~ stress + (1|Meaning) , full_data_multisyl, family="binomial",
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(m_by_stress)
summary(glht(m_by_stress, linfct = mcp(stress= "Tukey")), test = adjusted("holm"))
print ("FEM")
f_by_stress= glmer( fem ~ stress + (1|Meaning) , full_data_multisyl, family="binomial",
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(f_by_stress)
summary(glht(f_by_stress, linfct = mcp(stress= "Tukey")), test = adjusted("holm"))
print ("NEUT")
n_by_stress= glmer( neut ~ stress + (1|Meaning) , full_data_multisyl, family="binomial",
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(n_by_stress)
summary(glht(n_by_stress, linfct = mcp(stress= "Tukey")), test = adjusted("holm"))



print ("by syllables")
print ("MASC")
m_by_syls= glmer( masc ~ syls + (1|Meaning) , full_data, family="binomial",
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(m_by_syls)

print ("FEM")
f_by_syls= lmer( fem ~ syls + (1|Meaning) , full_data, family="binomial",
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(f_by_syls)

print ("NEUT")
n_by_syls= lmer( neut ~ syls + (1|Meaning) , full_data, family="binomial",
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))



print ("-------- not semantic --------")
print ("-------- Род считается несемантическим, если никакой из возможных семантических предикторов его не предсказывает --------")
print ("by final segment")
no_sem_by_final= glmer( no_sem ~ final + (1|Meaning) ,Full_data_sure, family="binomial",
                        control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(no_sem_by_final)

print ("by stress")
no_sem_by_stress= glmer( no_sem ~ stress + (1|Meaning) , Full_data_sure, family="binomial",
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(no_sem_by_stress)
summary(glht(no_sem_by_stress, linfct = mcp(stress= "Tukey")), test = adjusted("holm"))


no_sem_by_gender= glmer( no_sem ~ gender + (1|Meaning) , Full_data_sure, family="binomial",
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(no_sem_by_gender)
summary(glht(no_sem_by_gender, linfct = mcp(gender= "Tukey")), test = adjusted("holm"))
sink()
