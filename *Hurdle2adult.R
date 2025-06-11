library(lme4)

logisticdataset_noage_adultnonhh <- logisticdataset_noage_adult %>% filter(any_nonhh_contact == 1)

hurdle2_adult1 <- glmer(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 85808.01, -2Loglik = 85766.01, 20 param
summary(hurdle2_adult1)

hurdle2_adult1wavecount <- glmer(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 84768.03, -2Loglik = 84712.03, 27 param

hurdle2_adult1wavecountshort <- glmer(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 85321.85, -2Loglik = 85275.85, 22 param

## Is there overdispersion?
library("blmeco") 
dispersion_glmer(hurdle2_adult1wavecount)
#Yes (2.27), so consider negative binomial distribution

#Move to negative binomial distribution
library(glmmTMB)

hurdle2_adultnb1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44241.68, -2Loglik = 44197.68, 20 param

hurdle2_adultnb1wavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44112.59, -2Loglik = 44054.59, 27 param

hurdle2_adultnb1wavecountshort <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44137.12, -2Loglik = 44089.12, 22 param

hurdle2_adult2.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44107.95, -2Loglik = 44043.95, 30 param

hurdle2_adult2.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44117.93, -2Loglik = 44047.93, 33 param

hurdle2_adult2.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44090.41, -2Loglik = 44028.41, 29 param --> BEST IMPROVEMENT

hurdle2_adult2.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44113.52, -2Loglik = 44053.52, 28 param

hurdle2_adult2.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44095.36, -2Loglik = 44035.36, 28 param

hurdle2_adult2.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44111.69, -2Loglik = 44051.69, 28 param

hurdle2_adult2.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44110.84, -2Loglik = 44048.84, 29 param

hurdle2_adult2.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44114.83, -2Loglik = 44050.83, 30 param

hurdle2_adult2.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44114.12, -2Loglik = 44054.12, 28 param

hurdle2_adult2.10 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44114.24, -2Loglik = 44054.24, 28 param

hurdle2_adult2.11 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44095.70, -2Loglik = 44033.70, 29 param

hurdle2_adult2.12 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44101.50, -2Loglik = 44039.50, 29 param

hurdle2_adult2.13 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44116.30, -2Loglik = 44046.30, 33 param

hurdle2_adult2.14 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44108.84, -2Loglik = 44048.84, 28 param

pchisq(44054.59-44028.41, df=length(fixef(hurdle2_adult2.3)$cond)-length(fixef(hurdle2_adultnb1wavecount)$cond), lower.tail=FALSE)

hurdle2_adult3.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44086.29, -2Loglik = 44018.29, 32 param

hurdle2_adult3.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44095.74, -2Loglik = 44021.74, 35 param

hurdle2_adult3.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44091.71, -2Loglik = 44027.71, 30 param

hurdle2_adult3.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44073.96, -2Loglik = 44009.96, 30 param --> BEST IMPROVEMENT

hurdle2_adult3.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44090.13, -2Loglik = 44026.13, 30 param

hurdle2_adult3.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44089.63, -2Loglik = 44023.63, 31 param

hurdle2_adult3.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44092.52, -2Loglik = 44024.52, 32 param

hurdle2_adult3.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44092.03, -2Loglik = 44028.03, 30 param

hurdle2_adult3.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44092.11, -2Loglik = 44028.11, 30 param

hurdle2_adult3.10 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44073.98, -2Loglik = 44007.98, 31 param

hurdle2_adult3.11 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44079.02, -2Loglik = 44013.02, 31 param

hurdle2_adult3.12 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44094.39, -2Loglik = 44020.39, 35 param

hurdle2_adult3.13 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44086.98, -2Loglik = 44022.98, 30 param

pchisq(44028.41-44009.96, df=length(fixef(hurdle2_adult3.4)$cond)-length(fixef(hurdle2_adult2.3)$cond), lower.tail=FALSE)
pchisq(44028.41-44007.98, df=length(fixef(hurdle2_adult3.10)$cond)-length(fixef(hurdle2_adult2.3)$cond), lower.tail=FALSE)

hurdle2_adult4.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44069.43, -2Loglik = 43999.43, 33 param 

hurdle2_adult4.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44079.33, -2Loglik = 44003.33, 36 param 

hurdle2_adult4.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44075.44, -2Loglik = 44009.44, 31 param 

hurdle2_adult4.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44073.79, -2Loglik = 44007.79, 31 param 

hurdle2_adult4.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44073.25, -2Loglik = 44005.25, 32 param 

hurdle2_adult4.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44076.02, -2Loglik = 44006.02, 33 param 

hurdle2_adult4.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44075.16, -2Loglik = 44009.16, 31 param 

hurdle2_adult4.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44075.80, -2Loglik = 44009.80, 31 param 

hurdle2_adult4.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44058.99, -2Loglik = 43990.99, 32 param --> BEST IMPROVEMENT

hurdle2_adult4.10 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44064.06, -2Loglik = 43996.06, 32 param 

hurdle2_adult4.11 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44078.19, -2Loglik = 44002.19, 36 param 

hurdle2_adult4.12 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44070.36, -2Loglik = 44004.36, 31 param 

pchisq(44009.96-43990.99, df=length(fixef(hurdle2_adult4.9)$cond)-length(fixef(hurdle2_adult3.4)$cond), lower.tail=FALSE)

hurdle2_adult5.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44054.46, -2Loglik = 43980.46, 35 param

hurdle2_adult5.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44063.85, -2Loglik = 43983.85, 38 param

hurdle2_adult5.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44060.41, -2Loglik = 43990.41, 33 param

hurdle2_adult5.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44058.88, -2Loglik = 43988.88, 33 param

hurdle2_adult5.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44059.54, -2Loglik = 43987.54, 34 param

hurdle2_adult5.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44060.86, -2Loglik = 43986.86, 35 param

hurdle2_adult5.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44060.33, -2Loglik = 43990.33, 33 param

hurdle2_adult5.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44060.78, -2Loglik = 43990.78, 33 param

hurdle2_adult5.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44049.57, -2Loglik = 43977.57, 34 param --> BEST IMPROVEMENT

hurdle2_adult5.10 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44063.09, -2Loglik = 43983.09, 38 param

hurdle2_adult5.11 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44055.00, -2Loglik = 43985.00, 33 param

pchisq(43990.99-43977.57, df=length(fixef(hurdle2_adult5.9)$cond)-length(fixef(hurdle2_adult4.9)$cond), lower.tail=FALSE)

hurdle2_adult6.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44045.44, -2Loglik = 43967.44, 37 param --> BEST IMPROVEMENT

hurdle2_adult6.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44054.09, -2Loglik = 43970.09, 40 param

hurdle2_adult6.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44050.96, -2Loglik = 43976.96, 35 param

hurdle2_adult6.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44049.30, -2Loglik = 43975.30, 35 param

hurdle2_adult6.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44051.16, -2Loglik = 43975.16, 36 param

hurdle2_adult6.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44051.37, -2Loglik = 43973.37, 37 param

hurdle2_adult6.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44050.83, -2Loglik = 43976.83, 35 param

hurdle2_adult6.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44051.40, -2Loglik = 43977.40, 35 param

hurdle2_adult6.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44053.85, -2Loglik = 43969.85, 40 param

hurdle2_adult6.10 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44048.52, -2Loglik = 43974.52, 35 param

pchisq(43977.57-43967.44, df=length(fixef(hurdle2_adult6.1)$cond)-length(fixef(hurdle2_adult5.9)$cond), lower.tail=FALSE)

hurdle2_adult7.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44050.97, -2Loglik = 43960.97, 43 param

hurdle2_adult7.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44046.35, -2Loglik = 43966.35, 38 param

hurdle2_adult7.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44044.77, -2Loglik = 43964.77, 38 param

hurdle2_adult7.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44047.42, -2Loglik = 43965.42, 39 param

hurdle2_adult7.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44047.71, -2Loglik = 43963.71, 40 param

hurdle2_adult7.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44046.61, -2Loglik = 43966.61, 38 param

hurdle2_adult7.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44047.20, -2Loglik = 43967.20, 38 param

hurdle2_adult7.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44049.93, -2Loglik = 43959.93, 43 param

hurdle2_adult7.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44044.59, -2Loglik = 43964.59, 38 param

pchisq(43967.44-43964.77, df=length(fixef(hurdle2_adult7.3)$cond)-length(fixef(hurdle2_adult6.1)$cond), lower.tail=FALSE)
pchisq(43967.44-43964.59, df=length(fixef(hurdle2_adult7.9)$cond)-length(fixef(hurdle2_adult6.1)$cond), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS
# Can we remove a main effect?
hurdle2_adultnowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44050.99, -2Loglik = 43974.99, 36 param

hurdle2_adultnoemploystatus <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44096.68, -2Loglik = 44022.68, 35 param

hurdle2_adultnohhsize <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44045.50, -2Loglik = 43973.50, 34 param

hurdle2_adultnoelevrisk <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44072.57, -2Loglik = 43996.57, 36 param

hurdle2_adultnosymptoms <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44046.06, -2Loglik = 43970.06, 36 param

hurdle2_adultnogender <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_social_group_be + wavecount +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44057.84, -2Loglik = 43981.84, 36 param

hurdle2_adultnowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be +
    part_vacc:educationmainearner + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  family = truncated_nbinom2)
#AIC = 44171.92, -2Loglik = 44107.92, 30 param

pchisq(43973.50-43967.44, df=length(fixef(hurdle2_adult6.1)$cond)-length(fixef(hurdle2_adultnohhsize)$cond), lower.tail=FALSE)
pchisq(43970.06-43967.44, df=length(fixef(hurdle2_adult6.1)$cond)-length(fixef(hurdle2_adultnosymptoms)$cond), lower.tail=FALSE)

## Final model
finalmodelhurdle2adults <- hurdle2_adult6.1

library(DHARMa)
simulationoutput <- simulateResiduals(fittedModel = finalmodelhurdle2adults, n=1000)
plot(simulationoutput)
testOutliers(simulationoutput, type = "bootstrap")
testDispersion(simulationoutput)

df <- data.frame(
  Covariate = c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", "Weekday", "Weekend",
                "Not in labor force", "Employed", "Student", "Low education", "Medium education", "High education",
                "hh size 1", "hh size 2", "hh size 3", "hh size 4+",
                "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", "Symptoms No", "Symptoms Yes", "Face mask No", "Face mask Yes",
                "Female", "Male", "Social group 1&2", "Social group 3&4", "Social group 5&6", "Social group 7&8",
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Vacc No : Medium education", "Vacc Yes : Low education", "Vacc Yes : High education", "Vacc No : Face mask No", "Vacc Yes : Face mask Yes", 
                "Face mask No : Brussels Hoofdstede", "Face mask Yes : Vlaams Gewest", "Face mask Yes : Waals Gewest",
                "Holiday No : Brussels Hoofdstede", "Holiday Yes : Vlaams Gewest", "Holiday Yes : Waals Gewest", 
                "Vacc No : Social group 1&2", "Vacc Yes : Social group 3&4", "Vacc Yes : Social group 5&6", "Vacc Yes : Social group 7&8"),
  Estimate = c(0, 0.29229, 0.05189, 0, -0.03216, 0, 0.10211, 
               0, 0.53032, 0.52555, 0.06264, 0, -0.33020, 
               0, 0.16541, 0.20464, 0.09555, 
               0, 0.14983, 0, -0.14148, 0, -0.06427, 0, 0.43411, 
               0, 0.02292, 0, 0.17342, 0.01389, -0.11730, 
               0, -0.14855, -0.28487, -0.39504, -0.46811, -0.48263, -0.72389, -0.61347, 
               0, -0.01603, 0.54810, 0, -0.34117, 
               0, 0.30301, -0.08585, 0, -0.04223, 0.23180, 
               0, 0.09113, 0.28803, 0.39120),
  SE = c(0, 0.20044, 0.20938, 0, 0.12044, 0, 0.03723, 
         0, 0.07284, 0.11928, 0.14373, 0, 0.12661, 
         0, 0.07931, 0.09610, 0.10138, 
         0, 0.14532, 0, 0.05996, 0, 0.03972, 0, 0.18120, 
         0, 0.06773, 0, 0.13710, 0.16641, 0.16021, 
         0, 0.06725, 0.07304, 0.07653, 0.07918, 0.08077, 0.08528, 0.05928, 
         0, 0.14097, 0.11020, 0, 0.08498, 
         0, 0.17912, 0.18772, 
         0, 0.12654, 0.13694, 0, 0.11660, 0.14301, 0.13484)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", "Weekday", "Weekend",
                         "Not in labor force", "Employed", "Student", "Low education", "Medium education", "High education",
                         "hh size 1", "hh size 2", "hh size 3", "hh size 4+",
                         "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", "Symptoms No", "Symptoms Yes", "Face mask No", "Face mask Yes",
                         "Female", "Male", "Social group 1&2", "Social group 3&4", "Social group 5&6", "Social group 7&8",
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                         "Vacc No : Medium education", "Vacc Yes : Low education", "Vacc Yes : High education", "Vacc No : Face mask No", "Vacc Yes : Face mask Yes", 
                         "Face mask No : Brussels Hoofdstede", "Face mask Yes : Vlaams Gewest", "Face mask Yes : Waals Gewest",
                         "Holiday No : Brussels Hoofdstede", "Holiday Yes : Vlaams Gewest", "Holiday Yes : Waals Gewest", 
                         "Vacc No : Social group 1&2", "Vacc Yes : Social group 3&4", "Vacc Yes : Social group 5&6", "Vacc Yes : Social group 7&8"))

df$Covariate <- factor(df$Covariate, levels = covariate_order)

library(ggplot2)
# Plot using ggplot2
ggplot(df, aes(x = RelativeContacts, y = reorder(Covariate, RelativeContacts))) +
  geom_point(size = 2, color = "red") + 
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.5, linewidth = 0.8, color = "black") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(x = "Relative Number of having non-household contacts", y = "Covariates") +
  theme(axis.text.y = element_text(size = 10)) +
  scale_y_discrete(limits = covariate_order) # Ensure correct order
