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
system.time({hurdle2_adultnb1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))})
#AIC = 50259.91, -2Loglik = 50215.91, 20 param

system.time({hurdle2_adultnb1wavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))})
#AIC = 50138.76, -2Loglik = 50080.76, 27 param

system.time({hurdle2_adultnb1wavecountshort <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))})
#AIC = 50165.92, -2Loglik = 50117.92, 22 param

hurdle2_adult2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50156.06, -2Loglik = 50102.06, 30 param

hurdle2_adult2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50166.55, -2Loglik = 50106.55, 33 param

hurdle2_adult2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50133.79, -2Loglik = 50081.79, 29 param --> BEST IMPROVEMENT

hurdle2_adult2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50166.79, -2Loglik = 50116.79, 28 param

hurdle2_adult2.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50143.16, -2Loglik = 50093.16, 28 param

hurdle2_adult2.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50162.61, -2Loglik = 50112.61, 28 param

hurdle2_adult2.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50166.73, -2Loglik = 50114.73, 29 param

hurdle2_adult2.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50163.64, -2Loglik = 50109.64, 30 param

hurdle2_adult2.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50167.73, -2Loglik = 50117.73, 28 param

hurdle2_adult2.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50167.56, -2Loglik = 50117.56, 28 param

hurdle2_adult2.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50141.39, -2Loglik = 50089.39, 29 param

hurdle2_adult2.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50157.35, -2Loglik = 50105.35, 29 param

hurdle2_adult2.13 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50170.57, -2Loglik = 50110.57, 33 param

hurdle2_adult2.14 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50161.66, -2Loglik = 50111.66, 28 param

pchisq(50117.92-50081.79, df=length(fixef(hurdle2_adult2.3))-length(fixef(hurdle2_adultnb1wavecountshort)), lower.tail=FALSE)

hurdle2_adult3.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50125.91, -2Loglik = 50067.91, 32 param

hurdle2_adult3.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50134.29, -2Loglik = 50070.29, 35 param

hurdle2_adult3.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50134.93, -2Loglik = 50080.93, 30 param

hurdle2_adult3.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50112.10, -2Loglik = 50058.10, 30 param

hurdle2_adult3.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50131.78, -2Loglik = 50077.78, 30 param

hurdle2_adult3.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50135.46, -2Loglik = 50079.46, 32 param

hurdle2_adult3.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50132.08, -2Loglik = 50074.08, 33 param

hurdle2_adult3.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50135.67, -2Loglik = 50081.67, 30 param

hurdle2_adult3.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50135.50, -2Loglik = 50081.50, 30 param

hurdle2_adult3.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 31 param --> BEST IMPROVEMENT

hurdle2_adult3.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50124.89, -2Loglik = 50068.89, 31 param

hurdle2_adult3.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50138.89, -2Loglik = 50074.89, 35 param

hurdle2_adult3.13 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50130.05, -2Loglik = 50076.05, 30 param

pchisq(50081.79-50053.70, df=length(fixef(hurdle2_adult3.10))-length(fixef(hurdle2_adult2.3)), lower.tail=FALSE)

hurdle2_adult4.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 34 param

hurdle2_adult4.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 37 param

hurdle2_adult4.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 32 param

hurdle2_adult4.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 32 param

hurdle2_adult4.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 32 param

hurdle2_adult4.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 33 param

hurdle2_adult4.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 34 param

hurdle2_adult4.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 32 param

hurdle2_adult4.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 32 param

hurdle2_adult4.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 33 param

hurdle2_adult4.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 37 param

hurdle2_adult4.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecountshort +
    part_vacc:educationmainearner + part_face_mask:area_3_name + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 50109.70, -2Loglik = 50053.70, 32 param
































hurdle2_adult2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50248.26, -2Loglik = 50198.26, 23 param

hurdle2_adult2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50261.12, -2Loglik = 50205.12, 26 param

hurdle2_adult2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50224.61, -2Loglik = 50176.61, 22 param --> BEST IMPROVEMENT

hurdle2_adult2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50260.21, -2Loglik = 50214.21, 21 param

hurdle2_adult2.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50235.04, -2Loglik = 50189.04, 21 param

hurdle2_adult2.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50256.50, -2Loglik = 50210.50, 21 param

hurdle2_adult2.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50261.28, -2Loglik = 50213.28, 22 param

hurdle2_adult2.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50257.21, -2Loglik = 50207.21, 23 param

hurdle2_adult2.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50261.64, -2Loglik = 50215.64, 21 param

hurdle2_adult2.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50261.59, -2Loglik = 50215.59, 21 param

hurdle2_adult2.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50237.12, -2Loglik = 50189.12, 22 param

hurdle2_adult2.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50250.70, -2Loglik = 50202.70, 22 param

hurdle2_adult2.13 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50263.55, -2Loglik = 50207.55, 26 param

hurdle2_adult2.14 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50255.68, -2Loglik = 50209.68, 21 param

pchisq(50215.91-50176.61, df=length(fixef(hurdle2_adult2.3))-length(fixef(hurdle2_adultnb1)), lower.tail=FALSE)

hurdle2_adult3.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50217.06, -2Loglik = 50163.06, 25 param

hurdle2_adult3.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#Failed to converge

hurdle2_adult3.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50225.30, -2Loglik = 50175.30, 23 param

hurdle2_adult3.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e50)))
#Failed to converge

hurdle2_adult3.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50222.54, -2Loglik = 50172.54, 23 param

hurdle2_adult3.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50226.73, -2Loglik = 50174.73, 24 param

hurdle2_adult3.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50222.40, -2Loglik = 50168.40, 25 param

hurdle2_adult3.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50226.44, -2Loglik = 50176.44, 23 param

hurdle2_adult3.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50226.34, -2Loglik = 50176.34, 23 param

hurdle2_adult3.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#Failed to converge

hurdle2_adult3.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50215.04, -2Loglik = 50163.04, 24 param --> BEST IMPROVEMENT

hurdle2_adult3.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50228.78, -2Loglik = 50168.78, 28 param

hurdle2_adult3.13 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50220.92, -2Loglik = 50170.92, 23 param

pchisq(50176.61-50163.04, df=length(fixef(hurdle2_adult3.11))-length(fixef(hurdle2_adult2.3)), lower.tail=FALSE)

hurdle2_adult4.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50208.02, -2Loglik = 50150.02, 27 param

hurdle2_adult4.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50215.64, -2Loglik = 50161.64, 25 param

hurdle2_adult4.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50212.81, -2Loglik = 50158.81, 25 param

hurdle2_adult4.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50217.89, -2Loglik = 50161.89, 26 param

hurdle2_adult4.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50212.68, -2Loglik = 50154.68, 27 param

hurdle2_adult4.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50216.82, -2Loglik = 50162.82, 25 param

hurdle2_adult4.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50216.81, -2Loglik = 50162.81, 25 param

hurdle2_adult4.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50219.27, -2Loglik = 50155.27, 30 param

hurdle2_adult4.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50213.89, -2Loglik = 50159.89, 25 param

pchisq(50163.04-50150.02, df=length(fixef(hurdle2_adult4.1))-length(fixef(hurdle2_adult3.11)), lower.tail=FALSE)

hurdle2_adult5.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50207.79, -2Loglik = 50147.79, 28 param

hurdle2_adult5.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50205.25, -2Loglik = 50145.25, 28 param

hurdle2_adult5.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50211.22, -2Loglik = 50149.22, 29 param

hurdle2_adult5.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50206.97, -2Loglik = 50142.97, 30 param

hurdle2_adult5.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50209.77, -2Loglik = 50149.77, 28 param

hurdle2_adult5.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50209.69, -2Loglik = 50149.69, 28 param

hurdle2_adult5.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50212.37, -2Loglik = 50142.37, 33 param

hurdle2_adult5.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50206.99, -2Loglik = 50146.99, 28 param

pchisq(50150.02-50145.25, df=length(fixef(hurdle2_adult5.2))-length(fixef(hurdle2_adult4.1)), lower.tail=FALSE)

hurdle2_adult6.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50205.36, -2Loglik = 50143.36, 29 param

hurdle2_adult6.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50208.19, -2Loglik = 50144.19, 30 param

hurdle2_adult6.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50203.63, -2Loglik = 50137.63, 31 param

hurdle2_adult6.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50207.00, -2Loglik = 50145.00, 29 param

hurdle2_adult6.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50207.03, -2Loglik = 50145.03, 29 param

hurdle2_adult6.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50209.77, -2Loglik = 50137.77, 34 param

hurdle2_adult6.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + area_3_name:holiday + part_social_group_be:part_vacc + part_vacc:part_symp_none + holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50204.21, -2Loglik = 50142.21, 29 param

pchisq(50145.25-50137.63, df=length(fixef(hurdle2_adult6.3))-length(fixef(hurdle2_adult5.2)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE

pchisq(50145.25-50137.63, df=length(fixef(hurdle2_adult6.3))-length(fixef(hurdle2_adult5.2)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE
