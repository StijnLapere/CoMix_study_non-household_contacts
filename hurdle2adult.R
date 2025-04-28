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

## Is there overdispersion?
dispersion_glmer(hurdle2_adult1)
#Yes (2.27), so consider negative binomial distribution

#Move to negative binomial distribution
hurdle2_adultnb1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 50259.91, -2Loglik = 50215.91, 20 param

hurdle2_adult2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:part_vacc + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:employstatus + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_elevated_risk:part_face_mask + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:part_symp_none + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:area_3_name + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:holiday + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.13 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:hhsize_cat + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

hurdle2_adult2.14 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    holiday:wd + (1 | part_uid),
  data = logisticdataset_noage_adultnonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
