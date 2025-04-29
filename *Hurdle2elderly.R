library(lme4)

logisticdataset_noage_Elderlynonhh <- logisticdataset_noage_Elderly %>% filter(any_nonhh_contact == 1)

hurdle2_elderly1 <- glmer(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 24462.4, -2Loglik = 24426.4, 17 param
summary(hurdle2_elderly1)

## Is there overdispersion?
dispersion_glmer(hurdle2_elderly1)
#Yes (1.70), so consider negative binomial distribution

#Move to negative binomial distribution
hurdle2_elderlynb1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 18126.379, -2Loglik = 18088.379, 17 param

hurdle2_elderly2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18129.373, -2Loglik = 18085.373, 20 param

hurdle2_elderly2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.095, -2Loglik = 18088.095, 18 param

hurdle2_elderly2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18126.395, -2Loglik = 18086.395, 18 param

hurdle2_elderly2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.378, -2Loglik = 18088.378, 18 param

hurdle2_elderly2.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.890, -2Loglik = 18086.890, 19 param

hurdle2_elderly2.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.54, -2Loglik = 18086.54, 19 param

hurdle2_elderly2.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18127.065, -2Loglik = 18087.065, 18 param

hurdle2_elderly2.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18127.302, -2Loglik = 18087.302, 18 param

hurdle2_elderly2.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.893, -2Loglik = 18078.893, 19 param --> BEST IMPROVEMENT

hurdle2_elderly2.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18129.733, -2Loglik = 18087.733, 19 param

hurdle2_elderly2.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18130.433, -2Loglik = 18084.433, 21 param

hurdle2_elderly2.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.00, -2Loglik = 18088.00, 18 param

pchisq(18088.379-18078.893, df=length(fixef(hurdle2_elderly2.9))-length(fixef(hurdle2_elderlynb1)), lower.tail=FALSE)

hurdle2_elderly3.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18123.898, -2Loglik = 18075.898, 22 param

hurdle2_elderly3.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.630, -2Loglik = 18078.630, 20 param

hurdle2_elderly3.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.16, -2Loglik = 18076.16, 20 param --> BEST IMPROVEMENT

hurdle2_elderly3.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.892, -2Loglik = 18078.892, 20 param

hurdle2_elderly3.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18123.310, -2Loglik = 18077.310, 21 param

hurdle2_elderly3.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18123.068, -2Loglik = 18077.068, 21 param

hurdle2_elderly3.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18121.737, -2Loglik = 18077.737, 20 param

hurdle2_elderly3.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.316, -2Loglik = 18078.316, 20 param

hurdle2_elderly3.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18124.173, -2Loglik = 18078.173, 21 param

hurdle2_elderly3.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18125.275, -2Loglik = 18075.275, 23 param

hurdle2_elderly3.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.46, -2Loglik = 18078.46, 20 param

pchisq(18078.893-18076.16, df=length(fixef(hurdle2_elderly3.3))-length(fixef(hurdle2_elderly2.9)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENT ANYMORE










#Is there overdispersion?
# Calculate Pearson residuals
resid_pearson <- residuals(hurdle2_elderly1, type = "pearson")
# Calculate dispersion statistic
dispersion_stat <- sum(resid_pearson^2) / df.residual(hurdle2_elderly1)
#4.379894 --> CLEAR OVERDISPERSION

