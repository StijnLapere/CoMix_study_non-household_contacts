glmm_adulttotal2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23138.47, -2Loglik = 23090.47, 23 param

glmm_adulttotal2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23140.31, -2Loglik = 23086.31, 26 param

glmm_adulttotal2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# Rank deficient

glmm_adulttotal2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23138.06, -2Loglik = 23092.06, 22 param

glmm_adulttotal2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23131.57, -2Loglik = 23087.57, 21 param

glmm_adulttotal2.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23131.97, -2Loglik = 23087.97, 21 param

glmm_adulttotal2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23133.98, -2Loglik = 23089.98, 21 param

glmm_adulttotal2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23136.86, -2Loglik = 23090.86, 22 param

glmm_adulttotal2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23124.33, -2Loglik = 23076.33, 23 param --> BEST IMPROVEMENT

glmm_adulttotal2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23126.49, -2Loglik = 23082.49, 21 param

glmm_adulttotal2.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23134.41, -2Loglik = 23090.41, 21 param

glmm_adulttotal2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23137.59, -2Loglik = 23091.59, 22 param

glmm_adulttotal2.13 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23133.90, -2Loglik = 23087.90, 22 param

glmm_adulttotal2.14 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23139.88, -2Loglik = 23085.88, 26 param

glmm_adulttotal2.15 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23135.87, -2Loglik = 23091.87, 21 param

pchisq(23092.25-23076.33, df=length(fixef(glmm_adulttotal2.9))-length(fixef(glmm_adultstotal)), lower.tail=FALSE)
pchisq(23092.25-23082.49, df=length(fixef(glmm_adulttotal2.10))-length(fixef(glmm_adultstotal)), lower.tail=FALSE)


glmm_adulttotal3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23128.24, -2Loglik = 23074.24, 26 param

glmm_adulttotal3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23130.46, -2Loglik = 23070.46, 29 param

glmm_adulttotal3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23128.12, -2Loglik = 23076.12, 25 param

glmm_adulttotal3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23120.58, -2Loglik = 23070.58, 24 param

glmm_adulttotal3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23122.31, -2Loglik = 23072.31, 24 param

glmm_adulttotal3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23124.58, -2Loglik = 23074.58, 24 param

glmm_adulttotal3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23126.99, -2Loglik = 23074.99, 25 param

glmm_adulttotal3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23116.04, -2Loglik = 23066.04, 24 param --> BEST IMPROVEMENT

glmm_adulttotal3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23124.54, -2Loglik = 23074.54, 24 param

glmm_adulttotal3.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23127.68, -2Loglik = 23075.68, 25 param

glmm_adulttotal3.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23123.85, -2Loglik = 23071.85, 25 param

glmm_adulttotal3.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23129.81, -2Loglik = 23069.81, 29 param

glmm_adulttotal3.13 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23125.94, -2Loglik = 23075.94, 24 param

pchisq(23076.33-23066.04, df=length(fixef(glmm_adulttotal3.8))-length(fixef(glmm_adulttotal2.9)), lower.tail=FALSE)

glmm_adulttotal4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23119.93, -2Loglik = 23063.93, 27 param

glmm_adulttotal4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23122.12, -2Loglik = 23060.12, 30 param

glmm_adulttotal4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23119.85, -2Loglik = 23065.85, 26 param

glmm_adulttotal4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23112.60, -2Loglik = 23060.60, 25 param --> BEST IMPROVEMENT

glmm_adulttotal4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23112.89, -2Loglik = 23060.89, 25 param

glmm_adulttotal4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23116.39, -2Loglik = 23064.39, 25 param

glmm_adulttotal4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23118.68, -2Loglik = 23064.68, 26 param

glmm_adulttotal4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23116.91, -2Loglik = 23064.91, 25 param

glmm_adulttotal4.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23119.53, -2Loglik = 23065.53, 26 param

glmm_adulttotal4.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23115.63, -2Loglik = 23061.63, 26 param

glmm_adulttotal4.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23121.60, -2Loglik = 23059.60, 30 param

glmm_adulttotal4.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23117.62, -2Loglik = 23065.62, 25 param

pchisq(23066.04-23060.60, df=length(fixef(glmm_adulttotal4.4))-length(fixef(glmm_adulttotal3.8)), lower.tail=FALSE)

glmm_adulttotal5.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23117.10, -2Loglik = 23059.10, 28 param

glmm_adulttotal5.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23118.71, -2Loglik = 23054.71, 31 param

glmm_adulttotal5.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23116.36, -2Loglik = 23060.36, 27 param

glmm_adulttotal5.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23109.52, -2Loglik = 23055.52, 26 param --> BEST IMPROVEMENT

glmm_adulttotal5.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23113.41, -2Loglik = 23059.41, 26 param

glmm_adulttotal5.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23115.41, -2Loglik = 23059.41, 27 param

glmm_adulttotal5.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23113.53, -2Loglik = 23059.53, 26 param

glmm_adulttotal5.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23116.09, -2Loglik = 23060.09, 27 param

glmm_adulttotal5.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23112.23, -2Loglik = 23056.23, 27 param

glmm_adulttotal5.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23118.14, -2Loglik = 23054.14, 31 param

glmm_adulttotal5.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23114.22, -2Loglik = 23060.22, 26 param

pchisq(23060.60-23055.52, df=length(fixef(glmm_adulttotal5.4))-length(fixef(glmm_adulttotal4.4)), lower.tail=FALSE)

glmm_adulttotal6.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23114.31, -2Loglik = 23054.31, 29 param

glmm_adulttotal6.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23115.68, -2Loglik = 23049.68, 32 param

glmm_adulttotal6.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23113.21, -2Loglik = 23055.21, 28 param

glmm_adulttotal6.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23110.29, -2Loglik = 23054.29, 27 param

glmm_adulttotal6.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23112.45, -2Loglik = 23054.45, 28 param

glmm_adulttotal6.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23110.46, -2Loglik = 23054.46, 27 param

glmm_adulttotal6.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23113.06, -2Loglik = 23055.06, 28 param

glmm_adulttotal6.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23108.81, -2Loglik = 23050.81, 28 param

glmm_adulttotal6.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23115.05, -2Loglik = 23049.05, 32 param

glmm_adulttotal6.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    part_vacc:part_face_mask + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23111.11, -2Loglik = 23055.11, 27 param

pchisq(23055.52-23054.29, df=length(fixef(glmm_adulttotal6.4))-length(fixef(glmm_adulttotal5.4)), lower.tail=FALSE)
pchisq(23055.52-23050.81, df=length(fixef(glmm_adulttotal6.8))-length(fixef(glmm_adulttotal5.4)), lower.tail=FALSE)

# NO SIGNIFICANT IMPROVEMENTS ANYMORE

summary(glmm_adulttotal5.4)
