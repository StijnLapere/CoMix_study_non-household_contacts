library(lme4)

glmm_adultstotal <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
#AIC = 23134.25, -2Loglik = 23092.25, 20 param

glmm_adultstotalwavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
#AIC = 23043.72, -2Loglik = 22987.72, 27 param

pchisq(23092.25-22987.72, df=length(fixef(glmm_adultstotalwavecount))-length(fixef(glmm_adultstotal)), lower.tail=FALSE)

## Is there overdispersion?
dispersion_glmer(glmm_adultstotalwavecount)
#No (0.92), so continue with binomial distribution

## Include interaction effect
glmm_adulttotal2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23048.49, -2Loglik = 22986.49, 30 param

glmm_adulttotal2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23050.02, -2Loglik = 22982.02, 33 param

glmm_adulttotal2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_social_group_be:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# Rank deficient

glmm_adulttotal2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23047.58, -2Loglik = 22987.58, 29 param

glmm_adulttotal2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23042.50, -2Loglik = 22984.50, 28 param

glmm_adulttotal2.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23042.50, -2Loglik = 22984.50, 28 param

glmm_adulttotal2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23043.58, -2Loglik = 22985.58, 28 param

glmm_adulttotal2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23046.32, -2Loglik = 22986.32, 29 param

glmm_adulttotal2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23033.13, -2Loglik = 22971.13, 30 param --> BEST IMPROVEMENT

glmm_adulttotal2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23036.2, -2Loglik = 22978.2, 28 param

glmm_adulttotal2.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23043.79, -2Loglik = 22985.79, 28 param

glmm_adulttotal2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23047.09, -2Loglik = 22987.09, 29 param

glmm_adulttotal2.13 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23042.70, -2Loglik = 22982.70, 29 param

glmm_adulttotal2.14 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23048.97, -2Loglik = 22980.97, 33 param

glmm_adulttotal2.15 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23045.22, -2Loglik = 22987.22, 28 param

pchisq(22987.72-22971.13, df=length(fixef(glmm_adulttotal2.9))-length(fixef(glmm_adultstotalwavecount)), lower.tail=FALSE)

## Can we include another interaction effect?
glmm_adulttotal3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23037.60, -2Loglik = 22969.60, 33 param

glmm_adulttotal3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23039.51, -2Loglik = 22965.51, 36 param

glmm_adulttotal3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23036.99, -2Loglik = 22970.99, 32 param

glmm_adulttotal3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23030.91, -2Loglik = 22966.91, 31 param

glmm_adulttotal3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23032.16, -2Loglik = 22968.16, 31 param

glmm_adulttotal3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23033.48, -2Loglik = 22969.48, 31 param

glmm_adulttotal3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23035.84, -2Loglik = 22969.84, 32 param

glmm_adulttotal3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23025.06, -2Loglik = 22961.06, 31 param --> BEST IMPROVEMENT

glmm_adulttotal3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23033.24, -2Loglik = 22969.24, 31 param

glmm_adulttotal3.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23036.51, -2Loglik = 22970.51, 32 param

glmm_adulttotal3.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23031.98, -2Loglik = 22965.98, 32 param

glmm_adulttotal3.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23038.22, -2Loglik = 22964.22, 36 param

glmm_adulttotal3.13 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23034.61, -2Loglik = 22970.61, 31 param

pchisq(22971.13-22961.06, df=length(fixef(glmm_adulttotal3.8))-length(fixef(glmm_adulttotal2.9)), lower.tail=FALSE)

## Can we include another interaction effect?
glmm_adulttotal4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23029.50, -2Loglik = 22959.50, 34 param

glmm_adulttotal4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23031.38, -2Loglik = 22955.38, 37 param

glmm_adulttotal4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23028.93, -2Loglik = 22960.93, 33 param

glmm_adulttotal4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23023.07, -2Loglik = 22957.07, 32 param  --> BEST IMPROVEMENT

glmm_adulttotal4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23023.11, -2Loglik = 22957.11, 32 param

glmm_adulttotal4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23025.48, -2Loglik = 22959.48, 32 param

glmm_adulttotal4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23027.74, -2Loglik = 22959.74, 33 param

glmm_adulttotal4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23025.83, -2Loglik = 22959.83, 32 param

glmm_adulttotal4.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23028.58, -2Loglik = 22960.58, 33 param

glmm_adulttotal4.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23023.97, -2Loglik = 22955.97, 33 param

glmm_adulttotal4.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23030.22, -2Loglik = 22954.22, 37 param

glmm_adulttotal4.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23026.51, -2Loglik = 22960.51, 32 param

pchisq(22961.06-22957.07, df=length(fixef(glmm_adulttotal4.4))-length(fixef(glmm_adulttotal3.8)), lower.tail=FALSE)

## Can we include another interaction effect?
glmm_adulttotal5.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23027.98, -2Loglik = 22955.98, 35 param

glmm_adulttotal5.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23029.42, -2Loglik = 22951.42, 38 param

glmm_adulttotal5.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23026.88, -2Loglik = 22956.88, 34 param

glmm_adulttotal5.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23021.18, -2Loglik = 22953.18, 33 param --> BEST IMPROVEMENT

glmm_adulttotal5.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23023.89, -2Loglik = 22955.89, 33 param

glmm_adulttotal5.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23025.90, -2Loglik = 22955.90, 34 param

glmm_adulttotal5.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23023.90, -2Loglik = 22955.90, 33 param

glmm_adulttotal5.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23026.59, -2Loglik = 22956.59, 34 param

glmm_adulttotal5.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23022.03, -2Loglik = 22952.03, 34 param

glmm_adulttotal5.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23028.23, -2Loglik = 22950.23, 38 param

glmm_adulttotal5.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23024.56, -2Loglik = 22956.56, 33 param

pchisq(22957.07-22953.18, df=length(fixef(glmm_adulttotal5.4))-length(fixef(glmm_adulttotal4.4)), lower.tail=FALSE)

## Can we include another interaction effect?
glmm_adulttotal6.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23026.30, -2Loglik = 22952.30, 36 param

glmm_adulttotal6.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23027.56, -2Loglik = 22947.56, 39 param

glmm_adulttotal6.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23024.91, -2Loglik = 22952.91, 35 param

glmm_adulttotal6.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23021.96, -2Loglik = 22951.96, 34 param

glmm_adulttotal6.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23024.11, -2Loglik = 22952.11, 35 param

glmm_adulttotal6.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23022.02, -2Loglik = 22952.02, 34 param

glmm_adulttotal6.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23024.74, -2Loglik = 22952.74, 35 param

glmm_adulttotal6.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23019.83, -2Loglik = 22947.83, 35 param

glmm_adulttotal6.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23026.32, -2Loglik = 22946.32, 39 param

glmm_adulttotal6.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + holiday:wd + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23022.64, -2Loglik = 22952.64, 34 param

pchisq(22953.18-22947.83, df=length(fixef(glmm_adulttotal6.8))-length(fixef(glmm_adulttotal5.4)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE
#Can we remove a main effect?
glmm_adulttotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23072.75, -2Loglik = 23008.75, 31 param

glmm_adulttotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23029.76, -2Loglik = 22963.76, 32 param

glmm_adulttotalnowd <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23022.77, -2Loglik = 22956.77, 32 param

glmm_adulttotalnoemploystatus <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23022.82, -2Loglik = 22958.82, 31 param

glmm_adulttotalnoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23027.92, -2Loglik = 22963.92, 31 param

glmm_adulttotalnosymptoms <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_gender + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23035.63, -2Loglik = 22969.63, 32 param

glmm_adulttotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_social_group_be + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23032.43, -2Loglik = 22966.43, 32 param

glmm_adulttotalnosocialgroup <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23019.83, -2Loglik = 22957.83, 30 param --> BEST

glmm_adulttotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23109.52, -2Loglik = 22955.52, 26 param

pchisq(22957.83-22953.18, df=length(fixef(glmm_adulttotal5.4))-length(fixef(glmm_adulttotalnosocialgroup)), lower.tail=FALSE)

### Remove main effect of social group
## Can we remove another main effect?
glmm_adulttotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23070.20, -2Loglik = 23012.20, 28 param

glmm_adulttotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23028.35, -2Loglik = 22968.35, 29 param

glmm_adulttotalnowd <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23021.39, -2Loglik = 22961.39, 29 param

glmm_adulttotalnoemploystatus <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23020.00, -2Loglik = 22962.00, 28 param --> BEST

glmm_adulttotalnoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23042.84, -2Loglik = 22984.84, 28 param

glmm_adulttotalnosymptoms <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23034.07, -2Loglik = 22974.07, 29 param

glmm_adulttotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23031.37, -2Loglik = 22971.37, 29 param

glmm_adulttotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23109.18, -2Loglik = 23061.18, 23 param

pchisq(22962.00-22957.83, df=length(fixef(glmm_adulttotalnosocialgroup))-length(fixef(glmm_adulttotalnoemploystatus)), lower.tail=FALSE)

### Remove main effect of employmentstatus
## Can we remove another main effect?
glmm_adulttotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23069.51, -2Loglik = 23015.51, 26 param

glmm_adulttotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23028.75, -2Loglik = 22972.75, 27 param

glmm_adulttotalnowd <- glmer(
  any_nonhh_contact ~ area_3_name + holiday +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23021.57, -2Loglik = 22965.57, 27 param

glmm_adulttotalnoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23040.39, -2Loglik = 22986.39, 26 param

glmm_adulttotalnosymp <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_gender + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23034.18, -2Loglik = 22978.18, 27 param

glmm_adulttotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + wavecount +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23031.82, -2Loglik = 22975.82, 27 param

glmm_adulttotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender +
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 23108.18, -2Loglik = 23064.18, 21 param

pchisq(22965.57-22962.00, df=length(fixef(glmm_adulttotalnoemploystatus))-length(fixef(glmm_adulttotalnowd)), lower.tail=FALSE)

## FINAL MODEL
summary(glmm_adulttotalnoemploystatus)

library(DHARMa)
simulationoutput <- simulateResiduals(fittedModel = glmm_adulttotalnoemploystatus)
plot(simulationoutput)
testDispersion(simulationoutput)

df <- data.frame(
  Covariate = c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                "Weekday", "Weekend", "Low education", "Medium education", "High education",
                "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", "Symptoms No", "Symptoms Yes", "Face mask No", "Face mask Yes",
                "hh size 1", "hh size 2", "hh size 3", "hh size 4+", "Female", "Male",
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Vacc No : hh size 1", "Vacc Yes : hh size 2", "Vacc Yes : hh size 3", "Vacc Yes : hh size 4+",
                "Elevated risk No : Face mask No", "Elevated risk Yes : Face mask Yes", "Vacc No : Elevated risk No", "Vacc Yes : Elevated risk Yes", "Vacc No : Face mask No", "Vacc Yes : Face mask Yes"),
  Estimate = c(0, 0.84838, 0.20921, 0, 0.13139, 
               0, 0.08638, -0.28140, 0, 0.35113, 
               0, 0.30219, 0, -0.01198, 0, -0.20915, 0, 1.71873,
               0, -0.48526, -0.45668, -0.37944, 0, -0.28911,
               0, -0.16487, -0.32955, -0.51955, -0.50286, -0.42750, -0.54417, -0.70912,
               0, 0.28607, 0.13037, -0.27181, 
               0, 0.32328, 0, -0.20077, 0, -0.17285),
  SE = c(0, 0.15921, 0.16720, 0, 0.03938,
         0, 0.04493, 0.14003, 0, 0.10279, 
         0, 0.11017, 0, 0.11992, 0, 0.05117, 0, 0.07515,
         0, 0.12423, 0.15349, 0.15683, 0, 0.09443,
         0, 0.08440, 0.09055, 0.09451, 0.09712, 0.09993, 0.10211, 0.07438,
         0, 0.10838, 0.13215, 0.13978, 
         0, 0.09742, 0, 0.09793, 0, 0.08603)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                         "Weekday", "Weekend", "Low education", "Medium education", "High education",
                         "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", "Symptoms No", "Symptoms Yes", "Face mask No", "Face mask Yes",
                         "hh size 1", "hh size 2", "hh size 3", "hh size 4+", "Female", "Male",
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                         "Vacc No : hh size 1", "Vacc Yes : hh size 2", "Vacc Yes : hh size 3", "Vacc Yes : hh size 4+",
                         "Elevated risk No : Face mask No", "Elevated risk Yes : Face mask Yes", "Vacc No : Elevated risk No", "Vacc Yes : Elevated risk Yes", "Vacc No : Face mask No", "Vacc Yes : Face mask Yes"))

df$Covariate <- factor(df$Covariate, levels = covariate_order)

library(ggplot2)
# Plot using ggplot2
ggplot(df, aes(x = RelativeContacts, y = reorder(Covariate, RelativeContacts))) +
  geom_point(size = 2, color = "red") + 
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.5, linewidth = 0.8, color = "black") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(x = "Relative Odds of having non-household contacts", y = "Covariates") +
  theme(axis.text.y = element_text(size = 10)) +
  scale_y_discrete(limits = covariate_order) # Ensure correct order
