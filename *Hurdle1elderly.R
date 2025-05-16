library(lme4)

glmm_elderly <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + 
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8556.8, -2Loglik = 8520.8, 17 param
summary(glmm_elderly)

glmm_elderly2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8546.042, -2Loglik = 8496.042, 24 param

pchisq(8520.8-8496.042, df=length(fixef(glmm_elderly2))-length(fixef(glmm_elderly)), lower.tail=FALSE)

## Is there overdispersion?
library("blmeco") 
dispersion_glmer(glmm_elderly2)
#No (0.96), so continue with binomial distribution

glmm_elderlytotal2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8551.182, -2Loglik = 8495.182, 27 param

glmm_elderlytotal2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.064, -2Loglik = 8492.064, 25 param

glmm_elderlytotal2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_face_mask +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8542.441, -2Loglik = 8490.441, 25 param

glmm_elderlytotal2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.428, -2Loglik = 8492.428, 25 param

glmm_elderlytotal2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8547.137, -2Loglik = 8493.137, 26 param

glmm_elderlytotal2.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8543.388, -2Loglik = 8489.388, 26 param

glmm_elderlytotal2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8542.100, -2Loglik = 8490.100, 25 param --> BEST IMPROVEMENT

glmm_elderlytotal2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.978, -2Loglik = 8492.978, 25 param

glmm_elderlytotal2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8549.046, -2Loglik = 8495.046, 26 param

glmm_elderlytotal2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.434, -2Loglik = 8494.434, 26 param

glmm_elderlytotal2.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8545.871, -2Loglik = 8487.871, 28 param

glmm_elderlytotal2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8546.898, -2Loglik = 8494.898, 25 param

pchisq(8496.042-8490.100, df=length(fixef(glmm_elderlytotal2.7))-length(fixef(glmm_elderly2)), lower.tail=FALSE)

glmm_elderlytotal3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8547.341, -2Loglik = 8489.341, 28 param

glmm_elderlytotal3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8540.445, -2Loglik = 8486.445, 26 param

glmm_elderlytotal3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_vacc:part_face_mask +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8538.057, -2Loglik = 8484.057, 26 param --> BEST IMPROVEMENT

glmm_elderlytotal3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8540.403, -2Loglik = 8486.403, 26 param

glmm_elderlytotal3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8543.234, -2Loglik = 8487.234, 27 param

glmm_elderlytotal3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8539.346, -2Loglik = 8483.346, 27 param

glmm_elderlytotal3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8541.290, -2Loglik = 8487.290, 26 param

glmm_elderlytotal3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.897, -2Loglik = 8488.897, 27 param

glmm_elderlytotal3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.586, -2Loglik = 8488.596, 27 param

glmm_elderlytotal3.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8541.833, -2Loglik = 8481.833, 29 param

glmm_elderlytotal3.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8542.895, -2Loglik = 8488.895, 26 param

pchisq(8490.100-8484.057, df=length(fixef(glmm_elderlytotal3.3))-length(fixef(glmm_elderlytotal2.7)), lower.tail=FALSE)

glmm_elderlytotal4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8543.199, -2Loglik = 8483.199, 29 param

glmm_elderlytotal4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8536.363, -2Loglik = 8480.363, 27 param

glmm_elderlytotal4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8535.940, -2Loglik = 8479.940, 27 param

glmm_elderlytotal4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8538.307, -2Loglik = 8480.307, 28 param

glmm_elderlytotal4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.818, -2Loglik = 8476.818, 28 param --> BEST IMPROVEMENT

glmm_elderlytotal4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8537.336, -2Loglik = 8481.336, 27 param

glmm_elderlytotal4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8540.979, -2Loglik = 8482.979, 28 param

glmm_elderlytotal4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8540.757, -2Loglik = 8482.757, 28 param

glmm_elderlytotal4.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8537.742, -2Loglik = 8475.742, 30 param

glmm_elderlytotal4.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8538.862, -2Loglik = 8482.862, 27 param

pchisq(8484.057-8476.818, df=length(fixef(glmm_elderlytotal4.5))-length(fixef(glmm_elderlytotal3.3)), lower.tail=FALSE)

glmm_elderlytotal5.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8540.558, -2Loglik = 8476.558, 31 param

glmm_elderlytotal5.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8533.062, -2Loglik = 8473.062, 29 param --> BEST IMPROVEMENT

glmm_elderlytotal5.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8533.418, -2Loglik = 8473.418, 29 param

glmm_elderlytotal5.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8535.616, -2Loglik = 8473.616, 30 param

glmm_elderlytotal5.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.011, -2Loglik = 8474.011, 29 param

glmm_elderlytotal5.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8537.660, -2Loglik = 8475.660, 30 param

glmm_elderlytotal5.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8537.518, -2Loglik = 8475.518, 30 param

glmm_elderlytotal5.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.238, -2Loglik = 8468.238, 32 param

glmm_elderlytotal5.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8535.655, -2Loglik = 8475.655, 29 param

pchisq(8476.818-8473.062, df=length(fixef(glmm_elderlytotal5.2))-length(fixef(glmm_elderlytotal4.5)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE

# Can we remove a main effect?
glmm_elderlytotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8537.628, -2Loglik = 8483.628, 26 param

glmm_elderlytotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8535.861, -2Loglik = 8479.861, 27 param

glmm_elderlytotalnowd <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8533.260, -2Loglik = 8477.260, 27 param

glmm_elderlytotalnoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8532.126, -2Loglik = 8478.126, 26 param

glmm_elderlytotalnosymptoms <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8533.242, -2Loglik = 8477.242, 27 param

glmm_elderlytotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8538.760, -2Loglik = 8482.760, 27 param

glmm_elderlytotalnosocialgroup <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8531.468, -2Loglik = 8479.468, 25 param --> BEST

glmm_elderlytotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8545.833, -2Loglik = 8501.833, 21 param

pchisq(8479.468-8476.818, df=length(fixef(glmm_elderlytotal4.5))-length(fixef(glmm_elderlytotalnosocialgroup)), lower.tail=FALSE)

glmm_elderlytotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.350, -2Loglik = 8486.350, 23 param

glmm_elderlytotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8532.546, -2Loglik = 8482.546, 24 param

glmm_elderlytotalnowd <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8529.921, -2Loglik = 8479.921, 24 param --> BEST

glmm_elderlytotalnoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8529.842, -2Loglik = 8481.842, 23 param

glmm_elderlytotalnosymptoms <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8529.958, -2Loglik = 8479.958, 24 param

glmm_elderlytotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.801, -2Loglik = 8484.801, 24 param

glmm_elderlytotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8542.489, -2Loglik = 8504.489, 18 param

pchisq(8479.921-8479.468, df=length(fixef(glmm_elderlytotalnosocialgroup))-length(fixef(glmm_elderlytotalnowd)), lower.tail=FALSE)
pchisq(8481.842-8479.468, df=length(fixef(glmm_elderlytotalnosocialgroup))-length(fixef(glmm_elderlytotalnoeducation)), lower.tail=FALSE)
pchisq(8479.958-8479.468, df=length(fixef(glmm_elderlytotalnosocialgroup))-length(fixef(glmm_elderlytotalnosymptoms)), lower.tail=FALSE)


glmm_elderlytotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8532.827, -2Loglik = 8486.827, 22 param

glmm_elderlytotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8531.247, -2Loglik = 8483.247, 23 param

glmm_elderlytotalnoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday +  
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8528.285, -2Loglik = 8482.285, 22 param

glmm_elderlytotalnosymptoms <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8528.398, -2Loglik = 8480.398, 23 param --> BEST

glmm_elderlytotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + 
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8533.223, -2Loglik = 8485.223, 23 param

glmm_elderlytotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8541.362, -2Loglik = 8505.362, 17 param

pchisq(8482.285-8479.921, df=length(fixef(glmm_elderlytotalnowd))-length(fixef(glmm_elderlytotalnoeducation)), lower.tail=FALSE)
pchisq(8480.398-8479.921, df=length(fixef(glmm_elderlytotalnowd))-length(fixef(glmm_elderlytotalnosymptoms)), lower.tail=FALSE)

glmm_elderlytotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8531.222, -2Loglik = 8487.222, 21 param

glmm_elderlytotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8529.587, -2Loglik = 8483.587, 22 param

glmm_elderlytotalnoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday +  
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8526.745, -2Loglik = 8482.745, 21 param --> BEST

glmm_elderlytotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + 
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8531.724, -2Loglik = 8485.724, 22 param

glmm_elderlytotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8540.068, -2Loglik = 8506.068, 16 param

pchisq(8482.745-8480.398, df=length(fixef(glmm_elderlytotalnosymptoms))-length(fixef(glmm_elderlytotalnoeducation)), lower.tail=FALSE)

glmm_elderlytotalnoarea <- glmer(
  any_nonhh_contact ~ holiday +  
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8528.838, -2Loglik = 8488.838, 19 param

glmm_elderlytotalnoholiday <- glmer(
  any_nonhh_contact ~ area_3_name +   
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8527.936, -2Loglik = 8485.936, 20 param

glmm_elderlytotalnogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday +  
    part_vacc + part_elevated_risk + part_face_mask + 
    hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8529.855, -2Loglik = 8487.855, 20 param

glmm_elderlytotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday +  
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8538.392, -2Loglik = 8508.392, 14 param

pchisq(8485.936-8482.745, df=length(fixef(glmm_elderlytotalnoeducation))-length(fixef(glmm_elderlytotalnoholiday)), lower.tail=FALSE)
pchisq(8488.838-8482.745, df=length(fixef(glmm_elderlytotalnoeducation))-length(fixef(glmm_elderlytotalnoarea)), lower.tail=FALSE)

##Final model
pchisq(8482.745-8476.818, df=length(fixef(glmm_elderlytotal4.5))-length(fixef(glmm_elderlytotalnoeducation)), lower.tail=FALSE)

summary(glmm_elderlytotalnoeducation)

library(DHARMa)
simulationoutput <- simulateResiduals(fittedModel = glmm_elderlytotalnoeducation)
plot(simulationoutput)
testDispersion(simulationoutput)
