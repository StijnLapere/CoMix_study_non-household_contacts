glmm_elderlytotal2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8562.104, -2Loglik = 8520.104, 20 param

glmm_elderlytotal2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8554.443, -2Loglik = 8516.443, 18 param

glmm_elderlytotal2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8552.821, -2Loglik = 8514.821, 18 param --> BEST IMPROVEMENT

glmm_elderlytotal2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8555.364, -2Loglik = 8517.364, 18 param

glmm_elderlytotal2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8557.303, -2Loglik = 8517.303, 19 param

glmm_elderlytotal2.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8554.377, -2Loglik = 8514.377, 19 param

glmm_elderlytotal2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8553.330, -2Loglik = 8515.330, 18 param

glmm_elderlytotal2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8555.668, -2Loglik = 8517.668, 18 param

glmm_elderlytotal2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8559.892, -2Loglik = 8519.892, 19 param

glmm_elderlytotal2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8559.125, -2Loglik = 8519.125, 19 param

glmm_elderlytotal2.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8557.049, -2Loglik = 8513.049, 21 param

glmm_elderlytotal2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8556.916, -2Loglik = 8518.916, 18 param

pchisq(8520.8-8514.821, df=length(fixef(glmm_elderlytotal2.3))-length(fixef(glmm_elderly)), lower.tail=FALSE)

glmm_elderlytotal3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8557.971, -2Loglik = 8513.971, 21 param

glmm_elderlytotal3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8550.345, -2Loglik = 8510.345, 19 param

glmm_elderlytotal3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8550.918, -2Loglik = 8510.918, 19 param

glmm_elderlytotal3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8552.359, -2Loglik = 8510.359, 20 param

glmm_elderlytotal3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8549.917, -2Loglik = 8507.917, 20 param

glmm_elderlytotal3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.849, -2Loglik = 8508.849, 19 param --> BEST IMPROVEMENT

glmm_elderlytotal3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8551.738, -2Loglik = 8511.738, 19 param

glmm_elderlytotal3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8555.993, -2Loglik = 8513.993, 20 param

glmm_elderlytotal3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8555.333, -2Loglik = 8513.333, 20 param

glmm_elderlytotal3.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8552.993, -2Loglik = 8506.993, 22 param

glmm_elderlytotal3.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8552.905, -2Loglik = 8512.905, 19 param

pchisq(8514.821-8507.917, df=length(fixef(glmm_elderlytotal3.5))-length(fixef(glmm_elderlytotal2.3)), lower.tail=FALSE)
pchisq(8514.821-8508.849, df=length(fixef(glmm_elderlytotal3.6))-length(fixef(glmm_elderlytotal2.3)), lower.tail=FALSE)

glmm_elderlytotal4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8554.108, -2Loglik = 8508.108, 22 param

glmm_elderlytotal4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8546.765, -2Loglik = 8504.765, 20 param

glmm_elderlytotal4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8546.867, -2Loglik = 8504.867, 20 param

glmm_elderlytotal4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.399, -2Loglik = 8504.399, 21 param

glmm_elderlytotal4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8545.833, -2Loglik = 8501.833, 21 param --> BEST IMPROVEMENT

glmm_elderlytotal4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.022, -2Loglik = 8506.022, 20 param

glmm_elderlytotal4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8551.863, -2Loglik = 8507.863, 21 param

glmm_elderlytotal4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8551.454, -2Loglik = 8507.454, 21 param

glmm_elderlytotal4.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.937, -2Loglik = 8500.937, 23 param

glmm_elderlytotal4.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.851, -2Loglik = 8506.851, 20 param

pchisq(8508.849-8501.833, df=length(fixef(glmm_elderlytotal4.5))-length(fixef(glmm_elderlytotal3.6)), lower.tail=FALSE)

glmm_elderlytotal5.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8551.524, -2Loglik = 8501.524, 24 param

glmm_elderlytotal5.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8543.677, -2Loglik = 8497.677, 22 param --> BEST IMPROVEMENT

glmm_elderlytotal5.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.520, -2Loglik = 8498.520, 22 param

glmm_elderlytotal5.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8546.091, -2Loglik = 8498.091, 23 param

glmm_elderlytotal5.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.933, -2Loglik = 8498.933, 22 param

glmm_elderlytotal5.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.781, -2Loglik = 8500.781, 23 param

glmm_elderlytotal5.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8548.441, -2Loglik = 8500.441, 23 param

glmm_elderlytotal5.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8545.678, -2Loglik = 8493.678, 25 param

glmm_elderlytotal5.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8545.847, -2Loglik = 8499.847, 22 param

pchisq(8501.833-8497.677, df=length(fixef(glmm_elderlytotal5.2))-length(fixef(glmm_elderlytotal4.5)), lower.tail=FALSE)

glmm_elderlytotal6.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8549.206, -2Loglik = 8497.206, 25 param

glmm_elderlytotal6.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8542.397, -2Loglik = 8494.397, 23 param --> BEST IMPROVEMENT

glmm_elderlytotal6.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8544.046, -2Loglik = 8494.046, 24 param

glmm_elderlytotal6.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8542.756, -2Loglik = 8494.756, 23 param

glmm_elderlytotal6.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8546.638, -2Loglik = 8496.638, 24 param

glmm_elderlytotal6.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8546.316, -2Loglik = 8496.316, 24 param

glmm_elderlytotal6.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8543.727, -2Loglik = 8489.727, 26 param

glmm_elderlytotal6.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + 
    part_vacc:hhsize_elderly + part_vacc:part_elevated_risk + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8543.682, -2Loglik = 8495.682, 23 param

pchisq(8497.677-8494.397, df=length(fixef(glmm_elderlytotal6.2))-length(fixef(glmm_elderlytotal5.2)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE

summary(glmm_elderlytotal5.2)
