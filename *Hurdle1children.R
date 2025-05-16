library(lme4)

glmm_childrentotal <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + 
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8537.992, -2Loglik = 8513.992, 11 param

glmm_childrentotal2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8404.669, -2Loglik = 8366.669, 18 param

## Is there overdispersion?
dispersion_glmer(glmm_childrentotal2)
#No (0.97), so continue with binomial distribution

glmm_childrentotal2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8408.137, -2Loglik = 8366.137, 20 param

glmm_childrentotal2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8403.283, -2Loglik = 8361.283, 20 param

glmm_childrentotal2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8404.659, -2Loglik = 8358.659, 22 param

glmm_childrentotal2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.937, -2Loglik = 8362.937, 19 param --> BEST IMPROVEMENT

pchisq(8366.669-8366.137, df=length(fixef(glmm_childrentotal2.1))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)
pchisq(8366.669-8361.283, df=length(fixef(glmm_childrentotal2.2))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)
pchisq(8366.669-8358.659, df=length(fixef(glmm_childrentotal2.3))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)
pchisq(8366.669-8362.937, df=length(fixef(glmm_childrentotal2.4))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)

glmm_childrentotal3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8406.430, -2Loglik = 8362.430, 21 param 

glmm_childrentotal3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.301, -2Loglik = 8358.301, 21 param 

glmm_childrentotal3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.920, -2Loglik = 8354.920, 23 param 

pchisq(8362.937-8362.430, df=length(fixef(glmm_childrentotal3.1))-length(fixef(glmm_childrentotal2.4)), lower.tail=FALSE)
pchisq(8362.937-8358.301, df=length(fixef(glmm_childrentotal3.2))-length(fixef(glmm_childrentotal2.4)), lower.tail=FALSE)
pchisq(8362.937-8354.920, df=length(fixef(glmm_childrentotal3.3))-length(fixef(glmm_childrentotal2.4)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE

# Can we remove main effects?

glmm_childrentotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8491.868, -2Loglik = 8455.868, 17 param

glmm_childrentotalnohhsize <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    part_face_mask + part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8406.943, -2Loglik = 8370.943, 17 param

glmm_childrentotalnofacemask <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8505.610, -2Loglik = 8467.610, 18 param

glmm_childrentotalnosocialgroup <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8399.404, -2Loglik = 8365.404, 16 param

glmm_childrentotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.491, -2Loglik = 8508.491, 12 param

pchisq(8365.404-8362.937, df=length(fixef(glmm_childrentotal2.4))-length(fixef(glmm_childrentotalnosocialgroup)), lower.tail=FALSE)

glmm_childrentotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + hhsize_cat +
    part_face_mask + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8489.178, -2Loglik = 8459.178, 14 param

glmm_childrentotalnohhsize <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    part_face_mask + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.693, -2Loglik = 8372.693, 14 param

glmm_childrentotalnofacemask <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8501.658, -2Loglik = 8469.658, 15 param

glmm_childrentotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8530.689, -2Loglik = 8510.689, 9 param

pchisq(8372.693-8365.404, df=length(fixef(glmm_childrentotalnosocialgroup))-length(fixef(glmm_childrentotalnohhsize)), lower.tail=FALSE)

## FINAL MODEL
summary(glmm_childrentotalnosocialgroup)

library(DHARMa)
simulationoutput <- simulateResiduals(fittedModel = glmm_childrentotalnosocialgroup)
plot(simulationoutput)
testDispersion(simulationoutput)
