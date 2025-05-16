glmm_childrentotal2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8541.685, -2Loglik = 8513.685, 13 param

glmm_childrentotal2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.995, -2Loglik = 8506.995, 13 param

glmm_childrentotal2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8539.182, -2Loglik = 8507.182, 15 param

glmm_childrentotal2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.491, -2Loglik = 8508.491, 12 param --> BEST IMPROVEMENT

pchisq(8513.992-8506.995, df=length(fixef(glmm_childrentotal2.2))-length(fixef(glmm_childrentotal)), lower.tail=FALSE)
pchisq(8513.992-8508.491, df=length(fixef(glmm_childrentotal2.4))-length(fixef(glmm_childrentotal)), lower.tail=FALSE)

glmm_childrentotal3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8538.214, -2Loglik = 8508.214, 14 param 

glmm_childrentotal3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8532.706, -2Loglik = 8502.706, 14 param 

glmm_childrentotal3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8535.653, -2Loglik = 8501.653, 16 param 

pchisq(8508.491-8502.706, df=length(fixef(glmm_childrentotal3.2))-length(fixef(glmm_childrentotal2.4)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE

summary(glmm_childrentotal2.4)
