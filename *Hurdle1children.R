library(lme4)

glmm_children <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8222.7, -2Loglik = 8198.7, 11 param
## Is there overdispersion?
dispersion_glmer(glmm_children)
#No (0.99), so continue with binomial distribution

glmm_children2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8226.394, -2Loglik = 8198.394, 13 param

glmm_children2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8218.776, -2Loglik = 8190.776, 13 param

glmm_children2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8223.752, -2Loglik = 8191.752, 15 param

glmm_children2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8217.939, -2Loglik = 8191.939, 12 param --> BEST IMPROVEMENT

pchisq(8198.7-8191.939, df=length(fixef(glmm_children2.4))-length(fixef(glmm_children)), lower.tail=FALSE)

glmm_children3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8221.718, -2Loglik = 8191.718, 14 param

glmm_children3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8215.637, -2Loglik = 8185.637, 14 param --> BEST IMPROVEMENT

glmm_children3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8218.977, -2Loglik = 8184.976, 16 param

pchisq(8191.939-8185.637, df=length(fixef(glmm_children3.2))-length(fixef(glmm_children2.4)), lower.tail=FALSE)

glmm_children4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + holiday:area_3_name + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8219.266, -2Loglik = 8185.266, 16 param

glmm_children4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + holiday:area_3_name + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8216.661, -2Loglik = 8178.661, 18 param

pchisq(8185.637-8178.661, df=length(fixef(glmm_children4.2))-length(fixef(glmm_children3.2)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENT ANYMORE

summary(glmm_children3.2)
