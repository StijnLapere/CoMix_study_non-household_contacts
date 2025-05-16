library(lme4)

logisticdataset_noagegender_childrennonhh <- logisticdataset_noagegender_children %>% filter(any_nonhh_contact == 1)

hurdle2_children1 <- glmer(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 40206.65, -2Loglik = 40182.65, 11 param
summary(hurdle2_children1)

hurdle2_children1wavecount <- glmer(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 39906.75, -2Loglik = 39868.75, 18 param

hurdle2_children1wavecountshort <- glmer(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 40185.92, -2Loglik = 40157.92, 13 param

## Is there overdispersion?
library("blmeco") 
dispersion_glmer(hurdle2_children1wavecount)
#Yes (2.96), so consider negative binomial distribution

#Move to negative binomial distribution
hurdle2_childrennb1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 20322.84, -2Loglik = 20296.84, 11 param

hurdle2_childrennb1wavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#Failed to converge

hurdle2_childrennb1wavecountshort <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20292.53, -2Loglik = 20262.53, 13 param

hurdle2_children2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20294.23, -2Loglik = 20260.23, 15 param

hurdle2_children2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + holiday:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20295.47, -2Loglik = 20261.47, 15 param

hurdle2_children2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + hhsize_cat:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20295.87, -2Loglik = 20257.87, 17 param

hurdle2_children2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20291.45, -2Loglik = 20259.45, 14 param

pchisq(20262.53-20260.23, df=length(fixef(hurdle2_children2.1))-length(fixef(hurdle2_childrennb1wavecountshort)), lower.tail=FALSE)
pchisq(20262.53-20261.47, df=length(fixef(hurdle2_children2.2))-length(fixef(hurdle2_childrennb1wavecountshort)), lower.tail=FALSE)
pchisq(20262.53-20257.87, df=length(fixef(hurdle2_children2.3))-length(fixef(hurdle2_childrennb1wavecountshort)), lower.tail=FALSE)
pchisq(20262.53-20259.45, df=length(fixef(hurdle2_children2.4))-length(fixef(hurdle2_childrennb1wavecountshort)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS
# Can we remove a main effect?

hurdle2_childrennoarea <- glmer.nb(
  num_contacts ~ holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20321.44, -2Loglik = 20295.44, 11 param

hurdle2_childrennoholiday <- glmer.nb(
  num_contacts ~ area_3_name + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20351.89, -2Loglik = 20323.89, 12 param

hurdle2_childrennowd <- glmer.nb(
  num_contacts ~ area_3_name + holiday + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20295.21, -2Loglik = 20267.21, 12 param

hurdle2_childrennohhsize <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20296.69, -2Loglik = 20270.69, 11 param

hurdle2_childrennofacemask <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20299.25, -2Loglik = 20271.25, 12 param

hurdle2_childrennosocialgroup <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20287.38, -2Loglik = 20263.38, 10 param

hurdle2_childrennowavecountshort <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20322.84, -2Loglik = 20296.84, 11 param

pchisq(20263.38-20262.53, df=length(fixef(hurdle2_childrennb1wavecountshort))-length(fixef(hurdle2_childrennosocialgroup)), lower.tail=FALSE)

hurdle2_childrennoarea <- glmer.nb(
  num_contacts ~ holiday + wd + hhsize_cat +
    part_face_mask + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20316.91, -2Loglik = 20296.91, 10 param

hurdle2_childrennoholiday <- glmer.nb(
  num_contacts ~ area_3_name + wd + hhsize_cat +
    part_face_mask + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20346.57, -2Loglik = 20324.57, 9 param

hurdle2_childrennowd <- glmer.nb(
  num_contacts ~ area_3_name + holiday + hhsize_cat +
    part_face_mask + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20290.12, -2Loglik = 20268.12, 9 param

hurdle2_childrennohhsize <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd +
    part_face_mask + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20292.59, -2Loglik = 20272.59, 8 param

hurdle2_childrennofacemask <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20294.09, -2Loglik = 20272.09, 9 param

hurdle2_childrennowavecountshort <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 20317.33, -2Loglik = 20297.33, 8 param

pchisq(20268.12-20263.38, df=length(fixef(hurdle2_childrennosocialgroup))-length(fixef(hurdle2_childrennowd)), lower.tail=FALSE)

## FINAL MODEL
summary(hurdle2_childrennosocialgroup)
