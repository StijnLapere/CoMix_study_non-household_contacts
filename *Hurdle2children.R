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

## Is there overdispersion?
dispersion_glmer(hurdle2_children1)
#Yes (2.96), so consider negative binomial distribution

#Move to negative binomial distribution
hurdle2_childrennb1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 20322.84, -2Loglik = 20296.84, 11 param

hurdle2_children2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 20324.88, -2Loglik = 20294.88, 13 param

hurdle2_children2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 20325.75, -2Loglik = 20295.75, 13 param

hurdle2_children2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + hhsize_cat:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 20326.72, -2Loglik = 20292.72, 15 param

hurdle2_children2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 20321.46, -2Loglik = 20293.46, 12 param

pchisq(20296.84-20293.46, df=length(fixef(hurdle2_children2.4))-length(fixef(hurdle2_children1)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS

summary(hurdle2_children1)
