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

hurdle2_elderly1wavecount <- glmer(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 24376.76, -2Loglik = 24326.76, 24 param

hurdle2_elderly1wavecountshort <- glmer(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = poisson(link = "log"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 24435.98, -2Loglik = 24395.98, 19 param


## Is there overdispersion?
library("blmeco") 
dispersion_glmer(hurdle2_elderly1wavecount)
#Yes (1.70), so consider negative binomial distribution

#Move to negative binomial distribution
library(glmmTMB)

hurdle2_elderlynb1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15411.937, -2Loglik = 15373.937, 17 param

hurdle2_elderlynb1wavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15398.44, -2Loglik = 15346.44, 24 param

hurdle2_elderlynb1wavecountshort <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15400.423, -2Loglik = 15358.423, 19 param

hurdle2_elderly2.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15400.859, -2Loglik = 15342.859, 27 param

hurdle2_elderly2.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15400.437, -2Loglik = 15346.437, 25 param

hurdle2_elderly2.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15399.046, -2Loglik = 15345.046, 25 param

hurdle2_elderly2.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15400.197, -2Loglik = 15346.197, 25 param

hurdle2_elderly2.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15398.774, -2Loglik = 15342.774, 26 param

hurdle2_elderly2.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15401.008, -2Loglik = 15345.008, 26 param

hurdle2_elderly2.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15399.248, -2Loglik = 15345.248, 25 param

hurdle2_elderly2.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15399.565, -2Loglik = 15345.565, 25 param

hurdle2_elderly2.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15394.341, -2Loglik = 15338.341, 26 param --> BEST IMPROVEMENT

hurdle2_elderly2.10 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15401.979, -2Loglik = 15345.979, 26 param

hurdle2_elderly2.11 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15402.854, -2Loglik = 15342.854, 28 param

hurdle2_elderly2.12 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15400.394, -2Loglik = 15346.394, 25 param

pchisq(15346.44-15338.341, df=length(fixef(hurdle2_elderly2.9)$cond)-length(fixef(hurdle2_elderlynb1wavecount)$cond), lower.tail=FALSE)

hurdle2_elderly3.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.931, -2Loglik = 15334.931, 29 param

hurdle2_elderly3.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.336, -2Loglik = 15338.336, 27 param

hurdle2_elderly3.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15394.186, -2Loglik = 15336.186, 27 param

hurdle2_elderly3.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.149, -2Loglik = 15338.149, 27 param

hurdle2_elderly3.5 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15394.415, -2Loglik = 15334.415, 28 param

hurdle2_elderly3.6 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.735, -2Loglik = 15336.735, 28 param

hurdle2_elderly3.7 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15395.403, -2Loglik = 15337.403, 27 param

hurdle2_elderly3.8 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15395.706, -2Loglik = 15337.706, 27 param

hurdle2_elderly3.9 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15397.850, -2Loglik = 15337.850, 28 param

hurdle2_elderly3.10 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15399.149, -2Loglik = 15335.149, 30 param

hurdle2_elderly3.11 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.276, -2Loglik = 15338.276, 27 param

pchisq(15338.341-15336.186, df=length(fixef(hurdle2_elderly3.3)$cond)-length(fixef(hurdle2_elderly2.9)$cond), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS
# Can we remove a main effect?
hurdle2_elderlynoholiday <- glmmTMB(
  num_contacts ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15397.855, -2Loglik = 15343.855, 25 param

hurdle2_elderlynowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15401.678, -2Loglik = 15347.678, 25 param

hurdle2_elderlynoeducation <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15390.591, -2Loglik = 15338.591, 24 param

hurdle2_elderlynovacc <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15459.305, -2Loglik = 15405.305, 25 param

hurdle2_elderlynoelevrisk <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15405.777, -2Loglik = 15351.777, 25 param

hurdle2_elderlynosymptoms <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.030, -2Loglik = 15342.030, 25 param

hurdle2_elderlynogender <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15394.626, -2Loglik = 15340.626, 25 param

hurdle2_elderlynosocialgroup <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15389.168, -2Loglik = 15339.168, 23 param

hurdle2_elderlynohhsize <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15390.386, -2Loglik = 15338.386, 24 param

hurdle2_elderlynowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15408.166, -2Loglik = 15366.166, 19 param

pchisq(15339.168-15338.341, df=length(fixef(hurdle2_elderly2.9)$cond)-length(fixef(hurdle2_elderlynosocialgroup)$cond), lower.tail=FALSE)

hurdle2_elderlynoholiday <- glmmTMB(
  num_contacts ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15392.623, -2Loglik = 15344.623, 22 param

hurdle2_elderlynowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.487, -2Loglik = 15348.487, 22 param

hurdle2_elderlynoeducation <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15385.211, -2Loglik = 15339.211, 21 param

hurdle2_elderlynovacc <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15454.24, -2Loglik = 15406.24, 22 param

hurdle2_elderlynoelevrisk <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15400.78, -2Loglik = 15352.78, 22 param

hurdle2_elderlynosymptoms <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15390.753, -2Loglik = 15342.753, 22 param

hurdle2_elderlynogender <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15389.496, -2Loglik = 15341.496, 22 param

hurdle2_elderlynohhsize <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15385.194, -2Loglik = 15339.194, 21 param

hurdle2_elderlynowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15402.90, -2Loglik = 15366.90, 16 param

pchisq(15339.211-15339.168, df=length(fixef(hurdle2_elderlynosocialgroup)$cond)-length(fixef(hurdle2_elderlynoeducation)$cond), lower.tail=FALSE)
pchisq(15339.194-15339.168, df=length(fixef(hurdle2_elderlynosocialgroup)$cond)-length(fixef(hurdle2_elderlynohhsize)$cond), lower.tail=FALSE)

hurdle2_elderlynoholiday <- glmmTMB(
  num_contacts ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15388.655, -2Loglik = 15344.655, 20 param

hurdle2_elderlynowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15392.498, -2Loglik = 15348.498, 20 param

hurdle2_elderlynoeducation <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15381.236, -2Loglik = 15339.236, 19 param

hurdle2_elderlynovacc <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15450.287, -2Loglik = 15406.287, 20 param

hurdle2_elderlynoelevrisk <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15396.796, -2Loglik = 15352.796, 20 param

hurdle2_elderlynosymptoms <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15386.790, -2Loglik = 15342.790, 20 param

hurdle2_elderlynogender <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15385.534, -2Loglik = 15341.534, 20 param

hurdle2_elderlynowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15398.908, -2Loglik = 15366.908, 14 param

pchisq(15339.236-15339.194, df=length(fixef(hurdle2_elderlynohhsize)$cond)-length(fixef(hurdle2_elderlynoeducation)$cond), lower.tail=FALSE)

hurdle2_elderlynoholiday <- glmmTMB(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15384.698, -2Loglik = 15344.698, 18 param

hurdle2_elderlynowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15388.536, -2Loglik = 15348.536, 18 param

hurdle2_elderlynovacc <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15446.335, -2Loglik = 15406.335, 18 param

hurdle2_elderlynoelevrisk <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15392.898, -2Loglik = 15352.898, 18 param

hurdle2_elderlynosymptoms <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15382.834, -2Loglik = 15342.834, 18 param

hurdle2_elderlynogender <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15381.551, -2Loglik = 15341.551, 18 param

hurdle2_elderlynowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15394.94, -2Loglik = 15366.94, 12 param

pchisq(15341.551-15339.236, df=length(fixef(hurdle2_elderlynoeducation)$cond)-length(fixef(hurdle2_elderlynogender)$cond), lower.tail=FALSE)

hurdle2_elderlynoholiday <- glmmTMB(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15385.066, -2Loglik = 15347.066, 17 param

hurdle2_elderlynowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15388.823, -2Loglik = 15350.823, 17 param

hurdle2_elderlynovacc <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_elevated_risk + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15446.631, -2Loglik = 15408.631, 17 param

hurdle2_elderlynoelevrisk <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15393.391, -2Loglik = 15355.391, 17 param

hurdle2_elderlynosymptoms <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15383.10, -2Loglik = 15345.10, 17 param

hurdle2_elderlynowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  family = truncated_nbinom2)
#AIC = 15394.988, -2Loglik = 15368.988, 11 param

pchisq(15345.10-15341.551, df=length(fixef(hurdle2_elderlynogender)$cond)-length(fixef(hurdle2_elderlynosymptoms)$cond), lower.tail=FALSE)

## Final model
finalmodelhurdle2elderly <- hurdle2_elderlynogender

library(DHARMa)
simulationoutput <- simulateResiduals(fittedModel = finalmodelhurdle2elderly, n=1000)
plot(simulationoutput)
testOutliers(simulationoutput, type = "bootstrap")
testDispersion(simulationoutput)

df <- data.frame(
  Covariate = c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", 
                "Holiday No", "Holiday Yes", "Weekday", "Weekend",
                "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", "Symptoms No", "Symptoms Yes", "Face mask No", "Face mask Yes",
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Brussels Hoofdstede : Face mask No", "Vlaams Gewest : Face mask Yes", "Waals Gewest : Face mask Yes"),
  Estimate = c(0, -0.19596, -0.31952, 
               0, 0.12024, 0, 0.15602, 
               0, 0.64438, 0, -0.01671, 0, -0.15582, 0, -0.34694, 
               0, 0.16023, -0.30736, 0.02683, -0.14388, -0.20608, -0.24968, -0.35092, 
               0, 0.66708, 0.41927),
  SE = c(0, 0.29991, 0.31388, 
         0, 0.05137, 0, 0.05131, 
         0, 0.08850, 0, 0.08213, 0, 0.08292, 0, 0.25826, 
         0, 0.13101, 0.14189, 0.13849, 0.14363, 0.14566, 0.14726, 0.11279,
         0, 0.26995, 0.28472)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", 
                         "Holiday No", "Holiday Yes", "Weekday", "Weekend",
                         "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", "Symptoms No", "Symptoms Yes", "Face mask No", "Face mask Yes",
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                         "Brussels Hoofdstede : Face mask No", "Vlaams Gewest : Face mask Yes", "Waals Gewest : Face mask Yes"))

df$Covariate <- factor(df$Covariate, levels = covariate_order)

library(ggplot2)
# Plot using ggplot2
ggplot(df, aes(x = RelativeContacts, y = reorder(Covariate, RelativeContacts))) +
  geom_point(size = 2, color = "red") + 
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.5, linewidth = 0.8, color = "black") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(x = "Relative Number of having non-household contacts", y = "Covariates") +
  theme(axis.text.y = element_text(size = 10)) +
  scale_y_discrete(limits = covariate_order) # Ensure correct order

