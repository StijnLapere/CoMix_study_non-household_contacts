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
hurdle2_elderlynb1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#AIC = 18126.379, -2Loglik = 18088.379, 17 param

hurdle2_elderlynb1wavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 18118.381, -2Loglik = 18066.381, 24 param

hurdle2_elderlynb1wavecountshort <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 9e5)))
#AIC = 18116.993, -2Loglik = 18074.993, 19 param

hurdle2_elderly2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.822, -2Loglik = 18062.822, 27 param

hurdle2_elderly2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.163, -2Loglik = 18066.163, 25 param

hurdle2_elderly2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18117.965, -2Loglik = 18063.965, 25 param

hurdle2_elderly2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.381, -2Loglik = 18066.381, 25 param

hurdle2_elderly2.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.804, -2Loglik = 18064.804, 26 param

hurdle2_elderly2.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18121.00, -2Loglik = 18065.00, 26 param

hurdle2_elderly2.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18118.890, -2Loglik = 18064.890, 25 param

hurdle2_elderly2.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18119.363, -2Loglik = 18065.363, 25 param

hurdle2_elderly2.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18112.704, -2Loglik = 18056.704, 26 param --> BEST IMPROVEMENT

hurdle2_elderly2.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18121.821, -2Loglik = 18065.821, 26 param

hurdle2_elderly2.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.183, -2Loglik = 18062.183, 28 param

hurdle2_elderly2.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.18, -2Loglik = 18066.18, 25 param

pchisq(18066.381-18056.704, df=length(fixef(hurdle2_elderly2.9))-length(fixef(hurdle2_elderlynb1wavecount)), lower.tail=FALSE)

hurdle2_elderly3.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18115.166, -2Loglik = 18053.166, 29 param

hurdle2_elderly3.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18114.505, -2Loglik = 18056.505, 27 param

hurdle2_elderly3.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18111.442, -2Loglik = 18053.442, 27 param

hurdle2_elderly3.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18114.702, -2Loglik = 18056.702, 27 param

hurdle2_elderly3.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18115.036, -2Loglik = 18055.036, 28 param

hurdle2_elderly3.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18115.312, -2Loglik = 18055.312, 28 param

hurdle2_elderly3.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18113.368, -2Loglik = 18055.368, 27 param

hurdle2_elderly3.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18114.172, -2Loglik = 18056.172, 27 param

hurdle2_elderly3.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18116.063, -2Loglik = 18056.063, 28 param

hurdle2_elderly3.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18116.855, -2Loglik = 18052.855, 30 param

hurdle2_elderly3.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18114.461, -2Loglik = 18056.461, 27 param

pchisq(18056.704-18053.442, df=length(fixef(hurdle2_elderly3.3))-length(fixef(hurdle2_elderly2.9)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE

# Can we remove a main effect?

hurdle2_elderlynoholiday <- glmer.nb(
  num_contacts ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18113.170, -2Loglik = 18059.170, 25 param

hurdle2_elderlynowd <- glmer.nb(
  num_contacts ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18121.733, -2Loglik = 18067.733, 25 param

hurdle2_elderlyeducation <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18109.050, -2Loglik = 18057.050, 24 param

hurdle2_elderlynovacc <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18177.396, -2Loglik = 18123.396, 25 param

hurdle2_elderlynoelevrisk <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18126.20, -2Loglik = 18072.20, 25 param

hurdle2_elderlynosymp <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18115.711, -2Loglik = 18061.711, 25 param

hurdle2_elderlynogender <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18113.30, -2Loglik = 18059.30, 25 param

hurdle2_elderlynosocialgroup <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18108.158, -2Loglik = 18058.158, 23 param

hurdle2_elderlynohhsize <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18108.965, -2Loglik = 18056.965, 24 param

hurdle2_elderlynowavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.893, -2Loglik = 18078.893, 19 param

pchisq(18057.050-18056.704, df=length(fixef(hurdle2_elderly2.9))-length(fixef(hurdle2_elderlyeducation)), lower.tail=FALSE)
pchisq(18058.158-18056.704, df=length(fixef(hurdle2_elderly2.9))-length(fixef(hurdle2_elderlynosocialgroup)), lower.tail=FALSE)

hurdle2_elderlynoholiday <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18109.506, -2Loglik = 18059.506, 23 param

hurdle2_elderlynowd <- glmer.nb(
  num_contacts ~ area_3_name + holiday + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18118.065, -2Loglik = 18068.065, 23 param

hurdle2_elderlynovacc <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18173.755, -2Loglik = 18123.755, 23 param

hurdle2_elderlynoelevrisk <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.512, -2Loglik = 18072.512, 23 param

hurdle2_elderlynosymp <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18112.00, -2Loglik = 18062.00, 23 param

hurdle2_elderlynogender <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    part_social_group_be + hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18109.658, -2Loglik = 18059.658, 23 param

hurdle2_elderlynosocialgroup <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18104.225, -2Loglik = 18058.225, 21 param

hurdle2_elderlynohhsize <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18105.299, -2Loglik = 18057.299, 22 param

hurdle2_elderlynowavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18117.171, -2Loglik = 18079.171, 17 param

pchisq(18058.225-18057.050, df=length(fixef(hurdle2_elderlyeducation))-length(fixef(hurdle2_elderlynosocialgroup)), lower.tail=FALSE)

hurdle2_elderlynoholiday <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18104.642, -2Loglik = 18060.642, 20 param

hurdle2_elderlynowd <- glmer.nb(
  num_contacts ~ area_3_name + holiday + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18113.234, -2Loglik = 18069.234, 20 param

hurdle2_elderlynovacc <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18169.011, -2Loglik = 18125.011, 20 param

hurdle2_elderlynoelevrisk <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18118.112, -2Loglik = 18074.112, 20 param

hurdle2_elderlynosympnone <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18107.130, -2Loglik = 18063.130, 20 param

hurdle2_elderlynogender <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    hhsize_elderly + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18104.890, -2Loglik = 18060.890, 20 param

hurdle2_elderlynohhsize <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.504, -2Loglik = 18058.504, 19 param

hurdle2_elderlynowavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18112.259, -2Loglik = 18080.259, 14 param

pchisq(18058.504-18058.225, df=length(fixef(hurdle2_elderlynosocialgroup))-length(fixef(hurdle2_elderlynohhsize)), lower.tail=FALSE)

hurdle2_elderlynoholiday <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.905, -2Loglik = 18060.905, 18 param

hurdle2_elderlynowd <- glmer.nb(
  num_contacts ~ area_3_name + holiday + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18109.474, -2Loglik = 18069.474, 18 param

hurdle2_elderlynovacc <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18165.190, -2Loglik = 18125.190, 18 param

hurdle2_elderlynoelevrisk <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18114.304, -2Loglik = 18074.304, 18 param

hurdle2_elderlynosymp <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18103.446, -2Loglik = 18063.446, 18 param

hurdle2_elderlynogender <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18101.239, -2Loglik = 18061.239, 18 param

hurdle2_elderlynowavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18108.536, -2Loglik = 18080.536, 12 param

pchisq(18060.905-18058.504, df=length(fixef(hurdle2_elderlynohhsize))-length(fixef(hurdle2_elderlynoholiday)), lower.tail=FALSE)

hurdle2_elderlynowd <- glmer.nb(
  num_contacts ~ area_3_name + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.905, -2Loglik = 18060.905, 17 param

hurdle2_elderlynovacc <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.905, -2Loglik = 18060.905, 17 param

hurdle2_elderlynoelevrisk <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_face_mask + part_symp_none + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.905, -2Loglik = 18060.905, 17 param

hurdle2_elderlynosympnone <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.905, -2Loglik = 18060.905, 17 param

hurdle2_elderlynogender <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.905, -2Loglik = 18060.905, 17 param

hurdle2_elderlynowavecount <- glmer.nb(
  num_contacts ~ area_3_name + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18100.905, -2Loglik = 18060.905, 11 param

















hurdle2_elderly2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18129.373, -2Loglik = 18085.373, 20 param

hurdle2_elderly2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.095, -2Loglik = 18088.095, 18 param

hurdle2_elderly2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18126.395, -2Loglik = 18086.395, 18 param

hurdle2_elderly2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.378, -2Loglik = 18088.378, 18 param

hurdle2_elderly2.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.890, -2Loglik = 18086.890, 19 param

hurdle2_elderly2.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.54, -2Loglik = 18086.54, 19 param

hurdle2_elderly2.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18127.065, -2Loglik = 18087.065, 18 param

hurdle2_elderly2.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18127.302, -2Loglik = 18087.302, 18 param

hurdle2_elderly2.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.893, -2Loglik = 18078.893, 19 param --> BEST IMPROVEMENT

hurdle2_elderly2.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18129.733, -2Loglik = 18087.733, 19 param

hurdle2_elderly2.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18130.433, -2Loglik = 18084.433, 21 param

hurdle2_elderly2.12 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18128.00, -2Loglik = 18088.00, 18 param

pchisq(18088.379-18078.893, df=length(fixef(hurdle2_elderly2.9))-length(fixef(hurdle2_elderlynb1)), lower.tail=FALSE)

hurdle2_elderly3.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18123.898, -2Loglik = 18075.898, 22 param

hurdle2_elderly3.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.630, -2Loglik = 18078.630, 20 param

hurdle2_elderly3.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18120.16, -2Loglik = 18076.16, 20 param --> BEST IMPROVEMENT

hurdle2_elderly3.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.892, -2Loglik = 18078.892, 20 param

hurdle2_elderly3.5 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18123.310, -2Loglik = 18077.310, 21 param

hurdle2_elderly3.6 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_vacc:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18123.068, -2Loglik = 18077.068, 21 param

hurdle2_elderly3.7 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18121.737, -2Loglik = 18077.737, 20 param

hurdle2_elderly3.8 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.316, -2Loglik = 18078.316, 20 param

hurdle2_elderly3.9 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18124.173, -2Loglik = 18078.173, 21 param

hurdle2_elderly3.10 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + area_3_name:hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18125.275, -2Loglik = 18075.275, 23 param

hurdle2_elderly3.11 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynonhh,
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 18122.46, -2Loglik = 18078.46, 20 param

pchisq(18078.893-18076.16, df=length(fixef(hurdle2_elderly3.3))-length(fixef(hurdle2_elderly2.9)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENT ANYMORE










#Is there overdispersion?
# Calculate Pearson residuals
resid_pearson <- residuals(hurdle2_elderly1, type = "pearson")
# Calculate dispersion statistic
dispersion_stat <- sum(resid_pearson^2) / df.residual(hurdle2_elderly1)
#4.379894 --> CLEAR OVERDISPERSION

