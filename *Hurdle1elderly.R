library(lme4)

glmm_model <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
# Model is nearly unidentifiable
print(summary(glmm_model),correlation=TRUE) # Correlation between hhsizes +- 1

table(logisticdataset_noage_Elderly$any_nonhh_contact,logisticdataset_noage_Elderly$hhsize_elderly)
# No participants with hhsize = 1 without nonh contacts --> ref level of hhsize_elderly is 2 instead of 1

logisticdataset_noage_Elderlynohhsize1 <- logisticdataset_noage_Elderly %>%
  filter(hhsize_elderly != 1)

########## MODEL BUILDING ##########
glmm_model <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6021.4, -2loglik = 5987.4, 16 param
## Is there overdispersion?
library("blmeco") 
dispersion_glmer(glmm_model)
#No (0.97), so continue with binomial distribution

glmm_elderly2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6025.949, -2loglik = 5985.949, 19 param

glmm_elderly2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6021.158, -2loglik = 5985.158, 17 param

glmm_elderly2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6015.844, -2loglik = 5979.844, 17 param --> BEST IMPROVEMENT

glmm_elderly2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6023.126, -2loglik = 5987.126, 17 param

glmm_elderly2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6024.908, -2loglik = 5986.908, 18 param

glmm_elderly2.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6022.197, -2loglik = 5986.197, 17 param

glmm_elderly2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6016.268, -2loglik = 5989.268, 17 param

glmm_elderly2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6020.578, -2loglik = 5984.578, 17 param

glmm_elderly2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6021.043, -2loglik = 5983.043, 18 param

glmm_elderly2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6023.662, -2loglik = 5985.662, 18 param

glmm_elderly2.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6025.381, -2loglik = 5987.381, 18 param

glmm_elderly2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + holiday:wd +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6022.515, -2loglik = 5986.515, 17 param

pchisq(5987.4-5979.844, df=length(fixef(glmm_elderly2.3))-length(fixef(glmm_model)), lower.tail=FALSE)

glmm_elderly3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6020.343, -2loglik = 5978.343, 20 param

glmm_elderly3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6015.854, -2loglik = 5977.854, 18 param

glmm_elderly3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6017.541, -2loglik = 5979.541, 18 param

glmm_elderly3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6019.171, -2loglik = 5979.171, 19 param

glmm_elderly3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6015.859, -2loglik = 5977.859, 18 param

glmm_elderly3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6010.133, -2loglik = 5972.133, 18 param --> BEST IMPROVEMENT

glmm_elderly3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6015.091, -2loglik = 5977.091, 18 param

glmm_elderly3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6015.975, -2loglik = 5975.975, 19 param

glmm_elderly3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6018.334, -2loglik = 5978.334, 19 param

glmm_elderly3.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6019.834, -2loglik = 5979.834, 19 param

glmm_elderly3.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6016.979, -2loglik = 5978.979, 18 param

pchisq(5979.844-5972.133, df=length(fixef(glmm_elderly3.6))-length(fixef(glmm_elderly2.3)), lower.tail=FALSE)

glmm_elderly4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6014.658, -2loglik = 5970.658, 21 param

glmm_elderly4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6010.550, -2loglik = 5970.550, 19 param

glmm_elderly4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6011.849, -2loglik = 5971.849, 19 param

glmm_elderly4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6013.479, -2loglik = 5971.479, 20 param

glmm_elderly4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_vacc:hhsize_elderly +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6010.011, -2loglik = 5970.011, 19 param

glmm_elderly4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6009.258, -2loglik = 5969.258, 19 param --> IMPROVEMENT

glmm_elderly4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6009.556, -2loglik = 5967.556, 20 param --> IMPROVEMENT

glmm_elderly4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6012.805, -2loglik = 5970.805, 20 param

glmm_elderly4.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + area_3_name:hhsize_elderly +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6014.118, -2loglik = 5972.118, 20 param

glmm_elderly4.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6011.124, -2loglik = 5971.124, 19 param

pchisq(5972.133-5969.258, df=length(fixef(glmm_elderly4.6))-length(fixef(glmm_elderly3.6)), lower.tail=FALSE)
pchisq(5972.133-5967.556, df=length(fixef(glmm_elderly4.7))-length(fixef(glmm_elderly3.6)), lower.tail=FALSE)
### NO SIGNIFICANT IMPROVEMENTS ANYMORE

summary(glmm_elderly3.6) ## Effect of hhsize not significant

#We will not remove main effects, but this can be done later

## Consider model without hhsize -> use all observations
glmm2_elderly1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6456.316, -2loglik = 6424.316, 15 param
## Is there overdispersion?
dispersion_glmer(glmm2_elderly1)
#No (0.88), so continue with binomial distribution

glmm2_elderly2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6460.288, -2loglik = 6422.288, 18 param

glmm2_elderly2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6455.798, -2loglik = 6421.798, 16 param

glmm2_elderly2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_vacc:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6451.142, -2loglik = 6417.142, 16 param

glmm2_elderly2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6457.870, -2loglik = 6423.870, 16 param

glmm2_elderly2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6458.504, -2loglik = 6422.504, 17 param

glmm2_elderly2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6450.839, -2loglik = 6416.839, 16 param --> BEST IMPROVEMENT

glmm2_elderly2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6454.948, -2loglik = 6420.948, 16 param

glmm2_elderly2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6456.50, -2loglik = 6420.50, 17 param

glmm2_elderly2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6458.850, -2loglik = 6422.850, 17 param

glmm2_elderly2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + holiday:wd +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6456.868, -2loglik = 6422.868, 16 param

pchisq(6424.316-6416.839, df=length(fixef(glmm2_elderly2.7))-length(fixef(glmm2_elderly1)), lower.tail=FALSE)

glmm2_elderly3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6454.931, -2loglik = 6414.931, 19 param

glmm2_elderly3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6450.707, -2loglik = 6416.707, 17 param

glmm2_elderly3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6445.062, -2loglik = 6409.062, 17 param --> BEST IMPROVEMENT

glmm2_elderly3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6452.414, -2loglik = 6416.414, 17 param

glmm2_elderly3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6453.012, -2loglik = 6415.012, 18 param

glmm2_elderly3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6449.355, -2loglik = 6413.355, 17 param

glmm2_elderly3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6450.359, -2loglik = 6412.359, 18 param

glmm2_elderly3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6453.558, -2loglik = 6415.558, 18 param

glmm2_elderly3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6451.209, -2loglik = 6415.209, 17 param

pchisq(6416.839-6409.062, df=length(fixef(glmm2_elderly3.3))-length(fixef(glmm2_elderly2.7)), lower.tail=FALSE)

glmm2_elderly4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + part_social_group_be:part_vacc +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6449.051, -2loglik = 6407.051, 20 param

glmm2_elderly4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + part_vacc:part_elevated_risk +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6445.255, -2loglik = 6407.255, 18 param

glmm2_elderly4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + part_vacc:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6446.558, -2loglik = 6408.558, 18 param

glmm2_elderly4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + part_vacc:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6446.889, -2loglik = 6406.889, 19 param

glmm2_elderly4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + part_face_mask:part_symp_none +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6443.698, -2loglik = 6405.698, 18 param --> BEST IMPROVEMENT

glmm2_elderly4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6445.082, -2loglik = 6405.082, 19 param

glmm2_elderly4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + area_3_name:holiday +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6447.972, -2loglik = 6407.972, 19 param

glmm2_elderly4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdataset_noage_Elderly,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6445.454, -2loglik = 6407.454, 18 param

pchisq(6409.062-6405.698, df=length(fixef(glmm2_elderly4.5))-length(fixef(glmm2_elderly3.3)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENT ANYMORE

summary(glmm2_elderly3.3)











##### START REMOVING MAIN EFFECTS OF FIRST TYPE OF MODEL #####
## Can we remove a main effect?
glmm_elderlynoarea3 <- glmer(
  any_nonhh_contact ~ holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6012.852, -2loglik = 5978.852, 16 param

glmm_elderlynoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6009.646, -2loglik = 5973.646, 17 param

glmm_elderlynowd <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6009.224, -2loglik = 5973.224, 17 param

glmm_elderlynoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6008.015, -2loglik = 5974.015, 16 param

glmm_elderlynosympnone <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6008.489, -2loglik = 5972.489, 17 param --> SMALLEST INCREASE

glmm_elderlynogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6009.528, -2loglik = 5973.528, 17 param

glmm_elderlynosocialgroup <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6008.546, -2loglik = 5976.546, 15 param

glmm_elderlynohhsize <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_symp_none + part_gender +
    part_social_group_be + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6010.127, -2loglik = 5974.127, 17 param

pchisq(5972.489-5972.133, df=length(fixef(glmm_elderly3.6))-length(fixef(glmm_elderlynosympnone)), lower.tail=FALSE)

## Can we remove another main effect?
glmm_elderlynoarea3 <- glmer(
  any_nonhh_contact ~ holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6011.131, -2loglik = 5979.131, 15 param

glmm_elderlynoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6007.927, -2loglik = 5973.927, 16 param

glmm_elderlynowd <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6007.560, -2loglik = 5973.560, 16 param --> SMALLEST INCREASE

glmm_elderlynoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6006.415, -2loglik = 5974.415, 15 param

glmm_elderlynogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6007.889, -2loglik = 5973.889, 16 param

glmm_elderlynosocialgroup <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6006.952, -2loglik = 5976.952, 14 param

glmm_elderlynohhsize <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6008.485, -2loglik = 5974.485, 16 param

pchisq(5973.560-5972.489, df=length(fixef(glmm_elderlynosympnone))-length(fixef(glmm_elderlynowd)), lower.tail=FALSE)

## Can we remove another main effect?
glmm_elderlynoarea3 <- glmer(
  any_nonhh_contact ~ holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6010.158, -2loglik = 5980.158, 14 param

glmm_elderlynoholiday <- glmer(
  any_nonhh_contact ~ area_3_name + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6007.340, -2loglik = 5975.340, 15 param

glmm_elderlynoeducation <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6005.513, -2loglik = 5975.513, 14 param

glmm_elderlynogender <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask +
    part_social_group_be + hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6006.938, -2loglik = 5974.938, 15 param --> SMALLEST INCREASE

glmm_elderlynosocialgroup <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    hhsize_elderly + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6006.060, -2loglik = 5978.060, 13 param

glmm_elderlynohhsize <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + educationmainearner + 
    part_vacc + part_elevated_risk + part_face_mask + part_gender +
    part_social_group_be + part_vacc:part_face_mask + part_elevated_risk:part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noage_Elderlynohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 6007.516, -2loglik = 5975.516, 15 param

pchisq(5974.938-5973.560, df=length(fixef(glmm_elderlynowd))-length(fixef(glmm_elderlynogender)), lower.tail=FALSE)
