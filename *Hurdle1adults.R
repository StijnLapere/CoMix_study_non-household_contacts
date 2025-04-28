library(lme4)

glmm_adults <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
# Model is nearly unidentifiable
print(summary(glmm_adults),correlation=TRUE) # Correlation between hhsizes +- 1

table(logisticdataset_noage_adult$any_nonhh_contact,logisticdataset_noage_adult$hhsize_cat)
# No participants with hhsize = 1 without nonh contacts --> ref level of hhsize_cat is 2 instead of 1

logisticdataset_noage_adultnohhsize1 <- logisticdataset_noage_adult %>%
  filter(hhsize_cat != 1)

########## MODEL BUILDING ##########
glmm1_adult <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15804.4, -2Loglik = 15764.4, 19 param
## Is there overdispersion?
dispersion_glmer(glmm1_adult)
#No (0.94), so continue with binomial distribution

glmm1_adult2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15800.087, -2Loglik = 15763.087, 22 param

glmm1_adult2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15806.578, -2Loglik = 15754.578, 25 param

glmm1_adult2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# Rank deficient

glmm1_adult2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15807.893, -2Loglik = 15763.893, 21 param

glmm1_adult2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15797.067, -2Loglik = 15755.067, 20 param

glmm1_adult2.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15802.258, -2Loglik = 15760.258, 20 param

glmm1_adult2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15803.137, -2Loglik = 15761.137, 20 param

glmm1_adult2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15806.347, -2Loglik = 15762.347, 21 param

glmm1_adult2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15796.16, -2Loglik = 15752.16, 21 param --> BEST IMPROVEMENT

glmm1_adult2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15802.138, -2Loglik = 15760.138, 20 param

glmm1_adult2.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15803.755, -2Loglik = 15761.755, 20 param

glmm1_adult2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15807.592, -2Loglik = 15763.592, 21 param

glmm1_adult2.13 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15807.552, -2Loglik = 15763.552, 21 param

glmm1_adult2.14 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15811.003, -2Loglik = 15763.003, 23 param

glmm1_adult2.15 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15806.408, -2Loglik = 15764.408, 20 param

pchisq(15764.4-15755.067, df=length(fixef(glmm1_adult2.5))-length(fixef(glmm1_adult)), lower.tail=FALSE)
pchisq(15764.4-15752.16, df=length(fixef(glmm1_adult2.9))-length(fixef(glmm1_adult)), lower.tail=FALSE)

glmm1_adult3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15800.331, -2Loglik = 15750.331, 24 param

glmm1_adult3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15797.969, -2Loglik = 15741.969, 27 param

glmm1_adult3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15799.705, -2Loglik = 15751.705, 23 param

glmm1_adult3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15786.924, -2Loglik = 15740.924, 22 param --> BEST IMPROVEMENT

glmm1_adult3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15794.266, -2Loglik = 15748.266, 22 param

glmm1_adult3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15795.408, -2Loglik = 15749.408, 22 param

glmm1_adult3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15798.391, -2Loglik = 15750.391, 23 param

glmm1_adult3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15793.589, -2Loglik = 15747.589, 22 param

glmm1_adult3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15795.528, -2Loglik = 15749.528, 22 param

glmm1_adult3.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15799.310, -2Loglik = 15751.310, 23 param

glmm1_adult3.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15799.135, -2Loglik = 15751.135, 23 param

glmm1_adult3.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15802.82, -2Loglik = 15750.82, 25 param

glmm1_adult3.13 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15798.127, -2Loglik = 15752.127, 22 param

pchisq(15752.16-15740.924, df=length(fixef(glmm1_adult3.4))-length(fixef(glmm1_adult2.9)), lower.tail=FALSE)

glmm1_adult4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15791.988, -2Loglik = 15739.988, 25 param

glmm1_adult4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15788.866, -2Loglik = 15730.866, 28 param

glmm1_adult4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15790.282, -2Loglik = 15740.282, 24 param

glmm1_adult4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15785.036, -2Loglik = 15737.036, 23 param

glmm1_adult4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15787.115, -2Loglik = 15739.115, 23 param

glmm1_adult4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15789.539, -2Loglik = 15739.539, 24 param

glmm1_adult4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15784.687, -2Loglik = 15736.687, 23 param --> BEST IMPROVEMENT 

glmm1_adult4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15786.334, -2Loglik = 15738.334, 23 param

glmm1_adult4.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15790.123, -2Loglik = 15740.123, 24 param

glmm1_adult4.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15789.953, -2Loglik = 15739.953, 24 param

glmm1_adult4.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15793.568, -2Loglik = 15739.568, 26 param

glmm1_adult4.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15788.905, -2Loglik = 15740.905, 23 param

pchisq(15740.924-15730.866, df=length(fixef(glmm1_adult4.2))-length(fixef(glmm1_adult3.4)), lower.tail=FALSE)
pchisq(15740.924-15736.687, df=length(fixef(glmm1_adult4.7))-length(fixef(glmm1_adult3.4)), lower.tail=FALSE)

glmm1_adult5.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15789.688, -2Loglik = 15735.688, 26 param

glmm1_adult5.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15786.529, -2Loglik = 15726.529, 29 param

glmm1_adult5.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15788.058, -2Loglik = 15736.058, 25 param

glmm1_adult5.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15782.030, -2Loglik = 15732.030, 24 param --> BEST IMPROVEMENT

glmm1_adult5.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15784.927, -2Loglik = 15734.927, 24 param

glmm1_adult5.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15787.294, -2Loglik = 15735.294, 25 param

glmm1_adult5.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15784.524, -2Loglik = 15734.524, 24 param

glmm1_adult5.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15788.012, -2Loglik = 15736.012, 25 param

glmm1_adult5.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15787.697, -2Loglik = 15735.697, 25 param

glmm1_adult5.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15791.331, -2Loglik = 15735.331, 27 param

glmm1_adult5.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15786.650, -2Loglik = 15736.650, 24 param

pchisq(15736.687-15726.529, df=length(fixef(glmm1_adult5.2))-length(fixef(glmm1_adult4.7)), lower.tail=FALSE)
pchisq(15736.687-15732.030, df=length(fixef(glmm1_adult5.4))-length(fixef(glmm1_adult4.7)), lower.tail=FALSE)

glmm1_adult6.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15787.209, -2Loglik = 15731.209, 27 param

glmm1_adult6.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15784.012, -2Loglik = 15722.012, 30 param

glmm1_adult6.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15785.285, -2Loglik = 15731.285, 26 param

glmm1_adult6.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15782.136, -2Loglik = 15730.136, 26 param

glmm1_adult6.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15784.647, -2Loglik = 15730.647, 26 param

glmm1_adult6.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15781.921, -2Loglik = 15729.921, 25 param

glmm1_adult6.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15785.350, -2Loglik = 15731.350, 26 param

glmm1_adult6.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15784.889, -2Loglik = 15730.889, 26 param

glmm1_adult6.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + area_3_name:hhsize_cat + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15788.62, -2Loglik = 15730.62, 28 param

glmm1_adult6.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + hhsize_cat + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:hhsize_cat + part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    part_vacc:part_face_mask + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adultnohhsize1,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 15783.992, -2Loglik = 15731.992, 25 param

pchisq(15732.030-15722.012, df=length(fixef(glmm1_adult6.2))-length(fixef(glmm1_adult5.4)), lower.tail=FALSE)
pchisq(15732.030-15729.921, df=length(fixef(glmm1_adult6.6))-length(fixef(glmm1_adult5.4)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENT ANYMORE

summary(glmm1_adult5.4)

## Consider model without hhsize -> use all observations
glmm2_adult <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17129.650, -2Loglik = 17093.650, 17 param
## Is there overdispersion?
dispersion_glmer(glmm2_adult)
#No (0.87), so continue with binomial distribution

glmm2_adult2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17135.131, -2Loglik = 17093.131, 20 param

glmm2_adult2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17128.018, -2Loglik = 17080.018, 23 param

glmm2_adult2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_social_group_be:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#Rank deficient

glmm2_adult2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17132.578, -2Loglik = 17092.578, 19 param

glmm2_adult2.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17123.294, -2Loglik = 17085.294, 18 param --> BEST IMPROVEMENT

glmm2_adult2.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17127.303, -2Loglik = 17089.303, 18 param

glmm2_adult2.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17128.306, -2Loglik = 17090.306, 18 param

glmm2_adult2.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17131.237, -2Loglik = 17091.237, 19 param

glmm2_adult2.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17126.830, -2Loglik = 17088.830, 18 param

glmm2_adult2.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17128.8, -2Loglik = 17090.8, 18 param

glmm2_adult2.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17131.978, -2Loglik = 17091.978, 19 param

glmm2_adult2.12 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17132.36, -2Loglik = 17092.36, 19 param

glmm2_adult2.13 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17131.595, -2Loglik = 17093.595, 18 param

pchisq(17093.650-17080.018, df=length(fixef(glmm2_adult2.2))-length(fixef(glmm2_adult)), lower.tail=FALSE)
pchisq(17093.650-17085.294, df=length(fixef(glmm2_adult2.5))-length(fixef(glmm2_adult)), lower.tail=FALSE)

glmm2_adult3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17129.167, -2Loglik = 17085.167, 21 param

glmm2_adult3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17121.830, -2Loglik = 17071.830, 24 param

glmm2_adult3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17126.122, -2Loglik = 17084.122, 20 param

glmm2_adult3.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17120.856, -2Loglik = 17080.856, 19 param

glmm2_adult3.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17122.872, -2Loglik = 17082.872, 19 param

glmm2_adult3.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17125.458, -2Loglik = 17083.458, 20 param

glmm2_adult3.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17120.795, -2Loglik = 17080.795, 19 param --> BEST IMPROVEMENT

glmm2_adult3.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17122.524, -2Loglik = 17082.524, 19 param

glmm2_adult3.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17125.674, -2Loglik = 17083.674, 20 param

glmm2_adult3.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17126.036, -2Loglik = 17084.036, 20 param

glmm2_adult3.11 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17125.255, -2Loglik = 17085.255, 19 param

pchisq(17085.294-17071.830, df=length(fixef(glmm2_adult3.2))-length(fixef(glmm2_adult2.5)), lower.tail=FALSE)
pchisq(17085.294-17080.856, df=length(fixef(glmm2_adult3.4))-length(fixef(glmm2_adult2.5)), lower.tail=FALSE)
pchisq(17085.294-17080.795, df=length(fixef(glmm2_adult3.7))-length(fixef(glmm2_adult2.5)), lower.tail=FALSE)

glmm2_adult4.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17126.644, -2Loglik = 17080.644, 22 param

glmm2_adult4.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17119.330, -2Loglik = 17067.330, 25 param

glmm2_adult4.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17123.627, -2Loglik = 17079.627, 21 param

glmm2_adult4.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17117.525, -2Loglik = 17075.525, 20 param --> BEST IMPROVEMENT

glmm2_adult4.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17120.435, -2Loglik = 17078.435, 20 param

glmm2_adult4.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17122.967, -2Loglik = 17078.967, 21 param

glmm2_adult4.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17120.547, -2Loglik = 17078.547, 20 param

glmm2_adult4.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17123.552, -2Loglik = 17079.552, 21 param

glmm2_adult4.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17123.547, -2Loglik = 17079.547, 21 param

glmm2_adult4.10 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17122.734, -2Loglik = 17080.734, 20 param

pchisq(17080.795-17067.330, df=length(fixef(glmm2_adult4.2))-length(fixef(glmm2_adult3.7)), lower.tail=FALSE)
pchisq(17080.795-17075.525, df=length(fixef(glmm2_adult4.4))-length(fixef(glmm2_adult3.7)), lower.tail=FALSE)

glmm2_adult5.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    part_social_group_be:part_vacc + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17123.402, -2Loglik = 17075.402, 23 param

glmm2_adult5.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    part_social_group_be:employstatus + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17116.034, -2Loglik = 17062.034, 26 param

glmm2_adult5.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    part_vacc:educationmainearner + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17120.28, -2Loglik = 17074.28, 22 param

glmm2_adult5.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    part_vacc:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17117.031, -2Loglik = 17073.031, 22 param

glmm2_adult5.5 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    part_vacc:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17119.582, -2Loglik = 17073.582, 22 param

glmm2_adult5.6 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    part_face_mask:part_symp_none + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17117.325, -2Loglik = 17073.325, 21 param

glmm2_adult5.7 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17120.313, -2Loglik = 17074.313, 22 param

glmm2_adult5.8 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    area_3_name:holiday + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17120.119, -2Loglik = 17074.119, 22 param

glmm2_adult5.9 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + employstatus +
    educationmainearner + part_vacc + part_elevated_risk + 
    part_face_mask + part_symp_none + part_gender + part_social_group_be + 
    part_vacc:part_elevated_risk + part_elevated_risk:part_face_mask + part_vacc:part_face_mask + 
    holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noage_adult,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 17119.458, -2Loglik = 17075.458, 21 param

pchisq(17075.525-17062.034, df=length(fixef(glmm2_adult5.2))-length(fixef(glmm2_adult4.4)), lower.tail=FALSE)
pchisq(17075.525-17073.031, df=length(fixef(glmm2_adult5.4))-length(fixef(glmm2_adult4.4)), lower.tail=FALSE)
pchisq(17075.525-17073.325, df=length(fixef(glmm2_adult5.6))-length(fixef(glmm2_adult4.4)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENT ANYMORE

summary(glmm2_adult4.4)
