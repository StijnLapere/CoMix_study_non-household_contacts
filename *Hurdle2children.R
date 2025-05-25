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
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e5)))
#AIC = 20322.84, -2Loglik = 20296.84, 11 param

hurdle2_childrennb1wavecount <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e5)))
#Failed to converge

hurdle2_childrennb1wavecountshort <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e5)))
#AIC = 20292.53, -2Loglik = 20262.53, 13 param

hurdle2_children2.1 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e5)))
#AIC = 20294.23, -2Loglik = 20260.23, 15 param

hurdle2_children2.2 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + holiday:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e5)))
#AIC = 20295.47, -2Loglik = 20261.47, 15 param (warning)

hurdle2_children2.3 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + hhsize_cat:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e5)))
#AIC = 20295.87, -2Loglik = 20257.87, 17 param

hurdle2_children2.4 <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e5)))
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
  control = glmerControl(optCtrl = list(maxfun = 99e10)))
#AIC = 20321.44, -2Loglik = 20295.44, 11 param (warning)

hurdle2_childrennoholiday <- glmer.nb(
  num_contacts ~ area_3_name + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e10)))
#AIC = 20359.20, -2Loglik = 20331.20, 12 param (warning)

hurdle2_childrennowd <- glmer.nb(
  num_contacts ~ area_3_name + holiday + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e10)))
#AIC = 20303.46, -2Loglik = 20275.46, 12 param (warning)

hurdle2_childrennohhsize <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e10)))
#AIC = 20304.16, -2Loglik = 20278.16, 11 param (warning)

hurdle2_childrennofacemask <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e10)))
#AIC = 20309.61, -2Loglik = 20281.61, 12 param (warning)

hurdle2_childrennosocialgroup <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e10)))
#AIC = 20295.94, -2Loglik = 20271.94, 10 param (warning)

hurdle2_childrennowavecountshort <- glmer.nb(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99e10)))
#AIC = 20331.28, -2Loglik = 20305.28, 11 param (warning)

# Not converged, so stick to original model without interaction effects

## FINAL MODEL
summary(hurdle2_childrennb1wavecountshort)

df <- data.frame(
  Covariate = c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", 
                "Holiday No", "Holiday Yes", "Weekday", "Weekend",
                "Face mask No", "Face mask Yes", "hh size 2", "hh size 3", "hh size 4+",
                "Social group 1&2", "Social group 3&4", "Social group 5&6", "Social group 7&8",
                "1 wave", "2 waves", "3+ waves"),
  Estimate = c(0, 0.542237, 0.056997, 0, -0.356526, 0, -0.119205,
               0, 0.154681, 0, 0.403411, 0.258787,
               0, 0.015140, -0.007048, -0.100814,
               0, 0.053354, -0.266201),
  SE = c(0, 0.155035, 0.162004, 0, 0.044815, 0, 0.054824,
         0, 0.052325, 0, 0.144532, 0.137051,
         0, 0.105816, 0.121361, 0.127159,
         0, 0.078638, 0.062751)
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
                         "Face mask No", "Face mask Yes", "hh size 2", "hh size 3", "hh size 4+",
                         "Social group 1&2", "Social group 3&4", "Social group 5&6", "Social group 7&8",
                         "1 wave", "2 waves", "3+ waves"))

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
