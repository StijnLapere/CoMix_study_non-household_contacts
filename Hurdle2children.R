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
library(glmmTMB)

hurdle2_childrennb1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18946.371, -2Loglik = 18920.371, 11 param

hurdle2_childrennb1wavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18888.854, -2Loglik = 18848.854, 18 param

hurdle2_childrennb1wavecountshort <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecountshort +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18904.349, -2Loglik = 18874.349, 13 param

hurdle2_children2.1 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + part_face_mask:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18891.436, -2Loglik = 18847.436, 20 param

hurdle2_children2.2 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18892.696, -2Loglik = 18848.696, 20 param

hurdle2_children2.3 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + hhsize_cat:area_3_name + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18892.291, -2Loglik = 18844.291, 22 param

hurdle2_children2.4 <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd + 
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18889.323, -2Loglik = 18847.323, 19 param

pchisq(18848.854-18847.436, df=length(fixef(hurdle2_children2.1)$cond)-length(fixef(hurdle2_childrennb1wavecount)$cond), lower.tail=FALSE)
pchisq(18848.854-18848.696, df=length(fixef(hurdle2_children2.2)$cond)-length(fixef(hurdle2_childrennb1wavecount)$cond), lower.tail=FALSE)
pchisq(18848.854-18844.291, df=length(fixef(hurdle2_children2.3)$cond)-length(fixef(hurdle2_childrennb1wavecount)$cond), lower.tail=FALSE)
pchisq(18848.854-18847.323, df=length(fixef(hurdle2_children2.4)$cond)-length(fixef(hurdle2_childrennb1wavecount)$cond), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS
# Can we remove a main effect?
hurdle2_childrennoarea <- glmmTMB(
  num_contacts ~ holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18914.771, -2Loglik = 18878.771, 16 param

hurdle2_childrennoholiday <- glmmTMB(
  num_contacts ~ area_3_name + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18934.582, -2Loglik = 18896.582, 17 param

hurdle2_childrennowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18890.474, -2Loglik = 18852.474, 17 param

hurdle2_childrennohhsize <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18891.598, -2Loglik = 18855.598, 186 param

hurdle2_childrennofacemask <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18893.454, -2Loglik = 18855.454, 17 param

hurdle2_childrennosocialgroup <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18883.168, -2Loglik = 18849.168, 15 param

hurdle2_childrennowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18946.371, -2Loglik = 18920.371, 11 param

pchisq(18849.168-18848.854, df=length(fixef(hurdle2_childrennb1wavecount)$cond)-length(fixef(hurdle2_childrennosocialgroup)$cond), lower.tail=FALSE)

hurdle2_childrennoarea <- glmmTMB(
  num_contacts ~ holiday + wd + hhsize_cat +
    part_face_mask + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18909.518, -2Loglik = 18879.518, 13 param

hurdle2_childrennoholiday <- glmmTMB(
  num_contacts ~ area_3_name + wd + hhsize_cat +
    part_face_mask + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18928.816, -2Loglik = 18896.816, 14 param

hurdle2_childrennowd <- glmmTMB(
  num_contacts ~ area_3_name + holiday + hhsize_cat +
    part_face_mask + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18884.809, -2Loglik = 18852.809, 14 param

hurdle2_childrennohhsize <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd +
    part_face_mask + wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18886.458, -2Loglik = 18856.458, 13 param

hurdle2_childrennofacemask <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    wavecount +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18887.848, -2Loglik = 18855.848, 14 param

hurdle2_childrennowavecount <- glmmTMB(
  num_contacts ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask +
    (1 | part_uid),
  data = logisticdataset_noagegender_childrennonhh,
  family = truncated_nbinom2)
#AIC = 18940.466, -2Loglik = 18920.466, 8 param

pchisq(18852.809-18849.168, df=length(fixef(hurdle2_childrennosocialgroup)$cond)-length(fixef(hurdle2_childrennowd)$cond), lower.tail=FALSE)

## Final model
finalmodelhurdle2children <- hurdle2_childrennosocialgroup

library(DHARMa)
simulationoutput <- simulateResiduals(fittedModel = finalmodelhurdle2children, n=1000)
plot(simulationoutput)
testOutliers(simulationoutput, type = "bootstrap")
testDispersion(simulationoutput)

df <- data.frame(
  Covariate = c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", 
                "Holiday No", "Holiday Yes", "Weekday", "Weekend",
                "Face mask No", "Face mask Yes", "hh size 2", "hh size 3", "hh size 4+",
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves"),
  Estimate = c(0, 0.74603, 0.08782, 0, -0.47659, 0, -0.15833,
               0, 0.20427, 0, 0.54145, 0.35535, 
               0, 0.04055, -0.32295, -0.24859, -0.29275, -0.62410, -0.45236, -0.70276),
  SE = c(0, 0.22098, 0.23125, 
         0, 0.06766, 0, 0.08241, 
         0, 0.07894, 0, 0.20310, 0.19210, 
         0, 0.12141, 0.13182, 0.13532, 0.14991, 0.15497, 0.16261, 0.10692)
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
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves"))

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


