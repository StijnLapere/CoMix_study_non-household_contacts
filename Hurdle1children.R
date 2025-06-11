library(lme4)

glmm_childrentotal <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + 
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8537.992, -2Loglik = 8513.992, 11 param

glmm_childrentotal2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8404.669, -2Loglik = 8366.669, 18 param

## Is there overdispersion?
dispersion_glmer(glmm_childrentotal2)
#No (0.97), so continue with binomial distribution

glmm_childrentotal2.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8408.137, -2Loglik = 8366.137, 20 param

glmm_childrentotal2.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8403.283, -2Loglik = 8361.283, 20 param

glmm_childrentotal2.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8404.659, -2Loglik = 8358.659, 22 param

glmm_childrentotal2.4 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.937, -2Loglik = 8362.937, 19 param --> BEST IMPROVEMENT

pchisq(8366.669-8366.137, df=length(fixef(glmm_childrentotal2.1))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)
pchisq(8366.669-8361.283, df=length(fixef(glmm_childrentotal2.2))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)
pchisq(8366.669-8358.659, df=length(fixef(glmm_childrentotal2.3))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)
pchisq(8366.669-8362.937, df=length(fixef(glmm_childrentotal2.4))-length(fixef(glmm_childrentotal2)), lower.tail=FALSE)

glmm_childrentotal3.1 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd + part_face_mask:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8406.430, -2Loglik = 8362.430, 21 param 

glmm_childrentotal3.2 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd + holiday:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.301, -2Loglik = 8358.301, 21 param 

glmm_childrentotal3.3 <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd + hhsize_cat:area_3_name +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.920, -2Loglik = 8354.920, 23 param 

pchisq(8362.937-8362.430, df=length(fixef(glmm_childrentotal3.1))-length(fixef(glmm_childrentotal2.4)), lower.tail=FALSE)
pchisq(8362.937-8358.301, df=length(fixef(glmm_childrentotal3.2))-length(fixef(glmm_childrentotal2.4)), lower.tail=FALSE)
pchisq(8362.937-8354.920, df=length(fixef(glmm_childrentotal3.3))-length(fixef(glmm_childrentotal2.4)), lower.tail=FALSE)

## NO SIGNIFICANT IMPROVEMENTS ANYMORE

# Can we remove main effects?

glmm_childrentotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8491.868, -2Loglik = 8455.868, 17 param

glmm_childrentotalnohhsize <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    part_face_mask + part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8406.943, -2Loglik = 8370.943, 17 param

glmm_childrentotalnofacemask <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_social_group_be + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8505.610, -2Loglik = 8467.610, 18 param

glmm_childrentotalnosocialgroup <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8399.404, -2Loglik = 8365.404, 16 param

glmm_childrentotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + part_social_group_be + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8534.491, -2Loglik = 8508.491, 12 param

pchisq(8365.404-8362.937, df=length(fixef(glmm_childrentotal2.4))-length(fixef(glmm_childrentotalnosocialgroup)), lower.tail=FALSE)

glmm_childrentotalnoarea <- glmer(
  any_nonhh_contact ~ holiday + wd + hhsize_cat +
    part_face_mask + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8489.178, -2Loglik = 8459.178, 14 param

glmm_childrentotalnohhsize <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd +
    part_face_mask + wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8402.693, -2Loglik = 8372.693, 14 param

glmm_childrentotalnofacemask <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    wavecount + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8501.658, -2Loglik = 8469.658, 15 param

glmm_childrentotalnowavecount <- glmer(
  any_nonhh_contact ~ area_3_name + holiday + wd + hhsize_cat +
    part_face_mask + holiday:wd +
    (1 | part_uid),
  data = logisticdatasettotal_noagegender_children,
  family = binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#AIC = 8530.689, -2Loglik = 8510.689, 9 param

pchisq(8372.693-8365.404, df=length(fixef(glmm_childrentotalnosocialgroup))-length(fixef(glmm_childrentotalnohhsize)), lower.tail=FALSE)

## FINAL MODEL
summary(glmm_childrentotalnosocialgroup)

library(DHARMa)
simulationoutput <- simulateResiduals(fittedModel = glmm_childrentotalnosocialgroup)
plot(simulationoutput)
testDispersion(simulationoutput)

df <- data.frame(
  Covariate = c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                "Weekday", "Weekend", "Face mask No", "Face mask Yes",
                "hh size 2", "hh size 3", "hh size 4+", 
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Holiday No : Weekday", "Holiday Yes : Weekend"),
  Estimate = c(0, 1.41299, 0.15099, 0, -0.37373,
               0, -0.17913, 0, 0.75331,
               0, -0.48692, -0.58241,
               0, -0.02253, -0.36051, -0.29258, -0.55091, -0.73604, -0.72873, -1.03180,
               0, 0.29808),
  SE = c(0, 0.22347, 0.22712, 0, 0.07377,
         0, 0.09164, 0, 0.07403,
         0, 0.22348, 0.21387,
         0, 0.12434, 0.13314, 0.13949, 0.14679, 0.15240, 0.15755, 0.10426,
         0, 0.15172)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                         "Weekday", "Weekend", "Face mask No", "Face mask Yes",
                         "hh size 2", "hh size 3", "hh size 4+", 
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                         "Holiday No : Weekday", "Holiday Yes : Weekend"))

df$Covariate <- factor(df$Covariate, levels = covariate_order)

library(ggplot2)
# Plot using ggplot2
ggplot(df, aes(x = RelativeContacts, y = reorder(Covariate, RelativeContacts))) +
  geom_point(size = 2, color = "red") + 
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.5, linewidth = 0.8, color = "black") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(x = "Relative Odds of having non-household contacts", y = "Covariates") +
  theme(axis.text.y = element_text(size = 10)) +
  scale_y_discrete(limits = covariate_order) # Ensure correct order
