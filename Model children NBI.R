library(gamlss)

modelchildren1pvc <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                          area_3_name+holiday+wd+hhsize_cat+wavecount+
                          pvc(day_number)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = NBI, 
                        data = na.omit(finaldataset_noagegender_children),
                        control = gamlss.control(n.cyc = 100)) 
#17 iterations, AIC = 27151.6

finaldataset_noagegender_children2 <- finaldataset_noagegender_children
finaldataset_noagegender_children2$hhsize_cat <- as.numeric(factor(finaldataset_noagegender_children2$hhsize_cat, levels = c("2", "3", "4+")))

modelchildren1pvc2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                              area_3_name+holiday+wd+hhsize_cat+wavecount+
                              pvc(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = NBI, 
                            data = na.omit(finaldataset_noagegender_children2),
                            control = gamlss.control(n.cyc = 100)) 

modelchildren1cs <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                              area_3_name+holiday+wd+hhsize_cat+wavecount+
                              cs(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = NBI, 
                            data = na.omit(finaldataset_noagegender_children),
                            control = gamlss.control(n.cyc = 100)) 
#18 iterations, AIC = 27273.8


modelchildren2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                              area_3_name+holiday+wd+hhsize_cat+wavecount+part_face_mask:area_3_name+
                              pvc(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = NBI, 
                            data = na.omit(finaldataset_noagegender_children),
                            control = gamlss.control(n.cyc = 100)) 
#17 iterations, AIC = 27154.9

modelchildren2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#15 iterations, AIC = 27149.2 --> IMPROVEMENT

modelchildren2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+hhsize_cat:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#15 iterations, AIC = 27156.2

modelchildren2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:wd+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#17 iterations, AIC = 27153.5


modelchildren3.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             part_face_mask:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 27152.6

modelchildren3.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             hhsize_cat:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 27154

modelchildren3.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             holiday:wd+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 27151.3

### No additional interaction terms

## Can we remove main effects?
modelchildren2.2nosocialgroup <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 27143.3 --> Remove social group

modelchildren2.2nofacemask <- gamlss(num_nonhouseh_cont ~ part_social_group_be+
                                          area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 27254.6

modelchildren2.2nowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                                          area_3_name+holiday+hhsize_cat+wavecount+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 27152.1

modelchildren2.2nohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                                          area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100)) 
#13 iterations, AIC = 27144.9

modelchildren2.2nowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                                          area_3_name+holiday+wd+hhsize_cat+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100)) 
#18 iterations, AIC = 27175.7

## Can we remove another main effect?
modelchildren2.2nofacemask <- gamlss(num_nonhouseh_cont ~ area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 27248.4

modelchildren2.2nowd <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                          area_3_name+holiday+hhsize_cat+wavecount+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 27146.2

modelchildren2.2nohhsize <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                          area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100))
#15 iterations, AIC = 27138.9 --> Remove hhsize

modelchildren2.2nowavecount <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                          area_3_name+holiday+wd+hhsize_cat+holiday:area_3_name+
                                          pvc(day_number)+
                                          re(random = ~1|part_uid),
                                        sigma.formula = ~1, 
                                        family = NBI, 
                                        data = na.omit(finaldataset_noagegender_children),
                                        control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 27170.6

## Can we remove another main effect?
modelchildren2.2nofacemask <- gamlss(num_nonhouseh_cont ~ area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                     pvc(day_number)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~1, 
                                   family = NBI, 
                                   data = na.omit(finaldataset_noagegender_children),
                                   control = gamlss.control(n.cyc = 100))
#14 iterations, AIC = 27244.4

modelchildren2.2nowd <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                       area_3_name+holiday+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = NBI, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 100))
#15 iterations, AIC = 27141.8

modelchildren2.2nowavecount <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                       area_3_name+holiday+wd+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = NBI, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 27166.4

## No further reductions


### MODEL FOR SIGMA
modelchildrensigma1 <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                        area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                        pvc(day_number)+
                        re(random = ~1|part_uid),
                      sigma.formula = ~part_face_mask+
                        area_3_name+holiday+wd+wavecount+
                        pvc(day_number)+
                        re(random = ~1|part_uid), 
                      family = NBI, 
                      data = na.omit(finaldataset_noagegender_children),
                      control = gamlss.control(n.cyc = 100))
#50 iterations, AIC = 25966.5

modelchildrensigma2 <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 100))
#58 iterations, AIC = 25965

## Can we remove main effects in the model for sigma?
modelchildrensigma2nofacemask <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 100))
#48 iterations, AIC = 26009.7

modelchildrensigma2nowd <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 100))
#54 iterations, AIC = 25967.3

modelchildrensigma2nowavecount <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 100))
#23 iterations, AIC = 26086.9


#### Final model: modelchildrensigma2
finalmodelchildren <- modelchildrensigma2

## Compare with Poisson model ##
modelchildrenPO <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid), 
                              family = PO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 100))
#26 iterations, AIC = 70452.3

## GOF ##

# 1) Plot
# mean =~ 0, variance =~ 1, skewness =~ 0, kurtosis =~ 3 
# --> residuals are approximately normally distributed as they should be for an adequate model
plot(finalmodelchildren)
# mean = -0.0253, variance = 0.9446, skewness = -0.1405, kurtosis = 2.9641

# 2) rqres.plot has to be used in addition to the function plot due to discrete distribution family
rqres.plot(finalmodelchildren)
rqres.plot(modelchildrenPO,howmany=40,type="QQ",plot.type="all",all=FALSE)
### What is this??




df <- data.frame(
  Covariate = c("Face mask No", "Face mask Yes", "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", 
                "Holiday No", "Holiday Yes", "Weekday", "Weekend", 
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Brussels Hoofdstede : Holiday No","Vlaams Gewest : Holiday Yes", "Waals Gewest : Holiday Yes"),
  Estimate = c(0, 0.53104, 0, 1.31550, 0.12408,
               0, -0.32183, 0, -0.04058,
               0, 0.05629, -0.35745, -0.33201, -0.33650, -0.60372, -0.45255, -0.70211,
               0, -0.17951, -0.16132),
  SE = c(0, 0.03518, 0, 0.08915, 0.09316,
         0, 0.14813, 0, 0.05013,
         0, 0.06455, 0.07412, 0.07304, 0.08984, 0.09164, 0.10364, 0.04597,
         0, 0.15644, 0.16985)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Face mask No", "Face mask Yes", "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", 
                         "Holiday No", "Holiday Yes", "Weekday", "Weekend", 
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                         "Brussels Hoofdstede : Holiday No","Vlaams Gewest : Holiday Yes", "Waals Gewest : Holiday Yes"))

df$Covariate <- factor(df$Covariate, levels = covariate_order)

library(ggplot2)
# Plot using ggplot2
ggplot(df, aes(x = RelativeContacts, y = reorder(Covariate, RelativeContacts))) +
  geom_point(size = 2, color = "red") + 
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.5, linewidth = 0.8, color = "black") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(x = "Relative Number of non-household contacts", y = "Covariates") +
  theme(axis.text.y = element_text(size = 10)) +
  scale_y_discrete(limits = covariate_order) # Ensure correct order


