library(gamlss)

modelchildren1pvc <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                              area_3_name+holiday+wd+hhsize_cat+wavecount+
                              pvc(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = GPO, 
                            data = na.omit(finaldataset_noagegender_children),
                            control = gamlss.control(n.cyc = 1000)) 
#58 iterations, AIC = 27096.7

modelchildren1cs <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+
                             cs(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 1000)) 
#54 iterations, AIC = 27223.9

modelchildren2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+part_face_mask:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#54 iterations, AIC = 27097.7

modelchildren2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#60 iterations, AIC = 27091.2 --> BEST IMPROVEMENT

modelchildren2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+hhsize_cat:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#61 iterations, AIC = 27102

modelchildren2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:wd+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#48 iterations, AIC = 27098.8

pchisq(27096.7-27091.2, df=length(coef(modelchildren2.2))-length(coef(modelchildren1pvc)), lower.tail=FALSE)

modelchildren3.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+
                             holiday:area_3_name+part_face_mask:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#48 iterations, AIC = 27092.6

modelchildren3.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+
                             holiday:area_3_name+hhsize_cat:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#64 iterations, AIC = 27096.6

modelchildren3.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+
                             holiday:area_3_name+holiday:wd+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#55 iterations, AIC = 27093.1

## NO IMPROVEMENT ANYMORE

## CAN WE REMOVE A MAIN EFFECT?
modelchildrennosocialgroup <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#40 iterations, AIC = 27084.6 --> REMOVE SOCIAL GROUP

modelchildrennofacemask <- gamlss(num_nonhouseh_cont ~ part_social_group_be+
                                       area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 100)) 
#44 iterations, AIC = 27209.6

modelchildrennowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                                       area_3_name+holiday+hhsize_cat+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 100)) 
#65 iterations, AIC = 27091.1

modelchildrennohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                                       area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 100)) 
#46 iterations, AIC = 27089

modelchildrennowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                                       area_3_name+holiday+wd+hhsize_cat+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 100)) 
#80 iterations, AIC = 27178.3

## CAN WE REMOVE ANOTHER MAIN EFFECT?
modelchildrennofacemask <- gamlss(num_nonhouseh_cont ~ area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 1000))
#37 iterations, AIC = 27202.4

modelchildrennowd <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                       area_3_name+holiday+hhsize_cat+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 1000))
#40 iterations, AIC = 27084.5

modelchildrennohhsize <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                       area_3_name+holiday+wd+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 1000))
#54 iterations, AIC = 27082.2

modelchildrennowavecount <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                       area_3_name+holiday+wd+hhsize_cat+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~1, 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 1000)) 
#62 iterations, AIC = 27173.4

## NO LARGE IMPROVEMENTS ANYMORE

modelchildrensigma1 <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                       area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+
                                cs(day_number), 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 2000)) 
#40 iterations, AIC = 26871.4

modelchildrensigma2 <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000)) 
#50 iterations, AIC = 26876.1 --> No interaction effect

## Can we remove a main effect?
modelchildrensigmanofacemask <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~area_3_name+holiday+wd+hhsize_cat+wavecount+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000)) 
#65 iterations, AIC = 26890.2

modelchildrensigmanoarea <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                holiday+wd+hhsize_cat+wavecount+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000)) 
#153 iterations, AIC = 26926.9

modelchildrensigmanoholiday <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+wd+hhsize_cat+wavecount+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000))
#124 iterations, AIC = 26884.5

modelchildrensigmanowd <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+hhsize_cat+wavecount+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000)) 
#38 iterations, AIC = 26872.4

modelchildrensigmanohhsize <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+wavecount+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000))
#369 iterations, AIC = 26880.5

modelchildrensigmanowavecount <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000)) 
#47 iterations, AIC = 26951.5

### No main effects removed

### Final model: modelchildrensigma1
finalmodelchildrenGPO <- modelchildrensigma1

## GOF ##

# 1) Plot
# mean =~ 0, variance =~ 1, skewness =~ 0, kurtosis =~ 3 
# --> residuals are approximately normally distributed as they should be for an adequate model
plot(finalmodelchildrenGPO)
# mean = -0.0033, variance = 1.0647, skewness = -0.1327, kurtosis = 2.8683

# 2) rqres.plot has to be used in addition to the function plot due to discrete distribution family
rqres.plot(finalmodelchildrenGPO)
rqres.plot(finalmodelchildrenGPO,2,all=FALSE)
### What is this??

df <- data.frame(
  Covariate = c("Face mask No", "Face mask Yes", 
                "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                "Weekday", "Weekend","hh size 2", "hh size 3", "hh size 4+", 
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Brussels Hoofdstede : Holiday No","Vlaams Gewest : Holiday Yes", "Waals Gewest : Holiday Yes"),
  Estimate = c(0, 0.64448,
               0, 1.54759, 0.31861, 0, 0.21636,
               0, -0.11582, 0, -0.12145, -0.22847,
               0, 0.26429, -0.42429, -0.52857, -0.32656, -0.98693, -0.69578, -1.12101,
               0, -0.90490, -0.85924),
  SE = c(0, 0.04345,
         0, 0.09939, 0.10351, 0, 0.19486, 
         0, 0.05913, 0, 0.07236, 0.06835,
         0, 0.08712, 0.09385, 0.08947, 0.12179, 0.10672, 0.12746, 0.05831,
         0, 0.20413, 0.21731)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Face mask No", "Face mask Yes", 
                         "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                         "Weekday", "Weekend","hh size 2", "hh size 3", "hh size 4+", 
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
