library(gamlss)

modeladults1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+wd+
                          hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = NBI, 
                        data = na.omit(finaldataset_noage_adult),
                        control = gamlss.control(n.cyc = 100)) 
#19 iterations, AIC = 68265.7


modeladults2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                         part_face_mask+part_symp_none+area_3_name+holiday+wd+
                         hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                         part_social_group_be:part_vacc+
                         pvc(day_number, by = part_vacc:part_symp_none)+
                         re(random = ~1|part_uid),
                       sigma.formula = ~1, 
                       family = NBI, 
                       data = na.omit(finaldataset_noage_adult),
                       control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68253.5

modeladults2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68266.3

modeladults2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#25 iterations, AIC = 68269.4

modeladults2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68248.4

modeladults2.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68264.9

modeladults2.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68241.5 --> BEST IMPROVEMENT

modeladults2.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68267.7

modeladults2.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68256.3

modeladults2.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68261.7

modeladults2.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_elevated_risk:part_face_mask+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68252.7

modeladults2.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_face_mask:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68267.8

modeladults2.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#57 iterations, AIC = 68256.7

modeladults2.13 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           area_3_name:holiday+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68269.5

modeladults2.14 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           area_3_name:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68264.2

modeladults2.15 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           holiday:wd+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68267.8


pchisq(68265.7-68241.5, df=length(coef(modeladults2.6))-length(coef(modeladults1)), lower.tail=FALSE)

modeladults3.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 68231

modeladults3.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68241.9

modeladults3.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#23 iterations, AIC = 68245

modeladults3.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68225.5

modeladults3.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68240.6

modeladults3.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68243.5

modeladults3.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68232.1

modeladults3.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 68237.4

modeladults3.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 68225.2 --> BEST IMPROVEMENT

modeladults3.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_face_mask:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 68243.5

modeladults3.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68233.1

modeladults3.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+area_3_name:holiday+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68245.2

modeladults3.13 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68240.1

modeladults3.14 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 68243.5

pchisq(68241.5-68225.2, df=length(coef(modeladults3.9))-length(coef(modeladults2.6)), lower.tail=FALSE)

modeladults4.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68215.5

modeladults4.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68225.9

modeladults4.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 68228.7

modeladults4.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_vacc:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 68209.8 --> BEST IMPROVEMENT

modeladults4.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 68225.1

modeladults4.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_vacc:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 68227.2

modeladults4.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 68215.7

modeladults4.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68220.5

modeladults4.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_face_mask:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 68226.9

modeladults4.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68218.1

modeladults4.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+area_3_name:holiday+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68229.1

modeladults4.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68224.3

modeladults4.13 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 68227.3

pchisq(68225.2-68209.8, df=length(coef(modeladults4.4))-length(coef(modeladults3.9)), lower.tail=FALSE)

modeladults5.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68208.8

modeladults5.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 68210.9

modeladults5.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 68213.3

modeladults5.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 68210.1

modeladults5.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 68211.8

modeladults5.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 68202.5 --> BEST IMPROVEMENT

modeladults5.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68206.2

modeladults5.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_face_mask:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68211.5

modeladults5.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            part_vacc:educationmainearner+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68202.9

modeladults5.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            part_vacc:educationmainearner+area_3_name:holiday+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 68213.7

modeladults5.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            part_vacc:educationmainearner+area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68209.3

modeladults5.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            part_vacc:educationmainearner+holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#18 iterations, AIC = 68211.9

pchisq(68209.8-68202.5, df=length(coef(modeladults5.6))-length(coef(modeladults4.4)), lower.tail=FALSE)


modeladults6.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68202.5

modeladults6.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 68204.2

modeladults6.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#24 iterations, AIC = 68205.9

modeladults6.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 68203.2

modeladults6.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           part_vacc:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68204.5

modeladults6.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 68199.6

modeladults6.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           part_face_mask:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68204.3

modeladults6.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68197.3

modeladults6.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            part_vacc:educationmainearner+part_vacc:area_3_name+area_3_name:holiday+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 68206.2

modeladults6.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            part_vacc:educationmainearner+part_vacc:area_3_name+area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68201.9

modeladults6.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                            part_vacc:educationmainearner+part_vacc:area_3_name+holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 100))
#17 iterations, AIC = 68204.5

pchisq(68202.5-68197.3, df=length(coef(modeladults6.8))-length(coef(modeladults5.6)), lower.tail=FALSE)

### NO SIGNIFICANT IMPROVEMENTS ANYMORE -> continue with modeladults5.6
## Can we remove main effects?

modeladultsnosocgroup <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68197.5

modeladultsnoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name++wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68201.7

modeladultsnowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68213.2

modeladultsnohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68205.5

modeladultsnowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#9 iterations, AIC = 68370.4

modeladultsnogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 68200.6

modeladultsnoemploystatus <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+educationmainearner+
                           part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                           part_vacc:educationmainearner+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = NBI, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68196.1 --> BEST IMPROVEMENT

pchisq(68202.5-68196.1, df=length(coef(modeladults5.6))-length(coef(modeladultsnoemploystatus)), lower.tail=FALSE)


modeladultsnosocialgroup <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 68190.4

modeladultsnoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68195.3

modeladultsnowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68206.8

modeladultsnohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 100))
#27 iterations, AIC = 68198.2

modeladultsnowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 100))
#9 iterations, AIC = 68457.8

modeladultsnogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 100))
#16 iterations, AIC = 68194.2

pchisq(68196.1-68190.4, df=length(coef(modeladultsnoemploystatus))-length(coef(modeladultsnosocialgroup)), lower.tail=FALSE)

### NO SIGNIFICANT IMPROVEMENT

## Look at changes in additive term
modeladultsadditive1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 68212.5

modeladultsadditive2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                 part_vacc:educationmainearner+part_vacc:area_3_name+part_vacc:part_symp_none+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~1, 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_adult),
                               control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 68212.6

modeladultsadditive3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                 part_vacc:educationmainearner+part_vacc:area_3_name+
                                 cs(day_number)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~1, 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_adult),
                               control = gamlss.control(n.cyc = 100))
#83 iterations, AIC = 68229.9

modeladultsadditive4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                 part_vacc:educationmainearner+part_vacc:area_3_name+part_vacc:part_symp_none+
                                 cs(day_number)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~1, 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_adult),
                               control = gamlss.control(n.cyc = 100))
#24 iterations, AIC = 68230.4

### NO SIGNIFICANT IMPROVEMENT 

### MODEL FOR SIGMA ###
modeladultssigma1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid), 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 1000))
#73 iterations, AIC = 63240.6

modeladultssigma2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                              part_vacc:educationmainearner+part_vacc:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              pvc(day_number)+
                              re(random = ~1|part_uid), 
                            family = NBI, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#137 iterations, AIC = 63259.6


## Include interaction effects
modeladultssigma1.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                              part_vacc:educationmainearner+part_vacc:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid), 
                            family = NBI, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#82 iterations, AIC = 63242.9

modeladultssigma1.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#55 iterations, AIC = 63232.8 --> IMPROVEMENT

modeladultssigma1.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:educationmainearner+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#76 iterations, AIC = 63241

modeladultssigma1.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#75 iterations, AIC = 63241.3

## Can we include another interaction effect?
modeladultssigma2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+part_vacc:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#92 iterations, AIC = 63234.7

modeladultssigma2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+part_vacc:educationmainearner+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#79 iterations, AIC = 63232.5

modeladultssigma2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#84 iterations, AIC = 63233.6

## NO SIGNIFICANT IMPROVEMENT

## Can we remove main effects in the model for sigma?
modeladultssigmanosocialgroup <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#76 iterations, AIC = 63227.2 --> LOWEST AIC, remove social group

modeladultssigmanoarea <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#68 iterations, AIC = 63229.7

modeladultssigmanoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#60 iterations, AIC = 63239.8

modeladultssigmanowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#78 iterations, AIC = 63230.9

modeladultssigmanohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                wavecount+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#78 iterations, AIC = 63227.3

modeladultssigmanowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+part_gender+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#55 iterations, AIC = 63667.1

modeladultssigmanogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+educationmainearner+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#64 iterations, AIC = 63232.8

modeladultssigmanoeducation <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                part_vacc:educationmainearner+part_vacc:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+
                                part_elevated_risk:part_face_mask+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid), 
                              family = NBI, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#62 iterations, AIC = 63231.1

# Can we remove another main effect in the model for sigma?
modeladultssigmanoarea <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                   part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                   hhsize_cat+wavecount+part_gender+educationmainearner+
                                   part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                   part_vacc:educationmainearner+part_vacc:area_3_name+
                                   pvc(day_number, by = part_vacc:part_symp_none)+
                                   re(random = ~1|part_uid),
                                 sigma.formula = ~part_vacc+part_elevated_risk+
                                   part_face_mask+part_symp_none+holiday+wd+
                                   hhsize_cat+wavecount+part_gender+educationmainearner+
                                   part_elevated_risk:part_face_mask+
                                   pvc(day_number, by = part_vacc:part_symp_none)+
                                   re(random = ~1|part_uid), 
                                 family = NBI, 
                                 data = na.omit(finaldataset_noage_adult),
                                 control = gamlss.control(n.cyc = 1000))
#80 iterations, AIC = 63224.3

modeladultssigmanoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_elevated_risk:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid), 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 1000))
#93 iterations, AIC = 63234.2

modeladultssigmanowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                 part_vacc:educationmainearner+part_vacc:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+holiday+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_elevated_risk:part_face_mask+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_adult),
                               control = gamlss.control(n.cyc = 1000))
#90 iterations, AIC = 63225.4

modeladultssigmanohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                     part_vacc:educationmainearner+part_vacc:area_3_name+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     wavecount+part_gender+educationmainearner+
                                     part_elevated_risk:part_face_mask+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid), 
                                   family = NBI, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#78 iterations, AIC = 63222 --> Remove hhsize

modeladultssigmanowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        hhsize_cat+wavecount+part_gender+educationmainearner+
                                        part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                        part_vacc:educationmainearner+part_vacc:area_3_name+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid),
                                      sigma.formula = ~part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        hhsize_cat+part_gender+educationmainearner+
                                        part_elevated_risk:part_face_mask+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid), 
                                      family = NBI, 
                                      data = na.omit(finaldataset_noage_adult),
                                      control = gamlss.control(n.cyc = 1000))
#45 iterations, AIC = 63663.4

modeladultssigmanogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                     part_vacc:educationmainearner+part_vacc:area_3_name+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+educationmainearner+
                                     part_elevated_risk:part_face_mask+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid), 
                                   family = NBI, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#77 iterations, AIC = 63227.3

modeladultssigmanoeducation <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        hhsize_cat+wavecount+part_gender+educationmainearner+
                                        part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                        part_vacc:educationmainearner+part_vacc:area_3_name+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid),
                                      sigma.formula = ~part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        hhsize_cat+wavecount+part_gender+
                                        part_elevated_risk:part_face_mask+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid), 
                                      family = NBI, 
                                      data = na.omit(finaldataset_noage_adult),
                                      control = gamlss.control(n.cyc = 1000))
#67 iterations, AIC = 63226.3

# Can we remove another main effect in the model for sigma?
modeladultssigmanoarea <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                   part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                   hhsize_cat+wavecount+part_gender+educationmainearner+
                                   part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                   part_vacc:educationmainearner+part_vacc:area_3_name+
                                   pvc(day_number, by = part_vacc:part_symp_none)+
                                   re(random = ~1|part_uid),
                                 sigma.formula = ~part_vacc+part_elevated_risk+
                                   part_face_mask+part_symp_none+holiday+wd+
                                   wavecount+part_gender+educationmainearner+
                                   part_elevated_risk:part_face_mask+
                                   pvc(day_number, by = part_vacc:part_symp_none)+
                                   re(random = ~1|part_uid), 
                                 family = NBI, 
                                 data = na.omit(finaldataset_noage_adult),
                                 control = gamlss.control(n.cyc = 1000))
#75 iterations, AIC = 63219

modeladultssigmanoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                      part_vacc:educationmainearner+part_vacc:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+wd+
                                      wavecount+part_gender+educationmainearner+
                                      part_elevated_risk:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid), 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 1000))
#85 iterations, AIC = 63229.1

modeladultssigmanowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                 part_vacc:educationmainearner+part_vacc:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+holiday+
                                 wavecount+part_gender+educationmainearner+
                                 part_elevated_risk:part_face_mask+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_adult),
                               control = gamlss.control(n.cyc = 1000))
#79 iterations, AIC = 63220.2

modeladultssigmanowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        hhsize_cat+wavecount+part_gender+educationmainearner+
                                        part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                        part_vacc:educationmainearner+part_vacc:area_3_name+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid),
                                      sigma.formula = ~part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        part_gender+educationmainearner+
                                        part_elevated_risk:part_face_mask+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid), 
                                      family = NBI, 
                                      data = na.omit(finaldataset_noage_adult),
                                      control = gamlss.control(n.cyc = 1000))
#223 iterations, AIC = 63657.2

modeladultssigmanogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                     part_vacc:educationmainearner+part_vacc:area_3_name+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     wavecount+educationmainearner+
                                     part_elevated_risk:part_face_mask+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid), 
                                   family = NBI, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#70 iterations, AIC = 63221.6

modeladultssigmanoeducation <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        hhsize_cat+wavecount+part_gender+educationmainearner+
                                        part_vacc:part_face_mask+part_elevated_risk:part_face_mask+
                                        part_vacc:educationmainearner+part_vacc:area_3_name+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid),
                                      sigma.formula = ~part_vacc+part_elevated_risk+
                                        part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                        wavecount+part_gender+
                                        part_elevated_risk:part_face_mask+
                                        pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid), 
                                      family = NBI, 
                                      data = na.omit(finaldataset_noage_adult),
                                      control = gamlss.control(n.cyc = 1000))
#68 iterations, AIC = 63220.7

## NO LARGE IMPROVEMENTS ANYMORE

# Final model
finalmodeladults <- modeladultssigmanohhsize

## GOF ##

# 1) Plot
# mean =~ 0, variance =~ 1, skewness =~ 0, kurtosis =~ 3 
# --> residuals are approximately normally distributed as they should be for an adequate model
plot(finalmodeladults)
# mean = -0.0205, variance = 0.8921, skewness = -0.0831, kurtosis = 3.1862

# 2) rqres.plot has to be used in addition to the function plot due to discrete distribution family
rqres.plot(finalmodelelderly)
rqres.plot(finalmodelelderly,2,all=FALSE)
### What is this??




df <- data.frame(
  Covariate = c("Vacc No", "Vacc Yes","Face mask No", "Face mask Yes", "Symptoms No", "Symptoms Yes", 
                "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                "Weekday", "Weekend","hh size 1", "hh size 2", "hh size 3+", 
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Female", "Male",
                "Brussels Hoofdstede : Face mask No","Vlaams Gewest : Face mask Yes", "Waals Gewest : Face mask Yes"),
  Estimate = c(0, 0.680269, 0, 0.381465, 0, -0.055198,
               0, 0.022659, -0.178782, 0, -0.087728,
               0, 0.039205, 0, 0.086573, -0.102832,
               0, 0.113599, -0.236361, -0.008987, -0.297800, -0.350155, -0.313700, -0.454829,
               0, -0.121366, 0, 0.247030, 0.074947),
  SE = c(0, 0.040828, 0, 0.147819, 0, 0.034870,
         0, 0.144789, 0.150306, 0, 0.028112, 
         0, 0.028273, 0, 0.028278, 0.070358,
         0, 0.068108, 0.072007, 0.075504, 0.077364, 0.079367, 0.078051, 0.052329,
         0, 0.026794, 0, 0.153813, 0.161006)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Vacc No", "Vacc Yes","Face mask No", "Face mask Yes", "Symptoms No", "Symptoms Yes", 
                         "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                         "Weekday", "Weekend","hh size 1", "hh size 2", "hh size 3+", 
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                         "Female", "Male",
                         "Brussels Hoofdstede : Face mask No","Vlaams Gewest : Face mask Yes", "Waals Gewest : Face mask Yes"))

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
