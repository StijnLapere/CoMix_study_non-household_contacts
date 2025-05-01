library(gamlss)

modeladults1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                         part_face_mask+part_symp_none+area_3_name+holiday+wd+
                         hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                         pvc(day_number, by = part_vacc:part_symp_none)+
                         re(random = ~1|part_uid),
                       sigma.formula = ~1, 
                       family = GPO, 
                       data = na.omit(finaldataset_noage_adult),
                       control = gamlss.control(n.cyc = 1000)) 
#39 iterations, AIC = 66829.9


modeladults2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#50 iterations, AIC = 66822.1

modeladults2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#37 iterations, AIC = 66829.8

modeladults2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#37 iterations, AIC = 66834.5

modeladults2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#37 iterations, AIC = 66820.3

modeladults2.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#49 iterations, AIC = 66829.2

modeladults2.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#51 iterations, AIC = 66809.9 --> BEST IMPROVEMENT

modeladults2.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#39 iterations, AIC = 66831.9

modeladults2.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#44 iterations, AIC = 66818.9

modeladults2.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#48 iterations, AIC = 66828.6

modeladults2.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_elevated_risk:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#NOT CONVERGED

modeladults2.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_face_mask:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#39 iterations, AIC = 66831.7

modeladults2.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#36 iterations, AIC = 66822.1

modeladults2.13 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            area_3_name:holiday+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#42 iterations, AIC = 66833.3

modeladults2.14 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#41 iterations, AIC = 66829.4

modeladults2.15 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#43 iterations, AIC = 66831.9

pchisq(66829.9-66809.9, df=length(coef(modeladults2.6))-length(coef(modeladults1)), lower.tail=FALSE)


modeladults3.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#31 iterations, AIC = 66803.4

modeladults3.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#28 iterations, AIC = 66809.7

modeladults3.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#42 iterations, AIC = 66814.4

modeladults3.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#40 iterations, AIC = 66801.1

modeladults3.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#34 iterations, AIC = 66809

modeladults3.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#47 iterations, AIC = 66798.8 --> BEST IMPROVEMENT

modeladults3.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#42 iterations, AIC = 66808.5

modeladults3.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_face_mask:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#56 iterations, AIC = 66811.7

modeladults3.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#58 iterations, AIC = 66802.3

modeladults3.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+area_3_name:holiday+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#36 iterations, AIC = 66813.1

modeladults3.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#50 iterations, AIC = 66809.3

modeladults3.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#57 iterations, AIC = 66811.9

pchisq(66809.9-66798.8, df=length(coef(modeladults3.6))-length(coef(modeladults2.6)), lower.tail=FALSE)

modeladults4.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#34 iterations, AIC = 66794.1

modeladults4.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#35 iterations, AIC = 66799

modeladults4.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#33 iterations, AIC = 66803.2

modeladults4.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#52 iterations, AIC = 66791.8 --> BEST IMPROVEMENT

modeladults4.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:part_elevated_risk+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
# NOT CONVERGED

modeladults4.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#41 iterations, AIC = 66798

modeladults4.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_face_mask:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#33 iterations, AIC = 66800.6

modeladults4.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#41 iterations, AIC = 66792.6

modeladults4.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_vacc:area_3_name+area_3_name:holiday+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#50 iterations, AIC = 66801.7

modeladults4.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_vacc:area_3_name+area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#45 iterations, AIC = 66798

modeladults4.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_vacc:area_3_name+holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#38 iterations, AIC = 66800.8

pchisq(66798.8-66791.8, df=length(coef(modeladults4.4))-length(coef(modeladults3.6)), lower.tail=FALSE)

modeladults5.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#48 iterations, AIC = 66792.1

modeladults5.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#22 iterations, AIC = 66792.4

modeladults5.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#50 iterations, AIC = 66796.3

modeladults5.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#50 iterations, AIC = 66791.7

modeladults5.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#36 iterations, AIC = 66793.6

modeladults5.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#49 iterations, AIC = 66785.7 --> BEST IMPROVEMENT

modeladults5.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           area_3_name:holiday+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#53 iterations, AIC = 66794.7

modeladults5.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                            area_3_name:hhsize_cat+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#35 iterations, AIC = 66791

modeladults5.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                            part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                            holiday:wd+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#40 iterations, AIC = 66793.7

pchisq(66791.8-66785.7, df=length(coef(modeladults5.6))-length(coef(modeladults4.4)), lower.tail=FALSE)


modeladults6.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+part_social_group_be:part_vacc+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#31 iterations, AIC = 66785.9

modeladults6.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+part_social_group_be:employstatus+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#29 iterations, AIC = 66785.9

modeladults6.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+part_social_group_be:educationmainearner+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#23 iterations, AIC = 66790

modeladults6.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+part_vacc:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#26 iterations, AIC = 66785.9

modeladults6.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+part_face_mask:part_symp_none+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#55 iterations, AIC = 66787.7

modeladults6.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+area_3_name:holiday+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#39 iterations, AIC = 66788.4

modeladults6.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+area_3_name:hhsize_cat+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#35 iterations, AIC = 66785.2

modeladults6.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+holiday:wd+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#30 iterations, AIC = 66787.6

### NO SIGNIFICANT IMPROVEMENTS
## Can we remove main effects?

modeladultsnosocialgroup <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#35 iterations, AIC = 66780.5

modeladultsnoelevatedrisk <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#55 iterations, AIC = 66783.3

modeladultsnoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+wd+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#46 iterations, AIC = 66783.5

modeladultsnowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+
                           hhsize_cat+wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#39 iterations, AIC = 66789.7

modeladultsnohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           wavecount+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#36 iterations, AIC = 66786.3

modeladultsnowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+part_gender+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#46 iterations, AIC = 66905.1

modeladultsnogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+employstatus+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#49 iterations, AIC = 66783.5

modeladultsnoemploystatus <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                           part_face_mask+part_symp_none+area_3_name+holiday+wd+
                           hhsize_cat+wavecount+part_gender+educationmainearner+
                           part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                           part_face_mask:area_3_name+
                           pvc(day_number, by = part_vacc:part_symp_none)+
                           re(random = ~1|part_uid),
                         sigma.formula = ~1, 
                         family = GPO, 
                         data = na.omit(finaldataset_noage_adult),
                         control = gamlss.control(n.cyc = 1000))
#39 iterations, AIC = 66780.4 --> Remove employstatus

## Can we remove another main effect?
modeladultsnosocialgroup <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                     part_face_mask:area_3_name+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~1, 
                                   family = GPO, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#43 iterations, AIC = 66774.4 --> Remove socialgroup

modeladultsnoelevatedrisk <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                      part_face_mask:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 1000))
#40 iterations, AIC = 66777.7

modeladultsnoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+wd+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                 part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~1, 
                               family = GPO, 
                               data = na.omit(finaldataset_noage_adult),
                               control = gamlss.control(n.cyc = 1000))
#40 iterations, AIC = 66778.2

modeladultsnowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+
                            hhsize_cat+wavecount+part_gender+educationmainearner+
                            part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                            part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#NOT CONVERGED

modeladultsnohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~1, 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#37 iterations, AIC = 66780.6

modeladultsnowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                   part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                   hhsize_cat+part_gender+educationmainearner+
                                   part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                   part_face_mask:area_3_name+
                                   pvc(day_number, by = part_vacc:part_symp_none)+
                                   re(random = ~1|part_uid),
                                 sigma.formula = ~1, 
                                 family = GPO, 
                                 data = na.omit(finaldataset_noage_adult),
                                 control = gamlss.control(n.cyc = 1000))
#41 iterations, AIC = 66898.7

modeladultsnogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~1, 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#41 iterations, AIC = 66778.3

## Can we remove another main effect?
modeladultsnowd <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+
                            hhsize_cat+wavecount+part_gender+educationmainearner+
                            part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                            part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_adult),
                          control = gamlss.control(n.cyc = 1000))
#40 iterations, AIC = 66778.4

modeladultsnoelevatedrisk <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_cat+wavecount+part_gender+educationmainearner+
                                      part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                      part_face_mask:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_adult),
                                    control = gamlss.control(n.cyc = 1000))
# NOT CONVERGED

modeladultsnoholiday <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                 part_face_mask+part_symp_none+area_3_name+wd+
                                 hhsize_cat+wavecount+part_gender+educationmainearner+
                                 part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                 part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~1, 
                               family = GPO, 
                               data = na.omit(finaldataset_noage_adult),
                               control = gamlss.control(n.cyc = 1000))
#49 iterations, AIC = 66772.3

modeladultsnohhsize <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~1, 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#36 iterations, AIC = 66774.5

modeladultsnowavecount <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                   part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                   hhsize_cat+part_gender+educationmainearner+
                                   part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                   part_face_mask:area_3_name+
                                   pvc(day_number, by = part_vacc:part_symp_none)+
                                   re(random = ~1|part_uid),
                                 sigma.formula = ~1, 
                                 family = GPO, 
                                 data = na.omit(finaldataset_noage_adult),
                                 control = gamlss.control(n.cyc = 1000))
#38 iterations, AIC = 66892.6

modeladultsnogender <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~1, 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#55 iterations, AIC = 66772.3

## NO LARGE IMPROVEMENTS ANYMORE

modeladultsadditive1 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                     part_face_mask:area_3_name+
                                     pvc(day_number)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~1, 
                                   family = GPO, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#67 iterations, AIC = 66792.9

modeladultsadditive2 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                     part_face_mask:area_3_name+part_vacc:part_symp_none+
                                     pvc(day_number)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~1, 
                                   family = GPO, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#48 iterations, AIC = 66793.6

modeladultsadditive3 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                     part_face_mask:area_3_name+
                                     cs(day_number)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~1, 
                                   family = GPO, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#50 iterations, AIC = 66822.6

modeladultsadditive4 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                     part_face_mask:area_3_name+part_vacc:part_symp_none+
                                     cs(day_number)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~1, 
                                   family = GPO, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#42 iterations, AIC = 66823.5

## MODEL FOR SIGMA
modeladultssigma1 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                     part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                     hhsize_cat+wavecount+part_gender+educationmainearner+
                                     part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                     part_face_mask:area_3_name+
                                     pvc(day_number, by = part_vacc:part_symp_none)+
                                     re(random = ~1|part_uid),
                                   sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              pvc(day_number, by = part_vacc:part_symp_none), 
                                   family = GPO, 
                                   data = na.omit(finaldataset_noage_adult),
                                   control = gamlss.control(n.cyc = 1000))
#Don't converge

modeladultssigma2 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                              part_face_mask:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              pvc(day_number), 
                            family = GPO, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#Don't converge ...

modeladultssigma3 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                              part_face_mask:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner, 
                            family = GPO, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#86 iterations, AIC = 66163.5

modeladultssigma4 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                              part_face_mask:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              cs(day_number), 
                            family = GPO, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#68 iterations, AIC = 65970 --> Continue with this model

## Can we include an interaction term?
modeladultssigma4.1 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                              part_face_mask:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:part_face_mask+
                              cs(day_number), 
                            family = GPO, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#104 iterations, AIC = 65972.3

modeladultssigma4.2 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                              part_face_mask:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+
                              cs(day_number), 
                            family = GPO, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#83 iterations, AIC = 65959.2 --> BEST IMPROVEMENT

modeladultssigma4.3 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                              part_face_mask:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:educationmainearner+
                              cs(day_number), 
                            family = GPO, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#66 iterations, AIC = 65969.6

modeladultssigma4.4 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+
                              part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                              part_face_mask:area_3_name+
                              pvc(day_number, by = part_vacc:part_symp_none)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~part_vacc+part_elevated_risk+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_cat+wavecount+part_gender+educationmainearner+part_face_mask:area_3_name+
                              cs(day_number), 
                            family = GPO, 
                            data = na.omit(finaldataset_noage_adult),
                            control = gamlss.control(n.cyc = 1000))
#72 iterations, AIC = 65964.8

## Can we include another interaction term?
modeladultssigma5.1 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_vacc:part_face_mask+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#34 iterations, AIC = 65961.5

modeladultssigma5.2 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_vacc:educationmainearner+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#84 iterations, AIC = 65958.6

modeladultssigma5.3 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#101 iterations, AIC = 65952.4 --> BEST IMPROVEMENT

## Can we include another interaction term?
modeladultssigma6.1 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+part_vacc:part_face_mask+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#38 iterations, AIC = 65954.1

modeladultssigma6.2 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+part_vacc:educationmainearner+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#99 iterations, AIC = 65951.8

# NO LARGE IMPROVEMENTS ANYMORE

## Can we remove a main effect in the model for sigma?
modeladultssigmanoelevrisk <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#111 iterations, AIC = 65950.2

modeladultssigmanosympnone <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#46 iterations, AIC = 65957.9

modeladultssigmanoholiday <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#130 iterations, AIC = 65951.3

modeladultssigmanowd <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+
                                hhsize_cat+wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#49 iterations, AIC = 65950.3

modeladultssigmanohhsize <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                wavecount+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#38 iterations, AIC = 66012.2

modeladultssigmanowavecount <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+part_gender+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#62 iterations, AIC = 66114.9

modeladultssigmanogender <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+educationmainearner+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#61 iterations, AIC = 65949.5

modeladultssigmanoeducation <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+educationmainearner+
                                part_vacc:part_face_mask+part_vacc:area_3_name+part_vacc:educationmainearner+
                                part_face_mask:area_3_name+
                                pvc(day_number, by = part_vacc:part_symp_none)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_vacc+part_elevated_risk+
                                part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                hhsize_cat+wavecount+part_gender+part_vacc:area_3_name+part_face_mask:area_3_name+
                                cs(day_number), 
                              family = GPO, 
                              data = na.omit(finaldataset_noage_adult),
                              control = gamlss.control(n.cyc = 1000))
#75 iterations, AIC = 65964.6

pchisq(65952.4-65949.5,df=1,lower.tail = FALSE)

# NO SIGNIFICANT IMPROVEMENTS ANYMORE

finalmodeladultsGPO <- modeladultssigma5.3

## GOF ##

# 1) Plot
# mean =~ 0, variance =~ 1, skewness =~ 0, kurtosis =~ 3 
# --> residuals are approximately normally distributed as they should be for an adequate model
plot(finalmodeladultsGPO)
# mean = -0.0101, variance = 0.9780, skewness = 0.0241, kurtosis = 3.4853

# 2) rqres.plot has to be used in addition to the function plot due to discrete distribution family
rqres.plot(finalmodeladultsGPO)
rqres.plot(finalmodeladultsGPO,2,all=FALSE)
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
