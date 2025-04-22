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
