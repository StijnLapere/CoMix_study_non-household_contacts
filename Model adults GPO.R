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