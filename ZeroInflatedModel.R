library(gamlss)

model1ZIP <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                   part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                   hhsize_cat+wavecount+pvc(day_number, by = part_vacc:part_symp_none)+
                   re(random = ~1|part_uid),
                 sigma.formula = ~1, 
                 family = ZIP, 
                 data = na.omit(finaldataset_noagegender),
                 control = gamlss.control(n.cyc = 100)) 


model1ZIP2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                      part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                      hhsize_cat+wavecount+pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = ZIP2, 
                    data = na.omit(finaldataset_noagegender),
                    control = gamlss.control(n.cyc = 100))

modelZIP2_2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_social_group_be*part_vacc+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                       part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                       hhsize_cat+wavecount+part_social_group_be*adult_cat+
                       pvc(day_number, by = part_vacc:part_symp_none)+
                       re(random = ~1|part_uid),
                     sigma.formula = ~1, 
                     family = ZIP2, 
                     data = na.omit(finaldataset_noagegender),
                     control = gamlss.control(n.cyc = 50))

modelZIP2_2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_vacc*part_elevated_risk+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))


modelZIP2_2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount++part_vacc*part_face_mask+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_vacc*part_symp_none+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_vacc*adult_cat+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_elevated_risk*part_face_mask+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_elevated_risk*adult_cat+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_face_mask*part_symp_none+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_face_mask*area_3_name+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+area_3_name*holiday+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

modelZIP2_2.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+holiday*wd+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = ZIP2, 
                        data = na.omit(finaldataset_noagegender),
                        control = gamlss.control(n.cyc = 50))

