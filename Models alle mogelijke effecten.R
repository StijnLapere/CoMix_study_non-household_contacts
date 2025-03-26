var_area = c("area_3_name",
             "re(random = ~1|part_uid)")

var = c("area_3_name",
        "re(random = ~1|part_uid)")

library(gamlss)

model1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                  part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                  hhsize_cat+wavecount+pvc(day_number, by = part_vacc:part_symp_none)+
                  re(random = ~1|part_uid),
                sigma.formula = ~1, 
                family = NBI, 
                data = na.omit(finaldataset_noagegender),
                control = gamlss.control(n.cyc = 50)) 
#12 iterations, AIC = 121274

model2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_social_group_be*part_vacc+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50)) 
#25 iterations, AIC = 121274

model2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                   part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                   hhsize_cat+wavecount+part_social_group_be*adult_cat+
                   pvc(day_number, by = part_vacc:part_symp_none)+
                   re(random = ~1|part_uid),
                 sigma.formula = ~1, 
                 family = NBI, 
                 data = na.omit(finaldataset_noagegender),
                 control = gamlss.control(n.cyc = 50)) 
#14 iterations, AIC = 121260

model2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_vacc*part_elevated_risk+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50)) 
#14 iterations, AIC = 121274

model2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#15 iterations, AIC = 121254

model2.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_vacc*part_symp_none+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#9 iterations, AIC = 121269

model2.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_vacc*area_3_name+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#16 iterationsAIC = 121319

model2.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                       hhsize_cat+wavecount+part_vacc*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#7 iterations, AIC = 121271

model2.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_elevated_risk*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#13 iterations, AIC = 121347

model2.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_elevated_risk*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#15 iterations, AIC = 121272

model2.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_face_mask*part_symp_none+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#6 iterations, AIC = 121252

model2.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+part_face_mask*area_3_name+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#13 iterations, AIC = 121340

model2.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                      part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                      hhsize_cat+wavecount+part_face_mask*adult_cat+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noagegender),
                    control = gamlss.control(n.cyc = 50))
#12 iterations, AIC = 121262

model2.13 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#14 iterations, AIC = 121232 <- BEST MODEL

model2.14 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+holiday*wd+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#14 iterations, AIC = 121281










model3.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_social_group_be*part_vacc+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#17 iterations, AIC = 121342


model3.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_social_group_be*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#13 iterations, AIC = 121220


model3.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_vacc*part_elevated_risk+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#15 iterations, AIC = 121233


model3.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#14 iterations, AIC = 121213


model3.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_vacc*part_symp_none+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#17 iterations, AIC = 121228


model3.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_vacc*area_3_name+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#20 iterations, AIC = 121278


model3.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_vacc*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#14 iterations, AIC = 121229


model3.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#14 iterations, AIC = 121328


model3.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#21 iterations, AIC = 121205


model3.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*part_symp_none+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#13 iterations, AIC = 121213


model3.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*area_3_name+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#26 iterations, AIC = 121197


model3.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#21 iterations, AIC = 121157 <- BEST MODEL


model3.13 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                      part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                      hhsize_cat+wavecount+area_3_name*holiday+holiday*wd+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noagegender),
                    control = gamlss.control(n.cyc = 50))
#14 iterations, AIC = 121239





model4.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*part_vacc+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#45 iterations, AIC = 121200


model4.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#36 iterations, AIC = 121126 <- BEST MODEL

model4.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_vacc*part_elevated_risk+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#8 iterations, AIC = 121246

model4.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_vacc*part_face_mask+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#42 iterations, AIC = 121243

model4.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_vacc*part_symp_none+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#35 iterations, AIC = 121145

model4.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_vacc*area_3_name+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#41 iterations, AIC = 121269

model4.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_vacc*adult_cat+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#22 iterations, AIC = 121155

model4.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_elevated_risk*part_face_mask+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#27 iterations, AIC = 121248

model4.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_elevated_risk*adult_cat+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#12 iterations, AIC = 121258

model4.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_face_mask*part_symp_none+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#20 iterations, AIC = 121187

model4.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_face_mask*area_3_name+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#50 iterations, AIC = 121201

model4.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     holiday*wd+pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 50))
#23 iterations, AIC = 121164











model5.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_social_group_be*part_vacc+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))
#38 iterations, AIC = 121184

model5.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_vacc*part_elevated_risk+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_vacc*part_symp_none+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_vacc*area_3_name+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_vacc*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_elevated_risk*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_elevated_risk*adult_cat+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                     part_social_group_be*adult_cat+part_face_mask*part_symp_none+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender),
                   control = gamlss.control(n.cyc = 100))

model5.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                    part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                    hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                    part_social_group_be*adult_cat+part_face_mask*area_3_name+
                    pvc(day_number, by = part_vacc:part_symp_none)+
                    re(random = ~1|part_uid),
                  sigma.formula = ~1, 
                  family = NBI, 
                  data = na.omit(finaldataset_noagegender),
                  control = gamlss.control(n.cyc = 100))

model5.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                      part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                      hhsize_cat+wavecount+area_3_name*holiday+part_face_mask*adult_cat+
                      part_social_group_be*adult_cat+holiday*wd+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noagegender),
                    control = gamlss.control(n.cyc = 100))











model4.31 <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.32 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))


model4.33 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.34 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.35 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.36 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))


model4.37 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.38 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.39 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.310 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))


model4.3bis1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.3bis2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                         part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                         hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                         part_vacc*part_face_mask+
                         cs(day_number)+
                         re(random = ~1|part_uid),
                       sigma.formula = ~1, 
                       family = NBI, 
                       data = na.omit(finaldataset_noagegender))




#### MODEL FOR SIGMA

model4.3sigma <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                     part_vacc*part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                     part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                     hhsize_cat+wavecount+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid), 
                   family = NBI, 
                   data = na.omit(finaldataset_noagegender))

model4.3sigmabis <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                          part_vacc*part_face_mask+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+
                          pvc(day_number)+
                          re(random = ~1|part_uid), 
                        family = NBI, 
                        data = na.omit(finaldataset_noagegender))

modelsigma1.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                             hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                             part_vacc*part_face_mask+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                             hhsize_cat+wavecount+area_3_name*holiday+
                             pvc(day_number)+
                             re(random = ~1|part_uid), 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender))

modelsigma1.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                             hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                             part_vacc*part_face_mask+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                             hhsize_cat+wavecount+part_elevated_risk*adult_cat+
                             pvc(day_number)+
                             re(random = ~1|part_uid), 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender))

## FINAL MODEL
modelsigma1.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                             hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                             part_vacc*part_face_mask+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                             hhsize_cat+wavecount+part_vacc*part_face_mask+
                             pvc(day_number)+
                             re(random = ~1|part_uid), 
                           family = NBI, 
                           data = na.omit(finaldataset_noagegender))


modelsigma2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                          part_vacc*part_face_mask+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_vacc*part_face_mask+area_3_name*holiday+
                          pvc(day_number)+
                          re(random = ~1|part_uid), 
                        family = NBI, 
                        data = na.omit(finaldataset_noagegender))

modelsigma2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                          part_vacc*part_face_mask+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_vacc*part_face_mask+part_elevated_risk*adult_cat+
                          pvc(day_number)+
                          re(random = ~1|part_uid), 
                        family = NBI, 
                        data = na.omit(finaldataset_noagegender))




### FINAL MODEL: modelsigma1.3 ###

## Compare with Poisson model ##
modelsigma1.3poisson <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                          part_vacc*part_face_mask+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_vacc*part_face_mask+
                          pvc(day_number)+
                          re(random = ~1|part_uid), 
                        family = PO, 
                        data = na.omit(finaldataset_noagegender))
## GOF ##

# 1) Plot
# mean =~ 0, variance =~ 1, skewness =~ 0, kurtosis =~ 3 
# --> residuals are approximately normally distributed as they should be for an adequate model
plot(modelsigma1.3)
# mean = -0.0107, variance = 0.9101, skewness = -0.0765, kurtosis = 3.0897
# 1 large outlier in upper left plot

finaldataset_noagegender[which.max(abs(fitted(modelsigma1.3))), ]
finaldataset_noagegender_nooutlier <- finaldataset_noagegender[-which.max(abs(fitted(modelsigma1.3))), ]

modelsigma1.3nooutl <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+area_3_name*holiday+part_elevated_risk*adult_cat+
                          part_vacc*part_face_mask+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                          hhsize_cat+wavecount+part_vacc*part_face_mask+
                          pvc(day_number)+
                          re(random = ~1|part_uid), 
                        family = NBI, 
                        data = na.omit(finaldataset_noagegender_nooutlier))


plot(modelsigma1.3nooutl)


# 2) rqres.plot has to be used in addition to the function plot due to discrete distribution family
rqres.plot(modelsigma1.3)
rqres.plot(modelsigma1.3,2,all=FALSE)





df <- data.frame(
  Covariate = c("Social group 1&2", "Social group 3&4", "Social group 5&6", "Social group 7&8",
                "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", 
                "Face mask No", "Face mask Yes", "Symptoms No", "Symptoms Yes", 
                "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", "Holiday No", "Holiday Yes", 
                "Adults", "Children", "Elderly", "Weekday", "Weekend", 
                "hh size 1", "hh size 2", "hh size 3", "hh size 4+", 
                "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves",
                "Brussels Hoofdstede : Holiday No","Vlaams Gewest : Holiday Yes", "Waals Gewest : Holiday Yes", 
                "Elevated risk No : Adults", "Elevated risk Yes : Elderly", "Vacc No : Face mask No", "Vacc Yes : Face mask Yes"),
  Estimate = c(0,0.02785, -0.01106, -0.35952, 
               0, 0.46568, 0, -0.04904, 
               0, 0.75144, 0, -0.07920, 
               0, 0.76877, 0.06767, 0, -0.21115, 
               0, 0.86998, -0.10759, 0, 0.06506, 
               0, -0.06171, 0.21212, 0.04294, 
               0, -0.22771, -0.42237, -0.41292, -0.49135, -0.53341, -0.63664, -0.69726, 
               0, 0.03120, 0.04576, 
               0, 0.08984, 0, -0.08985),
  SE = c(0,0.01697, 0.01895, 0.01897, 
         0, 0.03299, 0, 0.01633, 
         0, 0.02455, 0, 0.01548, 
         0, 0.03040, 0.03232, 0, 0.05314, 
         0, 0.02692, 0.03283, 0, 0.01595, 
         0, 0.01611, 0.02146, 0.02257, 
         0, 0.02717, 0.03077, 0.03162, 0.03381, 0.03454, 0.03607, 0.01832, 
         0, 0.05586, 0.06003, 
         0, 0.04401, 0, 0.03349)
)

# Compute the relative number of contacts and confidence intervals
df <- df %>%
  mutate(
    RelativeContacts = exp(Estimate), 
    LowerCI = exp(Estimate - 1.96 * SE), 
    UpperCI = exp(Estimate + 1.96 * SE)
  )

covariate_order <- rev(c("Social group 1&2", "Social group 3&4", "Social group 5&6", "Social group 7&8",
                         "Vacc No", "Vacc Yes", "Elevated risk No", "Elevated risk Yes", "Face mask No", "Face mask Yes", 
                         "Symptoms No", "Symptoms Yes", "Brussels Hoofdstede", "Vlaams Gewest", "Waals Gewest", 
                         "Holiday No", "Holiday Yes", "Adults", "Children", "Elderly", "Weekday", "Weekend", 
                         "hh size 1", "hh size 2", "hh size 3", "hh size 4+",
                         "1 wave", "2 waves", "3 waves", "4 waves", "5 waves", "6 waves", "7 waves", "8+ waves", 
                         "Brussels Hoofdstede : Holiday No", "Vlaams Gewest : Holiday Yes", "Waals Gewest : Holiday Yes", 
                         "Elevated risk No : Adults", "Elevated risk Yes : Elderly", "Vacc No : Face mask No", "Vacc Yes : Face mask Yes"))

df$Covariate <- factor(df$Covariate, levels = covariate_order)

# Plot using ggplot2
ggplot(df, aes(x = RelativeContacts, y = reorder(Covariate, RelativeContacts))) +
  geom_point(size = 2, color = "red") + 
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.5, linewidth = 0.8, color = "black") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(x = "Relative Number of Contacts", y = "Covariates") +
  theme(axis.text.y = element_text(size = 10)) +
  scale_y_discrete(limits = covariate_order) # Ensure correct order




















model2 <- stepGAIC(model1, scope=list(lower=~pvc(day_number, by = part_vacc:part_symp_none)+re(random = ~1|part_uid),
                                      upper=~(part_social_group_be+part_vacc+part_elevated_risk+
                                                part_face_mask+part_symp_none+area_3_name+holiday+adult_cat+wd+
                                                hhsize_cat+wavecount)^2+pvc(day_number, by = part_vacc:part_symp_none)+
                                        re(random = ~1|part_uid)))



nrow(na.omit(finaldataset))
nrow(finaldataset)
nrow(finaldataset_noagegender)
nrow(na.omit(finaldataset_noagegender))
