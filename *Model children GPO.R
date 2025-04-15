library(gamlss)

modelchildren1pvc <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                              area_3_name+holiday+wd+hhsize_cat+wavecount+
                              pvc(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = GPO, 
                            data = na.omit(finaldataset_noagegender_children),
                            control = gamlss.control(n.cyc = 1000)) 

modelchildren1cs <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+
                             cs(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 1000)) 


modelchildren2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+part_face_mask:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#44 iterations, AIC = 27097.8

modelchildren2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#44 iterations, AIC = 27091.5

modelchildren2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+hhsize_cat:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#51 iterations, AIC = 27102

modelchildren2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_face_mask+
                             area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:wd+
                             pvc(day_number)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noagegender_children),
                           control = gamlss.control(n.cyc = 100)) 
#52 iterations, AIC = 27098.7
