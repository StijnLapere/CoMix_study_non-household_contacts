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
                                pvc(day_number)+
                                re(random = ~1|part_uid), 
                                     family = GPO, 
                                     data = na.omit(finaldataset_noagegender_children),
                                     control = gamlss.control(n.cyc = 2000)) 

modelchildrensigma2 <- gamlss(num_nonhouseh_cont ~ part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid),
                              sigma.formula = ~part_face_mask+
                                area_3_name+holiday+wd+hhsize_cat+wavecount+holiday:area_3_name+
                                pvc(day_number)+
                                re(random = ~1|part_uid), 
                              family = GPO, 
                              data = na.omit(finaldataset_noagegender_children),
                              control = gamlss.control(n.cyc = 2000)) 
