library(gamlss)

modelelderly1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                          part_face_mask+part_symp_none+area_3_name+holiday+wd+
                          hhsize_elderly+wavecount+part_gender+
                          pvc(day_number, by = part_vacc:part_symp_none)+
                          re(random = ~1|part_uid),
                        sigma.formula = ~1, 
                        family = GPO, 
                        data = na.omit(finaldataset_noage_Elderly),
                        control = gamlss.control(n.cyc = 100))
#96 iterations, AIC = 23821

modelelderly2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_social_group_be:part_vacc+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#95 iterations, AIC = 23826

modelelderly2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_elevated_risk+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000)) 
#93 iterations, AIC = 23822

modelelderly2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#92 iterations, AIC = 23775.4 --> BEST IMPROVEMENT

modelelderly2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#93 iterations, AIC = 23823

modelelderly2.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#94 iterations, AIC = 23824.8

modelelderly2.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:hhsize_elderly+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#89 iterations, AIC = 23817.1


modelelderly2.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_elevated_risk:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#72 iterations, AIC = 23819


modelelderly2.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#91 iterations, AIC = 23822.4

modelelderly2.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#67 iterations, AIC = 23814


modelelderly2.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+area_3_name:holiday+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 1000))
#80 iterations, AIC = 23822.9

modelelderly2.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+area_3_name:hhsize_elderly+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 1000))
#84 iterations, AIC = 23822.1

modelelderly2.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+holiday:wd+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 1000))
#95 iterations, AIC = 23820.5

pchisq(23821-23775.4, df=length(coef(modelelderly2.3))-length(coef(modelelderly1)), lower.tail=FALSE)

modelelderly3.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_social_group_be:part_vacc+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#99 iterations, AIC = 23780.3

modelelderly3.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_vacc:part_elevated_risk+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#96 iterations, AIC = 23776.6

modelelderly3.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_vacc:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#92 iterations, AIC = 23777.4

modelelderly3.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_vacc:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#84 iterations, AIC = 23778.4

modelelderly3.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_vacc:hhsize_elderly+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#71 iterations, AIC = 23818.5

modelelderly3.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_elevated_risk:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#97 iterations, AIC = 23820.2

modelelderly3.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_face_mask:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#101 iterations, AIC = 23823.8

modelelderly3.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#91 iterations, AIC = 23815.8

modelelderly3.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                             area_3_name:holiday+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 1000))
#95 iterations, AIC = 23776.9

modelelderly3.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                             area_3_name:hhsize_elderly+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 1000))
#92 iterations, AIC = 23823.5

modelelderly3.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                             holiday:wd+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = GPO, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 1000))
#64 iterations, AIC = 23775.7

### NO IMPROVEMENTS ANYMORE

## CAN WE REMOVE A MAIN EFFECT?
modelelderlynosocialgroup <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#72 iterations, AIC = 23770 --> REMOVE SOCIAL GROUP

modelelderlynoelevatedrisk <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#77 iterations, AIC = 23820.3

modelelderlynoarea <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#75 iterations, AIC = 23821.6

modelelderlynoholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#73 iterations, AIC = 23774.3

modelelderlynowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#87 iterations, AIC = 23823.6

modelelderlynohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            wavecount+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#85 iterations, AIC = 23821

modelelderlynowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+part_gender+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#82 iterations, AIC = 23845.7

modelelderlynogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = GPO, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 1000))
#78 iterations, AIC = 23822.1

## Can we remove another main effect?
modelelderlynoelevatedrisk <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 1000))
#77 iterations, AIC = 23768.2

modelelderlynoarea <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+holiday+wd+
                                      hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 1000))
#71 iterations, AIC = 23769.1

modelelderlynoholiday <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+wd+
                                      hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 1000))
#70 iterations, AIC = 23768.9

modelelderlynowd <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+
                                      hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 1000))
#88 iterations, AIC = 23818.3

modelelderlynohhsize <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      wavecount+part_gender+part_vacc:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 1000))
#84 iterations, AIC = 23815.5

modelelderlynowavecount <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_elderly+part_gender+part_vacc:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 1000))
#76 iterations, AIC = 23840.7

modelelderlynogender <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_elderly+wavecount+part_vacc:part_face_mask+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = GPO, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 1000))
#78 iterations, AIC = 23816.6

### NO SIGNIFICANT IMPROVEMENTS ANYMORE
