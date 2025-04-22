library(gamlss)

modelelderly1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                      hhsize_elderly+wavecount+part_gender+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noage_Elderly),
                    control = gamlss.control(n.cyc = 100)) 
#21 iterations, AIC = 24225.4

modelelderly2.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_social_group_be:part_vacc+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noage_Elderly),
                   control = gamlss.control(n.cyc = 100)) 
#21 iterations, AIC = 24229.2


modelelderly2.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_elevated_risk+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noage_Elderly),
                   control = gamlss.control(n.cyc = 100)) 
#21 iterations, AIC = 24226

modelelderly2.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noage_Elderly),
                   control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24226.2

modelelderly2.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:part_symp_none+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noage_Elderly),
                   control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24227.4

modelelderly2.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:area_3_name+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noage_Elderly),
                   control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24228.8

modelelderly2.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_vacc:hhsize_elderly+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24225.3


modelelderly2.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_elevated_risk:part_face_mask+
                     pvc(day_number, by = part_vacc:part_symp_none)+
                     re(random = ~1|part_uid),
                   sigma.formula = ~1, 
                   family = NBI, 
                   data = na.omit(finaldataset_noage_Elderly),
                   control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24224.4


modelelderly2.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_face_mask:part_symp_none+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noage_Elderly),
                    control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24227.4

modelelderly2.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noage_Elderly),
                    control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24215 --> BEST IMPROVEMENT


modelelderly2.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+area_3_name:holiday+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noage_Elderly),
                    control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24227.8

modelelderly2.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+area_3_name:hhsize_elderly+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 24227.5

modelelderly2.12 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+holiday:wd+
                      pvc(day_number, by = part_vacc:part_symp_none)+
                      re(random = ~1|part_uid),
                    sigma.formula = ~1, 
                    family = NBI, 
                    data = na.omit(finaldataset_noage_Elderly),
                    control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24226.1

pchisq(24225.4-24215, df=length(coef(modelelderly2.9))-length(coef(modelelderly1)), lower.tail=FALSE)




modelelderly3.1 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_social_group_be:part_vacc+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100)) 
#21 iterations, AIC = 24218.7


modelelderly3.2 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_vacc:part_elevated_risk+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100)) 
#21 iterations, AIC = 24215.7

modelelderly3.3 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_vacc:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24216.4

modelelderly3.4 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_vacc:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24217

modelelderly3.5 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_vacc:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24218.4

modelelderly3.6 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_vacc:hhsize_elderly+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24214.6


modelelderly3.7 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_elevated_risk:part_face_mask+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24214.2


modelelderly3.8 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            part_face_mask:part_symp_none+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24217

modelelderly3.9 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                             area_3_name:holiday+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24217.5

modelelderly3.10 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                             area_3_name:hhsize_elderly+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 24216.8

modelelderly3.11 <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                             holiday:wd+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~1, 
                           family = NBI, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24215.5

pchisq(24215-24214.2, df=length(coef(modelelderly3.7))-length(coef(modelelderly2.9)), lower.tail=FALSE)
### NO SIGNIFICANT IMPROVEMENT ANYMORE


## See if we can remove main effects
modelelderly2.9nosocialgroup <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24210.5 --> BEST IMPROVEMENT


modelelderly2.9noelevrisk <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24213

modelelderly2.9noholiday <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+wd+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24214.7

modelelderly2.9nowd <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+
                            hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24220.2

modelelderly2.9nohhsize <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            wavecount+part_gender+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 24213.4

modelelderly2.9nowavecount <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+part_gender+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 24274.8

modelelderly2.9nogender <- gamlss(num_nonhouseh_cont ~ part_social_group_be+part_vacc+part_elevated_risk+
                            part_face_mask+part_symp_none+area_3_name+holiday+wd+
                            hhsize_elderly+wavecount+part_face_mask:area_3_name+
                            pvc(day_number, by = part_vacc:part_symp_none)+
                            re(random = ~1|part_uid),
                          sigma.formula = ~1, 
                          family = NBI, 
                          data = na.omit(finaldataset_noage_Elderly),
                          control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 24214.4


### Remove main effect of social group
modelelderly2.9noelevrisk <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                         part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                         hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                         pvc(day_number, by = part_vacc:part_symp_none)+
                                         re(random = ~1|part_uid),
                                       sigma.formula = ~1, 
                                       family = NBI, 
                                       data = na.omit(finaldataset_noage_Elderly),
                                       control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24208.5 --> Remove elevated risk

modelelderly2.9noholiday <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                         part_face_mask+part_symp_none+area_3_name+wd+
                                         hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                         pvc(day_number, by = part_vacc:part_symp_none)+
                                         re(random = ~1|part_uid),
                                       sigma.formula = ~1, 
                                       family = NBI, 
                                       data = na.omit(finaldataset_noage_Elderly),
                                       control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 24210.1

modelelderly2.9nowd <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                         part_face_mask+part_symp_none+area_3_name+holiday+
                                         hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                         pvc(day_number, by = part_vacc:part_symp_none)+
                                         re(random = ~1|part_uid),
                                       sigma.formula = ~1, 
                                       family = NBI, 
                                       data = na.omit(finaldataset_noage_Elderly),
                                       control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24215.7

modelelderly2.9nohhsize <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                         part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                         wavecount+part_gender+part_face_mask:area_3_name+
                                         pvc(day_number, by = part_vacc:part_symp_none)+
                                         re(random = ~1|part_uid),
                                       sigma.formula = ~1, 
                                       family = NBI, 
                                       data = na.omit(finaldataset_noage_Elderly),
                                       control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 24208.5

modelelderly2.9nowavecount <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                         part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                         hhsize_elderly+part_gender+part_face_mask:area_3_name+
                                         pvc(day_number, by = part_vacc:part_symp_none)+
                                         re(random = ~1|part_uid),
                                       sigma.formula = ~1, 
                                       family = NBI, 
                                       data = na.omit(finaldataset_noage_Elderly),
                                       control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24270.5

modelelderly2.9nogender <- gamlss(num_nonhouseh_cont ~ part_vacc+part_elevated_risk+
                                         part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                         hhsize_elderly+wavecount+part_face_mask:area_3_name+
                                         pvc(day_number, by = part_vacc:part_symp_none)+
                                         re(random = ~1|part_uid),
                                       sigma.formula = ~1, 
                                       family = NBI, 
                                       data = na.omit(finaldataset_noage_Elderly),
                                       control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 24209.6

## Can we remove another main effect?
modelelderly2.9noholiday <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+wd+
                                      hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 24208.1

modelelderly2.9nowd <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+holiday+
                                      hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24213.7

modelelderly2.9nohhsize <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      wavecount+part_gender+part_face_mask:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 24206.4

modelelderly2.9nowavecount <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_elderly+part_gender+part_face_mask:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24268.6

modelelderly2.9nogender <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                      part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                      hhsize_elderly+wavecount+part_face_mask:area_3_name+
                                      pvc(day_number, by = part_vacc:part_symp_none)+
                                      re(random = ~1|part_uid),
                                    sigma.formula = ~1, 
                                    family = NBI, 
                                    data = na.omit(finaldataset_noage_Elderly),
                                    control = gamlss.control(n.cyc = 100))
#20 iterations, AIC = 24207.6

## Stop here and continue with modelelderly2.9noelevrisk

## Look at changes in additive term
#without by-statement
modelelderly2.9.1 <- gamlss(num_nonhouseh_cont ~ part_vacc+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                              pvc(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = NBI, 
                            data = na.omit(finaldataset_noage_Elderly),
                            control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 24278.8


#without by-statement but additional interaction
modelelderly2.9.2 <- gamlss(num_nonhouseh_cont ~ part_vacc+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                              part_vacc:part_symp_none+
                              pvc(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = NBI, 
                            data = na.omit(finaldataset_noage_Elderly),
                            control = gamlss.control(n.cyc = 100))
#22 iterations, AIC = 24276.8

#with cs instead of pvc
modelelderly2.9.3 <- gamlss(num_nonhouseh_cont ~ part_vacc+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                              cs(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = NBI, 
                            data = na.omit(finaldataset_noage_Elderly),
                            control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24293.9

modelelderly2.9.4 <- gamlss(num_nonhouseh_cont ~ part_vacc+
                              part_face_mask+part_symp_none+area_3_name+holiday+wd+
                              hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                              part_vacc:part_symp_none+
                              cs(day_number)+
                              re(random = ~1|part_uid),
                            sigma.formula = ~1, 
                            family = NBI, 
                            data = na.omit(finaldataset_noage_Elderly),
                            control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 24292

### NO SIGNIFICANT IMPROVEMENT 



modelelderlysigma1 <- gamlss(num_nonhouseh_cont ~ part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+wd+
                               hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                               pvc(day_number, by = part_vacc:part_symp_none)+
                               re(random = ~1|part_uid),
                             sigma.formula = ~part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+wd+
                               hhsize_elderly+wavecount+part_gender+
                               pvc(day_number, by = part_vacc:part_symp_none)+
                               re(random = ~1|part_uid), 
                             family = NBI, 
                             data = na.omit(finaldataset_noage_Elderly),
                             control = gamlss.control(n.cyc = 100))
#32 iterations, AIC = 22819.3

modelelderlysigma2 <- gamlss(num_nonhouseh_cont ~ part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+wd+
                               hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                               pvc(day_number, by = part_vacc:part_symp_none)+
                               re(random = ~1|part_uid),
                             sigma.formula = ~part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+wd+
                               hhsize_elderly+wavecount+part_gender+
                               pvc(day_number)+
                               re(random = ~1|part_uid), 
                             family = NBI, 
                             data = na.omit(finaldataset_noage_Elderly),
                             control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 22822.3

modelelderlysigma3 <- gamlss(num_nonhouseh_cont ~ part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+wd+
                               hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                               pvc(day_number, by = part_vacc:part_symp_none)+
                               re(random = ~1|part_uid),
                             sigma.formula = ~part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+wd+
                               hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                               pvc(day_number)+
                               re(random = ~1|part_uid), 
                             family = NBI, 
                             data = na.omit(finaldataset_noage_Elderly),
                             control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 22820.3

### IMPROVEMENT

## Can we remove main effects in model for sigma?
modelelderlysigma3nopart_vacc <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#27 iterations, AIC = 22820.4

modelelderlysigma3nosympnone <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 22820

modelelderlysigma3noholiday <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#21 iterations, AIC = 22818.8

modelelderlysigma3nowd <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#23 iterations, AIC = 22837.4

modelelderlysigma3nohhsize <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#28 iterations, AIC = 22814.9 --> Remove hhsize

modelelderlysigma3nowavecount <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#19 iterations, AIC = 22819.4

modelelderlysigma3nogender <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#42 iterations, AIC = 22820.6

## Can we remove another effect?
modelelderlysigma3novacc <- gamlss(num_nonhouseh_cont ~part_vacc+part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                       hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                       pvc(day_number, by = part_vacc:part_symp_none)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                       wavecount+part_gender+part_face_mask:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid), 
                                     family = NBI, 
                                     data = na.omit(finaldataset_noage_Elderly),
                                     control = gamlss.control(n.cyc = 100))
#31 iterations, AIC = 22814.9

modelelderlysigma3nosympnone <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+area_3_name+holiday+wd+
                                 wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#25 iterations, AIC = 22814.6

modelelderlysigma3noholiday <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                 hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number, by = part_vacc:part_symp_none)+
                                 re(random = ~1|part_uid),
                               sigma.formula = ~part_vacc+
                                 part_face_mask+part_symp_none+area_3_name+wd+
                                 wavecount+part_gender+part_face_mask:area_3_name+
                                 pvc(day_number)+
                                 re(random = ~1|part_uid), 
                               family = NBI, 
                               data = na.omit(finaldataset_noage_Elderly),
                               control = gamlss.control(n.cyc = 100))
#28 iterations, AIC = 22813.3

modelelderlysigma3nowd <- gamlss(num_nonhouseh_cont ~ part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+wd+
                               hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                               pvc(day_number, by = part_vacc:part_symp_none)+
                               re(random = ~1|part_uid),
                             sigma.formula = ~part_vacc+
                               part_face_mask+part_symp_none+area_3_name+holiday+
                               wavecount+part_gender+part_face_mask:area_3_name+
                               pvc(day_number)+
                               re(random = ~1|part_uid), 
                             family = NBI, 
                             data = na.omit(finaldataset_noage_Elderly),
                             control = gamlss.control(n.cyc = 100))
#28 iterations, AIC = 22832

modelelderlysigma3nowavecount <- gamlss(num_nonhouseh_cont ~ part_vacc+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~part_vacc+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             part_gender+part_face_mask:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid), 
                           family = NBI, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 100))
#34 iterations, AIC = 22813.9

modelelderlysigma3nogender <- gamlss(num_nonhouseh_cont ~ part_vacc+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                             pvc(day_number, by = part_vacc:part_symp_none)+
                             re(random = ~1|part_uid),
                           sigma.formula = ~part_vacc+
                             part_face_mask+part_symp_none+area_3_name+holiday+wd+
                             wavecount+part_face_mask:area_3_name+
                             pvc(day_number)+
                             re(random = ~1|part_uid), 
                           family = NBI, 
                           data = na.omit(finaldataset_noage_Elderly),
                           control = gamlss.control(n.cyc = 100))
#52 iterations, AIC = 22814.7

#### Final model: modelelderlysigma3nohhsize
finalmodelelderly <- modelelderlysigma3nohhsize

## Compare with Poisson model ##
modelelderlypoisson <- gamlss(num_nonhouseh_cont ~ part_vacc+
                                       part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                       hhsize_elderly+wavecount+part_gender+part_face_mask:area_3_name+
                                       pvc(day_number, by = part_vacc:part_symp_none)+
                                       re(random = ~1|part_uid),
                                     sigma.formula = ~part_vacc+
                                       part_face_mask+part_symp_none+area_3_name+holiday+wd+
                                       wavecount+part_gender+part_face_mask:area_3_name+
                                       pvc(day_number)+
                                       re(random = ~1|part_uid), 
                                     family = PO, 
                                     data = na.omit(finaldataset_noage_Elderly),
                                     control = gamlss.control(n.cyc = 100))
#5 iterations, AIC = 34037.2


## GOF ##

# 1) Plot
# mean =~ 0, variance =~ 1, skewness =~ 0, kurtosis =~ 3 
# --> residuals are approximately normally distributed as they should be for an adequate model
plot(finalmodelelderly)
# mean = -0.0080, variance = 0.8705, skewness = -0.0023, kurtosis = 3.2905

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

