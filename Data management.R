library(data.table)
contact_extra <- fread("C:/Users/stijn/Downloads/CoMix_BE_contact_extra.csv",fill=TRUE)
contact_common <- fread("C:/Users/stijn/Downloads/CoMix_BE_contact_common.csv",fill=TRUE)
participant_extra <- fread("C:/Users/stijn/Downloads/CoMix_BE_participant_extra.csv",fill=TRUE)
participant_common <- fread("C:/Users/stijn/Downloads/CoMix_BE_participant_common.csv",fill=TRUE)
hh_extra <- fread("C:/Users/stijn/Downloads/CoMix_BE_hh_extra.csv",fill=TRUE)
hh_common <- fread("C:/Users/stijn/Downloads/CoMix_BE_hh_common.csv",fill=TRUE)
sday <- fread("C:/Users/stijn/Downloads/CoMix_BE_sday.csv",fill=TRUE)

library(tidyr)
library(dplyr)

### This is incorrect, because only the waves with at least 1 contact are taken into account ###
counts_per_wave <- contact_extra %>%
  group_by(part_uid, wave) %>%
  summarise(count = n(), .groups = "drop") %>%  
  pivot_wider(names_from = wave, values_from = count, values_fill = list(count = 0))


### This is the correct version
counts_per_wave <- participant_extra %>%
  dplyr::select(part_uid, wave, n_cnt_all)

# Count number of household contacts per participant and wave
contact_extra_columns <- contact_extra %>%
  group_by(part_uid, wave) %>%
  summarise(num_househ_cont = sum(cnt_household), .groups = "drop")

# Compute number of non-household contacts
contactextrawaves <- counts_per_wave %>%
  left_join(contact_extra_columns, by = c("part_uid","wave")) %>%
  mutate(num_nonhouseh_cont = n_cnt_all-num_househ_cont)

# If n_cnt_all = 0, num_househ_cont and num_nonhouseh_cont will be NA
# NAs will be set equal to 0
for (i in 1:nrow(contactextrawaves)){
  if (contactextrawaves$n_cnt_all[i] == 0){
    contactextrawaves$num_househ_cont[i] = 0
    contactextrawaves$num_nonhouseh_cont[i] = 0
  }
}

# Calculate total number of waves participant participated
contactextrawaves <- contactextrawaves %>%
  arrange(part_uid,wave) %>%
  group_by(part_uid) %>%
  mutate(num_waves_participated = n_distinct(wave))


contactextrawaves_short <- contactextrawaves %>%
  mutate(part_uid_wave = as.integer(paste0(substr(part_uid, 4, nchar(part_uid)), sprintf("%02d", wave))))

participant_extra_columns <- participant_extra %>% dplyr::select("part_id","part_social_group_be","part_vacc","part_elevated_risk","part_face_mask","part_symp_none","area_3_name","part_age_group","part_hh_education_main_earner","part_income","part_education","part_occupation_main_earner","part_occupation","part_employstatus")

contactextrawaves_merged <- contactextrawaves_short %>%
  left_join(participant_extra_columns, by = c("part_uid_wave" = "part_id"))

comix_be_sday_columns <- sday %>% dplyr::select("part_id","holiday")

contactextrawaves_merged <- contactextrawaves_merged %>%
  left_join(comix_be_sday_columns, by = c("part_uid_wave" = "part_id"))

participant_common_columns <- participant_common %>% dplyr::select("part_id","part_age","part_gender")

contactextrawaves_merged <- contactextrawaves_merged %>%
  left_join(participant_common_columns, by = c("part_uid_wave" = "part_id"))

# For every participant, the age at the first wave of participation is used
unique_ids <- unique(contactextrawaves_merged$part_uid)
# Loop over each participant
for (id in unique_ids) {
  # Get the row indices for this participant
  rows <- which(contactextrawaves_merged$part_uid == id)
  
  # Get the first non-NA age for this participant
  first_wave_age <- contactextrawaves_merged$part_age[rows[!is.na(contactextrawaves_merged$part_age[rows])]][1]
  
  # Set the age for all rows for this participant to the first non-NA age
  contactextrawaves_merged$part_age[rows] <- first_wave_age
}

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(part_uid_wave_HH = as.character(paste0("HH", part_uid_wave)))

hh_common_columns <- hh_common %>% dplyr::select("hh_id","hh_size")

contactextrawaves_merged <- contactextrawaves_merged %>%
  left_join(hh_common_columns, by = c("part_uid_wave_HH" = "hh_id"))

# Extract weekday and date of each response
participant_extra_columns <- participant_extra %>%
  arrange(part_uid, wave) %>%  
  group_by(part_uid, wave) %>%
  slice(1) %>%  # Keep only the first row per (part_uid, wave)
  ungroup() %>% dplyr::select("part_uid","wave","survey_weekday","survey_date")

contactextrawaves_merged <- contactextrawaves_merged %>%
  left_join(participant_extra_columns, by = c("part_uid","wave"))

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(adult_cat = case_when(
    part_age >= 0 & part_age <= 17 ~ "Children",
    part_age >= 18 & part_age <= 64 ~ "Adult",
    part_age >= 65 ~ "Elderly",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

contactextrawaves_NAage <- contactextrawaves_merged %>%
  group_by(part_uid) %>%
  filter(any(is.na(adult_cat))) %>%
  dplyr::select(part_uid_wave,part_age,part_age_group) %>%
  ungroup()

contactextrawaves_merged$adult_cat[contactextrawaves_merged$part_uid == "be_20523"] = "Adult"
contactextrawaves_merged$adult_cat[contactextrawaves_merged$part_uid == "be_20524"] = "Adult"
contactextrawaves_merged$adult_cat[contactextrawaves_merged$part_uid == "be_20949"] = "Adult"
contactextrawaves_merged$adult_cat[contactextrawaves_merged$part_uid == "be_21152"] = "Adult"
contactextrawaves_merged$adult_cat[contactextrawaves_merged$part_uid == "be_21424"] = "Adult"
contactextrawaves_merged$adult_cat[contactextrawaves_merged$part_uid == "be_21502"] = "Adult"
contactextrawaves_merged$adult_cat[contactextrawaves_merged$part_uid == "be_21882"] = "Adult"

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(adult = case_when(
    adult_cat == "Adult" ~ 1,
    adult_cat == "Elderly" ~ 0,
    adult_cat == "Children" ~ 0,
    TRUE ~ NA_real_  # Handle missing or unexpected values
  ))

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(elderly = case_when(
    adult_cat == "Adult" ~ 0,
    adult_cat == "Elderly" ~ 1,
    adult_cat == "Children" ~ 0,
    TRUE ~ NA_real_  # Handle missing or unexpected values
  ))

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(wd = case_when(
    survey_weekday == "Monday" ~ "Weekday",
    survey_weekday == "Tuesday" ~ "Weekday",
    survey_weekday == "Wednesday" ~ "Weekday",
    survey_weekday == "Thursday" ~ "Weekday",
    survey_weekday == "Friday" ~ "Weekday",
    survey_weekday == "Saturday" ~ "Weekend",
    survey_weekday == "Sunday" ~ "Weekend",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(employstatus = case_when(
    part_employstatus == "employed full-time (34 hours or more)" ~ "Employed",
    part_employstatus == "employed part-time (less than 34 hours)" ~ "Employed",
    part_employstatus == "full-time parent homemaker" ~ "Not in labor force",
    part_employstatus == "long-term sick or disabled" ~ "Not in labor force",
    part_employstatus == "retired" ~ "Not in labor force",
    part_employstatus == "self employed" ~ "Employed",
    part_employstatus == "student/pupil" ~ "Student",
    part_employstatus == "unemployed and not looking for a job" ~ "Not in labor force",
    part_employstatus == "unemployed but looking for a job" ~ "Not in labor force",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(educationmainearner = case_when(
    part_hh_education_main_earner == "Without a diploma or primary education" ~ "Low",
    part_hh_education_main_earner == "General lower secondary education (first 3 years completed)" ~ "Low",
    part_hh_education_main_earner == "Technical artistic or professional lower secondary education (first 3 years completed)" ~ "Low",
    part_hh_education_main_earner == "General upper secondary education (6 years completed)" ~ "Medium",
    part_hh_education_main_earner == "Professional upper secondary (6 years)" ~ "Medium",
    part_hh_education_main_earner == "Technical or artistic upper secondary education (6 years)" ~ "Medium",
    part_hh_education_main_earner == "Higher education: graduat candidature bachelor" ~ "High",
    part_hh_education_main_earner == "University education: bachelor's degree post-graduate master's degree" ~ "High",
    part_hh_education_main_earner == "Complementary master" ~ "High",
    part_hh_education_main_earner == "Doctorate" ~ "High",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(area_3_name = recode(area_3_name,
                              "R\xe9gion Flamande / Vlaams Gewest" = "Vlaams Gewest",
                              "R\xe9gion Wallonne / Waals Gewest" = "Waals Gewest",
                              "R\xe9gion de Bruxelles-Capitale / Brussels Hoofdstede" = "Brussels Hoofdstede"))

table(contactextrawaves_merged$hh_size) #Make factor variable with hhsize in {1,2,3,4+}
contactextrawaves_merged <- contactextrawaves_merged %>%
  mutate(hhsize_cat = as.factor(ifelse(hh_size == 1, "1",
                                       ifelse(hh_size == 2, "2",
                                              ifelse(hh_size == 3, "3","4+")))))

contactextrawaves_merged2 <- contactextrawaves_merged %>%  
  group_by(part_uid) %>% 
  arrange(part_uid) %>%
  # add column wavecount which represents the n-th wave for each participant
  mutate(wavecount = as.factor(ifelse(row_number() == 1, "1",
                                      ifelse(row_number() == 2, "2",
                                             ifelse(row_number() == 3, "3",
                                                    ifelse(row_number() == 4, "4",
                                                           ifelse(row_number() == 5, "5",
                                                                  ifelse(row_number() == 6, "6",
                                                                         ifelse(row_number() == 7, "7", ">=8"))))))))) %>% 
  arrange(part_uid) %>% group_by(part_uid) %>%  as.data.frame()

contactextrawaves_merged2 <- contactextrawaves_merged2 %>%  
  group_by(part_uid) %>% 
  arrange(part_uid) %>%
  # add column wavecount which represents the n-th wave for each participant
  mutate(wavecountshort = as.factor(ifelse(row_number() == 1, "1",
                                      ifelse(row_number() == 2, "2",">=3")))) %>% 
  arrange(part_uid) %>% group_by(part_uid) %>%  as.data.frame()



contact_extra <- contact_extra %>%
  mutate(cnt_adult_cat = case_when(
    cnt_age_group == "0-11" ~ "Children",
    cnt_age_group == "0-4" ~ "Children",
    cnt_age_group == "12-17" ~ "Children",
    cnt_age_group == "18-29" ~ "Adult",
    cnt_age_group == "18-39" ~ "Adult",
    cnt_age_group == "18-64" ~ "Adult",
    cnt_age_group == "30-39" ~ "Adult",
    cnt_age_group == "30-49" ~ "Adult",
    cnt_age_group == "40-49" ~ "Adult",
    cnt_age_group == "40-59" ~ "Adult",
    cnt_age_group == "5-11" ~ "Children",
    cnt_age_group == "5-17" ~ "Children",
    cnt_age_group == "50-59" ~ "Adult",
    cnt_age_group == "50-69" ~ "Adult",
    cnt_age_group == "60-69" ~ "Adult",
    cnt_age_group == "70-120" ~ "Elderly",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

contact_extra_NAagegroup <- contact_extra %>%
  filter(is.na(cnt_adult_cat)) %>%
  dplyr::select(part_uid,wave,cnt_adult_cat,cnt_age_group,cnt_age)

table(contact_extra_NAagegroup$cnt_age)

contact_extra <- contact_extra %>%
  mutate(cnt_adult_cat = case_when(
    cnt_age_group == "0-11" ~ "Children",
    cnt_age_group == "0-4" ~ "Children",
    cnt_age_group == "12-17" ~ "Children",
    cnt_age_group == "18-29" ~ "Adult",
    cnt_age_group == "18-39" ~ "Adult",
    cnt_age_group == "18-64" ~ "Adult",
    cnt_age_group == "30-39" ~ "Adult",
    cnt_age_group == "30-49" ~ "Adult",
    cnt_age_group == "40-49" ~ "Adult",
    cnt_age_group == "40-59" ~ "Adult",
    cnt_age_group == "5-11" ~ "Children",
    cnt_age_group == "5-17" ~ "Children",
    cnt_age_group == "50-59" ~ "Adult",
    cnt_age_group == "50-69" ~ "Adult",
    cnt_age_group == "60-69" ~ "Adult",
    cnt_age_group == "70-120" ~ "Elderly",
    cnt_age == "15-19" ~ "Children",
    cnt_age == "65+" ~ "Elderly",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))
  
table(contact_extra$cnt_adult_cat,useNA = "ifany")

contacts_columns <- contact_extra %>% 
  filter(cnt_household == 0) %>%
  dplyr::select("part_uid","wave","cnt_adult_cat")

contacts_summary <- contacts_columns %>%
  group_by(part_uid, wave, cnt_adult_cat) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = cnt_adult_cat, values_from = count, values_fill = 0)

colnames(contacts_summary) <- c("part_uid","wave","cnt_adult","cnt_NA","cnt_elderly","cnt_children")

contactextrawaves_merged2 <- contactextrawaves_merged2 %>%
  left_join(contacts_summary, by = c("part_uid","wave"))

for (i in 1:nrow(contactextrawaves_merged2)){
  if (contactextrawaves_merged2$num_nonhouseh_cont[i] == 0){
    contactextrawaves_merged2$cnt_adult[i] = 0
    contactextrawaves_merged2$cnt_NA[i] = 0
    contactextrawaves_merged2$cnt_children[i] = 0
    contactextrawaves_merged2$cnt_elderly[i] = 0
  }
}

start_date <- as.Date("2020-12-23")

finaldataset <- contactextrawaves_merged2 %>%
  mutate(day_number = as.numeric(as.Date(survey_date) - start_date) / 365)

# Remove participants with social group not allocated
finaldataset <- finaldataset %>%
  filter(part_social_group_be != "Not allocated")

finaldataset$part_uid <- as.factor(finaldataset$part_uid)
finaldataset$wave <- as.factor(finaldataset$wave)
finaldataset$part_social_group_be <- as.factor(finaldataset$part_social_group_be)
finaldataset$part_social_group_be <- relevel(finaldataset$part_social_group_be, ref = "Group 1&2")
finaldataset$part_vacc <- factor(finaldataset$part_vacc, levels = c("Yes", "No"))
finaldataset$part_vacc <- relevel(finaldataset$part_vacc, ref = "No")
finaldataset$part_elevated_risk <- factor(finaldataset$part_elevated_risk, levels = c("yes", "no"))
finaldataset$part_elevated_risk <- relevel(finaldataset$part_elevated_risk, ref = "no")
finaldataset$part_face_mask <- factor(finaldataset$part_face_mask, levels = c("yes", "no"))
finaldataset$part_face_mask <- relevel(finaldataset$part_face_mask, ref = "no")
finaldataset$part_symp_none <- case_when(finaldataset$part_symp_none == 0 ~ "No",finaldataset$part_symp_none == 1 ~ "Yes")
finaldataset$part_symp_none <- factor(finaldataset$part_symp_none, levels = c("No", "Yes"))
finaldataset$part_symp_none <- relevel(finaldataset$part_symp_none, ref = "No")
finaldataset$area_3_name <- factor(finaldataset$area_3_name, levels = c("Vlaams Gewest", "Waals Gewest", "Brussels Hoofdstede"))
finaldataset$area_3_name <- relevel(finaldataset$area_3_name, ref = "Brussels Hoofdstede")
finaldataset$holiday <- case_when(finaldataset$holiday == 0 ~ "No",finaldataset$holiday == 1 ~ "Yes")
finaldataset$holiday <- factor(finaldataset$holiday, levels = c("No", "Yes"))
finaldataset$holiday <- relevel(finaldataset$holiday, ref = "No")
finaldataset$part_gender <- factor(finaldataset$part_gender, levels = c("M","F"))
finaldataset$part_gender <- relevel(finaldataset$part_gender, ref = "F")
finaldataset$hhsize_cat <- relevel(finaldataset$hhsize_cat, ref = "1")
finaldataset$adult_cat <- factor(finaldataset$adult_cat, levels = c("Adult","Children","Elderly"))
finaldataset$adult_cat <- relevel(finaldataset$adult_cat, ref = "Adult")
finaldataset$adult <- factor(finaldataset$adult, levels = c("0","1"))
finaldataset$adult <- relevel(finaldataset$adult, ref = "0")
finaldataset$elderly <- factor(finaldataset$elderly, levels = c("0","1"))
finaldataset$elderly <- relevel(finaldataset$elderly, ref = "0")
finaldataset$wd <- factor(finaldataset$wd, levels = c("Weekday","Weekend"))
finaldataset$wd <- relevel(finaldataset$wd, ref = "Weekday")
finaldataset$employstatus <- factor(finaldataset$employstatus, levels = c("Employed","Not in labor force", "Student"))
finaldataset$employstatus <- relevel(finaldataset$employstatus, ref = "Not in labor force")
finaldataset$educationmainearner <- factor(finaldataset$educationmainearner, levels = c("Low","Medium","High"))
finaldataset$educationmainearner <- relevel(finaldataset$educationmainearner, ref = "Medium")
finaldataset$wavecount <- relevel(finaldataset$wavecount, ref = "1")
finaldataset$wavecountshort <- relevel(finaldataset$wavecountshort, ref = "1")


colSums(is.na(finaldataset))

# Filter out participants with at least one vaccination status = NA
finaldataset_NAvacc <- finaldataset %>%
  group_by(part_uid) %>%
  filter(any(is.na(part_vacc))) %>% 
  dplyr::select(part_uid,wave,part_vacc,adult_cat) %>%
  ungroup()

# If part_vacc = Yes, the vaccination status of that participant will be Yes for the next waves as well
unique_ids <- unique(finaldataset$part_uid)
for (id in unique_ids) {
  rows <- which(finaldataset$part_uid == id)
  seen_yes <- FALSE
  for (i in rows) {
    if (!is.na(finaldataset$part_vacc[i]) && finaldataset$part_vacc[i] == "Yes") {
      seen_yes <- TRUE  # Mark that "yes" has appeared
    }
    # If "yes" was seen before and the current value is NA, replace with "yes"
    if (seen_yes && is.na(finaldataset$part_vacc[i])) {
      finaldataset$part_vacc[i] <- "Yes"
    }
  }
}

colSums(is.na(finaldataset))

# Filter out participants with at least one vaccination status = NA
finaldataset_NAvacc <- finaldataset %>%
  group_by(part_uid) %>%
  filter(any(is.na(part_vacc))) %>% 
  dplyr::select(part_uid,wave,part_vacc,adult_cat) %>%
  ungroup()

# Fill in missing values with previous value
finaldataset <- finaldataset %>% 
  group_by(part_uid) %>% 
  fill("part_vacc", .direction = "down")

# It is assumed that children are not vaccinated
finaldataset$part_vacc[finaldataset$adult_cat == "Children" & is.na(finaldataset$part_vacc)] <- "No"

finaldataset_NAgender <- finaldataset %>%
  group_by(part_uid) %>%
  filter(any(is.na(part_gender))) %>%
  dplyr::select(part_uid,wave,part_gender,adult_cat) %>%
  ungroup()

# For every participant, the gender at the first wave of participation is used
for (id in unique_ids) {
  rows <- which(finaldataset$part_uid == id)
  # Get the first non-NA gender for this participant
  first_wave_gender <- finaldataset$part_gender[rows[!is.na(finaldataset$part_gender[rows])]][1]
  # Set the gender for all rows for this participant to this gender
  finaldataset$part_gender[rows] <- first_wave_gender
}

colSums(is.na(finaldataset))

finaldataset_NAelevrisk <- finaldataset %>%
  group_by(part_uid) %>%
  filter(any(is.na(part_elevated_risk))) %>%
  dplyr::select(part_uid,wave,part_elevated_risk,adult_cat) %>%
  ungroup()

# It is assumed that children have no elevated risk
finaldataset$part_elevated_risk[finaldataset$adult_cat == "Children" & is.na(finaldataset$part_elevated_risk)] <- "no"


finaldataset <- finaldataset %>% 
  group_by(part_uid) %>% 
  fill("part_elevated_risk", .direction = "downup")

finaldataset_NAfacemask <- finaldataset %>%
  group_by(part_uid) %>%
  filter(any(is.na(part_face_mask))) %>%
  dplyr::select(part_uid,wave,part_face_mask,adult_cat) %>%
  ungroup()

# It is assumed that children do not have to wear a face mask
finaldataset$part_face_mask[finaldataset$adult_cat == "Children" & is.na(finaldataset$part_face_mask)] <- "no"

finaldataset <- finaldataset %>% 
  group_by(part_uid) %>% 
  fill("part_face_mask", .direction = "downup")

finaldataset_NAsympnone <- finaldataset %>%
  group_by(part_uid) %>%
  filter(any(is.na(part_symp_none))) %>%
  dplyr::select(part_uid,wave,part_symp_none,adult_cat) %>%
  ungroup()

colSums(is.na(finaldataset))

finaldataset_noagegender <- finaldataset %>%
  dplyr::select(-part_age,-part_gender,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

finaldataset_noage <- finaldataset %>%
  dplyr::select(-part_age,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

finaldataset_children <- finaldataset %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

finaldataset_noagegender_children <- finaldataset_noagegender %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

finaldataset_adult <- finaldataset %>%
  filter(adult_cat == "Adult")

finaldataset_noage_adult <- finaldataset_noage %>%
  filter(adult_cat == "Adult")

finaldataset_elderly <- finaldataset %>%
  filter(adult_cat == "Elderly")

finaldataset_noage_Elderly <- finaldataset_noage %>%
  filter(adult_cat == "Elderly")

table(finaldataset_noagegender_children$hhsize_cat) #No category 1
finaldataset_noagegender_children$hhsize_cat <- relevel(finaldataset_noagegender_children$hhsize_cat, ref = "2")

table(finaldataset_noage_Elderly$hhsize_cat) #Almost no participants in 2 highest categories
finaldataset_noage_Elderly <- finaldataset_noage_Elderly %>%
  mutate(hhsize_elderly = as.factor(ifelse(hh_size == 1, "1",
                                           ifelse(hh_size == 2, "2","3+"))))
finaldataset_noage_Elderly$hhsize_elderly <- relevel(finaldataset_noage_Elderly$hhsize_elderly, ref = "1")



##########################################################
############ Dataset for logistic regression #############
##########################################################

logisticdataset <- contact_extra %>%
  dplyr::select("part_uid","wave","cont_id", "cnt_household") %>%
  mutate(cnt_nonhousehold = case_when(
    cnt_household == "0" ~ "1",
    cnt_household == "1" ~ "0",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

logisticdataset <- logisticdataset %>%
  mutate(part_uid_wave = as.integer(paste0(substr(part_uid, 4, nchar(part_uid)), sprintf("%02d", wave))))

logisticdataset$wave <- as.factor(logisticdataset$wave)

# Include variables from finaldataset
othervariables <- finaldataset %>%
  dplyr::select("part_uid","wave","num_waves_participated","area_3_name",
         "part_age_group","part_hh_education_main_earner","part_income",
         "part_education","part_occupation_main_earner","part_occupation",
         "part_employstatus","holiday","part_age","hh_size","survey_weekday",
         "survey_date","adult_cat","adult","elderly","wd",
         "employstatus","educationmainearner","hhsize_cat","wavecount","wavecountshort",
         "part_vacc","part_elevated_risk","part_face_mask","part_symp_none",
         "part_gender")

logisticdataset <- logisticdataset %>%
  left_join(othervariables, by = c("part_uid","wave"))

participant_extra_columns <- participant_extra %>% dplyr::select("part_id","part_social_group_be")

logisticdataset <- logisticdataset %>%
  left_join(participant_extra_columns, by = c("part_uid_wave" = "part_id"))

logisticdataset <- logisticdataset %>%
  group_by(part_uid,wave,num_waves_participated,area_3_name,
           part_age_group,part_hh_education_main_earner,part_income,
           part_education,part_occupation_main_earner,part_occupation,
           part_employstatus,holiday,part_age,hh_size,survey_weekday,
           survey_date,adult_cat,adult,elderly,wd,
           employstatus,educationmainearner,hhsize_cat,wavecount,wavecountshort,
           part_vacc,part_elevated_risk,part_face_mask,part_symp_none,
           part_gender,part_social_group_be) %>%
  summarise(
    any_nonhh_contact = as.integer(any(cnt_nonhousehold == 1)),
    num_contacts = sum(cnt_nonhousehold == 1, na.rm = TRUE),
    .groups = "drop"
  )

start_date <- as.Date("2020-12-23")

logisticdataset <- logisticdataset %>%
  mutate(day_number = as.numeric(as.Date(survey_date) - start_date) / 365)

# Remove participants with social group not allocated
logisticdataset <- logisticdataset %>%
  filter(part_social_group_be != "Not allocated")

logisticdataset$part_uid <- as.factor(logisticdataset$part_uid)
logisticdataset$part_social_group_be <- as.factor(logisticdataset$part_social_group_be)
logisticdataset$part_social_group_be <- relevel(logisticdataset$part_social_group_be, ref = "Group 1&2")
logisticdataset$part_vacc <- factor(logisticdataset$part_vacc, levels = c("Yes", "No"))
logisticdataset$part_vacc <- relevel(logisticdataset$part_vacc, ref = "No")
logisticdataset$part_elevated_risk <- factor(logisticdataset$part_elevated_risk, levels = c("yes", "no"))
logisticdataset$part_elevated_risk <- relevel(logisticdataset$part_elevated_risk, ref = "no")
logisticdataset$part_face_mask <- factor(logisticdataset$part_face_mask, levels = c("yes", "no"))
logisticdataset$part_face_mask <- relevel(logisticdataset$part_face_mask, ref = "no")
logisticdataset$part_symp_none <- factor(logisticdataset$part_symp_none, levels = c("No", "Yes"))
logisticdataset$part_symp_none <- relevel(logisticdataset$part_symp_none, ref = "No")
logisticdataset$area_3_name <- factor(logisticdataset$area_3_name, levels = c("Vlaams Gewest", "Waals Gewest", "Brussels Hoofdstede"))
logisticdataset$area_3_name <- relevel(logisticdataset$area_3_name, ref = "Brussels Hoofdstede")
logisticdataset$holiday <- factor(logisticdataset$holiday, levels = c("No", "Yes"))
logisticdataset$holiday <- relevel(logisticdataset$holiday, ref = "No")
logisticdataset$part_gender <- factor(logisticdataset$part_gender, levels = c("M","F"))
logisticdataset$part_gender <- relevel(logisticdataset$part_gender, ref = "F")
logisticdataset$hhsize_cat <- relevel(logisticdataset$hhsize_cat, ref = "1")
logisticdataset$adult_cat <- factor(logisticdataset$adult_cat, levels = c("Adult","Children","Elderly"))
logisticdataset$adult_cat <- relevel(logisticdataset$adult_cat, ref = "Adult")
logisticdataset$adult <- factor(logisticdataset$adult, levels = c("0","1"))
logisticdataset$adult <- relevel(logisticdataset$adult, ref = "0")
logisticdataset$elderly <- factor(logisticdataset$elderly, levels = c("0","1"))
logisticdataset$elderly <- relevel(logisticdataset$elderly, ref = "0")
logisticdataset$wd <- factor(logisticdataset$wd, levels = c("Weekday","Weekend"))
logisticdataset$wd <- relevel(logisticdataset$wd, ref = "Weekday")
logisticdataset$employstatus <- factor(logisticdataset$employstatus, levels = c("Employed","Not in labor force", "Student"))
logisticdataset$employstatus <- relevel(logisticdataset$employstatus, ref = "Not in labor force")
logisticdataset$educationmainearner <- factor(logisticdataset$educationmainearner, levels = c("Low","Medium","High"))
logisticdataset$educationmainearner <- relevel(logisticdataset$educationmainearner, ref = "Medium")
logisticdataset$wavecount <- relevel(logisticdataset$wavecount, ref = "1")
logisticdataset$wavecountshort <- relevel(logisticdataset$wavecountshort, ref = "1")
logisticdataset$any_nonhh_contact <- factor(logisticdataset$any_nonhh_contact, levels = c("0","1"))
logisticdataset$any_nonhh_contact <- relevel(logisticdataset$any_nonhh_contact, ref = "0")

colSums(is.na(logisticdataset))

logisticdataset_noagegender <- logisticdataset %>%
  dplyr::select(-part_age,-part_gender,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

logisticdataset_noage <- logisticdataset %>%
  dplyr::select(-part_age,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

logisticdataset_children <- logisticdataset %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

logisticdataset_noagegender_children <- logisticdataset_noagegender %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

logisticdataset_adult <- logisticdataset %>%
  filter(adult_cat == "Adult")

logisticdataset_noage_adult <- logisticdataset_noage %>%
  filter(adult_cat == "Adult")

logisticdataset_elderly <- logisticdataset %>%
  filter(adult_cat == "Elderly")

logisticdataset_noage_Elderly <- logisticdataset_noage %>%
  filter(adult_cat == "Elderly")

table(logisticdataset_noagegender_children$hhsize_cat) #No category 1
logisticdataset_noagegender_children$hhsize_cat <- relevel(logisticdataset_noagegender_children$hhsize_cat, ref = "2")

table(logisticdataset_noage_Elderly$hhsize_cat) #Almost no participants in 2 highest categories
logisticdataset_noage_Elderly <- logisticdataset_noage_Elderly %>%
  mutate(hhsize_elderly = as.factor(ifelse(hh_size == 1, "1",
                                           ifelse(hh_size == 2, "2","3+"))))
logisticdataset_noage_Elderly$hhsize_elderly <- relevel(logisticdataset_noage_Elderly$hhsize_elderly, ref = "1")

## In fact, we also have to include participations with 0 reported contacts
logisticdatasettotal <- finaldataset %>%
  mutate(any_nonhh_contact = as.integer(num_nonhouseh_cont > 0))

logisticdatasettotal$any_nonhh_contact <- factor(logisticdatasettotal$any_nonhh_contact, levels = c("0","1"))
logisticdatasettotal$any_nonhh_contact <- relevel(logisticdatasettotal$any_nonhh_contact, ref = "0")

colSums(is.na(logisticdatasettotal))

logisticdatasettotal_noagegender <- logisticdatasettotal %>%
  dplyr::select(-part_age,-part_gender,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

logisticdatasettotal_noage <- logisticdatasettotal %>%
  dplyr::select(-part_age,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

logisticdatasettotal_children <- logisticdatasettotal %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

logisticdatasettotal_noagegender_children <- logisticdatasettotal_noagegender %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

logisticdatasettotal_adult <- logisticdatasettotal %>%
  filter(adult_cat == "Adult")

logisticdatasettotal_noage_adult <- logisticdatasettotal_noage %>%
  filter(adult_cat == "Adult")

logisticdatasettotal_elderly <- logisticdatasettotal %>%
  filter(adult_cat == "Elderly")

logisticdatasettotal_noage_Elderly <- logisticdatasettotal_noage %>%
  filter(adult_cat == "Elderly")

table(logisticdatasettotal_noagegender_children$hhsize_cat) #No category 1
logisticdatasettotal_noagegender_children$hhsize_cat <- relevel(logisticdatasettotal_noagegender_children$hhsize_cat, ref = "2")

table(logisticdatasettotal_noage_Elderly$hhsize_cat) #Almost no participants in 2 highest categories
logisticdatasettotal_noage_Elderly <- logisticdatasettotal_noage_Elderly %>%
  mutate(hhsize_elderly = as.factor(ifelse(hh_size == 1, "1",
                                           ifelse(hh_size == 2, "2","3+"))))
logisticdatasettotal_noage_Elderly$hhsize_elderly <- relevel(logisticdatasettotal_noage_Elderly$hhsize_elderly, ref = "1")


##########################################################
################# Dataset for clustering #################
##########################################################

nonhouseholdcontacts <- contact_extra %>%
  filter(cnt_household == 0) %>%
  dplyr::select("part_uid","wave","cont_id")

nonhouseholdcontacts <- nonhouseholdcontacts %>%
  mutate(part_uid_wave = as.integer(paste0(substr(part_uid, 4, nchar(part_uid)), sprintf("%02d", wave))))

physcontact <- contact_common %>%
  dplyr::select("part_id","cont_id","phys_contact","cnt_home","cnt_work","cnt_school","cnt_transport","cnt_leisure","cnt_otherplace")

nonhouseholdcontacts <- nonhouseholdcontacts %>%
  arrange(part_uid,wave) %>%
  group_by(part_uid) %>%
  left_join(physcontact, by = c("part_uid_wave" = "part_id","cont_id"))

nonhouseholdcontacts$wave <- as.factor(nonhouseholdcontacts$wave)

# Include variables from finaldataset
othervariables <- finaldataset %>%
  dplyr::select("part_uid","wave","num_waves_participated","area_3_name",
         "part_age_group","part_hh_education_main_earner","part_income",
         "part_education","part_occupation_main_earner","part_occupation",
         "part_employstatus","holiday","part_age","hh_size","survey_weekday",
         "survey_date","adult_cat","adult","elderly","wd",
         "employstatus","educationmainearner","hhsize_cat","wavecount","wavecountshort",
         "part_vacc","part_elevated_risk","part_face_mask","part_symp_none",
         "part_gender")

nonhouseholdcontacts <- nonhouseholdcontacts %>%
  left_join(othervariables, by = c("part_uid","wave"))

participant_extra_columns <- participant_extra %>% dplyr::select("part_id","part_social_group_be")

nonhouseholdcontacts <- nonhouseholdcontacts %>%
  left_join(participant_extra_columns, by = c("part_uid_wave" = "part_id"))

contact_extra <- contact_extra %>%
  mutate(cnt_adult_cat = case_when(
    cnt_age_group == "0-11" ~ "Children",
    cnt_age_group == "0-4" ~ "Children",
    cnt_age_group == "12-17" ~ "Children",
    cnt_age_group == "18-29" ~ "Adult",
    cnt_age_group == "18-39" ~ "Adult",
    cnt_age_group == "18-64" ~ "Adult",
    cnt_age_group == "30-39" ~ "Adult",
    cnt_age_group == "30-49" ~ "Adult",
    cnt_age_group == "40-49" ~ "Adult",
    cnt_age_group == "40-59" ~ "Adult",
    cnt_age_group == "5-11" ~ "Children",
    cnt_age_group == "5-17" ~ "Children",
    cnt_age_group == "50-59" ~ "Adult",
    cnt_age_group == "50-69" ~ "Adult",
    cnt_age_group == "60-69" ~ "Adult",
    cnt_age_group == "70-120" ~ "Elderly",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

contact_extra_NAagegroup <- contact_extra %>%
  filter(is.na(cnt_adult_cat)) %>%
  dplyr::select(part_uid,wave,cnt_adult_cat,cnt_age_group,cnt_age)

table(contact_extra_NAagegroup$cnt_age)

contact_extra <- contact_extra %>%
  mutate(cnt_adult_cat = case_when(
    cnt_age_group == "0-11" ~ "Children",
    cnt_age_group == "0-4" ~ "Children",
    cnt_age_group == "12-17" ~ "Children",
    cnt_age_group == "18-29" ~ "Adult",
    cnt_age_group == "18-39" ~ "Adult",
    cnt_age_group == "18-64" ~ "Adult",
    cnt_age_group == "30-39" ~ "Adult",
    cnt_age_group == "30-49" ~ "Adult",
    cnt_age_group == "40-49" ~ "Adult",
    cnt_age_group == "40-59" ~ "Adult",
    cnt_age_group == "5-11" ~ "Children",
    cnt_age_group == "5-17" ~ "Children",
    cnt_age_group == "50-59" ~ "Adult",
    cnt_age_group == "50-69" ~ "Adult",
    cnt_age_group == "60-69" ~ "Adult",
    cnt_age_group == "70-120" ~ "Elderly",
    cnt_age == "15-19" ~ "Children",
    cnt_age == "65+" ~ "Elderly",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

table(contact_extra$cnt_adult_cat,useNA = "ifany")

contacts_columns <- contact_extra %>% 
  filter(cnt_household == 0) %>%
  dplyr::select("part_uid","cont_id","wave","cnt_adult_cat")

contacts_columns$wave <- as.factor(contacts_columns$wave)

nonhouseholdcontacts <- nonhouseholdcontacts %>%
  left_join(contacts_columns, by = c("part_uid","wave","cont_id"))

nonhouseholdcontacts <- nonhouseholdcontacts %>%
  mutate(place = case_when(
    cnt_home == "TRUE" ~ "Home",
    cnt_work == "TRUE" ~ "Work",
    cnt_school == "TRUE" ~ "School",
    cnt_leisure == "TRUE" ~ "Leisure",
    cnt_otherplace == "TRUE" ~ "Other",
    cnt_transport == "TRUE" ~ "Transport",
    TRUE ~ NA_character_  # Handle missing or unexpected values
  ))

table(nonhouseholdcontacts$place,useNA="ifany")

start_date <- as.Date("2020-12-23")

nonhouseholdcontacts <- nonhouseholdcontacts %>%
  mutate(day_number = as.numeric(as.Date(survey_date) - start_date) / 365)

# Remove participants with social group not allocated
nonhouseholdcontacts <- nonhouseholdcontacts %>%
  filter(part_social_group_be != "Not allocated")

nonhouseholdcontacts$part_uid <- as.factor(nonhouseholdcontacts$part_uid)
nonhouseholdcontacts$part_social_group_be <- as.factor(nonhouseholdcontacts$part_social_group_be)
nonhouseholdcontacts$part_social_group_be <- relevel(nonhouseholdcontacts$part_social_group_be, ref = "Group 1&2")
nonhouseholdcontacts$part_vacc <- factor(nonhouseholdcontacts$part_vacc, levels = c("Yes", "No"))
nonhouseholdcontacts$part_vacc <- relevel(nonhouseholdcontacts$part_vacc, ref = "No")
nonhouseholdcontacts$part_elevated_risk <- factor(nonhouseholdcontacts$part_elevated_risk, levels = c("yes", "no"))
nonhouseholdcontacts$part_elevated_risk <- relevel(nonhouseholdcontacts$part_elevated_risk, ref = "no")
nonhouseholdcontacts$part_face_mask <- factor(nonhouseholdcontacts$part_face_mask, levels = c("yes", "no"))
nonhouseholdcontacts$part_face_mask <- relevel(nonhouseholdcontacts$part_face_mask, ref = "no")
nonhouseholdcontacts$part_symp_none <- factor(nonhouseholdcontacts$part_symp_none, levels = c("No", "Yes"))
nonhouseholdcontacts$part_symp_none <- relevel(nonhouseholdcontacts$part_symp_none, ref = "No")
nonhouseholdcontacts$area_3_name <- factor(nonhouseholdcontacts$area_3_name, levels = c("Vlaams Gewest", "Waals Gewest", "Brussels Hoofdstede"))
nonhouseholdcontacts$area_3_name <- relevel(nonhouseholdcontacts$area_3_name, ref = "Brussels Hoofdstede")
nonhouseholdcontacts$holiday <- factor(nonhouseholdcontacts$holiday, levels = c("No", "Yes"))
nonhouseholdcontacts$holiday <- relevel(nonhouseholdcontacts$holiday, ref = "No")
nonhouseholdcontacts$part_gender <- factor(nonhouseholdcontacts$part_gender, levels = c("M","F"))
nonhouseholdcontacts$part_gender <- relevel(nonhouseholdcontacts$part_gender, ref = "F")
nonhouseholdcontacts$hhsize_cat <- relevel(nonhouseholdcontacts$hhsize_cat, ref = "1")
nonhouseholdcontacts$adult_cat <- factor(nonhouseholdcontacts$adult_cat, levels = c("Adult","Children","Elderly"))
nonhouseholdcontacts$adult_cat <- relevel(nonhouseholdcontacts$adult_cat, ref = "Adult")
nonhouseholdcontacts$adult <- factor(nonhouseholdcontacts$adult, levels = c("0","1"))
nonhouseholdcontacts$adult <- relevel(nonhouseholdcontacts$adult, ref = "0")
nonhouseholdcontacts$elderly <- factor(nonhouseholdcontacts$elderly, levels = c("0","1"))
nonhouseholdcontacts$elderly <- relevel(nonhouseholdcontacts$elderly, ref = "0")
nonhouseholdcontacts$wd <- factor(nonhouseholdcontacts$wd, levels = c("Weekday","Weekend"))
nonhouseholdcontacts$wd <- relevel(nonhouseholdcontacts$wd, ref = "Weekday")
nonhouseholdcontacts$employstatus <- factor(nonhouseholdcontacts$employstatus, levels = c("Employed","Not in labor force", "Student"))
nonhouseholdcontacts$employstatus <- relevel(nonhouseholdcontacts$employstatus, ref = "Not in labor force")
nonhouseholdcontacts$educationmainearner <- factor(nonhouseholdcontacts$educationmainearner, levels = c("Low","Medium","High"))
nonhouseholdcontacts$educationmainearner <- relevel(nonhouseholdcontacts$educationmainearner, ref = "Medium")
nonhouseholdcontacts$wavecount <- relevel(nonhouseholdcontacts$wavecount, ref = "1")
nonhouseholdcontacts$wavecountshort <- relevel(nonhouseholdcontacts$wavecountshort, ref = "1")
nonhouseholdcontacts$phys_contact <- case_when(nonhouseholdcontacts$phys_contact == 1 ~ 1,nonhouseholdcontacts$phys_contact == 2 ~ 0)
nonhouseholdcontacts$phys_contact <- factor(nonhouseholdcontacts$phys_contact, levels = c("0","1"))
nonhouseholdcontacts$phys_contact <- relevel(nonhouseholdcontacts$phys_contact, ref = "0")
nonhouseholdcontacts$cnt_adult_cat <- factor(nonhouseholdcontacts$cnt_adult_cat, levels = c("Children","Adult","Elderly"))
nonhouseholdcontacts$cnt_adult_cat <- relevel(nonhouseholdcontacts$cnt_adult_cat, ref = "Adult")
nonhouseholdcontacts$place <- factor(nonhouseholdcontacts$place, levels = c("Home","Work","School","Leisure","Other","Transport"))
nonhouseholdcontacts$place <- relevel(nonhouseholdcontacts$place, ref = "Home")

colSums(is.na(nonhouseholdcontacts))

nonhouseholdcontacts_noagegender <- nonhouseholdcontacts %>%
  dplyr::select(-part_age,-part_gender,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

nonhouseholdcontacts_noage <- nonhouseholdcontacts %>%
  dplyr::select(-part_age,-part_hh_education_main_earner,-part_income,-part_education,-part_occupation_main_earner,-part_occupation,-part_employstatus)

nonhouseholdcontacts_children <- nonhouseholdcontacts %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

nonhouseholdcontacts_noagegender_children <- nonhouseholdcontacts_noagegender %>%
  filter(adult_cat == "Children") %>% dplyr::select(-employstatus)

nonhouseholdcontacts_adult <- nonhouseholdcontacts %>%
  filter(adult_cat == "Adult")

nonhouseholdcontacts_noage_adult <- nonhouseholdcontacts_noage %>%
  filter(adult_cat == "Adult")

nonhouseholdcontacts_elderly <- nonhouseholdcontacts %>%
  filter(adult_cat == "Elderly")

nonhouseholdcontacts_noage_Elderly <- nonhouseholdcontacts_noage %>%
  filter(adult_cat == "Elderly")

table(nonhouseholdcontacts_noagegender_children$hhsize_cat) #No category 1
nonhouseholdcontacts_noagegender_children$hhsize_cat <- relevel(nonhouseholdcontacts_noagegender_children$hhsize_cat, ref = "2")

table(nonhouseholdcontacts_noage_Elderly$hhsize_cat) #Almost no participants in 2 highest categories
nonhouseholdcontacts_noage_Elderly <- nonhouseholdcontacts_noage_Elderly %>%
  mutate(hhsize_elderly = as.factor(ifelse(hh_size == 1, "1",
                                           ifelse(hh_size == 2, "2","3+"))))
nonhouseholdcontacts_noage_Elderly$hhsize_elderly <- relevel(nonhouseholdcontacts_noage_Elderly$hhsize_elderly, ref = "1")


