### Number of participants
length(unique(finaldataset$part_uid)) #3977

### Total number of responses
nrow(finaldataset) #32740

### Total number of reported contacts
sum(finaldataset$n_contacts) #166380

library(dplyr)
minmaxcontacts <- participant_extra %>%
  group_by(wave, part_uid) %>%
  summarise(count = n_cnt_all) %>% 
  group_by(wave) %>%
  summarise(
    min_count = min(count),
    max_count = max(count)
  )

print(minmaxcontacts,n=32)

participantmaxcount <- participant_extra %>%
  group_by(wave, part_uid) %>%
  summarise(count = n_cnt_all) %>% 
  group_by(wave) %>%
  filter(count == max(count)) %>%
  ungroup()

print(participantmaxcount,n=32)



### Relative percentages of age category participation per wave
contactextrawaves_waveage <- finaldataset %>%
  filter(!is.na(adult_cat)) %>%
  group_by(wave, adult_cat) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(adult_cat)

mean_contacts_per_wave <- finaldataset %>%
  group_by(wave) %>%
  summarise(total_mean_contacts = mean(n_contacts), total_mean_nonhouseh_contacts = mean(num_nonhouseh_cont), .groups = "drop")

contactextrawaves_waveage <- contactextrawaves_waveage %>%
  left_join(mean_contacts_per_wave, by = "wave")

library(ggplot2)
# Create stacked barplot
ggplot(contactextrawaves_waveage, aes(x = wave, y = percentage, fill = adult_cat)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Age category") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_waveage, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = adult_cat, group = adult_cat), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="Elderly"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Age category") +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_waveage, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = adult_cat, group = adult_cat), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Elderly"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Non-household contacts", color = "Age category") +
  theme_minimal()

### Relative percentages of age distribution of new participation per wave 
contactextrawaves_newwaveage <- finaldataset %>%
  filter(!is.na(adult_cat) & wavecount == 1) %>%
  group_by(wave, adult_cat) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(adult_cat)

# Create stacked barplot
ggplot(contactextrawaves_newwaveage, aes(x = wave, y = percentage, fill = adult_cat)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Age category") +
  theme_minimal()

### Relative percentages of 1st/2nd wave
contactextrawaves_12wave <- finaldataset %>%
  filter(!is.na(adult_cat)) %>%
  mutate(numberparticipation = ifelse(wavecount == 1, "1st participation", "2nd+ participation")) %>%
  group_by(wave, numberparticipation) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)%>%
  ungroup() %>%
  mutate(numberparticipation = factor(numberparticipation, levels = c("2nd+ participation","1st participation")))

# Create stacked barplot
ggplot(contactextrawaves_12wave, aes(x = wave, y = percentage, fill = numberparticipation)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Wave of participation") +
  theme_minimal()

### Relative percentages of weekday/weekend participation per wave
contactextrawaves_waveweekday <- finaldataset %>%
  filter(!is.na(wd)) %>%
  group_by(wave, wd) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(wd)

contactextrawaves_waveweekday <- contactextrawaves_waveweekday %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_waveweekday, aes(x = wave, y = percentage, fill = wd)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Weekday/Weekend") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_waveweekday, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = wd, group = wd), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="Weekday"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Weekday/Weekend") +
  theme_minimal()

ggplot(contactextrawaves_waveweekday, aes(x = wave)) +
geom_line(aes(y = mean_contacts, color = wd, group = wd), size = 0.8) +
  geom_line(aes(y = total_mean_contacts, group="Weekday"), size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Weekday/Weekend") + 
  scale_color_manual(values = c("Weekday" = "red", "Weekend" = "blue", "Average" = "black"),
                     breaks = c("Weekday", "Weekend", "Average"),
                     labels = c("Weekday", "Weekend", "Average")) +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_waveweekday, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = wd, group = wd), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Weekday"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Weekday/Weekend") +
  theme_minimal()


### Relative percentages of region participation per wave
contactextrawaves_waveregion <- finaldataset %>%
  filter(!is.na(area_3_name)) %>%
  group_by(wave, area_3_name) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(area_3_name)


contactextrawaves_waveregion <- contactextrawaves_waveregion %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_waveregion, aes(x = wave, y = percentage, fill = area_3_name)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Area") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_waveregion, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = area_3_name, group = area_3_name), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="Vlaams Gewest"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Area") +
  theme_minimal()

ggplot(contactextrawaves_waveregion, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = area_3_name, group = area_3_name), size = 0.8) +
  geom_line(aes(y = total_mean_contacts, group="Vlaams Gewest"), size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Area") + 
  scale_color_manual(values = c("Vlaams Gewest" = "red", "Waals Gewest" = "green", "Brussels Hoofdstede" = "blue", "Average" = "black"),
                     breaks = c("Vlaams Gewest", "Waals Gewest", "Brussels Hoofdstede", "Average"),
                     labels = c("Vlaams Gewest", "Waals Gewest", "Brussels Hoofdstede", "Average")) +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_waveregion, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = area_3_name, group = area_3_name), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Vlaams Gewest"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Area") +
  theme_minimal()


### Relative percentages of holiday participation per wave
contactextrawaves_waveholiday <- finaldataset %>%
  filter(!is.na(holiday)) %>%
  group_by(wave, holiday) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(holiday) 

contactextrawaves_waveholiday <- contactextrawaves_waveholiday %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_waveholiday, aes(x = wave, y = percentage, fill = holiday)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Holiday") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_waveholiday, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = holiday, group = holiday), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="Yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Holiday") +
  theme_minimal()

ggplot(contactextrawaves_waveholiday, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = holiday, group = holiday), size = 0.8) +
  geom_line(aes(y = total_mean_contacts, group="Yes"), size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Holiday") + 
  scale_color_manual(values = c("No" = "red", "Yes" = "blue", "Average" = "black"),
                     breaks = c("No", "Yes", "Average"),
                     labels = c("No", "Yes", "Average")) +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_waveholiday, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = holiday, group = holiday), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Holiday") +
  theme_minimal()

### Relative percentages of household size participation per wave
contactextrawaves_wavehhsize <- finaldataset %>%
  filter(!is.na(hhsize_cat)) %>%
  group_by(wave, hhsize_cat) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)

contactextrawaves_wavehhsize <- contactextrawaves_wavehhsize %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_wavehhsize, aes(x = wave, y = percentage, fill = hhsize_cat)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Household size") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_wavehhsize, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = hhsize_cat, group = hhsize_cat), size = 1) +
  geom_line(aes(y = total_mean_contacts, group="1"), color = "black", size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Household size") +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_wavehhsize, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = hhsize_cat, group = hhsize_cat), size = 1) +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="1"), color = "black", size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average non-household contacts", color = "Household size") +
  theme_minimal()

### Relative percentages of symptomatic status participation per wave
contactextrawaves_wavesymptom <- finaldataset %>%
  group_by(wave, part_symp_none) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(part_symp_none) 

contactextrawaves_wavesymptom <- contactextrawaves_wavesymptom %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_wavesymptom, aes(x = wave, y = percentage, fill = part_symp_none)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Symptomatic status") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_wavesymptom, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_symp_none, group = part_symp_none), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="Yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Symptomatic status") +
  theme_minimal()

ggplot(contactextrawaves_wavesymptom, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_symp_none, group = part_symp_none), size = 0.8) +
  geom_line(aes(y = total_mean_contacts, group="Yes"), size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Symptomatic status") + 
  scale_color_manual(values = c("No" = "red", "Yes" = "blue", "Average" = "black"),
                     breaks = c("No", "Yes", "Average"),
                     labels = c("No", "Yes", "Average")) +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_wavesymptom, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_symp_none, group = part_symp_none), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Symptomatic status") +
  theme_minimal()

### Relative percentages of elevated risk participation per wave
contactextrawaves_waveelevrisk <- finaldataset %>%
  group_by(wave, part_elevated_risk) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)

contactextrawaves_waveelevrisk <- contactextrawaves_waveelevrisk %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_waveelevrisk, aes(x = wave, y = percentage, fill = part_elevated_risk)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Elevated risk") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_waveelevrisk, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_elevated_risk, group = part_elevated_risk), size = 1) +
  geom_line(aes(y = total_mean_contacts, group="yes"), color = "black", size = 1,linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Elevated risk") +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_waveelevrisk, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_elevated_risk, group = part_elevated_risk), size = 1) +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="yes"), color = "black", size = 1,linetype = "dashed") +
  labs(x = "Wave", y = "Average non-household contacts", color = "Elevated risk") +
  theme_minimal()

### Relative percentages of face mask usage participation per wave
contactextrawaves_wavefacemask <- finaldataset %>%
  filter(!is.na(part_face_mask)) %>%
  group_by(wave, part_face_mask) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)

contactextrawaves_wavefacemask <- contactextrawaves_wavefacemask %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_wavefacemask, aes(x = wave, y = percentage, fill = part_face_mask)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Face mask usage") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_wavefacemask, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_face_mask, group = part_face_mask), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Face mask usage") +
  theme_minimal()

ggplot(contactextrawaves_wavefacemask, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_face_mask, group = part_face_mask), size = 0.8) +
  geom_line(aes(y = total_mean_contacts, group="yes"), size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Face mask usage") + 
  scale_color_manual(values = c("no" = "red", "yes" = "blue", "Average" = "black"),
                     breaks = c("no", "yes", "Average"),
                     labels = c("no", "yes", "Average")) +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_wavefacemask, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_face_mask, group = part_face_mask), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Face mask usage") +
  theme_minimal()

### Relative percentages of vaccination status participation per wave
contactextrawaves_wavevacc <- finaldataset %>%
  group_by(wave, part_vacc) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)

contactextrawaves_wavevacc <- contactextrawaves_wavevacc %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_wavevacc, aes(x = wave, y = percentage, fill = part_vacc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Vaccination status") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_wavevacc, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_vacc, group = part_vacc), size = 1) +
  geom_line(aes(y = total_mean_contacts, group="Yes"), color = "black", size = 1,linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Vaccination status") +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_wavevacc, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_vacc, group = part_vacc), size = 1) +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Yes"), color = "black", size = 1,linetype = "dashed") +
  labs(x = "Wave", y = "Average non-household contacts", color = "Vaccination status") +
  theme_minimal()

### Relative percentages of gender participation per wave
contactextrawaves_wavegender <- finaldataset %>%
  group_by(wave, part_gender) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)

contactextrawaves_wavegender <- contactextrawaves_wavegender %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_wavegender, aes(x = wave, y = percentage, fill = part_gender)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Gender") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_wavegender, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_gender, group = part_gender), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="F"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Gender") +
  theme_minimal()

ggplot(contactextrawaves_wavegender, aes(x = wave))+
  geom_line(aes(y = mean_contacts, color = part_gender, group = part_gender), size = 0.8) +
  geom_line(aes(y = total_mean_contacts, group="F"), size = 1, linetype = "dashed") +
  labs(x = "Wave", y = "Average Contacts", color = "Gender") + 
  scale_color_manual(values = c("F" = "red", "M" = "blue","<NA>" = "green", "Average" = "black"),
                     breaks = c("F", "M", "<NA>", "Average"),
                     labels = c("F", "M", "<NA>", "Average")) +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_wavegender, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_gender, group = part_gender), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="F"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Gender") +
  theme_minimal()

### Relative percentages of social group participation per wave
contactextrawaves_wavesocialgroup <- finaldataset %>%
  filter(!is.na(part_social_group_be)) %>%
  group_by(wave, part_social_group_be) %>%
  summarise(count = n(), mean_contacts = sum(n_contacts)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)

contactextrawaves_wavesocialgroup <- contactextrawaves_wavesocialgroup %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_wavesocialgroup, aes(x = wave, y = percentage, fill = part_social_group_be)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Social Group") +
  theme_minimal()

# Total contacts
ggplot(contactextrawaves_wavesocialgroup, aes(x = wave)) +
  geom_line(aes(y = mean_contacts, color = part_social_group_be, group = part_social_group_be), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_contacts, group="Group 1&2"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average Contacts", color = "Social Group") +
  theme_minimal()

# Non-household contacts
ggplot(contactextrawaves_wavesocialgroup, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_social_group_be, group = part_social_group_be), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Group 1&2"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Social Group") +
  theme_minimal()



