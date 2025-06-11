### Number of participants
length(unique(finaldataset$part_uid)) #4208

### Total number of responses
nrow(finaldataset) #39028

### Total number of reported contacts
sum(finaldataset$n_cnt_all) #166208

### Total number of reported non-household contacts
sum(finaldataset$num_nonhouseh_cont) #118506

library(dplyr)
# Minimal and maximal number of contacts per wave
minmaxcontacts <- finaldataset %>%
  group_by(wave, part_uid) %>%
  summarise(count = n_cnt_all) %>% 
  group_by(wave) %>%
  summarise(
    min_count = min(count),
    max_count = max(count)
  )

print(minmaxcontacts,n=32)

# Participant with maximal number of contacts per wave
participantmaxcount <- finaldataset %>%
  group_by(wave, part_uid) %>%
  summarise(count = n_cnt_all) %>% 
  group_by(wave) %>%
  filter(count == max(count)) %>%
  ungroup()

print(participantmaxcount,n=32)

# Minimal and maximal number of non-household contacts per wave
minmaxcontacts <- finaldataset %>%
  group_by(wave, part_uid) %>%
  summarise(count = num_nonhouseh_cont) %>% 
  group_by(wave) %>%
  summarise(
    min_count = min(count),
    max_count = max(count)
  )

print(minmaxcontacts,n=32)

# Participant with maximal number of non-household contacts per wave
participantmaxcount <- finaldataset %>%
  group_by(wave, part_uid) %>%
  summarise(count = num_nonhouseh_cont) %>% 
  group_by(wave) %>%
  filter(count == max(count)) %>%
  ungroup()

print(participantmaxcount,n=32)

sum(finaldataset$n_cnt_all<100)/nrow(finaldataset)

### Relative percentages of age category participation per wave
contactextrawaves_waveage <- finaldataset %>%
  filter(!is.na(adult_cat)) %>%
  group_by(wave, adult_cat) %>%
  summarise(
    count = n(),
    mean_contacts = mean(n_cnt_all),
    mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    sd_nonhouseh_contacts = sd(num_nonhouseh_cont),
    se_nonhouseh_contacts = sd_nonhouseh_contacts / sqrt(count),
    .groups = "drop"
  ) %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

finaldataset %>%
  filter(!is.na(adult_cat)) %>%
  group_by(adult_cat) %>%
  summarise(
    mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    se_nonhouseh_contacts = sd(num_nonhouseh_cont) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ungroup()

mean_contacts_per_wave <- finaldataset %>%
  group_by(wave) %>%
  summarise(
    total_mean_contacts = mean(n_cnt_all),
    total_mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    sd_total_nonhouseh_contacts = sd(num_nonhouseh_cont),
    n_total = n(),
    se_total_nonhouseh_contacts = sd_total_nonhouseh_contacts / sqrt(n_total),
    .groups = "drop"
  )

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

contactextrawaves_waveage$wave_num <- as.numeric(contactextrawaves_waveage$wave)

library(readxl)
## Add stringency values to Figure 2
stringencyvalues <- read_excel("C:/Users/stijn/Documents/Stringency.xlsx", col_names = TRUE)
stringencyvalues <- as.numeric(stringencyvalues[1,])
stringencydf <- data.frame(wave_num = seq(min(contactextrawaves_waveage$wave_num), max(contactextrawaves_waveage$wave_num), length.out = length(stringencyvalues)), y_extra = stringencyvalues)

# Add rescaled y values to dataframe
stringencydf$y_rescaled <- 12.5/100 * stringencydf$y_extra

# Non-household contacts
ggplot(contactextrawaves_waveage, aes(x = wave_num)) +  
  annotate("rect", xmin = 1, xmax = 2, ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.5) +
  annotate("rect", xmin = 8, xmax = 9, ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.5) +
  annotate("rect", xmin = 15, xmax = 18, ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.5) +
  annotate("rect", xmin = 27, xmax = 28, ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.5) +
  geom_ribbon(aes(ymin = mean_nonhouseh_contacts - 1.96*se_nonhouseh_contacts,
                  ymax = mean_nonhouseh_contacts + 1.96*se_nonhouseh_contacts,
                  fill = adult_cat, group = adult_cat), alpha = 0.2) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = adult_cat, group = adult_cat), size = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = total_mean_nonhouseh_contacts - 1.96 * se_total_nonhouseh_contacts,
                  ymax = total_mean_nonhouseh_contacts + 1.96 * se_total_nonhouseh_contacts),
              fill = "black", group = 1, alpha = 0.2) +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group = 1),
            color = "black", size = 1) +
  geom_line(data = stringencydf, aes(x = wave_num, y = y_rescaled),
            color = "orange", size = 1) + 
  scale_y_continuous(
    name = "Average number of non-household contacts",
    sec.axis = sec_axis(trans = ~ (.)*100 /12.5 , name = "Stringency Index", breaks = seq(0, 100, 20))
  ) +
  scale_x_continuous(breaks = contactextrawaves_waveage$wave_num,
                     labels = contactextrawaves_waveage$wave) +
  labs(x = "Wave", y = "Average number of non-household contacts", color = "Age category", fill = "Age category") +
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank())

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

contactextrawaves_12345678wave <- finaldataset %>%
  filter(!is.na(adult_cat)) %>%
  group_by(wave, wavecount) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100)%>%
  ungroup()

contactextrawaves_12345678wave$wavecount <- factor(contactextrawaves_12345678wave$wavecount, levels = c("1", "2", "3", "4", "5", "6", "7", ">=8"))

# Create stacked barplot
ggplot(contactextrawaves_12345678wave, aes(x = wave, y = percentage, fill = wavecount)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Wave of participation") +
  theme_minimal()

### Relative percentages of weekday/weekend participation per wave
contactextrawaves_waveweekday <- finaldataset %>%
  filter(!is.na(wd)) %>%
  group_by(wave, wd) %>%
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  scale_color_manual(values = c("Vlaams Gewest" = "green", "Waals Gewest" = "blue", "Brussels Hoofdstede" = "red", "Average" = "black"),
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
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(holiday) 

contactextrawaves_waveholiday <- contactextrawaves_waveholiday %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Create stacked barplot
ggplot(contactextrawaves_waveholiday, aes(x = wave, y = percentage, fill = factor(holiday))) +
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
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  filter(!is.na(part_symp_none)) %>%
  group_by(wave, part_symp_none) %>%
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  filter(!is.na(part_elevated_risk)) %>%
  group_by(wave, part_elevated_risk) %>%
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  filter(!is.na(part_vacc)) %>%
  group_by(wave, part_vacc) %>%
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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

## Compute values for Table 2
length(unique(finaldataset$part_uid[finaldataset$adult_cat=="Children"]))
length(unique(finaldataset$part_uid[finaldataset$adult_cat=="Adult"]))
length(unique(finaldataset$part_uid[finaldataset$adult_cat=="Elderly"]))

cntdata_summary <- finaldataset %>%
  group_by(adult_cat) %>%
  summarise(
    total_adult = sum(cnt_adult, na.rm = TRUE),
    total_elderly = sum(cnt_elderly, na.rm = TRUE),
    total_children = sum(cnt_children, na.rm = TRUE),
    total_NA = sum(cnt_NA, na.rm = TRUE)
  )

table(finaldataset$adult_cat)
table(finaldataset$adult_cat)/39028

sum(finaldataset$num_nonhouseh_cont[finaldataset$adult_cat=="Children"])
sum(finaldataset$num_nonhouseh_cont[finaldataset$adult_cat=="Adult"])
sum(finaldataset$num_nonhouseh_cont[finaldataset$adult_cat=="Elderly"])

table(finaldataset$wd,finaldataset$adult_cat)
table(finaldataset$wd,finaldataset$adult_cat)/39028
table(finaldataset$wd)
table(finaldataset$wd)/39028

table(finaldataset$area_3_name,finaldataset$adult_cat)
table(finaldataset$area_3_name,finaldataset$adult_cat)/39028
table(finaldataset$area_3_name)
table(finaldataset$area_3_name)/39028

table(finaldataset$holiday,finaldataset$adult_cat)
table(finaldataset$holiday,finaldataset$adult_cat)/39028
table(finaldataset$holiday)
table(finaldataset$holiday)/39028

table(finaldataset$hhsize_cat,finaldataset$adult_cat)
table(finaldataset$hhsize_cat,finaldataset$adult_cat)/39028
table(finaldataset$hhsize_cat)
table(finaldataset$hhsize_cat)/39028
finaldataset %>%
  group_by(hhsize_cat) %>%
  summarise(mean_value = mean(num_nonhouseh_cont, na.rm = TRUE))

table(finaldataset$part_elevated_risk,finaldataset$adult_cat)
table(finaldataset$part_elevated_risk,finaldataset$adult_cat)/39028
table(finaldataset$part_elevated_risk)
table(finaldataset$part_elevated_risk)/39028

table(finaldataset$part_face_mask,finaldataset$adult_cat)
table(finaldataset$part_face_mask,finaldataset$adult_cat)/39028
table(finaldataset$part_face_mask)
table(finaldataset$part_face_mask)/39028

table(finaldataset$part_symp_none,finaldataset$adult_cat)
table(finaldataset$part_symp_none,finaldataset$adult_cat)/39028
table(finaldataset$part_symp_none)
table(finaldataset$part_symp_none)/39028

table(finaldataset$part_vacc,finaldataset$adult_cat)
table(finaldataset$part_vacc,finaldataset$adult_cat)/39028
table(finaldataset$part_vacc)
table(finaldataset$part_vacc)/39028

table(finaldataset$part_gender,finaldataset$adult_cat,useNA = "ifany")
table(finaldataset$part_gender,finaldataset$adult_cat,useNA = "ifany")/39028
table(finaldataset$part_gender,useNA = "ifany")
table(finaldataset$part_gender,useNA = "ifany")/39028

table(finaldataset$part_social_group_be,finaldataset$adult_cat)
table(finaldataset$part_social_group_be,finaldataset$adult_cat)/39028
table(finaldataset$part_social_group_be)
table(finaldataset$part_social_group_be)/39028

table(finaldataset$wavecount,finaldataset$adult_cat)
table(finaldataset$wavecount,finaldataset$adult_cat)/39028
table(finaldataset$wavecount)
table(finaldataset$wavecount)/39028

## Construct Figure 1
finaldataset %>%
  group_by(wavecount) %>%
  summarise(
    mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    se_nonhouseh_contacts = sd(num_nonhouseh_cont) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ungroup()

hist(finaldataset$num_nonhouseh_cont[finaldataset$num_nonhouseh_cont <= 10],
     breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5),
     main="",
     xlab = "Number of non-household contacts",
     ylab = "Frequency")

table(finaldataset$num_nonhouseh_cont)
sum(finaldataset$num_nonhouseh_cont<100)/nrow(finaldataset)


## Explore profiles of participants
## Are there a lot of participants with only 0 nonhousehold contacts
## Or are there also participants with a majority of 0 nonhousehold contacts and sometimes 1

library(dplyr)

# Samenvatten hoeveel keer elke participant elk uniek aantal contacten rapporteert
summary_table <- finaldataset %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    finaldataset %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

# Bekijk enkele rijen
head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
    total_reports = num_waves_participated,
    zero_reports = `0`,
    prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

library(ggplot2)

## Construct Figure 3
ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(x = "Proportion of waves with zero non-household contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_density(fill = "lightblue") +
  labs(
    title = "Density plot of proportion of zero nonhousehold contacts",
    x = "Proportion of zero nonhousehold contacts",
    y = "Density"
  ) +
  theme_minimal()

# How many nonzero nonhousehold contacts do participants with >80% have?
high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.8)

nonzero_contacts <- finaldataset %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

ggplot(nonzero_contacts, aes(x = num_nonhouseh_cont)) +
  geom_histogram(binwidth = 1, fill = "darkorange", color = "black") +
  labs(
    title = "Number of nonhousehold contacts for participants with >+80% zero nonhousehold contacts",
    x = "Number of nonhousehold contacts (if > 0)",
    y = "Frequency"
  ) +
  theme_minimal()

# How many nonzero nonhousehold contacts do participants with >90% have?
high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)

nonzero_contacts <- finaldataset %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# How many nonzero nonhousehold contacts do participants with <10% have?
high_zero_participants <- summary_table %>%
  filter(prop_zero <= 0.1)

nonzero_contacts <- finaldataset %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

### Subgroup-analysis 1: by employment status
table(finaldataset$employstatus,finaldataset$adult_cat)
dataemployed <- finaldataset %>% filter(employstatus == "Employed")
datanotemployed <- finaldataset %>% filter(employstatus == "Not in labor force")
datastudent <- finaldataset %>% filter(employstatus == "Student")

# Employed
summary_table <- dataemployed %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataemployed %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per employed participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataemployed %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Not in labor force
summary_table <- datanotemployed %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datanotemployed %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per unemployed participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datanotemployed %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)


# Student
summary_table <- datastudent %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datastudent %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per student participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datastudent %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)


### Subgroup-analysis 2: by age category
datachildren <- finaldataset %>% filter(adult_cat == "Children")
dataadults <- finaldataset %>% filter(adult_cat == "Adult")
dataelderly <- finaldataset %>% filter(adult_cat == "Elderly")

# Children
summary_table <- datachildren %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datachildren %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per child",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datachildren %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)


# Adults
summary_table <- dataadults %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataadults %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per adult",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataadults %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)


# Elderly
summary_table <- dataelderly %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataelderly %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per elderly",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataelderly %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)


### Subgroup-analysis 3: by area of residence
dataFlanders <- finaldataset %>% filter(area_3_name == "Vlaams Gewest")
dataWallonia <- finaldataset %>% filter(area_3_name == "Waals Gewest")
dataBrussels <- finaldataset %>% filter(area_3_name == "Brussels Hoofdstede")

# Vlaams Gewest
summary_table <- dataFlanders %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataFlanders %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per Flemish participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataFlanders %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Waals Gewest
summary_table <- dataWallonia %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataWallonia %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per Walloon participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataWallonia %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Brussels Hoofdstedelijk Gewest
summary_table <- dataBrussels %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataBrussels %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per Brussels participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataBrussels %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

### Subgroup-analysis 4: by social group
dataSoc12 <- finaldataset %>% filter(part_social_group_be == "Group 1&2")
dataSoc34 <- finaldataset %>% filter(part_social_group_be == "Group 3&4")
dataSoc56 <- finaldataset %>% filter(part_social_group_be == "Group 5&6")
dataSoc78 <- finaldataset %>% filter(part_social_group_be == "Group 7&8")

# Social group 1&2
summary_table <- dataSoc12 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataSoc12 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant from social group 1&2",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataSoc12 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Social group 3&4
summary_table <- dataSoc34 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataSoc34 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant from social group 3&4",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataSoc34 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Social group 5&6
summary_table <- dataSoc56 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataSoc56 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant from social group 5&6",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataSoc56 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Social group 7&8
summary_table <- dataSoc78 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataSoc78 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant from social group 7&8",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataSoc78 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

### Subgroup-analysis 5: by gender
dataMale <- finaldataset %>% filter(part_gender == "M")
dataFemale <- finaldataset %>% filter(part_gender == "F")

# Male
summary_table <- dataMale %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataMale %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per male participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataMale %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Female
summary_table <- dataFemale %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataFemale %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per female participant",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataFemale %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

### Subgroup-analysis 6: by household size
datahhsize1 <- finaldataset %>% filter(hhsize_cat == "1")
datahhsize2 <- finaldataset %>% filter(hhsize_cat == "2")
datahhsize3 <- finaldataset %>% filter(hhsize_cat == "3")
datahhsize4 <- finaldataset %>% filter(hhsize_cat == "4+")

# Household size 1
summary_table <- datahhsize1 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datahhsize1 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant in hhsize 1",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datahhsize1 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Household size 2
summary_table <- datahhsize2 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datahhsize2 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant in hhsize 2",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datahhsize2 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Household size 3
summary_table <- datahhsize3 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datahhsize3 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant in hhsize 3",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datahhsize3 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Household size 4+
summary_table <- datahhsize4 %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datahhsize4 %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant in hhsize 4+",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datahhsize4 %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

### Subgroup-analysis 7: by education main earner
dataloweducation <- finaldataset %>% filter(educationmainearner == "Low")
datamediumeducation <- finaldataset %>% filter(educationmainearner == "Medium")
datahigheducation <- finaldataset %>% filter(educationmainearner == "High")

# Low education
summary_table <- dataloweducation %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    dataloweducation %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant with low education",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- dataloweducation %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# Medium education
summary_table <- datamediumeducation %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datamediumeducation %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant with medium education",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datamediumeducation %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)

# High education
summary_table <- datahigheducation %>%
  group_by(part_uid, num_nonhouseh_cont) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = num_nonhouseh_cont, values_from = n, values_fill = 0) 

summary_table <- summary_table %>%
  left_join(
    datahigheducation %>% 
      dplyr::select(part_uid, num_waves_participated) %>% 
      distinct(),
    by = "part_uid"
  )

head(summary_table)

summary_table <- summary_table %>%
  rowwise() %>%
  summarise(part_uid = part_uid, 
            total_reports = num_waves_participated,
            zero_reports = `0`,
            prop_zero = zero_reports / total_reports
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = prop_zero)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of proportion of zero nonhousehold contacts per participant with high education",
    x = "Proportion of waves with 0 nonhousehold contacts",
    y = "Number of participants"
  ) +
  theme_minimal()

high_zero_participants <- summary_table %>%
  filter(prop_zero >= 0.9)
nrow(high_zero_participants)/length(summary_table$prop_zero)

nonzero_contacts <- datahigheducation %>%
  filter(
    part_uid %in% high_zero_participants$part_uid,
    num_nonhouseh_cont > 0
  )

table(nonzero_contacts$num_nonhouseh_cont)
mean(nonzero_contacts$num_nonhouseh_cont)
median(nonzero_contacts$num_nonhouseh_cont)


## Compute % of nonhousehold contacts per wave
zero_contact_per_wave <- finaldataset %>%
  group_by(wave) %>%
  summarise(
    n_wave = n(),
    n_zero = sum(num_nonhouseh_cont == 0),
    perc_zero = n_zero / n_wave,
    perc_nonzero = 1 - perc_zero
  )

contact_perc_wave <- finaldataset %>%
  group_by(wave) %>%
  summarise(
    n_wave = n(),
    n_zero = sum(num_nonhouseh_cont == 0),
    n_nonzero = sum(num_nonhouseh_cont > 0),
    perc_zero = n_zero / n_wave,
    perc_nonzero = n_nonzero / n_wave
  ) %>%
  dplyr::select(wave, perc_zero, perc_nonzero) %>%
  pivot_longer(
    cols = starts_with("perc_"),
    names_to = "contact_type",
    values_to = "percentage"
  ) %>%
  mutate(
    contact_type = recode(contact_type,
                          "perc_zero" = "0 contacts",
                          "perc_nonzero" = ">0 contacts")
  )

# Create stacked barplot
ggplot(contact_perc_wave, aes(x = wave, y = percentage, fill = contact_type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Wave", y = "% of participants", fill = "Number of contacts") +
  theme_minimal()



#### SES ####
SES_dataset %>%
  filter(!is.na(income_cat)) %>%
  group_by(income_cat) %>%
  summarise(
    mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    se_nonhouseh_contacts = sd(num_nonhouseh_cont) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ungroup()

contactextrawaves_waveincome <- SES_dataset %>%
  filter(!is.na(income_cat)) %>%
  group_by(wave, income_cat) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(income_cat)

contactextrawaves_waveincome <- contactextrawaves_waveincome %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Non-household contacts
ggplot(contactextrawaves_waveincome, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = income_cat, group = income_cat), size = 1) +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Medium"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Income") +
  theme_minimal()

SES_dataset %>%
  filter(!is.na(occupation_cat)) %>%
  group_by(occupation_cat) %>%
  summarise(
    mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    se_nonhouseh_contacts = sd(num_nonhouseh_cont) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ungroup()

contactextrawaves_waveoccupation <- SES_dataset %>%
  filter(!is.na(occupation_cat)) %>%
  group_by(wave, occupation_cat) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(occupation_cat)

contactextrawaves_waveoccupation <- contactextrawaves_waveoccupation %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Non-household contacts
ggplot(contactextrawaves_waveoccupation, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = occupation_cat, group = occupation_cat), size = 1) +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Unemployed"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Occupation") +
  theme_minimal()

finaldataset %>%
  filter(!is.na(employstatus)) %>%
  group_by(employstatus) %>%
  summarise(
    mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    se_nonhouseh_contacts = sd(num_nonhouseh_cont) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ungroup()

finaldataset %>%
  filter(!is.na(educationmainearner)) %>%
  group_by(educationmainearner) %>%
  summarise(
    mean_nonhouseh_contacts = mean(num_nonhouseh_cont),
    se_nonhouseh_contacts = sd(num_nonhouseh_cont) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ungroup()


#### Symptomatic status ####
finaldataset %>%
  filter(!is.na(part_symp_none)) %>%
  group_by(part_symp_none) %>%
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop")

contactextrawaves_wavesymptom <- finaldataset %>%
  filter(!is.na(part_symp_none)) %>%
  group_by(wave, part_symp_none) %>%
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(part_symp_none) 

contactextrawaves_wavesymptom <- contactextrawaves_wavesymptom %>%
  left_join(mean_contacts_per_wave, by = "wave")

# Non-household contacts
ggplot(contactextrawaves_wavesymptom, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_symp_none, group = part_symp_none), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Symptomatic status") +
  theme_minimal()

#### Symptomatic status without children
finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

contactextrawaves_wavesymptom <- finaldataset %>%
  filter(adult_cat != "Children") %>%
  filter(!is.na(part_symp_none)) %>%
  group_by(wave, part_symp_none) %>%
  summarise(count = n(), mean_contacts = sum(n_cnt_all)/count, mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, .groups = "drop") %>%
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

# Non-household contacts
ggplot(contactextrawaves_wavesymptom, aes(x = wave)) +
  geom_line(aes(y = mean_nonhouseh_contacts, color = part_symp_none, group = part_symp_none), size = 1, linetype = "dashed") +
  geom_line(aes(y = total_mean_nonhouseh_contacts, group="Yes"), color = "black", size = 1) +
  labs(x = "Wave", y = "Average non-household contacts", color = "Symptomatic status") +
  theme_minimal()

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, wd) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, area_3_name) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, holiday) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, hhsize_cat) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, part_elevated_risk) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, part_face_mask) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, part_vacc) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, part_gender) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, part_social_group_be) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, employstatus) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")

finaldataset %>%
  filter(adult_cat != "Children", !is.na(part_symp_none)) %>%
  group_by(part_symp_none, educationmainearner) %>%
  summarise(count = n(), mean_nonhouseh_contacts = sum(num_nonhouseh_cont)/count, se = sd(num_nonhouseh_cont)/count, .groups = "drop")


