##########################################################
################ Dataset for clustering ##################
##########################################################
## TABLE 1: Ward D2 with aggregated mean contacts per person ##
clusteringdataset <- nonhouseholdcontacts %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, place) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = place, values_from = n, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

# Kiezen van aantal clusters (bijv. 3)
k <- 6
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

# Stap 1: Zorg dat clusteringinformatie in nonhouseholdcontacts komt
nonhouseholdcontacts_clustered <- nonhouseholdcontacts %>%
  left_join(clusteringdataset %>% select(part_uid, cluster), by = "part_uid")

# Stap 2: Bereken het percentage per adult_cat per cluster
proporties_per_cluster <- nonhouseholdcontacts_clustered %>%
  group_by(cluster, adult_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(procent = n / sum(n) * 100)

# Eventueel: Wide format zodat elke cluster één rij is
proporties_wide <- proporties_per_cluster %>%
  select(cluster, adult_cat, procent) %>%
  tidyr::pivot_wider(names_from = adult_cat, values_from = procent, values_fill = 0)

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

proporties_en_aantallen <- proporties_wide %>%
  left_join(aantal_per_cluster, by = "cluster")

library(factoextra)
fviz_cluster(list(data = clust_scaled, cluster = clusters))


## TABLE 2: Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- nonhouseholdcontacts %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")

# Kiezen van aantal clusters (bijv. 3)
k <- 5
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

# Stap 1: Zorg dat clusteringinformatie in nonhouseholdcontacts komt
nonhouseholdcontacts_clustered <- nonhouseholdcontacts %>%
  left_join(clusteringdataset %>% select(part_uid, cluster), by = "part_uid")

# Stap 2: Bereken het percentage per adult_cat per cluster
proporties_per_cluster <- nonhouseholdcontacts_clustered %>%
  group_by(cluster, adult_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(procent = n / sum(n) * 100)

# Eventueel: Wide format zodat elke cluster één rij is
proporties_wide <- proporties_per_cluster %>%
  select(cluster, adult_cat, procent) %>%
  tidyr::pivot_wider(names_from = adult_cat, values_from = procent, values_fill = 0)

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

proporties_en_aantallen <- proporties_wide %>%
  left_join(aantal_per_cluster, by = "cluster")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))


######## Clustering per age category ########
#### Adults ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- nonhouseholdcontacts_noage_adult %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix^2, method = "ward.D2")

# Compute cophentic distance
res.coph <- cophenetic(hc)

# Correlation between cophenetic distance and
# the original distance
cor(dist_matrix, res.coph) #0.811

res.hc2 <- hclust(dist_matrix, method = "average")
cor(dist_matrix, cophenetic(res.hc2)) #0.952

res.hc3 <- hclust(dist_matrix, method = "complete")
cor(dist_matrix, cophenetic(res.hc3)) #0.8959

res.hc4 <- hclust(dist_matrix, method = "single")
cor(dist_matrix, cophenetic(res.hc4)) #0.799

res.hc5 <- hclust(dist_matrix, method = "centroid")
cor(dist_matrix, cophenetic(res.hc5)) #0.951

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 clusters

### 3 clusters ###
k <- 3
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))

## 2 clusters are very small
## Therefore, we will exclude the participants from these clusters and perform the clustering algorithm again

number_per_cluster <- clusteringdataset %>%
  count(cluster, name = "Count")

valid_clusters <- number_per_cluster %>%
  filter(Count >= 10) %>%
  pull(cluster)

filtered_data <- clusteringdataset %>%
  filter(cluster %in% valid_clusters)

clust_vars <- filtered_data %>%
  select(Home, Work, School, Leisure, Transport, Other)

# Standaardiseren
clust_scaled <- scale(clust_vars)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix^2, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 4 clusters

### 4 clusters ###
k <- 4
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clust_vars$cluster <- as.factor(clusters)

contactspercluser <- clust_vars %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clust_vars %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))

## Still the same problem ...

#### Children ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- nonhouseholdcontacts_noagegender_children %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 4 clusters

k <- 4
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Elderly ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- nonhouseholdcontacts_noage_Elderly %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 clusters

## 3 clusters
k <- 3
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

## 5 clusters
k <- 5
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

################## 2) Demographic ################## 
library(cluster)

### ADULTS ###

clust_data_adult <- nonhouseholdcontacts_noage_adult %>%
  select(area_3_name,part_gender,hhsize_cat,part_social_group_be,employstatus,educationmainearner)

clust_data_adult <- clust_data_adult %>%
  group_by(part_uid) %>%
  slice_head(n = 1) %>%
  ungroup()

clust_data_adult <- na.omit(clust_data_adult)

gower_dist <- daisy(clust_data_adult, metric = "gower")

hc <- hclust(gower_dist, method = "ward.D2")

plot(hc,main = "Dendogram with Gower distance")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = gower_dist)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 clusters

k <- 3
clusters <- cutree(hc,k=k)

clust_data_adult$cluster <- as.factor(clusters)

table(clust_data_adult$cluster)

# Zet alle variabelen als factors
clust_data_long <- clust_data_adult %>%
  pivot_longer(cols = -c(part_uid, cluster), names_to = "variable", values_to = "value")

# Bereken percentage per cluster en categorie
clust_props <- clust_data_long %>%
  group_by(cluster, variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, variable) %>%
  mutate(percentage = n / sum(n) * 100)

clust_props <- clust_props %>%
  mutate(variable = factor(variable, levels = c(
    "educationmainearner", "employstatus", "part_social_group_be", "hhsize_cat",
    "part_gender", "area_3_name"
  )))

ggplot(clust_props, aes(x = variable, y = value, fill = percentage)) +
  geom_tile(color = "white") +
  facet_wrap(~ cluster) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  geom_hline(yintercept = c(3.5, 5.5, 9.5, 13.5, 16.5), color = "black", size = 0.5) +
  labs(title = "Verdeling van demografische variabelen per cluster",
       x = "Variabele", y = "Categorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Gemiddeld aantal non-household contacten per cluster
# 1. Voeg clusters toe aan finaldataset via part_uid
final_with_clusters <- finaldataset_noage_adult %>%
  left_join(clust_data_adult %>% select(part_uid, cluster), by = "part_uid")

# 2. Bereken gemiddeld aantal niet-huishoudelijke contacten per cluster
cluster_means <- final_with_clusters %>%
  group_by(cluster) %>%
  summarise(mean_contacts = mean(num_nonhouseh_cont, na.rm = TRUE), median_contacts = median(num_nonhouseh_cont, na.rm = TRUE))

## Gemiddeld aantal contacten per locatie per cluster
clusteringdataset <- nonhouseholdcontacts_noage_adult %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, place) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = place, values_from = n, values_fill = 0)

final_with_clusters <- clusteringdataset %>%
  left_join(clust_data_adult %>% select(part_uid, cluster), by = "part_uid") %>%
  filter(!is.na(cluster))

contactspercluser <- final_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()


### CHILDREN ###

clust_data_children <- nonhouseholdcontacts_noagegender_children %>%
  select(area_3_name,hhsize_cat,part_social_group_be)

clust_data_children <- clust_data_children %>%
  group_by(part_uid) %>%
  slice_head(n = 1) %>%
  ungroup()

clust_data_children <- na.omit(clust_data_children)

gower_dist <- daisy(clust_data_children, metric = "gower")

hc <- hclust(gower_dist, method = "ward.D2")

plot(hc,main = "Dendogram with Gower distance")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = gower_dist)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 6 clusters

k <- 6
clusters <- cutree(hc,k=k)

clust_data_children$cluster <- as.factor(clusters)

table(clust_data_children$cluster)

# Zet alle variabelen als factors
clust_data_long <- clust_data_children %>%
  pivot_longer(cols = -c(part_uid, cluster), names_to = "variable", values_to = "value")

# Bereken percentage per cluster en categorie
clust_props <- clust_data_long %>%
  group_by(cluster, variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, variable) %>%
  mutate(percentage = n / sum(n) * 100)

clust_props <- clust_props %>%
  mutate(variable = factor(variable, levels = c(
    "part_social_group_be", "hhsize_cat", "area_3_name"
  )))

ggplot(clust_props, aes(x = variable, y = value, fill = percentage)) +
  geom_tile(color = "white") +
  facet_wrap(~ cluster, nrow = 1) + 
  scale_fill_gradient(low = "white", high = "darkblue") +
  geom_hline(yintercept = c(3.5, 6.5), color = "black", size = 0.5) +
  labs(title = "Verdeling van demografische variabelen per cluster",
       x = "Variabele", y = "Categorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Gemiddeld aantal non-household contacten per cluster
# 1. Voeg clusters toe aan finaldataset via part_uid
final_with_clusters <- finaldataset_noagegender_children %>%
  left_join(clust_data_children %>% select(part_uid, cluster), by = "part_uid")

# 2. Bereken gemiddeld aantal niet-huishoudelijke contacten per cluster
cluster_means <- final_with_clusters %>%
  group_by(cluster) %>%
  summarise(mean_contacts = mean(num_nonhouseh_cont, na.rm = TRUE), median_contacts = median(num_nonhouseh_cont, na.rm = TRUE))

## Gemiddeld aantal contacten per locatie per cluster
clusteringdataset <- nonhouseholdcontacts_noagegender_children %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, place) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = place, values_from = n, values_fill = 0)

final_with_clusters <- clusteringdataset %>%
  left_join(clust_data_children %>% select(part_uid, cluster), by = "part_uid") %>%
  filter(!is.na(cluster))

contactspercluser <- final_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()


### ELDERLY ###

clust_data_elderly <- nonhouseholdcontacts_noage_Elderly %>%
  select(area_3_name,hhsize_cat,part_social_group_be,part_gender)

clust_data_elderly <- clust_data_elderly %>%
  group_by(part_uid) %>%
  slice_head(n = 1) %>%
  ungroup()

clust_data_elderly <- na.omit(clust_data_elderly)

gower_dist <- daisy(clust_data_elderly, metric = "gower")

hc <- hclust(gower_dist, method = "ward.D2")

plot(hc,main = "Dendogram with Gower distance")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = gower_dist)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 5 clusters

# 5 clusters
k <- 5
clusters <- cutree(hc,k=k)

clust_data_elderly$cluster <- as.factor(clusters)

table(clust_data_elderly$cluster)

# Zet alle variabelen als factors
clust_data_long <- clust_data_elderly %>%
  pivot_longer(cols = -c(part_uid, cluster), names_to = "variable", values_to = "value")

# Bereken percentage per cluster en categorie
clust_props <- clust_data_long %>%
  group_by(cluster, variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, variable) %>%
  mutate(percentage = n / sum(n) * 100)

clust_props <- clust_props %>%
  mutate(variable = factor(variable, levels = c(
    "part_gender", "part_social_group_be", "hhsize_cat", "area_3_name"
  )))

ggplot(clust_props, aes(x = variable, y = value, fill = percentage)) +
  geom_tile(color = "white") +
  facet_wrap(~ cluster, nrow = 1) + 
  scale_fill_gradient(low = "white", high = "darkblue") +
  geom_hline(yintercept = c(3.5, 7.5, 11.5), color = "black", size = 0.5) +
  labs(title = "Verdeling van demografische variabelen per cluster",
       x = "Variabele", y = "Categorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Gemiddeld aantal non-household contacten per cluster
# 1. Voeg clusters toe aan finaldataset via part_uid
final_with_clusters <- finaldataset_noage_Elderly %>%
  left_join(clust_data_elderly %>% select(part_uid, cluster), by = "part_uid")

# 2. Bereken gemiddeld aantal niet-huishoudelijke contacten per cluster
cluster_means <- final_with_clusters %>%
  group_by(cluster) %>%
  summarise(mean_contacts = mean(num_nonhouseh_cont, na.rm = TRUE), median_contacts = median(num_nonhouseh_cont, na.rm = TRUE))

## Gemiddeld aantal contacten per locatie per cluster
clusteringdataset <- nonhouseholdcontacts_noage_Elderly %>%
  filter(!is.na(place)) %>%
  group_by(part_uid, place) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = place, values_from = n, values_fill = 0)

final_with_clusters <- clusteringdataset %>%
  left_join(clust_data_elderly %>% select(part_uid, cluster), by = "part_uid") %>%
  filter(!is.na(cluster))

contactspercluser <- final_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### SES ####
SES_dataset <- nonhouseholdcontacts %>% 
  dplyr::select(part_uid,wave,part_occupation,part_income,place)

ggplot(SES_dataset, aes(x = part_income)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()

SES_dataset$income_cat <- with(SES_dataset, case_when(
  part_income %in% c(" 0 -  549", " 550 -  999", " 1 000 -  1 299") ~ "Very low",
  part_income %in% c(" 1 300 -  1 499", " 1 500 -  1 699", " 1 700 -  1 899") ~ "Low",
  part_income %in% c(" 1 900 -  2 199", " 2 200 -  2 499", " 2 500 -  2 799", " 2 800 -  3 199") ~ "Middle",
  part_income %in% c(" 3 200 -  3 699", " 3 700 -  4 499") ~ "High",
  part_income %in% c(" 4 500 -  5 499", " 5 500 -  7 999", " 8 000 or more") ~ "Very high"
))
SES_dataset$income_cat <- factor(SES_dataset$income_cat, levels = c("Very low", "Low", "Middle", "High", "Very high"))
table(SES_dataset$income_cat,useNA="ifany")

table(SES_dataset$part_occupation,useNA = 'ifany')
SES_dataset$occupation_cat <- with(SES_dataset, case_when(
  part_occupation %in% c("liberal profession or profession for which qualification is required", "member of the general management senior executive responsible for 11 employees or more",
                         "member of the general management senior executive responsible for 5 employees or less", "member of the general management senior executive responsible for 6 to 10 employees",
                         "middle management that is not part of the general management responsible for 5 employees or less", "middle management that is not part of the general management responsible for 6 employees or more") ~ "Managers & Professionals",
  part_occupation %in% c("other employee who mainly performs office work") ~ "Office employees",
  part_occupation %in% c("other employee who does not do office work (eg teacher nurses ...)") ~ "Service employees",
  part_occupation %in% c("non-skilled worker", "skilled worker") ~ "Manual workers",
  part_occupation %in% c("craftsman trader with 5 employees or less", "farmer", "industrial wholesaler with 6 employees or more") ~ "Self-employed/Small business",
  part_occupation %in% c("house man or housewife", "never worked", "pre-retired", "retired", "student", "unable for work", "unemployed") ~ "Not in labour force"
))
SES_dataset$occupation_cat <- factor(SES_dataset$occupation_cat, levels = c("Managers & Professionals", "Office employees", "Service employees", "Manual workers", "Self-employed/Small business", "Not in labour force"))
table(SES_dataset$occupation_cat,useNA="ifany")

######## Clustering per income category ########
#### Very low ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), income_cat=="Very low") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 5 clusters

### 5 clusters ###
k <- 5
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Low ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), income_cat=="Low") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 4 clusters

### 4 clusters ###
k <- 4
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Middle ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), income_cat=="Middle") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 2 or 5 clusters

### 2 clusters ###
k <- 2
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

### 5 clusters ###
k <- 5
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### High ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), income_cat=="High") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 4 clusters

### 4 clusters ###
k <- 4
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Very high ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), income_cat=="Very high") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 5 clusters

### 5 clusters ###
k <- 5
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

######## Clustering per occupation category ########
#### Managers & Professionals ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), occupation_cat=="Managers & Professionals") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 clusters

### 3 clusters ###
k <- 3
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Office employees ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), occupation_cat=="Office employees") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 4 clusters

### 4 clusters ###
k <- 4
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Service employees ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), occupation_cat=="Service employees") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 5 clusters

### 5 clusters ###
k <- 5
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Manual workers ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), occupation_cat=="Manual workers") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 or 5 clusters

### 3 clusters ###
k <- 3
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

### 5 clusters ###
k <- 5
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Self-employed/Small business ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), occupation_cat=="Self-employed/Small business") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 or 6 clusters

### 3 clusters ###
k <- 3
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

### 6 clusters ###
k <- 6
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

#### Not in labour force ####
## Ward D2 with mean contacts per person accounted for number of waves ##
clusteringdataset <- SES_dataset %>%
  filter(!is.na(place), occupation_cat=="Not in labour force") %>%
  group_by(part_uid, wave, place) %>%
  summarise(n_contacts = n(), .groups = "drop") %>%  # aantal contacten per persoon per wave per locatie
  group_by(part_uid, place) %>%
  summarise(mean_contacts = mean(n_contacts), .groups = "drop") %>%  # gemiddeld per locatie over alle waves
  pivot_wider(names_from = place, values_from = mean_contacts, values_fill = 0)

# Verwijder part_id voor clustering
clust_input <- clusteringdataset %>% select(-part_uid)

# Standaardiseren
clust_scaled <- scale(clust_input)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 clusters

### 3 clusters ###
k <- 3
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clusteringdataset %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

valid_clusters <- aantal_per_cluster %>%
  filter(aantal_personen >= 10) %>%
  pull(cluster)

filtered_data <- clusteringdataset %>%
  filter(cluster %in% valid_clusters)

clust_vars <- filtered_data %>%
  select(Home, Work, School, Leisure, Transport, Other)

# Standaardiseren
clust_scaled <- scale(clust_vars)

# Afstandsmatrix en clustering
dist_matrix <- dist(clust_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

sil_scores <- c()

# Test voor aantal clusters van 2 tot 10
for (k in 2:10) {
  clusters_k <- cutree(hc, k = k)
  sil <- silhouette(clusters_k, dist = dist_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19,
     xlab = "Aantal clusters", ylab = "Gemiddelde silhouette width",
     main = "Silhouette-analyse")
# 3 clusters

### 3 clusters ###
k <- 3
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clust_vars$cluster <- as.factor(clusters)

contactspercluser <- clust_vars %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))  # gemiddeld contactpatroon per cluster

aantal_per_cluster <- clust_vars %>%
  count(cluster, name = "aantal_personen")

fviz_cluster(list(data = clust_scaled, cluster = clusters))

# Zet wide naar long: locatie als variabele
cluster_means_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "gem_aantal")

cluster_means_long$locatie <- factor(cluster_means_long$locatie,
                                     levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_means_long, aes(x = locatie, y = gem_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()



