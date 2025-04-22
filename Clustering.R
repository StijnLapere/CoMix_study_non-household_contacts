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

# Kiezen van aantal clusters (bijv. 3)
k <- 5
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

fviz_cluster(list(data = clust_scaled, cluster = clusters))

## TABLE 3&4: Ward D with mean contacts per person accounted for number of waves ##
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
hc <- hclust(dist_matrix, method = "ward.D")

# Compute cophentic distance
res.coph <- cophenetic(hc)

# Correlation between cophenetic distance and
# the original distance
cor(dist_matrix, res.coph) #0.2711

res.hc2 <- hclust(dist_matrix, method = "average")
cor(res.dist, cophenetic(res.hc2)) #0.5727

res.hc3 <- hclust(dist_matrix, method = "complete")
cor(res.dist, cophenetic(res.hc3)) #0.4699

res.hc4 <- hclust(dist_matrix, method = "single")
cor(res.dist, cophenetic(res.hc4)) #0.3508

res.hc5 <- hclust(dist_matrix, method = "centroid")
cor(res.dist, cophenetic(res.hc5)) #0.5975

res.hc6 <- hclust(dist_matrix, method = "ward.D2")
cor(res.dist, cophenetic(res.hc6)) #0.2988

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
# 6/7 clusters

### 2 clusters ###
k <- 2
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
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

### 6 clusters (median) ###
k <- 6
clusters <- cutree(hc, k = k)

# Toevoegen aan data
clusteringdataset$cluster <- as.factor(clusters)

contactspercluser <- clusteringdataset %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), median))  # gemiddeld contactpatroon per cluster

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
cluster_medians_long <- contactspercluser %>%
  pivot_longer(cols = -cluster, names_to = "locatie", values_to = "med_aantal")

cluster_medians_long$locatie <- factor(cluster_medians_long$locatie,
                                       levels = c("Home", "Work", "School", "Leisure", "Transport", "Other"))

# 3. Maak de lijnplot
ggplot(cluster_medians_long, aes(x = locatie, y = med_aantal, group = cluster)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ cluster) +
  labs(
    title = "Mediaan aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Mediaan aantal contacten"
  ) +
  theme_minimal()


## TABLE 5: Average linkage with mean contacts per person accounted for number of waves ##
#### VREEMD
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
hc <- hclust(dist_matrix, method = "average")

# Plot dendrogram
plot(hc, main = "Dendrogram contactprofielen", xlab = "", sub = "")

#Elbow plot
fviz_nbclust(clusteringdataset, FUN = hcut, method = "wss")

### 6 clusters ###
k <- 6
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
  facet_wrap(~ cluster) +
  labs(
    title = "Gemiddeld aantal contacten per locatie per cluster",
    x = "Locatie",
    y = "Gemiddeld aantal contacten"
  ) +
  theme_minimal()

################## 2) Demographic ################## 
library(cluster)

clust_data_adult <- nonhouseholdcontacts_noage_adult %>%
  select(area_3_name,part_gender,hhsize_cat,part_social_group_be,employstatus,educationmainearner)

clust_data_adult <- clust_data_adult %>%
  group_by(part_uid) %>%
  slice_head(n = 1) %>%
  ungroup()

clust_data_adult <- na.omit(clust_data_adult)

gower_dist <- daisy(clust_data_adult, metric = "gower")

hc <- hclust(gower_dist, method = "ward.D")

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

ggplot(clust_props, aes(x = variable, y = value, fill = percentage)) +
  geom_tile(color = "white") +
  facet_wrap(~ cluster) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Verdeling van demografische variabelen per cluster",
       x = "Variabele", y = "Categorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
