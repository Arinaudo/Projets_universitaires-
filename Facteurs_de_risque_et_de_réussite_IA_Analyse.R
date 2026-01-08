## Analyse supplémentaire des facteurs de risques & facteurs de réussite
## Edde Jansen
## Le 8 janvier 2026

### Loading Packages
library(tidyverse)
library(text2vec)
library(proxy)
library(pheatmap)
library(igraph)
library(ggraph)
library(tidygraph)
library(widyr)
library(tidytext)


# 1. LOAD YOUR REAL DATA ----
data <- read.csv("~/Dauphine-PSL/ISE/actorsAIIrencomplet.csv", sep = ",")

# Clean and prepare data
risk_factors <- data %>%
  select(
    company = `Nom.de.l.entreprise.étudiée`,
    text = `Facteurs.de.risque`
  ) %>%
  filter(!is.na(text), !is.na(company)) %>%
  mutate(
    text = str_trim(text),
    company = str_trim(company)
  ) %>%
  distinct(company, .keep_all = TRUE)

success_factors <- data %>%
  select(
    company = `Nom.de.l.entreprise.étudiée`,
    text = `Facteurs.de.réussite`
  ) %>%
  filter(!is.na(text), !is.na(company)) %>%
  mutate(
    text = str_trim(text),
    company = str_trim(company)
  ) %>%
  distinct(company, .keep_all = TRUE) 

# Stopwords français étendus
stopwords_fr <- c(
  stopwords::stopwords("fr"),  # Base française
  "peut", "peuvent", "doit", "doivent", "être", "avoir", 
  "faire", "fait", "plus", "très", "aussi", "cette", "ces",
  "risque", "risques", "ia", "ai", "entreprise", "entreprises"
)

### IMPORTANT


##### POUR ANALYSER CI-DESSOUS, SOIT UTILISER RISK_FACTORS, SOIT SUCCESS_FACTORS


### IMPORTANT


# Tokenisation PROPRE
word_tokens <- success_factors %>% 
  unnest_tokens(word, text) %>%
  filter(
    !word %in% stopwords_fr,           # Enlever stopwords
    str_length(word) > 3,               # Mots > 3 lettres
    !str_detect(word, "^\\d+$")         # Pas de chiffres seuls
  ) %>%
  count(company, word, sort = TRUE)

# Vérifier
word_tokens %>% 
  filter(company == "Aleph Alpha") %>%
  head(15)

# 2. STOPWORDS FRANÇAIS ----
stopwords_fr <- c(
  stopwords::stopwords("fr"),
  "peut", "peuvent", "doit", "doivent", "être", "avoir",
  "risque", "risques", "ia", "ai", "entreprise", "entreprises"
)

# 3. PREPROCESSING AVEC text2vec ----
prep_fun <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\d+", " ") %>%
    str_squish()
}

tok_fun <- function(x) {
  tokens <- word_tokenizer(x)
  lapply(tokens, function(t) {
    t[!t %in% stopwords_fr & nchar(t) > 3]
  })
}

# 4. CRÉER TF-IDF ----
tokens <- itoken(success_factors$text,
                 preprocessor = prep_fun,
                 tokenizer = tok_fun,
                 ids = success_factors$company)

vocab <- create_vocabulary(tokens)
vocab <- prune_vocabulary(vocab,
                          term_count_min = 2,
                          doc_proportion_max = 0.9)

cat("Taille du vocabulaire:", nrow(vocab), "\n")

vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(tokens, vectorizer)
tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm, tfidf)

rownames(dtm_tfidf) <- success_factors$company

# 5. TOP MOTS PAR ENTREPRISE (vérification) ----
library(Matrix)

top_words_per_company <- lapply(1:nrow(dtm_tfidf), function(i) {
  company <- success_factors$company[i]
  scores <- dtm_tfidf[i, ]
  top_idx <- order(scores, decreasing = TRUE)[1:10]
  top_terms <- colnames(dtm_tfidf)[top_idx]
  top_scores <- scores[top_idx]
  
  data.frame(
    company = company,
    word = top_terms,
    tfidf = as.numeric(top_scores),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# Vérifier pour Aleph Alpha
cat("\nTop mots pour Aleph Alpha:\n")
print(top_words_per_company %>% 
        filter(company == "Aleph Alpha") %>%
        arrange(desc(tfidf)))

# 6. MATRICE DE SIMILARITÉ ----
sim_matrix <- sim2(dtm_tfidf, method = "cosine", norm = "l2")
rownames(sim_matrix) <- success_factors$company
colnames(sim_matrix) <- success_factors$company

cat("\nDistribution des similarités:\n")
sim_values <- sim_matrix[lower.tri(sim_matrix)]
cat("Min:", round(min(sim_values), 3), "\n")
cat("Max:", round(max(sim_values), 3), "\n")
cat("Moyenne:", round(mean(sim_values), 3), "\n")
cat("Médiane:", round(median(sim_values), 3), "\n\n")

# 7. VISUALISATIONS ----
if(nrow(sim_matrix) <= 50) {
  pheatmap(sim_matrix,
           display_numbers = FALSE,
           cluster_rows = TRUE,
           cluster_cols = TRUE,
           main = "Similarité - Facteurs de risque IA (TF-IDF)",
           fontsize = 8)
}

# 8. CLUSTERING ----
dist_matrix <- as.dist(1 - sim_matrix)
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Dendrogramme", xlab = "", sub = "", cex = 0.6)

k <- min(4, ceiling(nrow(success_factors) / 8))
clusters <- cutree(hc, k = k)
success_factors$cluster <- clusters

cat("Nombre de clusters:", k, "\n")
print(table(clusters))

# Clustering hiérarchique
dist_matrix <- as.dist(1 - sim_matrix)
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Dendrogramme", xlab = "", sub = "", cex = 0.6)

# Déterminer nombre optimal de clusters
k <- min(6, ceiling(nrow(success_factors) / 10))  # Adaptatif
clusters <- cutree(hc, k = k)
success_factors$cluster <- clusters

cat("Nombre de clusters:", k, "\n")
cat("Répartition clusters:\n")
print(table(clusters))
cat("\n")

# 2. SOCIAL NETWORK (liens entre entreprises) ----
cat("Création du Social Network...\n")

# Convertir matrice de similarité en edge list
company_sim <- sim_matrix %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column("from") %>%
  pivot_longer(-from, names_to = "to", values_to = "weight") %>%
  filter(
    from != to,           # Pas d'auto-liens
    weight > 0.20,        # Seuil de similarité plus bas
    from < to             # Éviter doublons (A->B et B->A)
  ) %>%
  arrange(desc(weight))

cat("Nombre de connexions (seuil > 0.15):", nrow(company_sim), "\n")
cat("Top 10 connexions:\n")
print(head(company_sim, 10))
cat("\n")

# Si pas assez de connexions, baisser le seuil
if(nrow(company_sim) < 50) {
  company_sim <- sim_matrix %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column("from") %>%
    pivot_longer(-from, names_to = "to", values_to = "weight") %>%
    filter(
      from != to,
      weight > 0.20,      # Seuil encore plus bas
      from < to
    ) %>%
    arrange(desc(weight))
  
  cat("Avec seuil > 0.10:", nrow(company_sim), "connexions\n\n")
}

if(nrow(company_sim) > 0) {
  company_graph <- graph_from_data_frame(company_sim, directed = FALSE)
  
  # Ajouter les clusters comme attribut
  V(company_graph)$cluster <- clusters[match(V(company_graph)$name, success_factors$company)]
  
  # Calculer centralité
  V(company_graph)$degree <- degree(company_graph)
  V(company_graph)$betweenness <- betweenness(company_graph)
  
  cat("Statistiques du réseau:\n")
  cat("Nombre de nœuds:", vcount(company_graph), "\n")
  cat("Nombre d'arêtes:", ecount(company_graph), "\n")
  cat("Densité:", edge_density(company_graph), "\n")
  cat("Composantes connexes:", components(company_graph)$no, "\n\n")
  
  # Top entreprises par centralité
  top_central <- data.frame(
    company = V(company_graph)$name,
    degree = V(company_graph)$degree,
    betweenness = V(company_graph)$betweenness
  ) %>%
    arrange(desc(degree))
  
  cat("Top 10 entreprises par degré de connexion:\n")
  print(head(top_central, 10))
  cat("\n")
  
  # Visualisation
  set.seed(123)
  p <- ggraph(company_graph, layout = "fr") +
    geom_edge_link(aes(width = weight, alpha = weight), color = "gray70") +
    geom_node_point(aes(color = as.factor(cluster), size = degree)) +
    geom_node_text(aes(label = name, size = degree), 
                   repel = TRUE, max.overlaps = 20) +
    scale_edge_width(range = c(0.5, 2)) +
    scale_edge_alpha(range = c(0.2, 0.8)) +
    scale_size_continuous(range = c(2, 8)) +
    labs(
      title = "Social Network - Similarité entre entreprises IA",
      subtitle = paste0(vcount(company_graph), " entreprises, ", 
                        ecount(company_graph), " connexions"),
      color = "Cluster",
      size = "Connexions"
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  print(p)
  ggsave("social_network.png", width = 16, height = 12, dpi = 300)
  
  # Analyse des clusters
  cat("Analyse par cluster:\n")
  cluster_analysis <- success_factors %>%
    group_by(cluster) %>%
    summarise(
      n_companies = n(),
      companies = paste(head(company, 5), collapse = ", ")
    )
  print(cluster_analysis)
  
} else {
  cat("⚠️ Aucune connexion trouvée - similarités trop faibles!\n")
  cat("Suggestions:\n")
  cat("1. Vérifier la qualité des textes (longueur, contenu)\n")
  cat("2. Augmenter le vocabulaire (diminuer pruning)\n")
  cat("3. Utiliser une autre méthode de similarité\n")
}






