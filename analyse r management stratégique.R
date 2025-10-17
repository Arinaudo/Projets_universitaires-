#installation des packages----
install.packages(tidyverse)
library(tidyverse)
library(dplyr)
library(plm)# pour les données de panel
library(lmtest)# pour les tests de robustesse
library(sandwich)# pour des erreurs robustes
library(ggplot2)

#importation de la base de données ----
base_finale_1<- read.csv("C:/Users/alexi/Downloads/base_finale_1 (1).xls", header = TRUE, sep = ",")
panel <- pdata.frame(base_finale_1, index = c("Entreprise", "Année"))
panel$Employés[panel$Année == 2017]<-panel$Employés[panel$Année == 2017]*1000#correction d'une valeur sur l'année 2017(inconsistence des données de la commission européenne)
panel$Entreprise <- as.character(panel$Entreprise)  # convertir en caractère
panel$Entreprise <- ifelse(tolower(panel$Entreprise) %in% c("adobe", "adobe systems"), "Adobe", panel$Entreprise)

#ajout de différentes variables ----
panel$croissance_CA <- (panel$CA - dplyr::lag(panel$CA)) / dplyr::lag(panel$CA)
panel <- panel %>% 
  mutate(Année = as.numeric(Année))

panel <- panel %>%
  group_by(Entreprise) %>%
  arrange(Année, .by_group = TRUE) %>%
  mutate(
    RD_lag1 = R.D[match(Année - 1, Année)],
    RD_lag2 = R.D[match(Année - 2, Année)],
    RD_lag3 = R.D[match(Année - 3, Année)],
    RD_lag4 = R.D[match(Année - 4, Année)],
    RD_lag5 = R.D[match(Année - 5, Année)]
  ) %>%
  ungroup()
panel$Année<-panel$Année+2011
panel$RD_cumul <- rowSums(panel[, c("RD_lag1", "RD_lag2", "RD_lag3", "RD_lag4", "RD_lag5")], na.rm = TRUE)
panel$log_CA <- log(panel$CA+1)
panel$log_RD_cumul <- log(panel$RD_cumul + 1)
panel$log_Capex <- log(panel$Capex + 1)
panel$log_Employes <- log(panel$Employés + 1)

#création des modèles ----
View(panel)
summary(modele_cumul <- plm(CA ~ RD_cumul + Capex + Employés, 
                    data = panel, 
                    model = "within", 
                    na.action = na.omit))

#répresentation visuelle ----
windows()
donnees_propres <- base_finale_1[!is.na(base_finale_1$CA), ]
plot(donnees_propres$CA, donnees_propres$R.D, 
     xlab = "CA", ylab = "R.D", 
     main = "r et d en fonction de ca ")
abline(lm(R.D~CA, data = donnees_propres), col = "blue")
lines(lowess(donnees_propres$CA, donnees_propres$R.D), col = "red")
#test 
# Création d'un vecteur logique indiquant les lignes correspondant à Adobe
is_adobe <- tolower(donnees_propres$Entreprise) %in% c("adobe", "adobe systems")

# Tracé des points de toutes les entreprises avec taille différente pour Adobe
plot(donnees_propres$CA, donnees_propres$R.D, 
     xlab = "CA", ylab = "R.D", 
     main = "R&D en fonction de CA",
     pch = 16,
     col = ifelse(is_adobe, "red", "black"),          # rouge pour Adobe
     cex = ifelse(is_adobe, 2, 1))                  # taille 2 pour Adobe, 1 pour les autres

# Ajustement linéaire
abline(lm(R.D ~ CA, data = donnees_propres), col = "blue")

# Courbe lowess
lines(lowess(donnees_propres$CA, donnees_propres$R.D), col = "red")

# Légende
legend("topleft", legend = c("Autres entreprises", "Adobe"), 
       col = c("black", "red"), pch = 16, pt.cex = c(1, 2))

#test données filtrées 
# Suppression des valeurs extrêmes sur CA et R.D (au-dessus du 90e percentile)
seuil_CA <- quantile(donnees_propres$CA, 0.95, na.rm = TRUE)
seuil_RD <- quantile(donnees_propres$R.D, 0.95, na.rm = TRUE)

donnees_filtrees <- subset(donnees_propres, CA <= seuil_CA & R.D <= seuil_RD)

# (Re)création du vecteur pour identifier Adobe
is_adobe <- donnees_filtrees$Entreprise == "Adobe"  # adapte le nom de la colonne

# Tracé du nuage de points
plot(donnees_filtrees$CA, donnees_filtrees$R.D, 
     xlab = "CA", ylab = "R&D", 
     main = "R&D en fonction de CA (valeurs ≤ 90e percentile)",
     pch = 16,
     col = ifelse(is_adobe, "red", "black"),          # rouge pour Adobe
     cex = ifelse(is_adobe, 6, 1))                    # taille 2 pour Adobe, 1 pour les autres

# Ajustement linéaire
abline(lm(R.D ~ CA, data = donnees_filtrees), col = "blue")

# Courbe lowess
lines(lowess(donnees_filtrees$CA, donnees_filtrees$R.D), col = "red")

# Légende
legend("topleft", legend = c("Autres entreprises", "Adobe"), 
       col = c("black", "red"), pch = 16, pt.cex = c(1, 2))

#comparaison ----
moyenne_RD <- panel %>%
  group_by(Entreprise) %>%
  summarise(RD_moyenne = mean(R.D, na.rm = TRUE)) %>%
  arrange(desc(RD_moyenne))

print(moyenne_RD)#adobe est 24ème sur 792

adobe_RD <- panel$R.D[panel$Entreprise == "Adobe"]
autres_RD <- panel$R.D[panel$Entreprise != "Adobe"]

t.test(adobe_RD, autres_RD, alternative = "greater")

#test sur l'intensité 
panel <- panel %>%
  mutate(Intensite_RD = R.D / CA)
moyenne_intensite <- panel %>%
  group_by(Entreprise) %>%
  summarise(Intensite_moyenne = mean(Intensite_RD, na.rm = TRUE)) %>%
  arrange(desc(Intensite_moyenne))

View(moyenne_intensite)#modèle non pertinent à cause du poids des start-up 
