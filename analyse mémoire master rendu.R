#début du document----
#analyse mémoire master rendu
#se document permet l'analyse ainsi que la production de données descriptives pour mon mémoire de M1 en se basant sur une base de données créé avec python 
rm(list=ls())

#importation des packages et de la base de donnée----
library(dplyr)
library(plm)# pour les données de panel
library(lmtest)# pour les tests de robustesse
library(sandwich)# pour des erreurs robustes
library(ggplot2)
library(tidyverse)

base_finale_1<- read.csv("C:/Users/alexi/Downloads/base_finale_1.xls", header = TRUE, sep = ",")
summary(base_finale_1)
View(base_finale_1)

#transformation en panel
panel <- pdata.frame(base_finale_1, index = c("Entreprise", "Année"))
panel$Employés[panel$Année == 2017]<-panel$Employés[panel$Année == 2017]*1000#correction d'une valeur sur l'année 2017(inconsistence des données de la commission européenne)

#création des premiers modèles----
summary(modele1 <- plm(CA ~ R.D, data = panel, model = "within"))
summary(modele2 <- plm(CA ~ R.D+Capex, data = panel, model = "within"))
summary(modele3 <- plm(CA ~ R.D+Capex+Employés, data = panel, model = "within"))


#création des modèles avec lag de R&D----
#modèle log R&D lag 5
panel$RD_lag1 <- lag(panel$R.D, 1)
panel$RD_lag2 <- lag(panel$R.D, 2)
panel$RD_lag3 <- lag(panel$R.D, 3)
panel$RD_lag4 <- lag(panel$R.D, 4)
panel$RD_lag5 <- lag(panel$R.D, 5)
panel$RD_lag6 <- lag(panel$R.D, 6)
panel$RD_lag7 <- lag(panel$R.D, 7)
panel$RD_lag8 <- lag(panel$R.D, 8)
panel$RD_lag9 <- lag(panel$R.D, 9)


panel$log_CA <- log(panel$CA + 1)
panel$log_Capex <- log(panel$Capex + 1)
panel$log_Employes <- log(panel$Employés + 1)
panel$log_RD_lag3 <- log(panel$RD_lag3 + 1)  # pour éviter log(0)
panel$log_RD_lag5 <- log(panel$RD_lag5 + 1)  # pour éviter log(0)

summary(modele_lag5 <- plm(log_CA~log_RD_lag5+log_Employes+log_Capex, 
                           data = panel, 
                           model = "within", 
                           na.action = na.omit))


summary(modele_lag3 <- plm(log_CA~log_RD_lag3+log_Employes+log_Capex, 
                           data = panel, 
                           model = "within", 
                           na.action = na.omit))
#le modèle lag3 est bien meilleur 

#création d'un modèle avec R&D cumulé----

#modèle lag complet 

# Calcul du cumul de R&D passée
panel$RD_cumul <- rowSums(panel[, c("R.D","RD_lag1", "RD_lag2", "RD_lag3", "RD_lag4", "RD_lag5","RD_lag6","RD_lag7","RD_lag8","RD_lag9")], na.rm = TRUE)
panel$log_RD_cumul <- log(panel$RD_cumul + 1)

#modèle cumulé
summary(modele_cumul_normal<-plm(CA~RD_cumul+Capex+Employés, data= panel, model="within",na.action=na.omit))
summary(modele_cumul <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                    data = panel, 
                    model = "within", 
                    na.action = na.omit))

#création d'un modèle avec une moyenne glissante---- 

panel2 <- panel %>%
  arrange(Entreprise, Année) %>%
  group_by(Entreprise) %>%
  mutate(R.D_moyenne_mobile3 = zoo::rollmean(R.D, k = 3, fill = NA, align = "right")) %>%
  ungroup()

panel2$log_RD_moyenne_mobile <- log(panel2$R.D_moyenne_mobile3 + 1)
#modèle moyenne glissante 
summary(modele_moyenne_mobile <- plm(log_CA ~ log_RD_moyenne_mobile + log_Capex + log_Employes,
                             data = panel2,
                             model = "within",
                             na.action = na.omit))

#le modèle cumulé semble plus pertinent ----
summary(modele_within <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                            data = panel, 
                            model = "within", 
                            na.action = na.omit))
summary(modele_pooling <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                             data = panel, 
                             model = "pooling", 
                             na.action = na.omit))
summary(modele_random <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                              data = panel, 
                              model = "random", 
                              na.action = na.omit))

#test statistique sur le choix du modèle---- 
phtest(modele_within , modele_random)#test de hausman pour le modèle random
pFtest(modele_within, modele_pooling)#test de fischer modifié pour le modèle pooling 
#données et représentation visuelle pour le mémoire----
summary(panel)
# Tracé du nuage de points entre CA et R.D
donnees_propres <- base_finale_1[!is.na(base_finale_1$CA), ]

plot(donnees_propres$CA, donnees_propres$R.D, 
     xlab = "CA", ylab = "R.D", 
     main = "r et d en fonction de ca ")

# Ajout de la droite de régression linéaire
abline(lm(R.D~CA, data = donnees_propres), col = "blue")
lines(lowess(donnees_propres$CA, donnees_propres$R.D), col = "red")




#essai d'analyse sur une base de données non filtrée ----
sans_filtre<- read.csv("C:/Users/alexi/Downloads/base_finale_sans_filtre.xls", header = TRUE, sep = ",")
psans_filtre <- pdata.frame(sans_filtre, index = c("Entreprise", "Année"))
psans_filtre$Employés[psans_filtre$Année == 2017]<-psans_filtre$Employés[psans_filtre$Année == 2017]*1000#correction d'une valeur sur l'année 2017(inconsistence des données de la commission européenne)

#ajout des données

psans_filtre %>%
  count(Entreprise, Année) %>%
  filter(n > 1)
psans_filtre <- psans_filtre[-8981, ]  # Retire la ligne problématique


psans_filtre$RD_lag1 <- lag(psans_filtre$R.D, 1)
psans_filtre$RD_lag2 <- lag(psans_filtre$R.D, 2)
psans_filtre$RD_lag3 <- lag(psans_filtre$R.D, 3)
psans_filtre$RD_lag4 <- lag(psans_filtre$R.D, 4)
psans_filtre$RD_lag5 <- lag(psans_filtre$R.D, 5)
psans_filtre$RD_lag6 <- lag(psans_filtre$R.D, 6)
psans_filtre$RD_lag7 <- lag(psans_filtre$R.D, 7)
psans_filtre$RD_lag8 <- lag(psans_filtre$R.D, 8)
psans_filtre$RD_lag9 <- lag(psans_filtre$R.D, 9)
psans_filtre$log_CA <- log(psans_filtre$CA + 1)
psans_filtre$log_Capex <- log(psans_filtre$Capex + 1)
psans_filtre$log_Employes <- log(psans_filtre$Employés + 1)

psans_filtre$RD_cumul <- rowSums(psans_filtre[, c("R.D","RD_lag1", "RD_lag2", "RD_lag3", "RD_lag4", "RD_lag5","RD_lag6","RD_lag7","RD_lag8","RD_lag9")], na.rm = TRUE)
psans_filtre$log_RD_cumul <- log(psans_filtre$RD_cumul + 1)
psans_filtre$secteur_pharma <- ifelse(psans_filtre$Secteur == "Pharmaceuticals & Biotechnology", 1, 0)

summary(modele_sfcumul <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                            data = psans_filtre, 
                            model = "within", 
                            na.action = na.omit))
summary(modele_sfcumul_inter <- plm(log_CA ~ log_RD_cumul * secteur_pharma + log_Capex + log_Employes,
                             data = psans_filtre,
                             model = "within",
                             na.action = na.omit))

summary(modele_cumul_pharma <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                            data = panel, 
                            model = "within", 
                            na.action = na.omit))

summary(psans_filtre)
#représentation visuelle----
# Conversion explicite
df_plot <- as.data.frame(psans_filtre)

# Conversion des variables nécessaires en numériques/facteurs simples
df_plot <- df_plot %>%
  mutate(
    log_CA = as.numeric(log_CA),
    log_RD_cumul = as.numeric(log_RD_cumul),
    secteur_pharma = factor(secteur_pharma, labels = c("Autres secteurs", "Pharmaceutique"))
  ) %>%
  filter(!is.na(log_CA), !is.na(log_RD_cumul), !is.na(secteur_pharma)) %>%
  sample_frac(0.3)
library(ggplot2)

ggplot(df_plot, aes(x = log_RD_cumul, y = log_CA, color = secteur_pharma)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("gray30", "red3")) +
  labs(
    title = "Effet de la R&D cumulée sur le CA selon le secteur",
    x = "Log(R&D cumulée)",
    y = "Log(Chiffre d'affaires)",
    color = "Secteur"
  ) +
  theme_minimal(base_size = 13)
