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

#Création d'une variable de croissance
panel$croissance_CA <- (panel$CA - dplyr::lag(panel$CA)) / dplyr::lag(panel$CA)

#création des modèles avec lag de R&D----
#modèle log R&D lag 5
panel$R.D_lag3 <- lag(panel$R.D, 3)
panel$R.D_lag5 <- lag(panel$R.D, 5)
panel$log_CA <- log(panel$CA + 1)
panel$log_Capex <- log(panel$Capex + 1)
panel$log_Employes <- log(panel$Employés + 1)
panel$log_RD_lag3 <- log(panel$R.D_lag3 + 1)  # pour éviter log(0)
panel$log_RD_lag5 <- log(panel$R.D_lag5 + 1)  # pour éviter log(0)

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
panel$RD_lag1 <- lag(panel$R.D, 1)
panel$RD_lag2 <- lag(panel$R.D, 2)
panel$RD_lag3 <- lag(panel$R.D, 3)
panel$RD_lag4 <- lag(panel$R.D, 4)
panel$RD_lag5 <- lag(panel$R.D, 5)
# Calcul du cumul de R&D passée
panel$RD_cumul <- rowSums(panel[, c("RD_lag1", "RD_lag2", "RD_lag3", "RD_lag4", "RD_lag5")], na.rm = TRUE)
panel$log_RD_cumul <- log(panel$RD_cumul + 1)

#modèle cumulé
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

#le modèle cumulé semble plus pertinent 
summary(modele_within <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                            data = panel, 
                            model = "within", 
                            na.action = na.omit))
summary(modele_pooling <- plm(log_CA ~ log_RD_cumul + log_Capex + log_Employes, 
                             data = panel, 
                             model = "pooling", 
                             na.action = na.omit))

#test statistique sur le choix du modèle---- 
phtest(modele_within , modele_pooling)
