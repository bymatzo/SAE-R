library(httr)
library(jsonlite)
library(dplyr)

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
dfAdresse69 <- read.csv("https://raw.githubusercontent.com/asardell/IUT_SD2/refs/heads/main/data/adresses-69.csv", sep = ";", header = TRUE)

# Préparation des codes postaux et années
cp_69 <- unique(dfAdresse69$code_postal)
years <- c(2021, 2022, 2023, 2024, 2025)
etiquettes = c("A", "B", "C", "D", "E", "F", "G")

# Initialisation d'une DataFrame vide
df <- data.frame()

#Boucle sur les code postaux
for (cp in cp_69){
 
  params <- list(
    page = 1,
    size = 10000,
    select = "type_energie_principale_chauffage,etiquette_dpe,cout_chauffage,periode_construction,surface_habitable_logement,type_batiment,date_etablissement_dpe,conso_5_usages_ep,conso_5_usages_par_m2_ep,emission_ges_5_usages_par_m2,code_postal_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban",
    qs = paste("code_postal_ban :", cp)
  )
  url_encoded <- modify_url(base_url, query = params)
  response <- GET(url_encoded)
  content = fromJSON(rawToChar(response$content), flatten = FALSE)
 
  #Si + de 10 000, boucle sur les années
  if (content$total>10000){
    for (year in years){
      params <- list(
        page = 1,
        size = 10000,
        select = "type_energie_principale_chauffage,etiquette_dpe,cout_chauffage,periode_construction,surface_habitable_logement,type_batiment,date_etablissement_dpe,conso_5_usages_ep,conso_5_usages_par_m2_ep,emission_ges_5_usages_par_m2,code_postal_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban",
        qs = paste0("code_postal_ban : ", cp, " AND date_etablissement_dpe : [", year, "-01-01 TO ",year, "-12-31]")
      )
      url_encoded <- modify_url(base_url, query = params)
      response <- GET(url_encoded)
      content = fromJSON(rawToChar(response$content), flatten = FALSE)
     
      #Si + de 10 000, boucle sur les etiquettes
      if (content$total>10000){
        for (etiquette in etiquettes){
          params <- list(
            page = 1,
            size = 10000,
            select = "type_energie_principale_chauffage,etiquette_dpe,cout_chauffage,periode_construction,surface_habitable_logement,type_batiment,date_etablissement_dpe,conso_5_usages_ep,conso_5_usages_par_m2_ep,emission_ges_5_usages_par_m2,code_postal_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban",
            qs = paste0("code_postal_ban : ", cp, " AND date_etablissement_dpe : [", year, "-01-01 TO ",year, "-12-31] AND etiquette_dpe : ", etiquette)
          )
          url_encoded <- modify_url(base_url, query = params)
          response <- GET(url_encoded)
          content = fromJSON(rawToChar(response$content), flatten = FALSE)
          df = rbind(df, content$result)
        }
      }
     
      else{
        df = rbind(df, content$result)
      }
    }
  }
 
  else{
    df = rbind(df, content$result)
  }
}

# Afficher les données récupérées
View(df)

# Exporter la DataFrame en fichier CSV
write.csv(df, "C:/Users/simob/Documents/GitHub/SAE-R/data.csv", row.names = FALSE)
