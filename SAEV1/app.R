# --- Installation des packages ---
packages = c("shiny", "ggplot2", "dplyr", "bslib", "sf", "leaflet", "jsonlite")
to_install = setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(packages, require, character.only = TRUE))

# --- Packages nécessaires ---
library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(sf)
library(leaflet)
library(jsonlite)

# --- Chargement des données ---
data = read.csv(
  "https://raw.githubusercontent.com/bymatzo/SAE-R/refs/heads/main/data/data.csv",
  sep = ",", dec = "."
)
carte_france = st_read(
  "C:/Wissem/IUT ( SD2 )/Projet R Shiny/SAE-R/SAEV1/communes-100m.geojson",
  quiet = TRUE
)

# ============================
#    PRÉPA DONNÉES CARTE
# ============================

#Rhône uniquement
carte_rhone = subset(carte_france, departement == "69")

# CRS WGS84 (Leaflet)
if (!is.na(sf::st_crs(carte_rhone))) {
  if (sf::st_crs(carte_rhone)$epsg != 4326) {
    carte_rhone = sf::st_transform(carte_rhone, 4326)
  }
}

#Table d’agrégats par code postal
dpe69 = data[, c("code_postal_ban", "conso_5_usages_par_m2_ep", "etiquette_dpe")]
niv_dpe = c("A","B","C","D","E","F","G")
dpe69$dpe_num = match(dpe69$etiquette_dpe, niv_dpe)

agg = aggregate(
  cbind(conso_5_usages_par_m2_ep, dpe_num) ~ code_postal_ban,
  data = dpe69,
  FUN = function(x) mean(x, na.rm = TRUE)
)
names(agg)[2:3] = c("conso_moy", "dpe_moy")

# === ÉTAPE A : Chercher INSEE pour chaque code postal ===

# Codes postaux présents dans tes moyennes
cp_list = sort(unique(as.character(agg$code_postal_ban)))

# Petite fonction qui interroge le site officiel pour 1 code postal
fetch_cp_insee = function(cp) {
  url = paste0("https://geo.api.gouv.fr/communes?codePostal=", cp,
               "&fields=nom,code,codesPostaux&format=json")
  txt = tryCatch(readLines(url, warn = FALSE), error = function(e) NULL)
  if (is.null(txt)) return(NULL)
  js  = tryCatch(jsonlite::fromJSON(paste(txt, collapse = "")), error = function(e) NULL)
  if (is.null(js) || NROW(js) == 0) return(NULL)
  
  data.frame(
    code_postal_ban = rep(cp, nrow(js)),
    code_insee      = as.character(js$code),
    nom_commune     = as.character(js$nom),
    stringsAsFactors = FALSE
  )
}

# On lance la requête pour tous les codes postaux
lst = lapply(cp_list, fetch_cp_insee)

# On enlève les éléments vides pour éviter les soucis
if (length(lst) == 0) {
  stop("Aucune réponse de l'API. Vérifie ta connexion internet.")
}
lst = lst[ !vapply(lst, is.null, TRUE) ]

if (length(lst) == 0) {
  stop("L'API n'a renvoyé aucun résultat pour tes codes postaux.")
}

# On empile tous les petits tableaux en un seul
cp_insee = do.call(rbind, lst)
cp_insee = unique(cp_insee)

# On détecte le bon nom de colonne INSEE dans la carte (code ou code_insee)
col_code_insee = if ("code_insee" %in% names(carte_rhone)) "code_insee" else "code"

# On garde seulement les communes qui sont bien dans la carte du Rhône
codes_insee_rhone = as.character(carte_rhone[[col_code_insee]])
codes_insee_rhone = codes_insee_rhone[!is.na(codes_insee_rhone) & nzchar(codes_insee_rhone)]

cp_insee = cp_insee[ cp_insee$code_insee %in% codes_insee_rhone, ]

# Si tout a été filtré, on préfère prévenir gentiment
if (nrow(cp_insee) == 0) {
  stop("Après filtrage Rhône, la table CP→INSEE est vide. 
- Vérifie que tes CP sont bien du 69 dans 'agg$code_postal_ban'
- Vérifie que la carte contient bien les INSEE attendus.")
}

# === ÉTAPE B : Passer des CP aux communes (INSEE) et fusionner avec la carte ===

# On colle tes moyennes sur les INSEE
agg_insee = merge(agg, cp_insee, by = "code_postal_ban", all.x = TRUE)

# Regroupement par commune (si plusieurs CP pour la même commune)
agg_commune = aggregate(
  cbind(conso_moy, dpe_moy) ~ code_insee,
  data = agg_insee,
  FUN  = function(x) mean(x, na.rm = TRUE)
)

# On renomme la colonne INSEE de la carte si besoin
if (!("code_insee" %in% names(carte_rhone))) {
  names(carte_rhone)[names(carte_rhone) == "code"] = "code_insee"
}
carte_rhone$code_insee = as.character(carte_rhone$code_insee)

# Nouvelle fusion propre (une valeur par commune)
donnees_map = merge(carte_rhone, agg_commune, by = "code_insee", all.x = TRUE)

# ============================
#            UI
# ============================

ui = navbarPage(
  "Analyse DPE et émissions de CO₂",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Poppins")
  ),
  
  # --- Onglet 1 : Répartition DPE ---
  tabPanel("Répartition DPE",
           sidebarLayout(
             sidebarPanel(
               h4("⚙️ Options de filtrage"),
               selectInput(
                 inputId = "energie",
                 label = "Type d’énergie principale :",
                 choices = sort(unique(data$type_energie_principale_chauffage)),
                 selected = unique(data$type_energie_principale_chauffage)[1]
               ),
               selectInput(
                 inputId = "type_batiment",
                 label = "Type de bâtiment :",
                 choices = c("Tous", sort(unique(data$type_batiment))),
                 selected = "Tous"
               ),
               selectInput(
                 inputId = "periode_construction",
                 label = "Période de construction :",
                 choices = c("Tous", sort(unique(data$periode_construction))),
                 selected = "Tous"
               ),
               hr(),
               p("Ce graphique affiche la répartition en pourcentage des classes DPE pour le type d’énergie sélectionné.")
             ),
             mainPanel(
               plotOutput("graphique_dpe", height = "500px")
             )
           )
  ),
  
  # --- Onglet 2 : Boxplot des émissions CO₂ ---
  tabPanel("Émissions de CO₂",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "type_batiment_boxplot",
                 label = "Type de bâtiment :",
                 choices = c("Tous", sort(unique(data$type_batiment))),
                 selected = "Tous"
               ),
               selectInput(
                 inputId = "periode_construction_boxplot",
                 label = "Période de construction :",
                 choices = c("Tous", sort(unique(data$periode_construction))),
                 selected = "Tous"
               )
             ),
             mainPanel(
               plotOutput("graphique_boxplot", height = "550px")
             )
           )
  ),
  
  # --- Onglet 3 : Coût du chauffage (filtrable) ---
  tabPanel("Coût du chauffage",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "energie_cout",
                 label = "Type d’énergie principale :",
                 choices = sort(unique(data$type_energie_principale_chauffage)),
                 selected = unique(data$type_energie_principale_chauffage)[1]
               ),
               selectInput(
                 inputId = "type_batiment_cout",
                 label = "Type de bâtiment :",
                 choices = c("Tous", sort(unique(data$type_batiment))),
                 selected = "Tous"
               ),
               selectInput(
                 inputId = "periode_construction_cout",
                 label = "Période de construction :",
                 choices = c("Tous", sort(unique(data$periode_construction))),
                 selected = "Tous"
               ),
               hr(),
               p("Cet histogramme montre la distribution du coût de chauffage (€ / an) pour le type d’énergie sélectionné (avec suppression des 5 % des valeurs les plus élevées).")
             ),
             mainPanel(
               plotOutput("graphique_histogramme", height = "550px")
             )
           )
  ),
  
  # --- Onglet 4 : Nuage de points consommation vs émission ---
  tabPanel("Conso vs Émission",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "energie_scatter",
                 label = "Type d’énergie principale :",
                 choices = sort(unique(data$type_energie_principale_chauffage)),
                 selected = unique(data$type_energie_principale_chauffage)[1]
               ),
               selectInput(
                 inputId = "type_batiment_scatter",
                 label = "Type de bâtiment :",
                 choices = c("Tous", sort(unique(data$type_batiment))),
                 selected = "Tous"
               ),
               selectInput(
                 inputId = "periode_construction_scatter",
                 label = "Période de construction :",
                 choices = c("Tous", sort(unique(data$periode_construction))),
                 selected = "Tous"
               ),
               hr(),
               p("Nuage de points : consommation d'énergie (kWh/m²/an) vs émissions de CO₂ (kgCO₂/m²/an) filtré par type d’énergie, type de bâtiment et période de construction.")
             ),
             mainPanel(
               plotOutput("graphique_scatter", height = "550px")
             )
           )
  ),
  
  # --- Onglet 5 : Carte  ---
  tabPanel(" Carte",
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "var_color",
                 label   = "Variable à afficher :",
                 choices = c("Consommation moyenne (kWh/m²/an)" = "conso_moy",
                             "DPE moyen (1=A … 7=G)"           = "dpe_moy"),
                 selected = "conso_moy"
               ),
               sliderInput("opacity", "Opacité", min = 0.2, max = 1, value = 0.85, step = 0.05),
               checkboxInput("borders", "Bordures marquées", value = FALSE)
             ),
             mainPanel(
               leafletOutput("map_rhone", height = "650px")
             )
           )
  ),
  
  # --- Onglet 6 : À propos ---
  tabPanel("ℹ️ À propos",
           fluidPage(
             h3("À propos de cette application"),
             p("Cette application Shiny permet d’analyser les performances énergétiques (DPE) et les émissions de CO₂ des logements selon le type d’énergie de chauffage."),
             p("Réalisée avec ", strong("R Shiny"), " et ", strong("ggplot2"), ".")
           )
  )
)

# ============================
#          SERVER
# ============================

server = function(input, output, session) {
  
  # ---------- Fonctions de filtrage (inchangées) ----------
  filter_data = function(df, energie_sel, batiment_sel, periode_sel) {
    df = df %>% filter(type_energie_principale_chauffage == energie_sel)
    if (batiment_sel != "Tous") df = df %>% filter(type_batiment == batiment_sel)
    if (periode_sel != "Tous") df = df %>% filter(periode_construction == periode_sel)
    return(df)
  }
  
  filter_data_boxplot = function(df, batiment_sel, periode_sel) {
    if (batiment_sel != "Tous") df = df %>% filter(type_batiment == batiment_sel)
    if (periode_sel != "Tous") df = df %>% filter(periode_construction == periode_sel)
    return(df)
  }
  
  # ---------- Graphique 1  ----------
  output$graphique_dpe = renderPlot({
    df_filtre = filter_data(data, input$energie, input$type_batiment, input$periode_construction) %>%
      count(etiquette_dpe) %>%
      mutate(proportion = n / sum(n) * 100)
    
    df_filtre$etiquette_dpe = factor(
      df_filtre$etiquette_dpe,
      levels = c("A","B","C","D","E","F","G")
    )
    
    ggplot(df_filtre, aes(x = etiquette_dpe, y = proportion, fill = etiquette_dpe)) +
      geom_col(width = 0.7, color = "white", linewidth = 0.5) +
      geom_text(aes(label = paste0(round(proportion, 1), "%")),
                vjust = -0.5, size = 5, color = "black", fontface = "bold") +
      scale_fill_manual(values = c(
        "A" = "#009E3D","B" = "#6DBE45","C" = "#FFF200",
        "D" = "#F7A600","E" = "#E87511","F" = "#E30613","G" = "#B60000"
      )) +
      labs(
        title = paste("Répartition des classes DPE pour le chauffage :", input$energie),
        x = "Classe DPE",
        y = "Proportion (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank()
      ) +
      ylim(0, 100)
  })
  
  # ---------- Graphique 2  ----------
  output$graphique_boxplot = renderPlot({
    df_filtre = filter_data_boxplot(data, input$type_batiment_boxplot, input$periode_construction_boxplot)
    
    ggplot(df_filtre, aes(x = type_energie_principale_chauffage, 
                          y = emission_ges_5_usages_par_m2,
                          fill = type_energie_principale_chauffage)) +
      geom_boxplot(outlier.colour = "red", alpha = 0.7) +
      ylim(0, 130) + 
      labs(
        title = "Distribution des émissions de CO2 par type d’énergie principale",
        x = "Type d’énergie principale de chauffage",
        y = "Émissions de CO₂ (kgCO₂/m²/an)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text.x = element_text(family = "Poppins", color = "black", angle = 30, hjust = 1, size = 11),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        panel.grid.major.x = element_blank()
      )
  })
  
  # ---------- Graphique 3  ----------
  output$graphique_histogramme = renderPlot({
    df_filtre = filter_data(data, input$energie_cout, input$type_batiment_cout, input$periode_construction_cout) %>%
      filter(!is.na(cout_chauffage) & is.finite(cout_chauffage))
    
    validate(
      need(nrow(df_filtre) > 0,
           paste("Aucune donnée disponible pour", input$energie_cout))
    )
    
    seuil_95 = quantile(df_filtre$cout_chauffage, 0.95, na.rm = TRUE)
    df_filtre = df_filtre %>% filter(cout_chauffage <= seuil_95)
    
    ggplot(df_filtre, aes(x = cout_chauffage)) +
      geom_histogram(bins = 30, fill = "#2E86AB", color = "white", alpha = 0.8) +
      labs(
        title = paste("Répartition du coût de chauffage pour", input$energie_cout,
                      "(5 % valeurs extrêmes supprimées)"),
        x = "Coût du chauffage (€ / an)",
        y = "Nombre de logements"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)
      )
  })
  
  # ---------- Graphique 4  ----------
  output$graphique_scatter = renderPlot({
    df_filtre = filter_data(data, input$energie_scatter, input$type_batiment_scatter, input$periode_construction_scatter) %>%
      filter(!is.na(conso_5_usages_par_m2_ep) & is.finite(conso_5_usages_par_m2_ep)) %>%
      filter(!is.na(emission_ges_5_usages_par_m2) & is.finite(emission_ges_5_usages_par_m2))
    
    validate(
      need(nrow(df_filtre) > 0,
           paste(" Aucune donnée disponible pour", input$energie_scatter))
    )
    
    seuil_conso = quantile(df_filtre$conso_5_usages_par_m2_ep, 0.95, na.rm = TRUE)
    seuil_ges   = quantile(df_filtre$emission_ges_5_usages_par_m2, 0.95, na.rm = TRUE)
    
    df_filtre = df_filtre %>%
      filter(conso_5_usages_par_m2_ep <= seuil_conso,
             emission_ges_5_usages_par_m2 <= seuil_ges)
    
    ggplot(df_filtre, aes(x = conso_5_usages_par_m2_ep, y = emission_ges_5_usages_par_m2)) +
      geom_point(color = "#E74C3C", alpha = 0.7) +
      labs(
        title = paste("Consommation vs Émissions pour", input$energie_scatter,
                      "(5 % valeurs extrêmes supprimées)"),
        x = "Consommation d'énergie (kWh/m²/an)",
        y = "Émissions de CO₂ (kgCO₂/m²/an)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)
      )
  })
  
  # ---------- Carte : rendu de base ----------
  bbox_rhone = sf::st_bbox(donnees_map)
  lng0 = as.numeric((bbox_rhone$xmin + bbox_rhone$xmax) / 2)
  lat0 = as.numeric((bbox_rhone$ymin + bbox_rhone$ymax) / 2)
  
  output$map_rhone = leaflet::renderLeaflet({
    m = leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE))
    m = leaflet::addProviderTiles(m, providers$CartoDB.Positron)
    m = leaflet::setView(m, lng = lng0, lat = lat0, zoom = 9)
    m
  })
  
  # --- Fonction qui crée les couleurs de la carte ---
  make_pal = function(values) {
    rng = range(values, na.rm = TRUE)
    if (!is.finite(rng[1]) || rng[1] == rng[2]) rng = c(0, 1)
    leaflet::colorNumeric(
      palette = c("#f7fbff", "#6baed6", "#2171b5", "#08306b"),
      domain  = rng,
      na.color = "#d9d9d9"
    )
  }
  # --- Met à jour la carte quand on change un bouton ou un réglage ---
  observe({
    varname = input$var_color
    values  = donnees_map[[varname]]
    pal = make_pal(values)
    border_color  = if (isTRUE(input$borders)) "#2b2b2b" else "#666666"
    border_weight = if (isTRUE(input$borders)) 1.2 else 0.5
    titre_leg = if (identical(varname, "conso_moy")) "kWh/m²/an" else "DPE moyen (1=A…7=G)"
    commune_col = NA
    for (cn in c("nom","commune","nom_commune")) {
      if (cn %in% names(donnees_map)) { commune_col = cn; break }
    }
    commune_txt = if (!is.na(commune_col)) as.character(donnees_map[[commune_col]])
    else as.character(donnees_map$code_postal_ban)
    rnd = function(v) { y = round(v, 1); y[is.na(y)] = NA; y }
    labels = paste0(
      "<b>", commune_txt, "</b>",
      "<br>Consommation moyenne : ",
      ifelse(is.na(donnees_map$conso_moy), "NA", paste0(rnd(donnees_map$conso_moy), " kWh/m²/an")),
      "<br>DPE moyen (1=A…7=G) : ",
      ifelse(is.na(donnees_map$dpe_moy), "NA", as.character(rnd(donnees_map$dpe_moy)))
    )
    m = leaflet::leafletProxy("map_rhone")
    m = leaflet::clearShapes(m)
    m = leaflet::clearControls(m)
    m = leaflet::addPolygons(
      map  = m,
      data = donnees_map,
      fillColor   = pal(values),
      fillOpacity = input$opacity,
      color       = border_color,
      weight      = border_weight,
      opacity     = 1,
      smoothFactor = 0.2,
      highlight = leaflet::highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE),
      label = lapply(labels, htmltools::HTML),
      labelOptions = leaflet::labelOptions(textsize = "12px", direction = "auto")
    )
    m = leaflet::addLegend(
      map = m, position = "bottomright",
      pal = pal, values = values,
      opacity = 0.9, title = titre_leg,
      labFormat = leaflet::labelFormat(digits = 1)
    )
  })
  
}

# ============================
#        LANCEMENT
# ============================

shinyApp(ui = ui, server = server)
