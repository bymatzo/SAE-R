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

# rend numérique (gère factor/texte + virgules)
to_num <- function(v) {
  if (is.factor(v)) v <- as.character(v)
  v = gsub(",", ".", v)
  suppressWarnings(as.numeric(v))
}

# X/Y brutes
x_raw = to_num(data$coordonnee_cartographique_x_ban)  # Lambert-93 (m)
y_raw = to_num(data$coordonnee_cartographique_y_ban)

# lignes valides
ok0 = is.finite(x_raw) & is.finite(y_raw)
data_pts = data[ok0, , drop = FALSE]

if (nrow(data_pts) > 0) {
  # si ce ne sont pas des degrés, on reprojette 2154 -> 4326
  lon_like = all(abs(x_raw[ok0]) <= 180, na.rm = TRUE)
  lat_like = all(abs(y_raw[ok0]) <= 90,  na.rm = TRUE)
  
  if (lon_like && lat_like) {
    data_pts$lon = x_raw[ok0]
    data_pts$lat = y_raw[ok0]
  } else {
    pts_sf = sf::st_as_sf(
      data.frame(x = x_raw[ok0], y = y_raw[ok0]),
      coords = c("x","y"), crs = 2154
    )
    pts_wgs = sf::st_transform(pts_sf, 4326)
    xy = sf::st_coordinates(pts_wgs)
    data_pts$lon = xy[,1]
    data_pts$lat = xy[,2]
  }
}

# DPE lettres -> chiffres (A=1 ... G=7)
niv = c("A","B","C","D","E","F","G")
data_pts$dpe_num = match(data_pts$etiquette_dpe, niv)

# centre initial de la vue
if (nrow(data_pts) > 0) {
  lng0 = mean(data_pts$lon, na.rm = TRUE)
  lat0 = mean(data_pts$lat, na.rm = TRUE)
} else {
  lng0 = 4.85; lat0 <- 45.75  # fallback : Lyon
}

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
               h4("Options de filtrage"),
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
  
  # --- Onglet 5 : Carte (NOUVELLE UI) ---
  tabPanel(" Carte",
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "mode_carte",
                 label   = "Affichage :",
                 choices = c("Points (coordonnées)" = "points",
                             "Moyenne par code postal" = "cp"),
                 selected = "points"
               ),
               radioButtons(
                 inputId = "mesure_carte",
                 label   = "Mesure :",
                 choices = c("Conso moyenne (kWh/m²/an)" = "conso",
                             "DPE moyen (A=1 … G=7)"   = "dpe"),
                 selected = "conso"
               )
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
  
  # ---------- Carte : Leaflet (NOUVELLE LOGIQUE) ----------
  
  # palette simple
  make_pal = function(values) {
    rng = range(values, na.rm = TRUE)
    if (!is.finite(rng[1]) || rng[1] == rng[2]) rng = c(0, 1)
    leaflet::colorNumeric(
      palette  = c("#f7fbff", "#6baed6", "#2171b5", "#08306b"),
      domain   = rng,
      na.color = "#d9d9d9"
    )
  }
  
  output$map_rhone = leaflet::renderLeaflet({
    m = leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE))
    m = leaflet::addProviderTiles(m, providers$CartoDB.Positron)
    m = leaflet::setView(m, lng = lng0, lat = lat0, zoom = 9)
    m
  })
  
  observe({
    # mesure choisie
    mesure = input$mesure_carte  # "conso" ou "dpe"
    
    # base de travail : points reprojetés
    df = data_pts
    
    # valeur pour couleur / popup
    if (identical(mesure, "conso")) {
      tmp = df$conso_5_usages_par_m2_ep
      if (is.factor(tmp)) tmp <- as.character(tmp)
      tmp = gsub(",", ".", tmp)
      df$val = suppressWarnings(as.numeric(tmp))
      titre_leg = "kWh/m²/an"
      popup_label = "Conso"
    } else {
      df$val = suppressWarnings(as.numeric(df$dpe_num))
      titre_leg = "DPE moyen (1=A…7=G)"
      popup_label = "DPE"
    }
    
    # agrégat par code postal si demandé
    if (identical(input$mode_carte, "cp")) {
      agg_val = aggregate(val ~ code_postal_ban, data = df, FUN = function(x) mean(x, na.rm = TRUE))
      agg_lonlat = aggregate(cbind(lon, lat) ~ code_postal_ban, data = df, FUN = function(x) mean(x, na.rm = TRUE))
      df = merge(agg_val, agg_lonlat, by = "code_postal_ban", all = TRUE)
      df$popup_cp = as.character(df$code_postal_ban)
    } else {
      df$popup_cp = as.character(df$code_postal_ban)
    }
    
    pal = make_pal(df$val)
    
    m = leaflet::leafletProxy("map_rhone")
    m = leaflet::clearMarkers(m)
    m = leaflet::clearControls(m)
    
    ok  = is.finite(df$lon) & is.finite(df$lat)
    okv = ok & is.finite(df$val)
    kov = ok & !is.finite(df$val)
    
    if (any(okv)) {
      if (identical(input$mode_carte, "points")) {
        m = leaflet::addCircleMarkers(
          map = m,
          lng = df$lon[okv], lat = df$lat[okv],
          radius = 2,
          stroke = FALSE, fillOpacity = 0.8,
          fillColor = pal(df$val[okv]),
          popup = paste0("CP : ", df$popup_cp[okv],
                         "<br>", popup_label, " : ", round(df$val[okv], 1)),
          clusterOptions = leaflet::markerClusterOptions(
            maxClusterRadius = 40,
            spiderfyOnMaxZoom = TRUE,
            zoomToBoundsOnClick = TRUE
          )
        )
      } else {
        m <- leaflet::addCircleMarkers(
          map = m,
          lng = df$lon[okv], lat = df$lat[okv],
          radius = 4,
          stroke = FALSE, fillOpacity = 0.9,
          fillColor = pal(df$val[okv]),
          popup = paste0("CP : ", df$popup_cp[okv],
                         "<br>", popup_label, " : ", round(df$val[okv], 1))
        )
      }
    }
    
    if (any(kov)) {
      if (identical(input$mode_carte, "points")) {
        m = leaflet::addCircleMarkers(
          map = m,
          lng = df$lon[kov], lat = df$lat[kov],
          radius = 2,
          stroke = FALSE, fillOpacity = 0.6,
          fillColor = "#d9d9d9",
          popup = paste0("CP : ", df$popup_cp[kov], "<br>pas de donnée"),
          clusterOptions = leaflet::markerClusterOptions(
            maxClusterRadius = 40,
            spiderfyOnMaxZoom = TRUE,
            zoomToBoundsOnClick = TRUE
          )
        )
      } else {
        m = leaflet::addCircleMarkers(
          map = m,
          lng = df$lon[kov], lat = df$lat[kov],
          radius = 4,
          stroke = FALSE, fillOpacity = 0.6,
          fillColor = "#d9d9d9",
          popup = paste0("CP : ", df$popup_cp[kov], "<br>pas de donnée")
        )
      }
    }
    
    if (any(is.finite(df$val))) {
      m = leaflet::addLegend(
        map = m, position = "bottomright",
        pal = pal, values = df$val[is.finite(df$val)],
        opacity = 0.9, title = titre_leg,
        labFormat = leaflet::labelFormat(digits = 1)
      )
    }
  })
}

# ============================
#        LANCEMENT
# ============================

shinyApp(ui = ui, server = server)
