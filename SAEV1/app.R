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

# --- Chargement et préparation des données ---
data = read.csv(
  "https://raw.githubusercontent.com/bymatzo/SAE-R/refs/heads/main/data/data.csv",
  sep = ",", dec = "."
)

# Convertit des colonnes en numérique
to_num = function(v) {
  if (is.factor(v)) v = as.character(v)
  v = gsub(",", ".", v)
  suppressWarnings(as.numeric(v))
}

# X/Y brutes pour la carte
x_raw = to_num(data$coordonnee_cartographique_x_ban)
y_raw = to_num(data$coordonnee_cartographique_y_ban)

# Filtrage des lignes valides
ok0 = is.finite(x_raw) & is.finite(y_raw)
data_pts = data[ok0, , drop = FALSE]

# Convertit les coordonnées en degrés (WGS84)
if (nrow(data_pts) > 0) {
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

# Convertit lettres DPE → chiffres
niv = c("A","B","C","D","E","F","G")
data_pts$dpe_num = match(data_pts$etiquette_dpe, niv)

# Centre carte
if (nrow(data_pts) > 0) {
  lng0 = mean(data_pts$lon, na.rm = TRUE)
  lat0 = mean(data_pts$lat, na.rm = TRUE)
} else {
  lng0 = 4.85
  lat0 = 45.75  
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
  
  # --- Onglet 1 ---
  tabPanel("Répartition DPE",
           sidebarLayout(
             sidebarPanel(
               h4("Options de filtrage"),
               selectInput("energie", "Type d’énergie principale :", 
                           choices = sort(unique(data$type_energie_principale_chauffage)),
                           selected = unique(data$type_energie_principale_chauffage)[1]),
               selectInput("type_batiment", "Type de bâtiment :", 
                           choices = c("Tous", sort(unique(data$type_batiment))),
                           selected = "Tous"),
               selectInput("periode_construction", "Période de construction :", 
                           choices = c("Tous", sort(unique(data$periode_construction))),
                           selected = "Tous"),
               checkboxGroupInput("dpe_filtre", "Étiquettes DPE :", 
                                  choices = c("A","B","C","D","E","F","G"),
                                  selected = c("A","B","C","D","E","F","G")),
               hr(),
               p("Ce graphique affiche la répartition des classes DPE.")
             ),
             mainPanel(
               plotOutput("graphique_dpe", height = "500px")
             )
           )
  ),
  
  # --- Onglet 2 ---
  tabPanel("Émissions de CO₂",
           sidebarLayout(
             sidebarPanel(
               selectInput("type_batiment_boxplot","Type de bâtiment :", 
                           choices = c("Tous", sort(unique(data$type_batiment))),
                           selected = "Tous"),
               selectInput("periode_construction_boxplot","Période de construction :", 
                           choices = c("Tous", sort(unique(data$periode_construction))),
                           selected = "Tous"),
               hr(),
               uiOutput("kpi_moy_co2"),
               uiOutput("kpi_med_co2")
             ),
             mainPanel(
               plotOutput("graphique_boxplot", height = "550px")
             )
           )
  ),
  
  # --- Onglet 3 ---
  tabPanel("Coût du chauffage",
           sidebarLayout(
             sidebarPanel(
               selectInput("energie_cout", "Type d’énergie :", 
                           choices = sort(unique(data$type_energie_principale_chauffage)),
                           selected = unique(data$type_energie_principale_chauffage)[1]),
               selectInput("type_batiment_cout","Type de bâtiment :", 
                           choices = c("Tous", sort(unique(data$type_batiment))),
                           selected = "Tous"),
               selectInput("periode_construction_cout","Période de construction :", 
                           choices = c("Tous", sort(unique(data$periode_construction))),
                           selected = "Tous"),
               sliderInput("filtre_cout_max", "Limiter le coût :", 
                           min = 0, max = 10000, value = 10000),
               hr(),
               uiOutput("kpi_moy"), uiOutput("kpi_med")
             ),
             mainPanel(
               plotOutput("graphique_histogramme", height = "550px")
             )
           )
  ),
  
  # --- Onglet 4 (existant) ---
  tabPanel("Conso vs Émission",
           sidebarLayout(
             sidebarPanel(
               selectInput("energie_scatter","Énergie :",choices = sort(unique(data$type_energie_principale_chauffage))),
               selectInput("type_batiment_scatter","Bâtiment :",choices = c("Tous", sort(unique(data$type_batiment))),selected = "Tous"),
               selectInput("periode_construction_scatter","Période :",choices = c("Tous", sort(unique(data$periode_construction))),selected = "Tous"),
               hr(),
               uiOutput("kpi_corr"),
               hr(),
               p("Nuage conso vs CO₂ filtrable.")
             ),
             mainPanel(plotOutput("graphique_scatter", height = "550px"))
           )
  ),
  
  # --- Onglet 5 ---
  tabPanel("Carte",
           sidebarLayout(
             sidebarPanel(
               radioButtons("mode_carte","Affichage :",choices = c("Points"="points","Par code postal"="cp"),selected="points"),
               radioButtons("mesure_carte","Mesure :",choices = c("Conso"="conso","DPE"="dpe"),selected="conso")
             ),
             mainPanel(
               leafletOutput("map_rhone", height = "650px")
             )
           )
  ),
  
  # --- ONGLET 7 : NOUVEAU SCATTER DYNAMIQUE ---
  tabPanel("Nuage X/Y",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "var_x_dyn",
                 label = "Variable X :",
                 choices = c(
                   "Consommation (kWh/m²/an)" = "conso_5_usages_par_m2_ep",
                   "Émissions CO₂ (kgCO₂/m²/an)" = "emission_ges_5_usages_par_m2",
                   "Coût chauffage (€ / an)" = "cout_chauffage",
                   "Surface habitable (m²)" = "surface_habitable_logement"
                 )
               ),
               
               selectInput(
                 inputId = "var_y_dyn",
                 label = "Variable Y :",
                 choices = c(
                   "Consommation (kWh/m²/an)" = "conso_5_usages_par_m2_ep",
                   "Émissions CO₂ (kgCO₂/m²/an)" = "emission_ges_5_usages_par_m2",
                   "Coût chauffage (€ / an)" = "cout_chauffage",
                   "Surface habitable (m²)" = "surface_habitable_logement"
                 )
               ),
               
               selectInput(
                 inputId = "energie_dyn",
                 label = "Énergie :",
                 choices = c("Toutes", sort(unique(data$type_energie_principale_chauffage))),
                 selected = "Toutes"
               ),
               
               selectInput(
                 inputId = "bat_dyn",
                 label = "Type de bâtiment :",
                 choices = c("Tous", sort(unique(data$type_batiment))),
                 selected = "Tous"
               ),
               
               selectInput(
                 inputId = "periode_dyn",
                 label = "Période de construction :",
                 choices = c("Tous", sort(unique(data$periode_construction))),
                 selected = "Tous"
               ),
               
               hr(),
               uiOutput("kpi_corr_dyn"),
               hr(),
               p("Nuage de points dynamique avec filtrage complet et suppression automatique des 5% extrêmes.")
             ),
             mainPanel(
               uiOutput("kpi_corr_dyn"),
               plotOutput("graphique_scatter_dyn", height = "550px")
             )
           )
  ),
  
  tabPanel("À propos",
           fluidPage(
             h3("À propos de cette application"),
             p("Application Shiny d’analyse des DPE et émissions CO₂.")
           )
  )
)

# ============================
#          SERVER
# ============================

server = function(input, output, session) {
  
  # --- Fonctions de filtrage ---
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
  
  # ---------- Graphique 1 ----------
  output$graphique_dpe = renderPlot({
    df_filtre = filter_data(data, input$energie, input$type_batiment, input$periode_construction) %>%
      filter(etiquette_dpe %in% input$dpe_filtre) %>% 
      count(etiquette_dpe) %>%
      mutate(proportion = n / sum(n) * 100)
    
    df_filtre$etiquette_dpe = factor(df_filtre$etiquette_dpe, levels = c("A","B","C","D","E","F","G"))
    
    ggplot(df_filtre, aes(x = etiquette_dpe, y = proportion, fill = etiquette_dpe)) +
      geom_col(width = 0.7, color = "white", linewidth = 0.5) +
      geom_text(aes(label = paste0(round(proportion, 1), "%")), vjust = -0.5, size = 5, color = "black", fontface = "bold") +
      scale_fill_manual(values = c("A"="#009E3D","B"="#6DBE45","C"="#FFF200","D"="#F7A600","E"="#E87511","F"="#E30613","G"="#B60000")) +
      labs(title = paste("Répartition des classes DPE :", input$energie),
           x = "Classe DPE", y = "Proportion (%)") +
      theme_minimal()
  })
  
  # ---------- Graphique 2 ----------
  moyenne_co2 <- reactiveVal(NA)
  mediane_co2 <- reactiveVal(NA)
  output$graphique_boxplot = renderPlot({
    df_filtre = filter_data_boxplot(data, input$type_batiment_boxplot, input$periode_construction_boxplot)
    
    y = suppressWarnings(as.numeric(gsub(",", ".", as.character(df_filtre$emission_ges_5_usages_par_m2))))
    ok = is.finite(y)
    df_filtre = df_filtre[ok, ]
    y = y[ok]
    
    moyenne_co2(round(mean(y), 1))
    mediane_co2(round(median(y), 1))
    
    ggplot(df_filtre, aes(x = type_energie_principale_chauffage, y = y, fill = type_energie_principale_chauffage)) +
      geom_boxplot(outlier.colour = "red", alpha = 0.7) +
      ylim(0, 130) +
      labs(title = "Émissions de CO₂ par énergie", x = "", y = "kgCO₂/m²/an") +
      theme_minimal()
  })
  
  output$kpi_moy_co2 <- renderUI(HTML(paste("Moyenne : <b>", moyenne_co2(), "</b>")))
  output$kpi_med_co2 <- renderUI(HTML(paste("Médiane : <b>", mediane_co2(), "</b>")))
  
  # ---------- Graphique 3 ----------
  moyenne_cout <- reactiveVal(NA)
  mediane_cout <- reactiveVal(NA)
  
  output$graphique_histogramme = renderPlot({
    df_filtre = filter_data(data, input$energie_cout, input$type_batiment_cout, input$periode_construction_cout)
    
    x = suppressWarnings(as.numeric(gsub(",", ".", as.character(df_filtre$cout_chauffage))))
    ok = is.finite(x)
    df_filtre = df_filtre[ok, ]
    x = x[ok]
    
    if (!is.null(input$filtre_cout_max)) {
      ok2 = x <= input$filtre_cout_max
      df_filtre = df_filtre[ok2, ]
      x = x[ok2]
    }
    
    seuil_95 = quantile(x, 0.95, na.rm = TRUE)
    ok3 = x <= seuil_95
    df_filtre = df_filtre[ok3, ]
    x = x[ok3]
    
    df_filtre$cout_num = x
    moyenne_cout(round(mean(x), 1))
    mediane_cout(round(median(x), 1))
    
    ggplot(df_filtre, aes(x = cout_num)) +
      geom_histogram(bins = 30, fill = "#2E86AB", color = "white") +
      labs(title = "Répartition du coût du chauffage", x = "Coût (€ / an)", y = "Nombre de logements") +
      theme_minimal()
  })
  
  output$kpi_moy <- renderUI(HTML(paste("Moyenne : <b>", moyenne_cout(), "€</b>")))
  output$kpi_med <- renderUI(HTML(paste("Médiane : <b>", mediane_cout(), "€</b>")))
  
  # ---------- Graphique 4 (existant) ----------
  correlation_scatter <- reactiveVal(NA)
  
  output$graphique_scatter = renderPlot({
    df_filtre = filter_data(data, input$energie_scatter, input$type_batiment_scatter, input$periode_construction_scatter)
    
    cx = suppressWarnings(as.numeric(gsub(",", ".", as.character(df_filtre$conso_5_usages_par_m2_ep))))
    cy = suppressWarnings(as.numeric(gsub(",", ".", as.character(df_filtre$emission_ges_5_usages_par_m2))))
    
    ok = is.finite(cx) & is.finite(cy)
    df_filtre = df_filtre[ok, ]
    cx = cx[ok]; cy = cy[ok]
    
    seuil_conso = quantile(cx, 0.95, na.rm = TRUE)
    seuil_ges = quantile(cy, 0.95, na.rm = TRUE)
    
    ok2 = (cx <= seuil_conso) & (cy <= seuil_ges)
    df_filtre2 = df_filtre[ok2, ]
    cx2 = cx[ok2]; cy2 = cy[ok2]
    
    df_filtre2$conso_num = cx2
    df_filtre2$ges_num = cy2
    correlation_scatter(round(cor(cx2, cy2), 3))
    
    ggplot(df_filtre2, aes(x = conso_num, y = ges_num)) +
      geom_point(aes(color = "Points"), alpha = 0.7) +
      geom_smooth(aes(color = "Régression"), method = "lm", se = FALSE) +
      scale_color_manual(values = c("Points" = "#E74C3C", "Régression" = "blue")) +
      labs(title = paste("Conso vs Émissions pour", input$energie_scatter),
           x = "kWh/m²/an", y = "kgCO₂/m²/an") +
      theme_minimal()
  })
  
  output$kpi_corr <- renderUI(HTML(paste("Corrélation : <b>", correlation_scatter(), "</b>")))
  
  # ---------- NOUVEAU GRAPHIQUE 7 : SCATTER DYNAMIQUE ----------
  output$graphique_scatter_dyn = renderPlot({
    df = data
    
    # --- Filtre énergie ---
    if (input$energie_dyn != "Toutes") {
      df = df %>% filter(type_energie_principale_chauffage == input$energie_dyn)
    }
    
    # --- Filtre bâtiment ---
    if (input$bat_dyn != "Tous") {
      df = df %>% filter(type_batiment == input$bat_dyn)
    }
    
    # --- Filtre période ---
    if (input$periode_dyn != "Tous") {
      df = df %>% filter(periode_construction == input$periode_dyn)
    }
    
    # --- Extraction X / Y ---
    vx = suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[input$var_x_dyn]]))))
    vy = suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[input$var_y_dyn]]))))
    
    ok = is.finite(vx) & is.finite(vy)
    df2 = df[ok, ]
    df2$vx = vx[ok]
    df2$vy = vy[ok]
    
    # --- Suppression des 5% extrêmes ---
    qx = quantile(df2$vx, probs = c(0.05, 0.95), na.rm = TRUE)
    qy = quantile(df2$vy, probs = c(0.05, 0.95), na.rm = TRUE)
    
    df2 = df2 %>%
      filter(vx >= qx[1], vx <= qx[2],
             vy >= qy[1], vy <= qy[2])
    
    # --- Calcul corrélation ---
    corr = suppressWarnings(cor(df2$vx, df2$vy))
    corr = round(corr, 3)
    output$kpi_corr_dyn <- renderUI(HTML(paste("Corrélation : <b>", corr, "</b>")))
    
    # --- Graphique ---
    if (input$energie_dyn == "Toutes") {
      ggplot(df2, aes(x = vx, y = vy)) +
        geom_point(alpha = 0.6, color = "#2E86AB") +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        labs(
          title = "Nuage X/Y (toutes énergies confondues)",
          x = input$var_x_dyn,
          y = input$var_y_dyn
        ) +
        theme_minimal()
    } else {
      ggplot(df2, aes(x = vx, y = vy, color = type_energie_principale_chauffage)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(
          title = paste("Nuage X/Y -", input$energie_dyn),
          x = input$var_x_dyn,
          y = input$var_y_dyn,
          color = "Énergie"
        ) +
        theme_minimal()
    }
  })
  
  # ---------- Carte ----------
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
  
}

# ============================
#        LANCEMENT
# ============================

shinyApp(ui = ui, server = server)

