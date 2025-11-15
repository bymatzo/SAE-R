# ========
# PACKAGES
# ========

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

# =======
# MODULES
# =======

# --- Téléchargement PNG ---
downloadPlotUI <- function(id, label = "Télécharger en PNG") {
  ns <- NS(id)
  div(style="display:flex;justify-content:flex-end;margin-top:8px;",
      downloadButton(ns("download"), label, class="btn btn-primary"))
}

downloadPlotServer <- function(id, plot_expr, filename="graph.png", width=1200, height=900, dpi=120) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() filename,
      content  = function(file) {
        p <- plot_expr(); if (is.null(p)) return()
        ggplot2::ggsave(filename=file, plot=p, device="png", width=width/dpi, height=height/dpi, dpi=dpi, units="in")
      })
  })
}

# --- Export CSV ---
downloadDataUI <- function(id, label = "Exporter en CSV") {
  ns <- NS(id)
  div(style = "display:flex; justify-content:flex-end; margin-top:6px;",
      downloadButton(ns("download"), label, class = "btn btn-secondary"))
}

downloadDataServer <- function(id, data_expr, filename = "data.csv") {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() filename,
      content = function(file) {
        df <- data_expr()
        if (is.null(df)) return()
        if (!is.data.frame(df)) df <- as.data.frame(df)
        write.csv(df, file, row.names = FALSE)
      })
  })
}

# ====
# DATA
# ====

# --- Chargement des données ---
data = read.csv(
  "https://raw.githubusercontent.com/bymatzo/SAE-R/refs/heads/main/data/data.csv",
  sep = ",", dec = "."
)

# --- Préparation des données --
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

# Convertit les coordonnées en degrés (WGS84) si elles sont en Lambert 93
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

# Convertit lettres DPE en chiffres (A=1 ... G=7)
niv = c("A","B","C","D","E","F","G")
data_pts$dpe_num = match(data_pts$etiquette_dpe, niv)

# Initialisation du centre de la vue
if (nrow(data_pts) > 0) {
  lng0 = mean(data_pts$lon, na.rm = TRUE)
  lat0 = mean(data_pts$lat, na.rm = TRUE)
} else {
  lng0 = 4.85
  lat0 = 45.75  
}

# Initialise ordre d'apparition des périodes
periodes <- sort(unique(data$periode_construction))
periodes <- c(periodes[grepl("^avant", periodes, ignore.case = TRUE)],
              periodes[!grepl("^avant", periodes, ignore.case = TRUE)])


# ==
# UI
# ==

ui = navbarPage(
  "Analyse DPE et émissions de CO2",
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
                 choices = c("Tous", periodes),
                 selected = "Tous"
               ),
               checkboxGroupInput(
                 inputId = "dpe_filtre",
                 label = "Étiquettes DPE à afficher :",
                 choices = c("A","B","C","D","E","F","G"),
                 selected = c("A","B","C","D","E","F","G")
               ),
               hr(),
               p("Ce graphique affiche la répartition en pourcentage des classes DPE pour le type d’énergie sélectionné.")
             ),
             mainPanel(
               plotOutput("graphique_dpe", height = "500px"),
               downloadPlotUI("dl_dpe"),
               downloadDataUI("dl_dpe_data")
             )
           )
  ),
  
  # --- Onglet 2 : Boxplot des émissions CO2 ---
  tabPanel("Émissions de CO2",
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
                 choices = c("Tous", periodes),
                 selected = "Tous"
               ),
               hr(),
               uiOutput("kpi_moy_co2"),
               uiOutput("kpi_med_co2"),
             ),
             mainPanel(
               plotOutput("graphique_boxplot", height = "550px"),
               downloadPlotUI("dl_boxplot"),
               downloadDataUI("dl_boxplot_data")
             )
           )
  ),
  
  # --- Onglet 3 : Coût du chauffage ---
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
                 choices = c("Tous", periodes),
                 selected = "Tous"
               ),
               sliderInput(
                 "filtre_cout_max",
                 "Limiter le coût du chauffage :",
                 min = 0,
                 max = 10000,
                 value = 10000
               ),
               hr(),
               uiOutput("kpi_moy"),
               uiOutput("kpi_med"),
               hr(),
               p("Cet histogramme montre la distribution du coût de chauffage (€ / an) pour le type d’énergie sélectionné (avec suppression des 5 % des valeurs les plus élevées).")
             ),
             mainPanel(
               plotOutput("graphique_histogramme", height = "550px"),
               downloadPlotUI("dl_histogramme"),
               downloadDataUI("dl_histogramme_data")
             )
           )
  ),
  
  # --- Onglet 4 : Nuage de points consommation vs émission ---
  tabPanel("Conso vs Émission",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "var_x",
                 "Variable X :",
                 choices = c(
                   "Consommation (kWh/m²/an)" = "conso_5_usages_par_m2_ep",
                   "Émissions CO2 (kgCO2/m²/an)" = "emission_ges_5_usages_par_m2",
                   "Surface habitable" = "surface_habitable_logement"
                 ),
                 selected = "conso_5_usages_par_m2_ep"
               ),
               selectInput(
                 "var_y",
                 "Variable Y :",
                 choices = c(
                   "Émissions CO2 (kgCO2/m²/an)" = "emission_ges_5_usages_par_m2",
                   "Consommation (kWh/m²/an)" = "conso_5_usages_par_m2_ep",
                   "Surface habitable" = "surface_habitable_logement"
                 ),
                 selected = "emission_ges_5_usages_par_m2"
               ),
               hr(),
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
                 choices = c("Tous", periodes),
                 selected = "Tous"
               ),
               hr(),
               uiOutput("kpi_corr"),
               hr(),
               p("Nuage de points : consommation d'énergie (kWh/m²/an) vs émissions de CO2 (kgCO2/m²/an) filtré par type d’énergie, type de bâtiment et période de construction.")
             ),
             mainPanel(
               plotOutput("graphique_scatter", height = "550px"),
               downloadPlotUI("dl_scatter"),
               downloadDataUI("dl_scatter_data")
             )
           )
  ),
  
  # --- Onglet 5 : Carte  ---
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
  
  # --- Onglet 6 : A propos ---
  tabPanel("A propos",
           fluidPage(
             h3("À propos de cette application"),
             p("Cette application Shiny permet d’analyser les performances énergétiques (DPE) et les émissions de CO2 des logements selon le type d’énergie de chauffage."),
             p("Réalisée avec ", strong("R Shiny"), " et ", strong("ggplot2"), ".")
           )
  ),
  
  # --- Onglet 7 : Paramètres ---
  tabPanel("Paramètres",
           fluidPage(
             h3("Paramètres"),
             p("Cette page sera utilisée pour configurer l'application.")
           )
  )
  
)

# ======
# SERVER
# ======

server = function(input, output, session) {
  
  # ---Fonctions de filtrage ---
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
  
  # --- Graphique 1 : Barplot répartition DPE ---
  plot_dpe <- reactive({
    df_filtre <- filter_data(data, input$energie, input$type_batiment, input$periode_construction) %>%
      filter(etiquette_dpe %in% input$dpe_filtre) %>% 
      count(etiquette_dpe) %>%
      mutate(proportion = n / sum(n) * 100)
    
    df_filtre$etiquette_dpe <- factor(
      df_filtre$etiquette_dpe,
      levels = c("A","B","C","D","E","F","G")
    )
    
    p <- ggplot(df_filtre, aes(x = etiquette_dpe, y = proportion, fill = etiquette_dpe)) +
      geom_col(width = 0.7, color = "white", linewidth = 0.5) +
      geom_text(aes(label = paste0(round(proportion, 1), "%")),
                vjust = -0.5, size = 5, color = "black", fontface = "bold") +
      scale_fill_manual(
        name = "Etiquette DPE",
        values = c(
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
    
    return(p)
  })
  
  output$graphique_dpe <- renderPlot(plot_dpe())
  downloadPlotServer("dl_dpe", plot_expr = plot_dpe, filename = "dpe.png")
  downloadDataServer("dl_dpe_data",
                     data_expr = function() {
                       df <- filter_data(data, input$energie, input$type_batiment, input$periode_construction)
                       df <- df[df$etiquette_dpe %in% input$dpe_filtre, ]
                       df[, c("type_energie_principale_chauffage", "type_batiment","periode_construction", "etiquette_dpe")]
                     },filename = "dpe_data.csv")
  
  # --- Graphique 2 : Boxplot emission CO2 ---
  moyenne_co2 <- reactiveVal(NA)
  mediane_co2 <- reactiveVal(NA)
  
  plot_boxplot <- reactive({
    df_filtre = filter_data_boxplot(data, input$type_batiment_boxplot, input$periode_construction_boxplot)
    
    y = df_filtre$emission_ges_5_usages_par_m2
    if (is.factor(y)) y = as.character(y)
    y = suppressWarnings(as.numeric(gsub(",", ".", y)))
    
    ok = is.finite(y)
    df_filtre = df_filtre[ok, , drop = FALSE]
    y = y[ok]
    
    moyenne_co2(round(mean(y), 1))
    mediane_co2(round(median(y), 1))
    
    ggplot(df_filtre, aes(x = type_energie_principale_chauffage, 
                          y = emission_ges_5_usages_par_m2,
                          fill = type_energie_principale_chauffage)) +
      geom_boxplot(outlier.colour = "red", alpha = 0.7) +
      ylim(0, 130) + 
      labs(
        title = "Distribution des émissions de CO2 par type d’énergie principale",
        x = "Type d’énergie principale de chauffage",
        y = "Émissions de CO2 (kgCO2/m²/an)"
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
  
  output$graphique_boxplot <- renderPlot(plot_boxplot())
  downloadPlotServer("dl_boxplot", plot_expr = plot_boxplot, filename = "boxplot_co2.png")
  downloadDataServer("dl_boxplot_data",
                     data_expr = function() {
                       df <- filter_data_boxplot(data, input$type_batiment_boxplot, input$periode_construction_boxplot)
                       df$emission_num <- suppressWarnings(as.numeric(gsub(",", ".", df$emission_ges_5_usages_par_m2)))
                       df <- df[is.finite(df$emission_num), ]
                       df[, c("type_energie_principale_chauffage", "emission_num","type_batiment", "periode_construction")]
                     },filename = "co2_data.csv")
  
  output$kpi_moy_co2 <- renderUI({HTML(paste("Moyenne : <b>", moyenne_co2(), " kgCO2/m²/an</b>"))})
  output$kpi_med_co2 <- renderUI({HTML(paste("Médiane : <b>", mediane_co2(), " kgCO2/m²/an</b>"))})
  
  # --- Graphique 3 : Histogramme coût du chauffage ---
  moyenne_cout <- reactiveVal(NA)
  mediane_cout <- reactiveVal(NA)
  
  plot_histogramme <- reactive({
    df_filtre = filter_data(
      data,
      input$energie_cout,
      input$type_batiment_cout,
      input$periode_construction_cout
    )
    
    x = df_filtre$cout_chauffage
    if (is.factor(x)) x = as.character(x)
    x = gsub(",", ".", x)
    x = suppressWarnings(as.numeric(x))
    
    ok = is.finite(x)
    df_filtre = df_filtre[ok, , drop = FALSE]
    x = x[ok]
    
    if (!is.null(input$filtre_cout_max))
      x <- x[x <= input$filtre_cout_max]
    
    seuil_95 = quantile(x, 0.95, na.rm = TRUE)
    x2 = x[x <= seuil_95]
    moyenne_cout(round(mean(x2), 1))
    mediane_cout(round(median(x2), 1))
    
    df_filtre2 = df_filtre[df_filtre$cout_chauffage <= seuil_95, , drop = FALSE]
    df_filtre2$cout_num = x2
    
    ggplot(df_filtre2, aes(x = cout_num)) +
      geom_histogram(bins = 30, fill = "#2E86AB", color = "white", alpha = 0.8) +
      labs(title = paste("Répartition du coût de chauffage pour", input$energie_cout),
           x = "Coût du chauffage (€ / an)",
           y = "Nombre de logements") +
      theme_minimal()
  })
  
  output$graphique_histogramme <- renderPlot(plot_histogramme())
  downloadPlotServer("dl_histogramme", plot_expr = plot_histogramme, filename = "histogramme_cout.png")
  downloadDataServer("dl_histogramme_data",
                     data_expr = function() {
                       df <- filter_data(data, input$energie_cout, input$type_batiment_cout, input$periode_construction_cout)
                       df$cout_num <- suppressWarnings(as.numeric(gsub(",", ".", df$cout_chauffage)))
                       df <- df[is.finite(df$cout_num) & df$cout_num <= input$filtre_cout_max, ]
                       seuil <- quantile(df$cout_num, 0.95, na.rm = TRUE)
                       df <- df[df$cout_num <= seuil, ]
                       df[, c("type_energie_principale_chauffage", "type_batiment", "periode_construction", "cout_num")]
                      },filename = "cout_chauffage_data.csv")
  
  output$kpi_moy <- renderUI({HTML(paste("Moyenne : <b>", moyenne_cout(), "€</b>"))})
  output$kpi_med <- renderUI({HTML(paste("Médiane : <b>", mediane_cout(), "€</b>"))})
  
  # --- Graphique 4 : Nuage de points consommation vs émission ---
  correlation_scatter <- reactiveVal(NA)
  
  plot_scatter <- reactive({
    df_filtre = filter_data(
      data,
      input$energie_scatter,
      input$type_batiment_scatter,
      input$periode_construction_scatter
    )
    
    # valeurs numériques
    cx = suppressWarnings(as.numeric(gsub(",", ".", df_filtre[[input$var_x]])))
    cy = suppressWarnings(as.numeric(gsub(",", ".", df_filtre[[input$var_y]])))
    
    ok = is.finite(cx) & is.finite(cy)
    cx = cx[ok]; cy = cy[ok]
    
    # filtrage 95 %
    seuil_x = quantile(cx, 0.95, na.rm = TRUE)
    seuil_y = quantile(cy, 0.95, na.rm = TRUE)
    
    ok2 = cx <= seuil_x & cy <= seuil_y
    cx2 = cx[ok2]; cy2 = cy[ok2]
    
    # calcul corrélation
    correlation_scatter(round(cor(cx2, cy2), 3))
    df_plot <- data.frame(x = cx2,y = cy2)
    
    ggplot(df_plot, aes(x = x, y = y)) +
      geom_point(color = "#E74C3C", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        title = paste("Relation entre", input$var_x, "et", input$var_y),
        x = input$var_x,
        y = input$var_y
      ) +
      theme_minimal()
  })
  
  
  output$graphique_scatter <- renderPlot(plot_scatter())
  downloadPlotServer("dl_scatter", plot_expr = plot_scatter, filename = "scatter_conso_co2.png")
  downloadDataServer("dl_scatter_data",
                     data_expr = function() {
                       df <- filter_data(data, input$energie_scatter, input$type_batiment_scatter, input$periode_construction_scatter)
                       df$x <- suppressWarnings(as.numeric(gsub(",", ".", df[[input$var_x]])))
                       df$y <- suppressWarnings(as.numeric(gsub(",", ".", df[[input$var_y]])))
                       df <- df[is.finite(df$x) & is.finite(df$y), ]
                       seuil_x <- quantile(df$x, 0.95, na.rm = TRUE)
                       seuil_y <- quantile(df$y, 0.95, na.rm = TRUE)
                       df <- df[df$x <= seuil_x & df$y <= seuil_y, ]
                       df[[input$var_x]] <- df$x
                       df[[input$var_y]] <- df$y
                       df[, c("type_energie_principale_chauffage","type_batiment","periode_construction", input$var_x, input$var_y)]
                     }, filename = "scatter_data.csv")
  
  output$kpi_corr <- renderUI({HTML(paste("Corrélation : <b>", correlation_scatter(), "</b>"))})

  # --- Carte ---
  
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
    mesure = input$mesure_carte  # "conso" ou "dpe"
    df = data_pts
    
    if (identical(mesure, "conso")) {
      tmp = df$conso_5_usages_par_m2_ep
      if (is.factor(tmp)) tmp = as.character(tmp)
      tmp = gsub(",", ".", tmp)
      df$val = suppressWarnings(as.numeric(tmp))
      titre_leg = "kWh/m²/an"
      popup_label = "Conso"
    } else {
      df$val = suppressWarnings(as.numeric(df$dpe_num))
      titre_leg = "DPE moyen (1=A…7=G)"
      popup_label = "DPE"
    }
    
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
        m = leaflet::addCircleMarkers(
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

# =========
# LANCEMENT
# =========

shinyApp(ui = ui, server = server)
