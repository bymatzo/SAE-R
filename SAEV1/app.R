# --- Packages nécessaires ---
library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

# --- Chargement des données ---
data <- read.csv("C:/Users/darta/OneDrive/Bureau/IUT/2EME Année/SAE-R/data/data.csv")

# --- Interface utilisateur ---
ui <- navbarPage(
  "🌍 Analyse DPE et émissions de CO₂",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Poppins")
  ),
  
  # --- Onglet 1 : Répartition DPE ---
  tabPanel("📊 Répartition DPE",
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
  tabPanel("🌫️ Émissions de CO₂",
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
  tabPanel("🔥 Coût du chauffage",
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
  tabPanel("📈 Conso vs Émission",
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
  
  # --- Onglet 5 : À propos ---
  tabPanel("ℹ️ À propos",
           fluidPage(
             h3("À propos de cette application"),
             p("Cette application Shiny permet d’analyser les performances énergétiques (DPE) et les émissions de CO₂ des logements selon le type d’énergie de chauffage."),
             p("Réalisée avec ", strong("R Shiny"), " et ", strong("ggplot2"), ".")
           )
  )
)

# --- Serveur ---
server <- function(input, output) {
  
  # Fonction pour filtrer selon tous les paramètres
  filter_data <- function(df, energie_sel, batiment_sel, periode_sel) {
    df <- df %>% filter(type_energie_principale_chauffage == energie_sel)
    if(batiment_sel != "Tous") df <- df %>% filter(type_batiment == batiment_sel)
    if(periode_sel != "Tous") df <- df %>% filter(periode_construction == periode_sel)
    return(df)
  }
  
  # Filtrage spécifique pour le boxplot (sans filtre type énergie)
  filter_data_boxplot <- function(df, batiment_sel, periode_sel) {
    if(batiment_sel != "Tous") df <- df %>% filter(type_batiment == batiment_sel)
    if(periode_sel != "Tous") df <- df %>% filter(periode_construction == periode_sel)
    return(df)
  }
  
  # --- Graphique 1 : Répartition DPE ---
  output$graphique_dpe <- renderPlot({
    df_filtre <- filter_data(data, input$energie, input$type_batiment, input$periode_construction) %>%
      count(etiquette_dpe) %>%
      mutate(proportion = n / sum(n) * 100)
    
    df_filtre$etiquette_dpe <- factor(
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
  
  # --- Graphique 2 : Boxplot des émissions CO₂ (sans filtre énergie) ---
  output$graphique_boxplot <- renderPlot({
    df_filtre <- filter_data_boxplot(data, input$type_batiment_boxplot, input$periode_construction_boxplot)
    
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
  
  # --- Graphique 3 : Histogramme coût chauffage (filtrage 95%) ---
  output$graphique_histogramme <- renderPlot({
    df_filtre <- filter_data(data, input$energie_cout, input$type_batiment_cout, input$periode_construction_cout) %>%
      filter(!is.na(cout_chauffage) & is.finite(cout_chauffage))
    
    validate(
      need(nrow(df_filtre) > 0,
           paste("⚠️ Aucune donnée disponible pour", input$energie_cout))
    )
    
    seuil_95 <- quantile(df_filtre$cout_chauffage, 0.95, na.rm = TRUE)
    df_filtre <- df_filtre %>% filter(cout_chauffage <= seuil_95)
    
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
  
  # --- Graphique 4 : Nuage de points conso vs émission (filtrage 95%) ---
  output$graphique_scatter <- renderPlot({
    df_filtre <- filter_data(data, input$energie_scatter, input$type_batiment_scatter, input$periode_construction_scatter) %>%
      filter(!is.na(conso_5_usages_par_m2_ep) & is.finite(conso_5_usages_par_m2_ep)) %>%
      filter(!is.na(emission_ges_5_usages_par_m2) & is.finite(emission_ges_5_usages_par_m2))
    
    validate(
      need(nrow(df_filtre) > 0,
           paste("⚠️ Aucune donnée disponible pour", input$energie_scatter))
    )
    
    # Filtrage 95% pour X et Y
    seuil_conso <- quantile(df_filtre$conso_5_usages_par_m2_ep, 0.95, na.rm = TRUE)
    seuil_ges <- quantile(df_filtre$emission_ges_5_usages_par_m2, 0.95, na.rm = TRUE)
    
    df_filtre <- df_filtre %>%
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
}

# --- Lancement de l'application ---
shinyApp(ui = ui, server = server)
