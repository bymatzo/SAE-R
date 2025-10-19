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
               hr(),
               p("Ce graphique affiche la répartition en pourcentage des classes DPE pour le type d’énergie sélectionné.")
             ),

# stat_boxplot ------------------------------------------------------------

             
             mainPanel(
               plotOutput("graphique_dpe", height = "500px")
             )
           )
  ),
  
  # --- Onglet 2 : Boxplot des émissions CO₂ ---
  tabPanel("🌫️ Émissions de CO₂",
           fluidPage(
             h3("Comparaison des émissions de CO₂ selon le type d’énergie principale"),
             p("Ce graphique montre la distribution des émissions de CO₂ (en kgCO₂/m²/an) selon le type d’énergie utilisée pour le chauffage."),
             plotOutput("graphique_boxplot", height = "550px")
           )
  ),
  
  # --- Onglet 3 : À propos ---
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
  
  # --- Graphique 1 : Répartition DPE ---
  output$graphique_dpe <- renderPlot({
    
    df_filtre <- data %>%
      filter(type_energie_principale_chauffage == input$energie) %>%
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
        "A" = "#009E3D",
        "B" = "#6DBE45",
        "C" = "#FFF200",
        "D" = "#F7A600",
        "E" = "#E87511",
        "F" = "#E30613",
        "G" = "#B60000"
      ),
      guide = guide_legend(
        title = "Classe DPE",
        title.position = "top",
        direction = "vertical",
        ncol = 1
      )) +
      labs(
        title = paste("Répartition des classes DPE pour le chauffage :", input$energie),
        x = "Classe DPE",
        y = "Proportion (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = c(0.95, 0.75),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.5), color = NA),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank()
      ) +
      ylim(0, 100)
  })
  
  # --- Graphique 2 : Boxplot des émissions CO₂ ---
  output$graphique_boxplot <- renderPlot({
    
    ggplot(data, aes(x = type_energie_principale_chauffage, 
                     y = emission_ges_5_usages_par_m2,
                     fill = type_energie_principale_chauffage)) +
      geom_boxplot(outlier.colour = "red", alpha = 0.7) +
      ylim (0, 130) + 
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
}

# --- Lancement de l'application ---
shinyApp(ui = ui, server = server)





