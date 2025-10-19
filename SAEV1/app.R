# --- Packages nécessaires ---
library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)   # pour un thème moderne

# --- Chargement des données ---
data <- read.csv("C:/Users/darta/OneDrive/Bureau/IUT/2EME Année/SAE-R/data/data.csv")

# --- Interface utilisateur ---
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",   
    base_font = font_google("Poppins")
  ),
  
  titlePanel("🌍 Analyse des classes DPE selon le type d’énergie"),
  
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
    
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Répartition DPE",
                 br(),
                 plotOutput("graphique_dpe", height = "500px")
        ),
        tabPanel("ℹ️ À propos",
                 br(),
                 p("Cette application permet de visualiser la performance énergétique des logements selon leur type d’énergie de chauffage."),
                 p("Données issues de :", strong("data.csv")),
                 p("Réalisée avec ", strong("R Shiny"), " et ", strong("ggplot2"), ".")
        )
      )
    )
  )
)

# --- Serveur ---
server <- function(input, output) {
  
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
      )) +
      labs(
        title = paste("Répartition des classes DPE pour le chauffage :", input$energie),
        x = "Classe DPE",
        y = "Proportion (%)",
        fill = "Classe DPE"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        panel.grid.major.x = element_blank()
      ) +
      ylim(0, 100)
  })
}

# --- Lancement de l'application ---
shinyApp(ui = ui, server = server)



