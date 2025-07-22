# survey-analysis-app
A Shiny app for interactive survey analysis, featuring filtering tools and profile analysis based on factorial technique such as MCA.

Ce projet a été réalisé dans le cadre de mon stage au service commun de la documentation de mon Université.

Cette application est une application web interactive développée avec Shiny (R), permettant d'explorer et d’analyser les résultats de questionnaires. Elle propose des outils pour :
  la visualisation des distributions de réponses,
  des analyses univariées et bivariées,
  une analyse de profils via des méthodes factorielles telles que l’Analyse des Correspondances Multiples (ACM).


Les données d'enquête sont souvent complexes et volumineuses. Cette application facilite leur compréhension en :

offrant des visualisations claires et personnalisables, permettant de filtrer les réponses pour réaliser des analyses ciblées. On retrouve aussi un outil permettant la mise en oeuvre de profil de répondants ou des logiques de réponses communes.

Pour utiliser l'application : https://celiapy.shinyapps.io/analyse_questionnaire/
Pour la faire tourner en local :

Installez R et RStudio sur votre machine.
Installez les packages nécessaires : 
install.packages(c("readxl","ggplot2","FactoMineR","factoextra","dplyr","tidyr", "scales",  "plotly", "visdat", "missMDA", "shiny", "readr", "ggpie", "stringr","shinydashboard","purrr","quanteda.textplots","quanteda.textstats","quanteda","readtext", "igraph","wordcloud","shinyWidgets"))

Ouvrez les fichiers ui.R et server.R et lancez l’application avec :
shiny::runApp()

