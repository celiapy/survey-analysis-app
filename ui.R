library(shiny)
library(plotly)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Outil d'analyse"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import du fichier", icon = icon("upload"), tabName = "import_fichier"),
      menuItem("Analyse simple", tabName = "analyse_simple", icon = icon("chart-pie"),
        menuSubItem("Données qualitatives",tabName="simple_quali"),
        menuSubItem("Données quantitatives",tabName="simple_quanti")),
      menuItem("Analyse bivariée", tabName = "analyse_bivariee", icon = icon("chart-bar"),
               menuSubItem("Données qualitatives", tabName = "bivar_quali"),
               menuSubItem("Données quantitatives", tabName = "bivar_quanti")),
      menuItem("Analyse multivariée", tabName = "analyse_profil", icon = icon("user-tag")),
      menuItem("Analyse de texte", tabName="analyse_txt",icon=icon("pen-fancy")),
      menuItem("Documentation", tabName="documentation",icon=icon("book-open"))
    )
  ),
  
  dashboardBody(
    #forcer la hauteur uniforme des valueBox
    tags$head(
      tags$style(HTML("
      .small-box {
        min-height: 120px !important;
        height: 120px !important;
      }
      #éviter les débordements de texte
      .small-box .inner {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      
      .sidebar-menu li a {
      font-size: 18px !important;
    }
      
      /* Changer la couleur de fond de l'en-tête */
      .skin-blue .main-header .navbar {
        background-color: #631313;
      }
      

      /* Changer la couleur de la zone du titre dans l'en-tête */
      .skin-blue .main-header .logo {
        background-color: #631313;  
        color: #fce8f2 !important;
        font-weight: bold;
      }
      
      .skin-blue .main-header .logo:hover {
      background-color: #e63329;
      }
      
      .skin-blue .main-header .sidebar-toggle:hover {
      background-color: #e63329 !important; /* couleur de fond au survol */
      color: #fce8f2 !important;            /* couleur du texte/icône au survol */
      }

      /* Fond de la sidebar */
    .skin-blue .main-sidebar {
      background-color: #ffffff;
    }

    /* Couleur des textes dans la sidebar */
    .skin-blue .main-sidebar .sidebar a {
      color: #631313;
    }
    
    /* Changer la couleur de fond au survol (hover) des éléments de menu */
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li:hover > a {
    background-color: #e63329;  /* couleur au survol */
    }

    /* Changer la couleur de fond de l'élément actif */
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li.active > a {
   background-color: #e63329 !important;  /* couleur de fond pour l'onglet actif */
   color: #fce8f2 !important;
    }
    
    /*  Apparence normale des sous-éléments */
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li > .treeview-menu > li > a {
    background-color: #ffffff;  /* même fond que la sidebar */
    color: #e63329;             /* même texte que les éléments principaux */
    }

    /* Apparence au survol des sous-éléments*/
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li > .treeview-menu > li > a:hover {
    background-color: #e63329;  /* même hover que les éléments principaux */
    color: #fce8f2 !important;
    }

    /*Apparence si sous-élément actif */
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li > .treeview-menu > li.active > a {
    background-color: #e63329 !important;
    color: #fce8f2 !important;
    }
    
    /* Appliquer un fond rouge à l'en-tête de toutes les boxes avec bordure */
    .skin-blue .box.box-solid > .box-header {
    background-color: #e63329 !important;
    border-bottom: 1px solid #b32a20 !important;
    }

    /* Couleur du titre dans l'en-tête */
    .skin-blue .box.box-solid > .box-header .box-title {
    color: #fce8f2 !important;
    font-weight: bold;
    }
    
    /* Modifier la bordure extérieure des boxes*/
    .skin-blue .box.box-solid {
    border: 1px solid #e63329 !important;
    border-radius: 6px; /* facultatif : coins arrondis */
    }

         /*Couleur de la case cochée */
    .skin-blue .checkbox input:checked + span::before,
    .skin-blue .radio input:checked + span::before {
      background-color: #631313 !important; /* fond de la case cochée */
      border-color: #631313 !important;     /* bordure */
    }
    
    /* Couleur du survol */
    .skin-blue .checkbox:hover input + span::before,
    .skin-blue .radio:hover input + span::before {
      border-color: #e63329 !important;
    }


     /* Select Input */
    .selectize-control.single .selectize-input {
      background-color: #ffffff;
      border-color: #e63329;
      color: #631313;
      font-weight: bold;
    }

    .selectize-control.single .selectize-input.focus {
      border-color: #e63329;
      box-shadow: 0 0 3px #e63329;
    }

    .selectize-dropdown {
      background-color: #fff0f0;
      border-color: #e63329;
      color: #631313;
    }
    
   .material-switch > label::before {
      width: 42px !important;
      height: 22px !important;
      transition: 0.3s;
      background-color: #ccc !important;
    }
    .material-switch > label::after {
      content: '';
      position: absolute;
      top: 2px !important;
      left: 22px !important;
      width: 18px !important;
      height: 18px !important;
      background: white !important;
      border-radius: 50%;
      transition: 0.3s;
    }
    .material-switch > input[type='checkbox']:checked + label::after {
      left: 42px !important;
    }
    
    "))
    ),
    
    tabItems(
      tabItem(tabName = "import_fichier",
              fluidRow(
                column(width=12,offset=2,
                  box(title = "Importer un fichier",status="primary",solidHeader = TRUE,width=8,
                      fileInput("fichier","Choisissez un fichier",accept=c(".csv",".xlsx"), multiple = TRUE, placeholder = "Max. 5 fichiers"),
                      uiOutput("choix_feuille_ui")
                )),
                uiOutput("taux_abandon"),
                box(title = "Répartition des valeurs de 'Dernière page'",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("plot_derniere_page")
                )
              )
      ),
      tabItem(tabName = "simple_quali",
              fluidRow(
                uiOutput("qualitative_blocks_ui")
              )
      )
      
      ,
      tabItem(tabName = "simple_quanti",
              tabItem(
                tabName = "simple_quanti",
                fluidRow(
                  box(
                    title = "📊 Données quantitatives : Résumés et visualisations",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,       # <--- rend le box pliable/dépliable
                    collapsed = TRUE,         # <--- démarre replié
                    HTML("
        <p><b>Cette section vous permet d'explorer les variables numériques présentes dans vos données.</b></p>

        <ul>
          <li><b>Histogramme</b> : Généré automatiquement pour chaque variable continue.</li>
          <li><b>Statistique dynamique</b> : Affichage de la médiane en haut à gauche.</li>
        </ul>

        <p><b>🟢 Analyse du taux de satisfaction/préférence (Net Promoter Score)</b></p>
        <p>Un score est généré automatiquement si les données sont sur une échelle de 0 à 5 ou 0 à 10.</p>

        <ul>
          <li><b>Échelle 0 à 10</b> (standard NPS) :
            <ul>
              <li>9-10 : <span style='color:green;'><b>Promoteur</b></span></li>
              <li>7-8 : <span style='color:orange;'><b>Passif</b></span></li>
              <li>0-6 : <span style='color:red;'><b>Détracteur</b></span></li>
            </ul>
          </li>
          <li><b>Échelle 0 à 5</b> (adaptée) :
            <ul>
              <li>5 : <span style='color:green;'><b>Promoteur</b></span></li>
              <li>4 : <span style='color:orange;'><b>Passif</b></span></li>
              <li>0-3 : <span style='color:red;'><b>Détracteur</b></span></li>
            </ul>
          </li>
        </ul>

        <p>Les valeurs numériques sont nettoyées automatiquement avant traitement.
        Le graphique NPS permet d’évaluer visuellement le taux de satisfaction/préférence 
        global selon la formule :</p>
        <blockquote><b>Taux NPS = Taux de Promoteurs − Taux de Détracteurs</b></blockquote>
        
        <p><b>🟢 Classement (Plus grand effectif)</b></p>
        <p>Un graphique affiche les variables ayant reçu le plus grand nombre de notes maximales (5 ou 10), selon l’échelle utilisée. 
Il permet de visualiser les éléments les plus plébiscités par les répondants.</p>

<ul>
  <li>Les notes autres que la valeur maximale sont ignorées dans ce classement.</li>
  <li>Si aucune note maximale n'est présente dans les données, un message d'information s’affiche à la place du graphique.</li>
</ul>
      ")
                  ),
                  uiOutput("quantitative_blocks_ui")
                )
              )
              
      )
      ,
      
      tabItem(tabName = "bivar_quali",
              fluidRow(uiOutput("bivar_quali_blocks_ui"))
      ),
      tabItem(tabName = "bivar_quanti",
        fluidRow(
          box(
            title = "📈 Analyse bivariée quantitative : Résumés et visualisations",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,  # Démarre fermé
            HTML("
        <p><b>Cette section vous permet d'explorer (filtrer) une variable quantitative en fonction d’une variable secondaire.</b></p>

        <ul>
          <li><b>Un diagramme en barres</b> : Généré automatiquement pour chaque variable continue.</li>
          <li><b>Statistique dynamique</b> : Affichage au dessous du graphe de la médiane pour chaque groupe de la variable secondaire.</li>
        </ul>
        <p><b>🟢 Analyse du taux de satisfaction/préférence (Net Promoter Score)</b></p>
        <p>Un score est généré automatiquement si les données sont sur une échelle de 0 à 5 ou 0 à 10.</p>

        <ul>
          <li><b>Échelle 0 à 10</b> (standard NPS) :
            <ul>
              <li>9-10 : <span style='color:green;'><b>Promoteur</b></span></li>
              <li>7-8 : <span style='color:orange;'><b>Passif</b></span></li>
              <li>0-6 : <span style='color:red;'><b>Détracteur</b></span></li>
            </ul>
          </li>
          <li><b>Échelle 0 à 5</b> (adaptée) :
            <ul>
              <li>5 : <span style='color:green;'><b>Promoteur</b></span></li>
              <li>4 : <span style='color:orange;'><b>Passif</b></span></li>
              <li>0-3 : <span style='color:red;'><b>Détracteur</b></span></li>
            </ul>
          </li>
        </ul>

        <p>Les valeurs numériques sont nettoyées automatiquement avant traitement.
        Le graphique NPS permet d’évaluer visuellement le taux de satisfaction/préférence 
        global selon la formule :</p>
        <blockquote><b>Taux NPS = Taux de Promoteurs − Taux de Détracteurs</b></blockquote>
       
        <p><b>🟢 Classement (Plus grand effectif)</b></p>
        <p>Un graphique affiche les variables ayant reçu le plus grand nombre de notes maximales (5 ou 10) en fonction d'une variable de regroupement, selon l’échelle utilisée. 
Il permet de visualiser les éléments les plus plébiscités par les répondants.</p>

<ul>
  <li>Les notes autres que la valeur maximale sont ignorées dans ce classement.</li>
  <li>Si aucune note maximale n'est présente dans les données, un message d'information s’affiche à la place du graphique.</li>
</ul>

      ")
          ),
          uiOutput("bivar_quanti_blocks_ui")
        )
      )
      ,
      tabItem(
        tabName = "analyse_profil",
        fluidRow(
          box(
            title = "🧭 Analyse des Correspondances Multiples (ACM) : Profils types",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML("
        <p><b>Cette section permet de réaliser une Analyse des Correspondances Multiples (ACM)</b> pour identifier les profils types à partir de plusieurs variables qualitatives.</p>

        <p><b>Objectif</b> : Explorer les relations entre modalités de différentes variables catégorielles, détecter des regroupements de comportements ou d'opinions similaires.</p>

        <p><b>Fonctionnement général</b></p>
        <ul>
          <li>Un bloc d’analyse est généré pour chaque fichier importé.</li>
          <li>L’utilisateur peut sélectionner :
            <ul>
              <li>Des <b>variables actives</b> (les variables principales utilisées dans l’ACM)</li>
              <li>Des <b>variables supplémentaires</b> (projetées mais non utilisées dans le calcul des dimensions)</li>
              <li>Des <b>modalités spécifiques</b> pour filtrer les sous-variables (dans le cas de questions multi-items)</li>
            </ul>
          </li>
          <li><b>Gestion des valeurs manquantes</b> :
            <ul>
              <li>Garder les valeurs manquantes</li>
              <li>Supprimer les lignes incomplètes</li>
              <li>Remplacer automatiquement (imputation via <code>imputeMCA()</code>)</li>
            </ul>
          </li>
          <li><b>Filtrage des modalités selon leur importance</b> :
            <ul>
              <li>Afficher uniquement les modalités avec une distance &gt; 1 sur le plan factoriel</li>
            </ul>
          </li>
        </ul>

        <p><b>📈 Visualisations</b></p>
        <ul>
          <li>Deux graphes interactifs sont générés par fichier, via <code>plotly</code> :</li>
          <ul>
            <li><b>Graphe Profil 1</b> : basé sur les dimensions 1 et 2</li>
            <li><b>Graphe Profil 2</b> : basé sur les dimensions 2 et 3</li>
          </ul>
          <li>Les modalités des variables actives sont en <span style='color:blue;'><b>bleu</b></span>, les modalités supplémentaires en <span style='color:red;'><b>rouge</b></span>.</li>
          <li>La distance à l’origine est utilisée pour filtrer les modalités peu informatives.</li>
        </ul>

        <p><b>Détails techniques</b></p>
        <ul>
          <li>Les questions multi-items (type <code>Code[xx]</code>) sont automatiquement gérées.</li>
          <li>Les modalités <code>[Autres], [Commentaires]</code> sont exclues par défaut.</li>
          <li>Les noms de colonnes sont nettoyés (extraction du dernier contenu entre crochets).</li>
          <li><code>FactoMineR</code> est utilisé pour l’ACM, complété par <code>factoextra</code> pour les visualisations.</li>
          <li>L’imputation via <code>imputeMCA()</code> n’influence pas la construction des axes (seules les valeurs observées comptent).</li>
        </ul>

        <p><b>Utilisation typique</b></p>
        <p>Exemple : Identifier des profils types d’étudiants selon leurs usages de documentation, pratiques numériques ou attentes pédagogiques.</p>
      ")
          ),
          box(width = 12,
              uiOutput("profil_blocks_ui")  # UI dynamique générée côté serveur
          )
        )
      )
      ,
    
      tabItem(tabName = "analyse_txt",
              fluidRow(
                uiOutput("texte_blocks_ui")
              )
      ),
      tabItem(
        tabName = "documentation",
        fluidRow(
          column(
            12,
            tags$h3("📖 Documentation de l'application", style = "margin-bottom:15px;"),
            div(
              style = "background:#f9f9f9; padding:15px; border-radius:8px; box-shadow:0 1px 3px rgba(0,0,0,0.1);",
              tags$p("Consultez le guide utilisateur 👇", style = "margin-bottom:8px;"),
              tags$a(
                href = "Documentation.pdf", target = "_blank",
                style = "color:#007BFF; font-weight:600; text-decoration:none;",
                "Cliquez ici pour voir la documentation complète"
              )
            ),
            br(),
            uiOutput("doc_blocks_ui")
          )
        )
      )
      
  )
)
)
