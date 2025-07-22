#############################Import des librairies##############################
library(readxl)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(tidyr)
library(scales)
library(plotly)
library(visdat)
library(missMDA)
library(shiny)
library(readr)
library(ggpie)
library(stringr)
library(shinydashboard)
library(purrr)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda)
library(readtext)
library(igraph)
library(wordcloud)
library(shinyWidgets)

################################################################################
#########################Définition des fonctions###############################

# Fonction qui lit un fichier CSV ou Excel
lire_fichier <- function(path, fichier,sheet=NULL) {
  if (grepl("\\.csv$", fichier, ignore.case = TRUE)) {
    read.csv(path, fileEncoding = "UTF-8")
  } else if (grepl("\\.xlsx$", fichier, ignore.case = TRUE)) {
    if (!is.null(sheet)){
      read_excel(path,sheet=sheet)
    } else{
      read_excel(path)
    }
  } else {
    stop("Impossible de lire ce format de fichier.")
  }
}

#-------------------------PARTIE SIMPLE QUANTITATIVE----------------------------
### Fonction 1:
#Calcul du nps d'une seule modalité, affiche un diagramme en barre avec le pourcentage de détracteur,passif et promoteur + la valeur du NPS.

calc_nps_auto_by_code <- function(data, code_prefix, afficher_graph = TRUE) {
  # Chercher les colonnes correspondant au préfixe
  cols_found <- names(data) %>% str_subset(stringr::fixed(code_prefix))%>%
    str_subset(regex("\\[Autres? ?\\.*\\]", ignore_case = TRUE), negate = TRUE)
  if (length(cols_found) == 0) {
    # Si aucune colonne trouvée, essayer de prendre la variable simple sans crochets
    if (code_prefix %in% names(data)) {
      cols_found <- code_prefix
    } else {
      stop("Aucune colonne trouvée pour ce code_prefix.")
    }
  }
  if (length(cols_found) > 1) 
    warning("⚠️ Plusieurs colonnes trouvées, seule la première sera utilisée.")
  
  var <- cols_found[1]
  
  #Extraire le titre de la colonne avec des crochets (si disponible)
  brackets_content <- str_extract_all(var, "\\[.*?\\]")[[1]]
  if (length(brackets_content) >= 2) {
    titre_question <- brackets_content[2] %>% str_remove_all("\\[|\\]")
  } else {
    titre_question <- var %>% str_replace(".*?\\]\\.\\s*", "") %>% str_extract("^[^\\[]+") %>% str_trim()
  }
  titre_question <- stringr::str_wrap(titre_question, width = 80)
  
  #Préparer les données pour les notes
  notes <- as.numeric(data[[var]])
  notes <- na.omit(notes)
  if (length(notes) == 0) stop("❌ La variable contient uniquement des NA.")
  
  #Calculer les classes notes de 1 à 5 
  if (all(notes %in% 0:5)) {
    nps_type <- case_when(
      notes == 5 ~ "Promoteur",
      notes == 4 ~ "Passif",
      notes %in% 0:3 ~ "Détracteur"
    )
  } else if (all(notes %in% 0:10)) {
    nps_type <- case_when(
      notes >= 9 ~ "Promoteur",
      notes >= 7 ~ "Passif",
      TRUE ~ "Détracteur"
    )
  } else {
    stop("❌ Notes hors échelle attendue (0-5 ou 0–10).")
  }
  
  #Calcul des pourcentages
  df <- tibble(type = nps_type) %>%
    count(type, name = "n") %>%
    mutate(pct = round(100 * n / sum(n), 1))
  
  p <- df %>% filter(type == "Promoteur") %>% pull(pct) %>% { ifelse(length(.) == 0, 0, .) }
  d <- df %>% filter(type == "Détracteur") %>% pull(pct) %>% { ifelse(length(.) == 0, 0, .) }
  nps <- p - d
  
  #Réglage du graphique
  if (afficher_graph) {
    fig <- plot_ly(df, x = ~type, y = ~pct, type = "bar",
                   text = ~paste0(pct, "%"),
                   hovertext = ~paste0(pct, "%","\n nombre : ",n),
                   hoverinfo = "text",
                   textposition="auto",
                   marker = list(color = c("#E74C3C", "#F1C40F", "#2ECC71"))) %>%
      layout(
        yaxis = list(title = "Pourcentage", ticksuffix = "%", range = c(0, max(df$pct) + 10)),
        xaxis = list(title = ""),
        showlegend = FALSE,
        margin = list(t = 120, b = 100),
        annotations = list(
          list(text = paste0("Taux de satisfaction/préférence : <b>", round(nps, 1), "</b>"), x = 0.5, y = 0.9,
               xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(size = 16, color = "#2C3E50"), align = "center",
               bgcolor = "#f0f0f0", bordercolor = "#ccc", borderpad = 6, borderwidth = 1),
          list(text = paste0(titre_question),
               x = 0.5, y = 1.1, xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(size = 12, color = "gray50"), align = "center")
        )
      )
    return(fig)
  }
  return(nps)
}

### Fonction 2:
#Calcul du nps des variables ayant plusieurs modalités

plot_nps_grouped <- function(data, code_prefix, titre = NULL) {
  cols_found <- names(data) %>% str_subset(code_prefix)%>%
    str_subset(regex("\\[Autres? ?\\.*\\]", ignore_case = TRUE), negate = TRUE)
  if (length(cols_found) == 0) stop("Aucune colonne ne correspond au préfixe donné.")
  
  calc_nps_par_colonne <- function(notes) {
    notes <- as.numeric(notes)
    notes <- na.omit(notes)
    if (length(notes) == 0) return(NA)
    if (all(notes %in% 0:5)) {
      nps_type <- case_when(notes == 5 ~ "Promoteur", notes == 4 ~ "Passif", TRUE ~ "Détracteur")
    } else if (all(notes %in% 0:10)) {
      nps_type <- case_when(notes >= 9 ~ "Promoteur", notes >= 7 ~ "Passif", TRUE ~ "Détracteur")
    } else return(NA)
    
    tab <- table(nps_type)
    total <- sum(tab)
    nps <- 100 * (sum(tab["Promoteur"], na.rm = TRUE) - sum(tab["Détracteur"], na.rm = TRUE)) / total
    round(nps, 1)
  }
  
  nps_results <- purrr::map_dfr(cols_found, function(colname) {
    notes <- data[[colname]]
    notes <- notes[!is.na(notes) & notes != "N/A"]
    nps <- calc_nps_par_colonne(notes)
    tibble(Colonne = colname, taux = nps)
  })
  
  nps_results<- nps_results %>%
    filter(!is.na(taux)) %>%
    mutate(
      Texte_complet = Colonne,
      Texte_brut = str_replace(Texte_complet, ".*?\\]\\.\\s*", ""),
      Texte_question = str_extract(Texte_brut, "^[^\\[]+"),
      Ressource = str_extract(Texte_brut, "\\[(.*?)\\]") %>% str_remove_all("\\[|\\]"),
      Ressource = str_wrap(Ressource, width = 30),
      tooltip = paste0(Ressource, "<br>Taux de satisfaction/préférence : ", taux)
    )
  
  nps_results$Ressource = ifelse(nchar(nps_results$Ressource) > 60,
                                 paste0(substr(nps_results$Ressource, 1, 57), "..."),
                                 nps_results$Ressource)
  
  if (is.null(titre)) {
    titre <- nps_results$Texte_question[1] %>% str_trim()}
  titre <- stringr::str_wrap(titre, width = 30)%>% paste(collapse = "\n")
  
  gg <- ggplot(nps_results, aes(x = reorder(Ressource, taux), y = taux, fill = taux, text = tooltip)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = taux), hjust = -0.2, size = 4) +
    scale_fill_gradient2(low = "red", mid = "gray90", high = "green", midpoint = 0) +
    labs(title = titre, x = "", y = "taux de satisfaction/préférence") +
    theme_minimal()
  
  ggplotly(gg, tooltip = "text")
}

### Fonction 3:
##Alternative au NPS : exple : Pour Afficher les ressources du plus utilisé au moins utilisé
plot_classement_grouped <- function(data, code_prefix, titre = NULL) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Identifier les colonnes cibles
  cols_found <- names(data) %>%
    str_subset(stringr::fixed(code_prefix)) %>%
    str_subset(regex("\\[Autres?.*?\\]", ignore_case = TRUE), negate = TRUE) %>%
    str_subset(regex("\\[Commentaires?.*?\\]", ignore_case = TRUE), negate = TRUE)
  
  if (length(cols_found) == 0) stop("Aucune colonne trouvée pour ce code_prefix.")
  
  question_text <- cols_found %>%
    str_replace("^.*?\\]\\.\\s*", "") %>%
    str_replace("\\s*\\[.*$", "") %>%
    unique()
  
  titre <- titre %||% question_text[1]
  titre <- stringr::str_wrap(titre, width = 40)
  
  # Format long
  df_long <- data %>%
    select(all_of(cols_found)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Note") %>%
    filter(!is.na(Note)) %>%
    mutate(
      Note_num = suppressWarnings(as.numeric(Note)),
      Ressource = str_extract(Variable, "\\[(?!.*\\[).*?\\]$"),
      Ressource = str_remove_all(Ressource, "\\[|\\]"),
      Ressource = ifelse(nchar(Ressource) > 40, paste0(substr(Ressource, 1, 37), "..."), Ressource),
      Ressource=stringr::str_wrap(Ressource, width = 25)
    ) %>%
    filter(!is.na(Note_num))
  
  # Vérifier le max global
  note_maximale <- max(df_long$Note_num, na.rm = TRUE)
  
  if (!(note_maximale %in% c(5, 10)) || nrow(df_long %>% filter(Note_num == note_maximale)) == 0) {
    message_plot <- ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Aucune valeur égale à 5 ou à 10 n'a été donnée", size = 5, hjust = 0.5) +
      labs(title = titre)
    
    return(ggplotly(message_plot))
  }
  else {
    # Lister toutes les ressources
    ressource_uniques <- unique(df_long$Ressource)
    
    # Filtrer ceux qui ont donné la note maximale
    df_filtre <- df_long %>%
      filter(Note_num == note_maximale)
    
    # Compter les occurrences de la note maximale
    count_data <- df_filtre %>%
      count(Ressource, name = "Effectif") %>%
      right_join(data.frame(Ressource = ressource_uniques), by = "Ressource") %>%
      mutate(
        Effectif = replace_na(Effectif, 0),
        Pourcentage = Effectif / sum(Effectif, na.rm = TRUE) * 100,
        Label = paste0(round(Pourcentage, 1), "%")
      ) %>%
      arrange(desc(Effectif))
  }
  
  # Graphique
  plot <- ggplot(count_data, aes(x = reorder(Ressource, Effectif), y = Effectif)) +
    geom_col(fill = "#2C7BB6", width = 0.6) +
    geom_text(aes(label = Label), hjust = -0.1, size = 3.0)+
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    coord_flip() +
    labs(
      title = paste0(titre),
      x = "",
      y = paste0("Nombre de répondants ayant mis '", note_maximale, "'")
    ) +
    theme_minimal(base_size = 12)
  
  ggplotly(plot)
}

### Fonction 4:
### Traitement des autres variables quantitatives hors echelle 0-10/0-5 : histogramme

plot_histogramme <- function(data, code_prefix, titre = NULL) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  cols_found <- names(data) %>% str_subset(stringr::fixed(code_prefix))
  if (length(cols_found) == 0) stop("Aucune colonne trouvée pour ce code_prefix.")
  if (length(cols_found) > 1) warning("Plusieurs colonnes trouvées, seule la première est utilisée.")
  var <- cols_found[1]
  
  df_clean <- data %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "N/A") %>%
    mutate(var_label = (.data[[var]])) %>%
    count(var_label, name = "Frequence") %>%
    arrange(desc(Frequence)) %>%
    mutate(Pourcentage = Frequence / sum(Frequence) * 100)
  
  if (is.null(titre)) {
    brackets_content <- str_extract_all(var, "\\[.*?\\]")[[1]]
    titre <- if (length(brackets_content) >= 2) brackets_content[2] else var
    titre <- stringr::str_wrap(titre, width = 60)
  }
  
  plot_ly(
    df_clean,
    x = ~var_label,
    y = ~Frequence,
    type = "bar",
    text = ~paste0(round(Pourcentage, 1), "%"),
    textposition = "auto",
    marker = list(color = "#3498DB")
  ) %>%
    layout(
      title = list(text = titre, x = 0.5),
      xaxis = list(title = ""),
      yaxis = list(title = "Fréquence"),
      margin = list(b = 120)
    )
}

#-------------------------PARTIE SIMPLE QUALITATIVE----------------------------
### Fonction 5:
# Affichage du diagramme en barre pour les variables ayant plusieurs modalités

plot_question_by_code <- function(data, code_prefix, titre = NULL) {
  
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Le package 'colorspace' est requis. Installez-le avec install.packages('colorspace').")
  }
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Trouver les colonnes correspondant au code_prefix
  cols_found <- names(data) %>%
    str_subset(stringr::fixed(code_prefix)) %>%
    str_subset(regex("\\[Autres?.*?\\]", ignore_case = TRUE), negate = TRUE)%>%
    str_subset(regex("\\[Commentaires?.*?\\]", ignore_case = TRUE), negate = TRUE)
  
  if (length(cols_found) == 0) stop("Aucune colonne trouvée pour ce code_prefix.")
  
  question_text <- cols_found %>%
    str_replace("^.*?\\]\\.\\s*", "") %>%
    str_replace("\\s*\\[.*$", "") %>%
    unique()
  
  titre <- titre %||% question_text[1]
  titre <- stringr::str_wrap(titre, width = 80)
  
  # Nettoyer les données
  df_long <- data %>%
    select(all_of(cols_found)) %>%
    pivot_longer(cols = everything(), values_to = "Id", names_to = "Variable") %>%
    filter(!is.na(Id), Id != "N/A") %>%
    mutate(
      Modalite = str_extract(Variable, "\\[(?!.*\\[).*?\\]$"),
      Modalite = str_remove_all(Modalite, "\\[|\\]"),
      Modalite = ifelse(nchar(Modalite) > 40, paste0(substr(Modalite, 1, 37), "..."), Modalite)
    ) %>%
    filter(Modalite != "Autre")
  
  # Identifier les modalités de réponse uniques et les trier
  modalites_id <- df_long %>%
    count(Id, sort = TRUE) %>%
    arrange(Id) %>%
    pull(Id)
  
  modalites_id_wrapped <- stringr::str_wrap(modalites_id, width = 20)
  names(modalites_id_wrapped) <- modalites_id
  df_long$Id <- modalites_id_wrapped[df_long$Id]
  
  n_modalites <- length(modalites_id)
  if (n_modalites > 15) {
    warning("Plus de 15 modalités : certaines couleurs pourraient ne pas être bien distinguables.")
  }
  
  
  # Générer palette de couleurs et l'associer aux Id
  palette_auto <- colorspace::qualitative_hcl(n_modalites, palette = "Dark 3")
  names(palette_auto) <- modalites_id_wrapped
  
  # Générer le graphique
  plot_gg <- df_long %>%
    count(Modalite, Id, name = "Frequence") %>%
    group_by(Modalite) %>%
    mutate(Pourcentage = Frequence / sum(Frequence) * 100) %>%
    ungroup() %>%
    mutate(Id = factor(Id, levels = modalites_id_wrapped)) %>%
    ggplot(aes(
      x = Modalite,
      y = Pourcentage,
      fill = Id,
      text = paste0(
        "Modalité : ", Modalite,
        "<br>Réponse : ", Id,
        "<br>Nombre : ", Frequence,
        "<br>Pourcentage : ", round(Pourcentage, 1), "%"
      )
    )) +
    geom_col(position = "fill") +
    geom_text(
      aes(label = paste0(round(Pourcentage, 1), "%")),
      position = position_fill(vjust = 0.5),
      size = 3
    ) +
    scale_fill_manual(values = palette_auto) +
    labs(title = titre, x = "", y = "Pourcentage") +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
      legend.title = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5)
    )
  
  # Retourner graphique interactif
  ggplotly(plot_gg, tooltip = "text")
}

## Fonction 5.1
## Calcul de la moyenne des proportion pour chacune des modalité 
# Uniquement cas multichoix"
Pourcentage_totaux <- function(data, code_prefix) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  cols_found <- names(data) %>%
    str_subset(stringr::fixed(code_prefix)) %>%
    str_subset(regex("\\[Autres?.*?\\]", ignore_case = TRUE), negate = TRUE) %>%
    str_subset(regex("\\[Commentaires?.*?\\]", ignore_case = TRUE), negate = TRUE)
  
  if (length(cols_found) == 0) return(NULL)
  
  df_long <- data %>%
    select(all_of(cols_found)) %>%
    pivot_longer(cols = everything(), values_to = "Id", names_to = "Variable") %>%
    filter(!is.na(Id), Id != "N/A") %>%
    mutate(
      Modalite = str_extract(Variable, "\\[(?!.*\\[).*?\\]$"),
      Modalite = str_remove_all(Modalite, "\\[|\\]"),
      Modalite = ifelse(nchar(Modalite) > 40, paste0(substr(Modalite, 1, 37), "..."), Modalite),
      Id = stringr::str_wrap(Id, width = 20)
    ) %>%
    filter(Modalite != "Autre")
  
  df_plot <- df_long %>%
    count(Modalite, Id, name = "Frequence") %>%
    complete(Modalite, Id, fill = list(Frequence = 0)) %>%
    group_by(Modalite) %>%
    mutate(Pourcentage = Frequence / sum(Frequence) * 100) %>%
    ungroup()
  
  # Obtenir toutes les Id (triées pour cohérence)
  modalites_id <- df_plot %>% distinct(Id) %>% arrange(Id) %>% pull(Id)
  
  # Générer palette auto
  palette_auto <- colorspace::qualitative_hcl(length(modalites_id), palette = "Dark 3")
  names(palette_auto) <- modalites_id
  
  # Calcul de la moyenne non pondérée
  df_summary <- df_plot %>%
    group_by(Id) %>%
    summarise(Pourcentage_Moyen = mean(Pourcentage, na.rm = TRUE)) %>%
    arrange(Id) %>%
    mutate(Couleur = palette_auto[Id])
  
  return(df_summary)
}


### Fonction 6:
#Affichage du camembert pour les questions simple / une modalité
plot_camembert_question <- function(data, code_prefix, titre = NULL) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Le package 'colorspace' est requis. Installez-le avec install.packages('colorspace').")
  }
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  ordre_personnalise <- c("Licence", "Master", "Doctorat")
  ordre_personnalise_lower <- tolower(ordre_personnalise)
  
  # Sélection de la colonne cible
  cols_found <- names(data) %>% stringr::str_subset(stringr::fixed(code_prefix))
  if (length(cols_found) == 0) stop("Aucune colonne trouvée pour ce code_prefix.")
  if (length(cols_found) > 1) warning("Plusieurs colonnes trouvées, seule la première est utilisée.")
  var <- cols_found[1]
  
  # Nettoyage des données
  df_clean <- data %>%
    dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "N/A") %>%
    dplyr::mutate(var_label_raw = as.character(.data[[var]])) %>%
    dplyr::mutate(var_label = ifelse(
      tolower(var_label_raw) %in% ordre_personnalise_lower,
      ordre_personnalise[match(tolower(var_label_raw), ordre_personnalise_lower)],
      var_label_raw
    )) %>%
    dplyr::count(var_label, name = "Frequence") %>%
    dplyr::mutate(
      Pourcentage = Frequence / sum(Frequence) * 100,
      label = paste0(var_label, "<br>", Frequence, "<br>", round(Pourcentage, 1), "%")
    )
  
  # Gestion de l’ordre personnalisé
  modalites <- df_clean$var_label
  match_exact <- setequal(tolower(modalites), ordre_personnalise_lower) &&
    length(modalites) == length(ordre_personnalise)
  
  if (match_exact) {
    df_clean$var_label <- factor(stringr::str_wrap(df_clean$var_label, width = 35), levels = stringr::str_wrap(ordre_personnalise, width = 35))
  } else {
    df_clean <- df_clean %>% arrange(var_label)
    df_clean$var_label <- factor(stringr::str_wrap(df_clean$var_label, width = 35), levels = stringr::str_wrap(df_clean$var_label, width = 35))
    
  }
  
  n_modalites <- nrow(df_clean)
  if (n_modalites > 15) {
    warning("Plus de 15 modalités : certaines couleurs pourraient ne pas être bien distinguables.")
  }
  
  palette_auto <- colorspace::qualitative_hcl(n_modalites, palette = "Dark 3")
  
  # Gestion du titre (comme version 1)
  if (is.null(titre)) {
    brackets_content <- stringr::str_extract_all(var, "\\[.*?\\]")[[1]]
    titre <- if (length(brackets_content) >= 2) brackets_content[2] else var
  }
  titre <- stringr::str_wrap(titre, width = 90)
  
  sort_value <- if (match_exact) TRUE else FALSE
  
  # Graphique plotly
  plotly::plot_ly(
    df_clean,
    labels = ~var_label,
    values = ~Pourcentage,
    type = "pie",
    sort = sort_value,
    hole = 0.5,
    textinfo = "label+percent",
    hovertext = ~label,
    hoverinfo = "text",
    insidetextorientation = "horizontal",
    marker = list(colors = palette_auto)
  ) %>%
    plotly::layout(
      title = list(text = titre, x = 0.5),  # Centré
      margin = list(t = 100, r = 180),      # Espace pour légende à droite
      legend = list(
        orientation = "v",
        font = list(size = 12),
        x = 1.05,
        y = 0.5,
        xanchor = "left",
        yanchor = "middle"
      )
    )
}

#-------------------------PARTIE BIVARIEE QUALITATIVE---------------------------
### Fonction 7:
#Affichage d'un diagramme en barre selon les réponses a 2 questions : la première multimodalité ou simple et l'autre 1 modalité simple.

plot_bivariate_by_code <- function(data, code_prefix, group_var, titre = NULL) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Le package 'colorspace' est requis. Installez-le avec install.packages('colorspace').")
  }
  
  cols_found <- names(data) %>%
    str_subset(stringr::fixed(code_prefix)) %>%
    str_subset(regex("\\[Autres? ?\\.*\\]", ignore_case = TRUE), negate = TRUE) %>%
    str_subset(regex("\\[Commentaires? ?\\.*\\]", ignore_case = TRUE), negate = TRUE)
  
  if (length(cols_found) == 0) stop("Aucune colonne ne correspond au code_prefix donné.")
  
  data_long <- data %>%
    pivot_longer(cols = all_of(cols_found), names_to = "Variable", values_to = "Reponse") %>%
    filter(!is.na(Reponse), Reponse != "N/A", !is.na(.data[[group_var]]))
  
  ordre_personnalise <- list(
    niveau_etude = c("Licence", "Master", "Doctorat")
  )
  
  ordre_personnalise_lower <- lapply(ordre_personnalise, tolower)
  
  modalites_reponse <- data_long %>%
    distinct(Reponse) %>%
    arrange(Reponse) %>%
    pull()
  
  modalites_reponse_lower <- tolower(modalites_reponse)
  
  ordre_match <- NULL
  for (nom in names(ordre_personnalise_lower)) {
    if (all(sort(ordre_personnalise_lower[[nom]]) == sort(modalites_reponse_lower))) {
      ordre_match <- nom
      break
    }
  }
  if (!is.null(ordre_match)) {
    modalites_reponse <- ordre_personnalise[[ordre_match]]
  }
  
  # 🔁 Ajout : Recode les réponses avec retour à la ligne
  modalites_reponse_wrapped <- stringr::str_wrap(modalites_reponse, width = 30)
  names(modalites_reponse_wrapped) <- modalites_reponse
  
  modalites_group <- data %>%
    filter(!is.na(.data[[group_var]])) %>%
    distinct(.data[[group_var]]) %>%
    arrange(.data[[group_var]]) %>%
    pull()
  
  modalites_group_lower <- tolower(modalites_group)
  
  ordre_match_group <- NULL
  for (nom in names(ordre_personnalise_lower)) {
    if (all(sort(ordre_personnalise_lower[[nom]]) == sort(modalites_group_lower))) {
      ordre_match_group <- nom
      break
    }
  }
  if (!is.null(ordre_match_group)) {
    modalites_group <- ordre_personnalise[[ordre_match_group]]
  }
  
  n_modalites <- length(modalites_reponse)
  if (n_modalites > 15) {
    warning("Plus de 15 modalités : certaines couleurs pourraient ne pas être bien distinguables.")
  }
  
  palette_auto <- colorspace::qualitative_hcl(n_modalites, palette = "Dark 3")
  names(palette_auto) <- modalites_reponse_wrapped  # 🔁 changement ici
  
  data_summary <- data_long %>%
    count(Variable, .data[[group_var]], Reponse, name = "n") %>%
    group_by(Variable, .data[[group_var]]) %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      Texte_brut = str_replace(Variable, ".*?\\]\\.\\s*", ""),
      Texte_question = if_else(
        str_detect(Texte_brut, "\\["),
        str_extract(Texte_brut, "^[^\\[]+"),
        Texte_brut
      ) %>% str_trim(),
      Modalite = if_else(
        str_detect(Texte_brut, "\\[.*\\]"),
        str_extract(Texte_brut, "\\[(.*?)\\]") %>% str_remove_all("\\[|\\]"),
        Texte_brut
      )
    ) %>%
    ungroup() %>%
    mutate(
      Modalite = str_wrap(Modalite, width = 50),
      Reponse = modalites_reponse_wrapped[as.character(Reponse)],  # 🔁 recodage ici
      Reponse = factor(Reponse, levels = modalites_reponse_wrapped),
      !!group_var := factor(.data[[group_var]], levels = modalites_group),
      group_var_tronq = factor(
        ifelse(
          nchar(as.character(.data[[group_var]])) > 27,
          paste0(substr(as.character(.data[[group_var]]), 1, 24), "..."),
          as.character(.data[[group_var]])
        ),
        levels = ifelse(
          nchar(as.character(modalites_group)) > 27,
          paste0(substr(as.character(modalites_group), 1, 24), "..."),
          as.character(modalites_group)
        )
      )
    )
  
  if (is.null(titre)) {
    titre <- unique(data_summary$Texte_question)[1] %>% str_trim()
  }
  titre <- stringr::str_wrap(titre, width = 60)
  
  gg <- ggplot(data_summary, aes(x = group_var_tronq, y = pct, fill = Reponse,
                                 text = paste0("Modalité: ", Modalite, 
                                               "<br>Réponse: ", Reponse,
                                               "<br>Effectif : ", n,
                                               "<br>", pct, "%"))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(pct, "%")),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3, color = "black") +
    facet_wrap(~Modalite, ncol = 3) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = palette_auto) +
    labs(x = group_var, y = "Pourcentage de réponses", fill = "Réponse") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1),
      panel.spacing = unit(0.8, "lines"),
      strip.text = element_text(size = 9),
      axis.title.x = element_text(size = 9)
    )
  
  ggplotly(gg, tooltip = "text")
}


#-------------------------PARTIE BIVARIEE QUANTITATIVE---------------------------
### Fonction 8:
#Affichage d'un graphique montrant le NPS de var simple ou multimodalité en fonction d'une var simple.

if (!requireNamespace("colorspace", quietly = TRUE)) {
  stop("Le package 'colorspace' est requis. Installez-le avec install.packages('colorspace').")
}

# 1. Fonction principale NPS avec gestion couleurs colorspace
plot_bivariate_NPS_by_code <- function(data, code_prefix, group_var, titre = NULL) {
  
  ordre_personnalise <- c("Licence", "Master", "Doctorat")
  ordre_personnalise_lower <- tolower(ordre_personnalise)
  
  cols_found <- names(data) %>%
    str_subset(stringr::fixed(code_prefix)) %>%
    str_subset(regex("\\[Autres?.*?\\]", ignore_case = TRUE), negate = TRUE)
  if (length(cols_found) == 0) stop("Aucune colonne ne correspond au code_prefix donné.")
  
  calc_nps_par_colonne <- function(notes) {
    notes <- as.numeric(notes)
    notes <- na.omit(notes)
    if (length(notes) == 0) return(NA)
    
    nps_type <- if (all(notes %in% 0:5)) {
      case_when(
        notes == 5 ~ "Promoteur",
        notes == 4 ~ "Passif",
        notes %in% 0:3 ~ "Détracteur"
      )
    } else {
      case_when(
        notes >= 9 ~ "Promoteur",
        notes >= 7 ~ "Passif",
        TRUE ~ "Détracteur"
      )
    }
    
    tab <- table(nps_type)
    100 * (sum(tab["Promoteur"], na.rm = TRUE) - sum(tab["Détracteur"], na.rm = TRUE)) / sum(tab) %>% round(1)
  }
  
  nps_results <- data %>%
    select(all_of(cols_found), all_of(group_var)) %>%
    pivot_longer(cols = all_of(cols_found), names_to = "Ressource", values_to = "Notes") %>%
    filter(!is.na(Notes), Notes != "N/A", !is.na(.data[[group_var]])) %>%
    mutate(group_var_clean = str_to_title(str_trim(.data[[group_var]]))) %>%
    group_by(Ressource, group_var_clean) %>%
    summarise(NPS = calc_nps_par_colonne(Notes), .groups = "drop") %>%
    mutate(
      Texte_complet = Ressource,
      Texte_brut = str_replace(Texte_complet, ".*?\\]\\.\\s*", ""),
      Texte_question = str_extract(Texte_brut, "^[^\\[]+"),
      Ressource = str_extract(Texte_brut, "\\[(.*?)\\]") %>% str_remove_all("\\[|\\]"),
      tooltip = paste0(Ressource, "<br>NPS : ", NPS, "%<br>", group_var, " : ", group_var_clean)
    )
  
  # Modalités du group_var
  modalites <- unique(nps_results$group_var_clean)
  modalites <- modalites[!is.na(modalites)]
  
  # Si toutes dans l’ordre personnalisé, forcer ordre + palette colorspace avec ce nombre
  if (all(tolower(modalites) %in% ordre_personnalise_lower) &&
      all(ordre_personnalise_lower %in% tolower(modalites))) {
    modalites <- ordre_personnalise
    nps_results$group_var_clean <- factor(nps_results$group_var_clean, levels = modalites)
  } else {
    modalites <- sort(modalites)
    nps_results$group_var_clean <- factor(nps_results$group_var_clean, levels = modalites)
  }
  
  n_modalites <- length(modalites)
  
  if (n_modalites > 15) warning("Plus de 15 modalités, certaines couleurs peuvent être difficiles à distinguer.")
  
  palette_auto <- colorspace::qualitative_hcl(n_modalites, palette = "Dark 3")
  names(palette_auto) <- modalites
  
  if (is.null(titre)) {
    titre <- nps_results$Texte_question[1] %>% str_trim() %>% str_wrap(width = 60)
  }
  
  nps_results$Ressource <- ifelse(nchar(nps_results$Ressource) > 50,
                                  paste0(substr(nps_results$Ressource, 1, 47), "..."),
                                  nps_results$Ressource)
  
  titre <- str_wrap(titre, width = 70)
  
  gg <- ggplot(nps_results, aes(x = Ressource, y = NPS, fill = group_var_clean, text = tooltip)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_fill_manual(values = palette_auto) +
    labs(title = titre, x = "", y = "Taux de satisfaction/préférence", fill = str_replace_all(group_var, "(.{1,30})(\\s)", "\\1\n")) +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 11, hjust = 0.5)
          )
  
  ggplotly(gg, tooltip = "text")
}

### Fonction 9:
####Classement:
plot_bivariate_classement <- function(data, code_prefix, var_group, titre = NULL, stacked = FALSE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  if (!(var_group %in% names(data))) {
    stop(paste("La variable de regroupement", var_group, "n'existe pas dans les données."))
  }
  
  cols_found <- names(data) %>%
    str_subset(stringr::fixed(code_prefix)) %>%
    str_subset(regex("\\[Autres?.*?\\]", ignore_case = TRUE), negate = TRUE) %>%
    str_subset(regex("\\[Commentaires?.*?\\]", ignore_case = TRUE), negate = TRUE)
  
  if (length(cols_found) == 0) stop("Aucune colonne trouvée pour ce code_prefix.")
  
  question_text <- cols_found %>%
    str_replace("^.*?\\]\\.\\s*", "") %>%
    str_replace("\\s*\\[.*$", "") %>%
    unique()
  
  titre <- titre %||% question_text[1]
  titre_wrap <- stringr::str_wrap(titre, width = 60)
  
  df_long <- data %>%
    select(all_of(c(cols_found, var_group))) %>%
    pivot_longer(cols = all_of(cols_found), names_to = "Variable", values_to = "Note") %>%
    filter(!is.na(Note)) %>%
    mutate(
      Note_num = suppressWarnings(as.numeric(Note)),
      Ressource = str_extract(Variable, "\\[(?!.*\\[).*?\\]$"),
      Ressource = str_remove_all(Ressource, "\\[|\\]"),
      Ressource = ifelse(nchar(Ressource) > 40, paste0(substr(Ressource, 1, 37), "..."), Ressource)
    ) %>%
    filter(!is.na(Note_num))
  
  note_maximale <- max(df_long$Note_num, na.rm = TRUE)
  
  if (!(note_maximale %in% c(5, 10)) || nrow(df_long %>% filter(Note_num == note_maximale)) == 0) {
    message_plot <- ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Aucune valeur égale à 5 ou à 10 n'a été donnée", size = 5, hjust = 0.5) +
      labs(title = titre_wrap)
    
    return(ggplotly(message_plot))
  }
  
  df_filtre <- df_long %>%
    filter(Note_num == note_maximale) %>%
    filter(!is.na(.data[[var_group]])) %>%
    filter(!.data[[var_group]] %in% c("N/A", "NA"))
  
  ressources_uniques <- unique(df_long$Ressource)
  groupes_uniques <- unique(data[[var_group]])
  groupes_uniques <- groupes_uniques[!is.na(groupes_uniques) & !(groupes_uniques %in% c("N/A", "NA"))]
  
  ordre_personnalise <- list(
    niveau_etude = c("Licence", "Master", "Doctorat")
  )
  
  modalites_group_lower <- tolower(groupes_uniques)
  ordre_personnalise_lower <- lapply(ordre_personnalise, tolower)
  
  ordre_match_group <- NULL
  for (nom in names(ordre_personnalise_lower)) {
    if (all(sort(ordre_personnalise_lower[[nom]]) == sort(modalites_group_lower))) {
      ordre_match_group <- nom
      break
    }
  }
  
  if (!is.null(ordre_match_group)) {
    df_filtre <- df_filtre %>%
      mutate(!!var_group := factor(.data[[var_group]], levels = ordre_personnalise[[ordre_match_group]]))
  }
  
  count_data <- df_filtre %>%
    group_by(Ressource, Groupe = .data[[var_group]]) %>%
    summarise(Effectif = n(), .groups = "drop") %>%
    complete(Ressource = ressources_uniques, Groupe = groupes_uniques, fill = list(Effectif = 0))
  
  if (stacked) {
    # BARRES EMPILÉES = % par groupe
    total_par_groupe <- count_data %>%
      group_by(Groupe) %>%
      summarise(Total_groupe = sum(Effectif), .groups = "drop")
    
    count_data <- count_data %>%
      left_join(total_par_groupe, by = "Groupe") %>%
      mutate(Pourcentage = ifelse(Total_groupe > 0, (Effectif / Total_groupe) * 100, 0)) %>%
      mutate(Ressource = reorder(Ressource, Effectif))
    
  } else {
    # BARRES JUXTAPOSÉES = % par ressource
    total_max_par_ressource <- count_data %>%
      group_by(Ressource) %>%
      summarise(Total_max = sum(Effectif), .groups = "drop")
    
    count_data <- count_data %>%
      left_join(total_max_par_ressource, by = "Ressource") %>%
      mutate(Pourcentage = ifelse(Total_max > 0, (Effectif / Total_max) * 100, 0)) %>%
      mutate(Ressource = reorder(Ressource, Total_max))
  }
  
  if (!is.null(ordre_match_group)) {
    count_data <- count_data %>%
      mutate(Groupe = factor(Groupe, levels = ordre_personnalise[[ordre_match_group]]))
  }
  
  pos_geom_text <- if (stacked) position_stack(vjust = 0.5) else position_dodge(width = 0.6)
  pos_geom_col <- if (stacked) "stack" else "dodge"
  
  plot <- ggplot(count_data, aes(x = Ressource, y = Effectif, fill = Groupe)) +
    geom_col(position = pos_geom_col, width = 0.8) +
    geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
              position = pos_geom_text, 
              size = 3, 
              hjust = ifelse(stacked, 0.5, -0.1)) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.15))) +
    coord_flip() +
    labs(
      title = paste0(titre_wrap, " (note maximale utilisée : ", note_maximale, ")"),
      x = "",
      y = paste0("Effectifs ayant mis '", note_maximale, "'"),
      fill = str_wrap(var_group, width = 20)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 11, hjust = 0.5)
    )
  
  ggplotly(plot)
}

### Fonction 10:
# 2. Fonction barplot valeurs hors NPS avec palette colorspace
plot_bar_by_group <- function(data, cols, group_var) {
  
  ordre_personnalise <- c("Licence", "Master", "Doctorat")
  ordre_personnalise_lower <- tolower(ordre_personnalise)
  
  df_long <- data %>%
    select(all_of(c(group_var, cols))) %>%
    pivot_longer(cols = all_of(cols), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value), !value %in% c("NA", "N/A", "")) %>%
    mutate(
      group_var_clean = str_trim(.data[[group_var]]),
      group_var_clean = ifelse(
        tolower(group_var_clean) %in% ordre_personnalise_lower,
        ordre_personnalise[match(tolower(group_var_clean), ordre_personnalise_lower)],
        str_to_title(group_var_clean)
      )) %>%
    group_by(group_var_clean, variable, value) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(variable, group_var_clean) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()
  
  modalites <- unique(df_long$group_var_clean)
  modalites <- modalites[!is.na(modalites)]
  
  if (all(tolower(modalites) %in% ordre_personnalise_lower) &&
      all(ordre_personnalise_lower %in% tolower(modalites))) {
    modalites <- ordre_personnalise
    df_long$group_var_clean <- factor(df_long$group_var_clean, levels = modalites)
  } else {
    modalites <- sort(modalites)
    df_long$group_var_clean <- factor(df_long$group_var_clean, levels = modalites)
  }
  
  n_modalites <- length(modalites)
  
  if (n_modalites > 15) warning("Plus de 15 modalités, certaines couleurs peuvent être difficiles à distinguer.")
  
  palette_auto <- colorspace::qualitative_hcl(n_modalites, palette = "Dark 3")
  names(palette_auto) <- modalites
  
  titre_legende <- str_wrap(group_var, width = 30)
  p <- ggplot(df_long, aes(x = value, y = pct, fill = group_var_clean,
                           text = paste0("Variable : ", variable,
                                         "<br>Réponse : ", value,
                                         "<br>Effectif : ", n,
                                         "<br>", round(pct, 1), "%"))) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ variable, scales = "free_x") +
    scale_fill_manual(values = palette_auto) +
    labs(
      x = "Réponse",
      y = "Pourcentage",
      fill = titre_legende
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 11, hjust = 0.5))
  
  ggplotly(p, tooltip = "text")
}
################################################################################
##############################FONCTION SERVER###################################

server <- function(input, output, session) {
  
  fichiers_excel <- reactive({
    req(input$fichier)
    fichiers <- input$fichier
    validate(need(nrow(fichiers) <= 5, "Veuillez sélectionner au maximum 5 fichiers."))
    fichiers
  })
  
  
  feuilles_excel <- reactive({
    fichiers <- fichiers_excel()
    lapply(seq_len(nrow(fichiers)), function(i) {
      path <- fichiers$datapath[i]
      if (grepl("\\.xlsx$", fichiers$name[i], ignore.case = TRUE)) {
        excel_sheets(path)
      } else {
        NULL
      }
    })
  })
  
  output$choix_feuille_ui <- renderUI({
    feuilles <- feuilles_excel()
    fichiers <- fichiers_excel()
    
    if (length(feuilles) > 0) {
      ui_list <- lapply(seq_along(feuilles), function(i) {
        if (!is.null(feuilles[[i]]) && length(feuilles[[i]]) > 1) {
          selectInput(paste0("feuille_", i),
                      label = paste("Feuille pour", fichiers$name[i]),
                      choices = feuilles[[i]])
        }
      })
      do.call(tagList, ui_list)
    }
  })
  
  data_reactive <- reactive({
    fichiers <- fichiers_excel()
    feuilles <- feuilles_excel()
    
    lapply(seq_len(nrow(fichiers)), function(i) {
      path <- fichiers$datapath[i]
      nom_fichier <- fichiers$name[i]
      feuille_choisie <- if (!is.null(feuilles[[i]]) && length(feuilles[[i]]) > 1) {
        input[[paste0("feuille_", i)]]
      } else {
        NULL
      }
      
      df <- lire_fichier(path, nom_fichier, sheet = feuille_choisie)
      
      df[] <- lapply(df, function(col) {
        if (is.character(col) || is.factor(col)) {
          col[col %in% c("N/A", "null")] <- NA
        }
        return(col)
      })
      
      mots_a_suppr <- c("ID de la réponse","Date de soumission","Tête de série","Date de lancement",
                        "Code d'accès","Date de la dernière action","Langue de départ","Prénom",
                        "lastname.","Adresse de courriel","mail","etape","Code Etudiant","code étudiant")
      colonnes_a_supprimer <- names(df)[str_detect(names(df), regex(paste(mots_a_suppr, collapse = "|"), ignore_case = TRUE))]
      df <- df %>% select(-all_of(colonnes_a_supprimer))
      df
    })
  })
  courbe_data_list <- reactive({
    fichiers <- fichiers_excel()
    data_list <- data_reactive()
    
    lapply(seq_along(data_list), function(i) {
      df <- data_list[[i]]
      nom_col <- names(df)[str_detect(names(df), regex("Dernière page", ignore_case = TRUE))]
      
      if (length(nom_col) == 0) {
        return(NULL)  # Ignorer les fichiers sans colonne "Dernière page"
      }
      
      col_dp <- nom_col[1]
      
      df <- df %>%
        mutate(
          val_brute = .data[[col_dp]],
          val_num = as.integer(str_extract(val_brute, "\\d+")),
          val = ifelse(is.na(val_num) | val_num < 0, 0L, val_num)
        )
      
      max_page <- max(df$val, na.rm = TRUE)
      toutes_pages <- tibble(page = 0:max_page)
      
      toutes_pages %>%
        rowwise() %>%
        mutate(nb_personnes = sum(df$val >= page)) %>%
        ungroup()
    }) 
  })
  
  output$plot_derniere_page <- renderPlotly({
    fichiers <- fichiers_excel()$name
    data_list <- courbe_data_list()
    
    p <- plot_ly()
    
    for (i in seq_along(data_list)) {
      data <- data_list[[i]]
      if (!is.null(data)) {
        fichier <- fichiers[i]
        
        p <- p %>%
          add_trace(
            data = data,
            x = ~page,
            y = ~nb_personnes,
            type = "scatter",
            mode = "lines+markers",
            name = fichier,
            line = list(width = 3),
            marker = list(size = 6)
          )
      }
    }
    
    p %>%
      layout(
        title = "Nombre de personnes par page",
        xaxis = list(title = "Page"),
        yaxis = list(title = "Nombre de personnes"),
        legend = list(orientation = "h", x = 0.1, y = -0.2)
      )
  })
  
  output$taux_abandon <- renderUI({
    data_list <- courbe_data_list()
    fichiers <- fichiers_excel()$name
    
    boxes <- lapply(seq_along(fichiers), function(i) {
      fichier <- fichiers[i]
      data <- data_list[[i]]
      
      if (is.null(data)) {
        valueBox(
          "0 dernière page",
          paste("Taux d'abandon -", fichier),
          icon = icon("exclamation-circle"),
          color = "yellow",
          width = 4
        )
      } else {
        nb_total <- data$nb_personnes[data$page == 0]
        nb_rempli <- data$nb_personnes[data$page == max(data$page)]
        taux <- (nb_total - nb_rempli) * 100 / nb_total
        
        valueBox(
          paste0(round(taux, 1), "%"),
          paste("Taux d'abandon -", fichier),
          icon = icon("user-slash"),
          color = "red",
          width = 4
        )
      }
    })
    
    rows <- split(boxes, ceiling(seq_along(boxes) / 3))
    ui_rows <- lapply(rows, function(row) {
      fluidRow(
        lapply(row, function(box) {
          div(style = "padding: 8px;", box)
        })
      )
    })
    
    tagList(ui_rows)
  })
  #---------------------------------SIMPLE QUALI---------------------------------
  # Réactifs pour stocker les sélections par fichier
  selected_modalites <- reactiveValues()
  code_prefixes <- reactiveValues()

  # Liste des questions qualitatives par fichier
  questions_quali_list <- reactive({
    req(data_reactive())
    lapply(data_reactive(), function(df) {
      vars <- names(df)[!sapply(df, is.numeric)]
      
      # Filtrer les variables avec 15 modalités ou moins
      vars_filtrees <- vars[sapply(df[, vars, drop = FALSE], function(col) {
        length(unique(na.omit(col))) <= 15
      })]
      
      parsed <- tibble(original = vars_filtrees) %>%
        mutate(
          has_brackets = str_detect(original, "\\[.*\\]"),
          code = if_else(has_brackets, str_extract(original, "^[^\\[]+"), original),
          question = if_else(
            has_brackets,
            str_replace(original, "^.*?\\]\\.\\s*", "") %>% str_replace("\\s*\\[.*$", ""),
            original
          ),
          question_affichee = if_else(has_brackets, paste0(code, " — ", question), question)
        ) %>%
        distinct(original, .keep_all = TRUE)
      
      choix <- parsed$code
      names(choix) <- parsed$question_affichee
      choix
    })
  })
  
  
  # Génère dynamiquement les blocs UI pour chaque fichier
  output$qualitative_blocks_ui <- renderUI({
    req(data_reactive())
    n <- length(data_reactive())
    fichiers <- fichiers_excel()$name
    questions_list <- questions_quali_list()
    
    blocs <- lapply(seq_len(n), function(i) {
      ns <- paste0("quali_", i)
      box_width <- if (n == 1) 12 else 12
      
      box(title = paste("Fichier :", fichiers[i]),
          status = "info", solidHeader = TRUE, width = box_width,
          selectInput(ns, "Choisissez une question :", choices = questions_list[[i]]),
          uiOutput(paste0("single_modalite_ui_", i)),
          plotlyOutput(paste0("result_plot_", i)),
          uiOutput(paste0("percentages_box_", i))
      )
    })
    
    # Regrouper les blocs 2 par 2 dans des fluidRow séparés
    rows <- lapply(seq(1, length(blocs), by = 2), function(i) {
      fluidRow(
        blocs[[i]],
        if ((i + 1) <= length(blocs)) blocs[[i + 1]] else NULL
      )
    })
    
    tagList(rows)
  })
  
  
  # Génère le UI pour chaque bloc de modalité
  observe({
    lapply(seq_along(data_reactive()), function(i) {
      local({
        idx <- i
        observeEvent(input[[paste0("quali_", idx)]], {
          code_prefixes[[as.character(idx)]] <- input[[paste0("quali_", idx)]]
          selected_modalites[[as.character(idx)]] <- ""
        })
      })
    })
  })
  
  # UI pour la sélection des modalités spécifiques
  lapply(1:5, function(i) {
    output[[paste0("single_modalite_ui_", i)]] <- renderUI({
      req(data_reactive()[[i]], code_prefixes[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefixes[[as.character(i)]]
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
      if (length(cols_found) > 1) {
        libelles <- setNames(cols_found, cols_found)
        selectInput(paste0("single_modalite_", i), "Choisissez une modalité spécifique :",
                    choices = c("Aucune (voir tout le groupe)" = "", libelles),
                    selected = "")
      } else NULL
    })
  })
  
  # Observe les changements de modalité
  observe({
    lapply(1:5, function(i) {
      observeEvent(input[[paste0("single_modalite_", i)]], {
        selected_modalites[[as.character(i)]] <- input[[paste0("single_modalite_", i)]]
      })
    })
  })
  
  # Rendu des graphiques pour chaque fichier
  lapply(1:5, function(i) {
    output[[paste0("result_plot_", i)]] <- renderPlotly({
      req(data_reactive()[[i]], code_prefixes[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefixes[[as.character(i)]]
      selected_modalite_val <- selected_modalites[[as.character(i)]]
      
      if (!is.null(selected_modalite_val) && selected_modalite_val != "") {
        plot_camembert_question(df, selected_modalite_val)
      } else if (length(names(df) %>% str_subset(stringr::fixed(prefix))) > 1) {
        plot_question_by_code(df, prefix)
      } else {
        plot_camembert_question(df, prefix)
      }
    })
  })
  
  lapply(1:5, function(i) {
    output[[paste0("percentages_box_", i)]] <- renderUI({
      req(data_reactive()[[i]], code_prefixes[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefixes[[as.character(i)]]
      selected_mod <- selected_modalites[[as.character(i)]]
      
      if (!is.null(selected_mod) && selected_mod != "") return(NULL)
      
      df_summary <- Pourcentage_totaux(df, prefix)
      req(df_summary)
      
      fluidRow(
        lapply(seq_len(nrow(df_summary)), function(j) {
          id <- df_summary$Id[j]
          pct <- round(df_summary$Pourcentage_Moyen[j], 1)
          bg_color <- df_summary$Couleur[j]
          
          column(
            width = 3,
            div(style = paste0("background-color:", bg_color,
                               "; padding: 10px; border-radius: 6px; margin: 5px; color: white;"),
                strong(id),
                tags$br(),
                paste0(pct, " %")
            )
          )
        })
      )
    })
  })
  
#---------------------------------SIMPLE QUANTI---------------------------------
  # Réactifs pour stocker les sélections
  selected_modalites <- reactiveValues()
  display_mode <- reactiveValues()  # Stocke le mode choisi (nps / classement)
  
  
  # Liste des questions quantitatives par fichier
  questions_quanti_list <- reactive({
    req(data_reactive())
    lapply(data_reactive(), function(df) {
      df <- df %>% select(-matches("Dernière page", ignore.case = TRUE))
      vars <- names(df)[sapply(df, is.numeric)]
      parsed <- tibble(original = vars) %>%
        mutate(
          has_brackets = str_detect(original, "\\[.*\\]"),
          code = if_else(has_brackets, str_extract(original, "^[^\\[]+"), original),
          question = if_else(
            has_brackets,
            str_replace(original, "^.*?\\]\\.\\s*", "") %>% str_replace("\\s*\\[.*$", ""),
            original
          ),
          question_affichee = if_else(has_brackets, paste0(code, " — ", question), question)
        ) %>%
        distinct(original, .keep_all = TRUE)
      choix <- parsed$code
      names(choix) <- parsed$question_affichee
      choix
    })
  })
  
  # UI dynamique
  output$quantitative_blocks_ui <- renderUI({
    req(data_reactive())
    n <- length(data_reactive())
    fichiers <- fichiers_excel()$name
    questions_list <- questions_quanti_list()
    box_width <- if (n == 1) 12 else 12
    
    blocs <- lapply(seq_len(n), function(i) {
      ns <- paste0("quanti_", i)
      box(
        title = paste("Fichier :", fichiers[i]),
        status = "info", solidHeader = TRUE, width = box_width,
        
        # Ligne combinée : Question + Affichage
        fluidRow(
          column(width = 6,
                 selectInput(ns, "Choisissez une question quantitative :", choices = questions_list[[i]])
          ),
          column(width = 6,
                 uiOutput(paste0("display_mode_ui_", i))  # Positionné à côté
          )
        ),
        
        uiOutput(paste0("single_modalite_ui_quanti_", i)),
        
        # Médiane + Graphique
        fluidRow(
          column(width = 3,
                 div(style = "display: flex; align-items: center; justify-content: center; height: 130px;",
                     valueBoxOutput(paste0("mediane_quanti_", i), width = 12))
          ),
          column(width = 9,
                 plotlyOutput(paste0("result_plot_quanti_", i), height = "540px"))
        )
      )
    })
    
    if (n == 1) {
      tagList(blocs)
    } else {
      rows <- lapply(seq(1, length(blocs), by = 2), function(i) {
        fluidRow(
          blocs[[i]],
          if ((i + 1) <= length(blocs)) blocs[[i + 1]] else NULL
        )
      })
      tagList(rows)
    }
  })
  
  
  # UI modalités spécifiques
  lapply(1:5, function(i) {
    output[[paste0("single_modalite_ui_quanti_", i)]] <- renderUI({
      req(input[[paste0("quanti_", i)]], data_reactive())
      df <- data_reactive()[[i]]
      prefix <- input[[paste0("quanti_", i)]]
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
      if (length(cols_found) > 1) {
        libelles <- setNames(cols_found, cols_found)
        selectInput(paste0("single_modalite_quanti_", i), "Choisissez une modalité spécifique :",
                    choices = c("Aucune (voir tout le groupe)" = "", libelles), selected = "")
      } else NULL
    })
  })
  
  # UI nouveau sélecteur NPS/Classement
  lapply(1:5, function(i) {
    output[[paste0("display_mode_ui_", i)]] <- renderUI({
      req(data_reactive()[[i]], input[[paste0("quanti_", i)]])
      df <- data_reactive()[[i]]
      prefix <- input[[paste0("quanti_", i)]]
      
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
      
      colname <- if (length(cols_found) > 0) cols_found[1] else NULL
      if (is.null(colname)) return(NULL)
      
      notes <- suppressWarnings(as.numeric(df[[colname]]))
      notes <- na.omit(notes)
      
      if (length(notes) > 0 && (all(notes %in% 0:5) || all(notes %in% 0:10))) {
        selected_val <- display_mode[[as.character(i)]] %||% "nps"
        return(
          fluidRow(
            column(
              width = 12, align = "center",
              div(
                style = "display: inline-flex; align-items: center; gap: 10px; margin-top: 20px;",
                tags$span(style = "margin-left: 40px; margin-top: -14px;","NPS"),
                shinyWidgets::materialSwitch(
                  inputId = paste0("display_mode_select_", i),
                  label = NULL,
                  value = (isolate(display_mode[[as.character(i)]] %||% "nps") == "classement"),
                  status = "primary",
                  right = TRUE
                ),
                tags$span(style = "margin-left: 24px; margin-top: -14px;", "Classement")
              )
            )
          )
        )
      } else {
        return(NULL)
      }
    })
  })
  
  
  
  
  # Observateurs
  observe({
    lapply(1:5, function(i) {
      observeEvent(input[[paste0("single_modalite_quanti_", i)]], {
        selected_modalites[[as.character(i)]] <- input[[paste0("single_modalite_quanti_", i)]]
      }, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("display_mode_select_", i)]], {
        display_mode[[as.character(i)]] <- if (isTRUE(input[[paste0("display_mode_select_", i)]])) {
          "classement"
        } else {
          "nps"
        }
      }, ignoreNULL = TRUE, ignoreInit = FALSE)
    })
  })
  
  
  # Graphiques
  lapply(1:5, function(i) {
    output[[paste0("result_plot_quanti_", i)]] <- renderPlotly({
      req(data_reactive()[[i]], input[[paste0("quanti_", i)]])
      df <- data_reactive()[[i]]
      prefix <- input[[paste0("quanti_", i)]]
      selected_modalite_val <- selected_modalites[[as.character(i)]]
      mode_selected <- if (isTRUE(input[[paste0("display_mode_select_", i)]])) "classement" else "nps"
      
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres? ?\\.*\\]", ignore_case = TRUE))]
      
      colname <- if (!is.null(selected_modalite_val) && selected_modalite_val != "") {
        selected_modalite_val
      } else if (length(cols_found) > 0) cols_found[1] else return(NULL)
      
      if (!colname %in% names(df)) return(NULL)
      
      notes <- suppressWarnings(as.numeric(df[[colname]]))
      notes <- na.omit(notes)
      
      if (length(notes) == 0) return(plot_histogramme(df, prefix))
      
      is_nps_range <- all(notes %in% 0:5) || all(notes %in% 0:10)
      
      if (is_nps_range) {
        if (mode_selected == "classement") {
          if (!is.null(selected_modalite_val) && selected_modalite_val != "") {
            return(plot_histogramme(df, selected_modalite_val))
          } else {
            if (!any(str_detect(names(df), stringr::fixed(paste0(prefix, "["))))) {
              return(plot_histogramme(df, prefix))
            } else {
              return(plot_classement_grouped(df, prefix))
            }
          }
        } else {
          if (!is.null(selected_modalite_val) && selected_modalite_val != "") {
            return(calc_nps_auto_by_code(df, selected_modalite_val))
          } else {
            if (!any(str_detect(names(df), stringr::fixed(paste0(prefix, "["))))) {
              return(calc_nps_auto_by_code(df, prefix))
            } else {
              return(plot_nps_grouped(df, prefix))
            }
          }
        }
      } else {
        return(plot_histogramme(df, prefix))
      }
    })
  })
  
  
  # Médians
  lapply(1:5, function(i) {
    output[[paste0("mediane_quanti_", i)]] <- renderValueBox({
      req(data_reactive()[[i]], input[[paste0("quanti_", i)]])
      df <- data_reactive()[[i]]
      prefix <- input[[paste0("quanti_", i)]]
      selected_modalite_val <- selected_modalites[[as.character(i)]]
      
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres? ?\\.*\\]", ignore_case = TRUE))]
      
      colname <- NULL
      if (!is.null(selected_modalite_val) && selected_modalite_val != "") {
        colname <- selected_modalite_val
      } else if (length(cols_found) == 1) {
        colname <- cols_found[1]
      } else if (length(cols_found) > 1) {
        return(valueBox("", "Question (multi-Choix)", icon = icon("ban"), color = "orange"))
      } else return(NULL)
      
      if (!colname %in% names(df)) return(NULL)
      notes <- suppressWarnings(as.numeric(df[[colname]]))
      notes <- na.omit(notes)
      
      if (length(notes) == 0) {
        valueBox("Pas de données", "Médiane", icon = icon("circle-info"), color = "teal")
      } else {
        valueBox(paste0(round(median(notes), 1)), "Médiane", icon = icon("circle-info"), color = "aqua", width = 3)
      }
    })
  })
  
####################################################################
  ### Analyse Bivariée
####################################################################
  # Réactifs pour stocker les sélections
  code_prefix_bivar <- reactiveValues()
  modalite_bivar <- reactiveValues()
  colonne_bivariee2 <- reactiveValues()
  
  # UI dynamique pour chaque fichier (QUALI BIVAR)
  output$bivar_quali_blocks_ui <- renderUI({
    req(data_reactive())
    fichiers <- fichiers_excel()$name
    n <- length(data_reactive())
    
    # Tous les blocs seront sur 12 colonnes 
    box_width <- 12
    
    blocs <- lapply(seq_len(n), function(i) {
      df <- data_reactive()[[i]]
      
      # Colonnes à exclure
      colonnes_autres <- names(df)[str_detect(names(df), regex("\\[Autres?.*?\\]", ignore_case = TRUE))|
                                     str_detect(names(df), regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
      colonnes_10plus <- names(df)[sapply(df, function(col) {
        (is.character(col) || is.factor(col)) && length(unique(na.omit(col))) > 15
      })]
      
      colonnes_valides <- setdiff(names(df), union(colonnes_autres, colonnes_10plus))
      
      ns <- paste0("bivarquali_", i)
      box(
        title = paste("Fichier :", fichiers[i]),
        status = "info", solidHeader = TRUE, width = box_width,
        selectInput(ns, "Variable principale :", choices = questions_quali_list()[[i]]),
        uiOutput(paste0("modalite_bivar_ui_", i)),
        selectInput(paste0("colonne_bivariee2_", i), "Variable secondaire :", choices = colonnes_valides),
        plotlyOutput(paste0("bivar_quali_plot_", i))
      )
    })
    
    tagList(blocs)  # Tous les blocs l’un sous l’autre
  })
  
  # Observe les sélections
  observe({
    lapply(seq_len(length(data_reactive())), function(i) {
      observeEvent(input[[paste0("bivarquali_", i)]], {
        code_prefix_bivar[[as.character(i)]] <- input[[paste0("bivarquali_", i)]]
        modalite_bivar[[as.character(i)]] <- ""
      })
      observeEvent(input[[paste0("colonne_bivariee2_", i)]], {
        colonne_bivariee2[[as.character(i)]] <- input[[paste0("colonne_bivariee2_", i)]]
      })
    })
  })
  
  # UI des modalités spécifiques
  lapply(1:5, function(i) {
    output[[paste0("modalite_bivar_ui_", i)]] <- renderUI({
      req(data_reactive()[[i]], code_prefix_bivar[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefix_bivar[[as.character(i)]]
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
      
      if (length(cols_found) > 1) {
        selectInput(paste0("modalite_bivar_", i), "Modalité spécifique :", 
                    choices = c("Toutes les modalités" = "", cols_found), selected = "")
      } else NULL
    })
  })
  
  # Stocker les modalités sélectionnées
  observe({
    lapply(1:5, function(i) {
      observeEvent(input[[paste0("modalite_bivar_", i)]], {
        modalite_bivar[[as.character(i)]] <- input[[paste0("modalite_bivar_", i)]]
      })
    })
  })
  
  # Graphe bivarié quali
  lapply(1:5, function(i) {
    output[[paste0("bivar_quali_plot_", i)]] <- renderPlotly({
      req(data_reactive()[[i]], code_prefix_bivar[[as.character(i)]], colonne_bivariee2[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefix_bivar[[as.character(i)]]
      second_var <- colonne_bivariee2[[as.character(i)]]
      modalite <- modalite_bivar[[as.character(i)]]
      
      if (!is.null(modalite) && modalite != "") {
        plot_bivariate_by_code(df, modalite, second_var)
      } else {
        plot_bivariate_by_code(df, prefix, second_var)
      }
    })
  })
###################################################  
  ### Cas Bivarié quantitatif
  # Réactifs
  code_prefix_bivar_num <- reactiveValues()
  modalite_bivar_num <- reactiveValues()
  group_var_num <- reactiveValues()
  display_mode_bivar <- reactiveValues()  # Nouveau : mode d'affichage (NPS / classement)
  
  # UI dynamique
  output$bivar_quanti_blocks_ui <- renderUI({
    req(data_reactive())
    fichiers <- fichiers_excel()$name
    n <- length(data_reactive())
    
    blocs <- lapply(seq_len(n), function(i) {
      box_width <- 12  # Toujours pleine largeur pour un affichage vertical
      
      qualitatives <- names(data_reactive()[[i]])[sapply(data_reactive()[[i]], 
                                                         function(col) {(is.character(col) || is.factor(col))&& 
                                                             length(unique(na.omit(col))) <= 15})]
      qualitatives <- qualitatives[!str_detect(qualitatives, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
      qualitatives <- qualitatives[!str_detect(qualitatives, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
      
      box(
        title = paste("Fichier :", fichiers[i]), 
        width = box_width,
        status = "info", 
        solidHeader = TRUE,
        
        # Nouvelle ligne avec question quantitative + sélecteur côte à côte
        fluidRow(
          column(
            width = 8,
            selectInput(paste0("code_prefix_bivar_num_", i), "Question quantitative :", choices = questions_quanti_list()[[i]])
          ),
          column(
            width = 4,
            uiOutput(paste0("display_mode_bivar_ui_", i))
          )
        ),
        
        uiOutput(paste0("modalite_bivar_num_ui_", i)),
        selectInput(paste0("group_var_num_", i), "Variable Secondaire :", choices = qualitatives),
        plotlyOutput(paste0("bivar_quanti_plot_", i)),
        uiOutput(paste0("medianes_groupes_", i))
      )
    })
    
    tagList(blocs)  
  })
  
  
  # Observe les sélections
  observe({
    lapply(1:5, function(i) {
      observeEvent(input[[paste0("code_prefix_bivar_num_", i)]], {
        code_prefix_bivar_num[[as.character(i)]] <- input[[paste0("code_prefix_bivar_num_", i)]]
        modalite_bivar_num[[as.character(i)]] <- ""
        
        # ✔ Ne pas écraser le choix existant s'il existe déjà
        if (is.null(display_mode_bivar[[as.character(i)]])) {
          display_mode_bivar[[as.character(i)]] <- "nps"
        }
      })
      observeEvent(input[[paste0("group_var_num_", i)]], {
        group_var_num[[as.character(i)]] <- input[[paste0("group_var_num_", i)]]
      })
    })
  })
  
  
  # UI modalités spécifiques
  lapply(1:5, function(i) {
    output[[paste0("modalite_bivar_num_ui_", i)]] <- renderUI({
      req(data_reactive()[[i]], code_prefix_bivar_num[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefix_bivar_num[[as.character(i)]]
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
      
      if (length(cols_found) > 1) {
        selectInput(paste0("modalite_bivar_num_", i), "Variable spécifique :",
                    choices = c("Toutes les variables" = "", cols_found), selected = "")
      } else NULL
    })
  })
  
  # Stocker la modalité sélectionnée
  observe({
    lapply(1:5, function(i) {
      observeEvent(input[[paste0("modalite_bivar_num_", i)]], {
        modalite_bivar_num[[as.character(i)]] <- input[[paste0("modalite_bivar_num_", i)]]
      })
    })
  })
  
  # Nouveau : UI du sélecteur "Voir avec NPS / Voir classement"
  lapply(1:5, function(i) {
    output[[paste0("display_mode_bivar_ui_", i)]] <- renderUI({
      req(data_reactive()[[i]], code_prefix_bivar_num[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefix_bivar_num[[as.character(i)]]
      modalite <- modalite_bivar_num[[as.character(i)]]
      
      cols_found <- names(df) %>% str_subset(stringr::fixed(prefix))
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
      cols_found <- cols_found[!str_detect(cols_found, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
      
      col_to_check <- if (!is.null(modalite) && modalite != "") modalite else if(length(cols_found)>0) cols_found[1] else NULL
      if (is.null(col_to_check)) return(NULL)
      
      notes <- suppressWarnings(as.numeric(df[[col_to_check]]))
      notes <- na.omit(notes)
      
      if (length(notes) > 0 && (all(notes %in% 0:5) || all(notes %in% 0:10))) {
        selected_val <- display_mode_bivar[[as.character(i)]] %||% "nps"
        fluidRow(
          column(
            width = 12, align = "center",
            div(
              style = "display: inline-flex; align-items: center; gap: 10px; margin-top: 20px;",
              tags$span(style = "margin-left: 40px; margin-top: -14px;", "NPS"),
              shinyWidgets::materialSwitch(
                inputId = paste0("display_mode_bivar_select_", i),
                label = NULL,
                value = (isolate(display_mode_bivar[[as.character(i)]] %||% "nps") == "classement"),
                status = "default",
                right = TRUE
              ),
              tags$span(style = "margin-left: 24px; margin-top: -14px;", "Classement")
            )
          )
        )
        
      } else {
        NULL
      }
    })
  })
  
  # Stocker la sélection du mode d'affichage
  observe({
    lapply(1:5, function(i) {
      observeEvent(input[[paste0("display_mode_bivar_select_", i)]], {
        display_mode_bivar[[as.character(i)]] <- if (isTRUE(input[[paste0("display_mode_bivar_select_", i)]])) {
          "classement"
        } else {
          "nps"
        }
      }, ignoreNULL = TRUE, ignoreInit = FALSE)
      
    })
  })
  
  # Graphe bivarié quanti avec condition affichage NPS ou classement
  lapply(1:5, function(i) {
    output[[paste0("bivar_quanti_plot_", i)]] <- renderPlotly({
      req(data_reactive()[[i]], code_prefix_bivar_num[[as.character(i)]], group_var_num[[as.character(i)]])
      df <- data_reactive()[[i]]
      prefix <- code_prefix_bivar_num[[as.character(i)]]
      group_var <- group_var_num[[as.character(i)]]
      modalite <- modalite_bivar_num[[as.character(i)]]
      #mode_selected <- input[[paste0("display_mode_bivar_select_", i)]] %||% "nps"
      mode_selected <- display_mode_bivar[[as.character(i)]] %||% "nps"
      
      cols_to_use <- if (!is.null(modalite) && modalite != "") {
        modalite
      } else {
        names(df) %>% str_subset(stringr::fixed(prefix))
      }
      
      plotdata <- df %>%
        select(all_of(c(group_var, cols_to_use))) %>%
        pivot_longer(cols = all_of(cols_to_use), names_to = "variable", values_to = "value") %>%
        mutate(value = suppressWarnings(as.numeric(value))) %>%
        filter(!is.na(value))
      
      is_nps <- all(plotdata$value %in% 0:10) || all(plotdata$value %in% 0:5)
      
      if (is_nps) {
        if (mode_selected == "classement") {
          # Détermine si c’est multi-modalité
          cols_found <- names(df) %>%
            str_subset(stringr::fixed(prefix)) %>%
            discard(~ str_detect(.x, regex("\\[Autres?.*?\\]", ignore_case = TRUE))) %>%
            discard(~ str_detect(.x, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE)))
          
          is_multi_modalite <- length(cols_found) > 1
          stacked_val <- FALSE
          
          if (is_multi_modalite && (is.null(modalite) || modalite == "")) {
            stacked_val <- TRUE
          }
          
          plot_bivariate_classement(
            data = df,
            code_prefix = if (!is.null(modalite) && modalite != "") modalite else prefix,
            var_group = group_var,
            stacked = stacked_val
          )
        }
        else {
          plot_bivariate_NPS_by_code(df, if (!is.null(modalite) && modalite != "") modalite else prefix, group_var)
        }
      } else {
        plot_bar_by_group(df, cols = cols_to_use, group_var = group_var)
      }
    })
  })
  
  # Médianes par groupes (inchangé)
  lapply(1:5, function(i) {
    output[[paste0("medianes_groupes_", i)]] <- renderUI({
      req(data_reactive()[[i]], code_prefix_bivar_num[[as.character(i)]], group_var_num[[as.character(i)]])
      
      df <- data_reactive()[[i]]
      prefix <- code_prefix_bivar_num[[as.character(i)]]
      group_var <- group_var_num[[as.character(i)]]
      modalite <- modalite_bivar_num[[as.character(i)]]
      
      cols_found <- names(df) %>%
        str_subset(stringr::fixed(prefix)) %>%
        discard(~ str_detect(.x, regex("\\[Autres?.*?\\]", ignore_case = TRUE)))%>%
        discard(~ str_detect(.x, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE)))
      
      if (length(cols_found) > 1 && (is.null(modalite) || modalite == "")) {
        return(
          valueBox(
            value = "",
            subtitle = "Question multi-modalité",
            icon = icon("ban"),
            color = "orange",
            width = 3
          )
        )
      }
      
      cols_to_use <- if (!is.null(modalite) && modalite != "") modalite else cols_found
      
      plotdata <- df %>%
        select(all_of(c(group_var, cols_to_use))) %>%
        pivot_longer(cols = all_of(cols_to_use), names_to = "variable", values_to = "value") %>%
        mutate(
          value = suppressWarnings(as.numeric(value)),
          group = .data[[group_var]]
        ) %>%
        filter(!is.na(value), !is.na(group)) %>%
        group_by(group) %>%
        summarise(mediane = median(value, na.rm = TRUE), .groups = "drop")
      
      div(
        style = "display: flex; flex-wrap: wrap; gap: 10px;",
        lapply(1:nrow(plotdata), function(j) {
          div(
            style = "flex: 1 0 100px; max-width: 130px;",
            valueBox(
              value = round(plotdata$mediane[j], 1),
              subtitle =  paste("médiane :", plotdata$group[j]),
              icon = icon("circle-info"),
              color = "aqua",
              width = NULL
            )
          )
        })
      )
    })
  })
  
  
  ###Analyse de Texte
  
  # 1. Génération dynamique des interfaces par fichier
  output$texte_blocks_ui <- renderUI({
    req(data_reactive())
    n <- length(data_reactive())
    fichiers <- fichiers_excel()$name
    box_width <- if (n == 1) 12 else 6
    
    blocs <- lapply(seq_len(n), function(i) {
      df <- data_reactive()[[i]]
      
      # Colonnes contenant "[Autres]" dans le nom
      autres_cols <- names(df)[grepl("\\[Autres?.*?\\]", names(df), ignore.case = TRUE)|
                                 grepl("\\[Commentaires?.*?\\]", names(df), ignore.case = TRUE)]
      
      # Colonnes avec > 15 modalités uniques (en excluant les NA)
      vars_plus_15_modalites <- names(df)[sapply(df, function(col) {
        is.character(col) || is.factor(col)
      }) & sapply(df, function(col) {
        length(unique(na.omit(col))) > 15
      })]
      
      # Combinaison des deux (sans doublons)
      colonnes_texte <- unique(c(autres_cols, vars_plus_15_modalites))
      
      
      box(
        title = paste("Fichier :", fichiers[i]),
        width = box_width, status = "info", solidHeader = TRUE,
        
        selectInput(paste0("question_texte_autres_", i), "Variable contenant du texte :", choices = colonnes_texte),
        uiOutput(paste0("liste_reponses_autres_", i)),
        plotOutput(paste0("nuage_mots_", i)),
        uiOutput(paste0("table_mots_", i))
      )
    })
    
    # Affichage en une ou deux colonnes selon le nombre
    if (n == 1) {
      tagList(blocs)
    } else {
      rows <- lapply(seq(1, length(blocs), by = 2), function(i) {
        fluidRow(
          blocs[[i]],
          if ((i + 1) <= length(blocs)) blocs[[i + 1]] else NULL
        )
      })
      tagList(rows)
    }
  })
  
  lapply(1:5, function(i) {
    output[[paste0("liste_reponses_autres_", i)]] <- renderUI({
      req(input[[paste0("question_texte_autres_", i)]], data_reactive()[[i]])
      df <- data_reactive()[[i]]
      reponses <- df[[input[[paste0("question_texte_autres_", i)]]]]
      reponses <- na.omit(reponses)
      reponses <- reponses[reponses != ""]
      
      if (length(reponses) == 0) {
        return(h4("Aucune réponse disponible pour cette question."))
      }
      
      tagList(
        h4(paste("Réponses à :", input[[paste0("question_texte_autres_", i)]])),
        div(style = "max-height: 500px; overflow-y: auto; border: 1px solid #ccc; padding: 10px;",
          lapply(seq_along(reponses), function(j) {
            div(style = "margin-bottom: 10px; padding: 8px; border-bottom: 1px solid #ddd;",
                strong(paste0("Réponse ", j, " : ")),
                span(reponses[[j]]))
        }))
      )
    })
    
    output[[paste0("nuage_mots_", i)]] <- renderPlot({
      req(input[[paste0("question_texte_autres_", i)]], data_reactive()[[i]])
      df <- data_reactive()[[i]]
      reponses <- df[[input[[paste0("question_texte_autres_", i)]]]]
      reponses <- na.omit(reponses)
      reponses <- reponses[reponses != ""]
      
      if (length(reponses) == 0) return(NULL)
      
      texte <- corpus(reponses)
      texte <- tokens(texte, what = "word", remove_punct = TRUE)
      texte <- tokens_remove(texte, stopwords("french"), padding = FALSE)
      texte <- dfm(texte)
      
      textplot_wordcloud(texte, max_words = 30, scale = c(7, 0.8), color = c('pink', 'orange', 'red'))
    })
    
    # UI + scroll autour du tableau
    output[[paste0("table_mots_", i)]] <- renderUI({
      div(
        style = "max-height: 300px; overflow-y: auto; border: 1px solid #ccc; padding: 10px;",
        tableOutput(paste0("table_mots_rempli", i))
      )
    })
    
    output[[paste0("table_mots_rempli", i)]] <- renderTable({
      req(input[[paste0("question_texte_autres_", i)]], data_reactive()[[i]])
      df <- data_reactive()[[i]]
      reponses <- df[[input[[paste0("question_texte_autres_", i)]]]]
      reponses <- na.omit(reponses)
      reponses <- reponses[reponses != ""]
      
      if (length(reponses) == 0) return(NULL)
      
      texte <- corpus(reponses)
      texte <- tokens(texte, what = "word", remove_punct = TRUE)
      texte <- tokens_remove(texte, stopwords("french"), padding = FALSE)
      texte <- dfm(texte)
      
      freq <- textstat_frequency(texte, n = 20)
      table_df <- freq[, c("feature", "frequency")]
      colnames(table_df) <- c("Mot", "Nombre d'apparition")
      table_df
    })
  })
  
  
  ###################################################################
  ##Analyse de Profils
  ################################################################### 
  data_list <- data_reactive
  
  output$profil_blocks_ui <- renderUI({
    req(data_list())
    
    lapply(seq_along(data_list()), function(i) {
      ns <- function(id) paste0("profil", i, "_", id)
      
      # Nom du fichier ou de la data.frame
      fichier <- fichiers_excel()$name[i]
      box(
        title = paste("Analyse des profils -", fichier),  # Utilisation du nom du fichier
        width = 12, status = "primary", solidHeader = TRUE,
        
        selectizeInput(ns("var_actives"), "Variables actives :", 
                       choices = questions_quali_list()[[i]], multiple = TRUE),
        
        uiOutput(ns("modalite_ui")),
        selectizeInput(ns("var_supplementaires"), "Variables supplémentaires :", choices = questions_quali_list()[[i]], multiple = TRUE),
        
        radioButtons(ns("radio"), "Gestion des valeurs manquantes :", choices = list(
          "Avec valeurs manquantes" = 1,
          "Supprimer" = 2,
          "Remplacer" = 3
        )),
        
        htmlOutput(ns("nb_lignes_supprimees_text")),
        
        checkboxInput(ns("filtrer_distance"), 
                      "Afficher uniquement les modalités avec distance > 1", 
                      value = FALSE),
        tagList(
          h4("Graphe ACM — Profil 1"),
          plotlyOutput(ns("graphe_profil1")),
          
          h4("Graphe ACM — Profil 2"),
          plotlyOutput(ns("graphe_profil2"))
        )
        
      )
    })
  })
  
  
  observeEvent(data_list(), {
    nb_fichiers <- length(data_list())
    
    lapply(seq_len(nb_fichiers), function(i) {
      local({
        my_i <- i
        ns <- function(id) paste0("profil", my_i, "_", id)
        
        vals_acm <- reactiveValues(actives = character(), supplementaires = character())
        modalites_memoires <- reactiveValues()
        
        output[[ns("modalite_ui")]] <- renderUI({
          req(input[[ns("var_actives")]], data_list()[[my_i]], questions_quali_list()[[my_i]])
          df <- data_list()[[my_i]]
          question_list <- questions_quali_list()[[my_i]]
          selected_labels <- input[[ns("var_actives")]]
          if (length(selected_labels) == 0) return(NULL)
          
          ui_list <- lapply(selected_labels, function(lbl) {
            code <- lbl
            if (is.null(code) || is.na(code)) return(NULL)
            cols_found <- names(df)[startsWith(names(df), code)]
            cols_found <- cols_found[!stringr::str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))]
            cols_found <- cols_found[!stringr::str_detect(cols_found, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
            if (length(cols_found) > 1) {
              id_input <- paste0("modalite_", code)
              selectizeInput(inputId = ns(id_input),
                             label = paste0("Modalités spécifiques pour ", lbl, " :"),
                             choices = setNames(cols_found, cols_found),
                             selected = isolate(modalites_memoires[[id_input]]),
                             multiple = TRUE)
            } else NULL
          })
          
          do.call(tagList, ui_list)
        })
        
        observe({
          req(input[[ns("var_actives")]])
          question_list <- questions_quali_list()[[my_i]]
          for (lbl in input[[ns("var_actives")]]) {
            code <- lbl
            if (is.null(code)) next
            id_input <- paste0("modalite_", code)
            observeEvent(input[[ns(id_input)]], {
              modalites_memoires[[id_input]] <- input[[ns(id_input)]]
            }, ignoreNULL = FALSE)
          }
        })
        
        observe({
          req(questions_quali_list()[[my_i]])
          choix <- questions_quali_list()[[my_i]]
          
          updateSelectizeInput(session, ns("var_actives"), choices = choix, server = TRUE)
          updateSelectizeInput(session, ns("var_supplementaires"), choices = choix, server = TRUE)
        })
        
        modalites_selectionnees <- reactive({
          req(input[[ns("var_actives")]], questions_quali_list()[[my_i]])
          question_list <- questions_quali_list()[[my_i]]
          selected_labels <- input[[ns("var_actives")]]
          res <- list()
          for (lbl in selected_labels) {
            code <- lbl
            if (is.null(code)) next
            id_input <- paste0("modalite_", code)
            val <- modalites_memoires[[id_input]]
            if (!is.null(val) && length(val) > 0) res[[code]] <- val
          }
          return(res)
        })
        
        rename_columns_for_display <- function(noms) {
          sapply(noms, function(nom) {
            # Extraire la partie avant le premier crochet → code question
            code_question <- sub("\\[.*", "", nom)
            code_question <- trimws(code_question)  # Nettoyage espaces
            
            # Extraire tous les contenus entre crochets
            matches <- stringr::str_match_all(nom, "\\[([^\\]]+)\\]")[[1]]
            
            # Si on a au moins deux crochets, on prend le contenu du 2e
            if (nrow(matches) >= 2) {
              libelle_modalite <- matches[2, 2]
              return(paste0(code_question, ".[", libelle_modalite, "]"))
            } else {
              return(nom)  # Pas deux crochets : on garde tel quel
            }
          }, USE.NAMES = FALSE)
        }
        
        donnees_modifiees <- reactive({
          req(data_list()[[my_i]], input[[ns("var_actives")]], questions_quali_list()[[my_i]])
          df <- data_list()[[my_i]]
          question_list <- questions_quali_list()[[my_i]]
          selection_modalites <- modalites_selectionnees()
          
          actives_cols <- character(0)
          supp_cols <- character(0)
          
          # Collecte des colonnes actives
          for (lbl in input[[ns("var_actives")]]) {
            code <- lbl
            if (!is.null(selection_modalites[[code]])) {
              actives_cols <- c(actives_cols, selection_modalites[[code]])
            } else {
              cols_found <- names(df)[startsWith(names(df), code)]
              cols_found <- cols_found[!stringr::str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))&
                                         !stringr::str_detect(cols_found, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
              actives_cols <- c(actives_cols, cols_found)
            }
          }
          
          # Collecte des colonnes supplémentaires
          if (!is.null(input[[ns("var_supplementaires")]])) {
            for (lbl in input[[ns("var_supplementaires")]]) {
              code <- lbl
              if (!is.null(selection_modalites[[code]])) {
                supp_cols <- c(supp_cols, selection_modalites[[code]])
              } else {
                cols_found <- names(df)[startsWith(names(df), code)]
                cols_found <- cols_found[!stringr::str_detect(cols_found, regex("\\[Autres?.*?\\]", ignore_case = TRUE))&
                                           !stringr::str_detect(cols_found, regex("\\[Commentaires?.*?\\]", ignore_case = TRUE))]
                supp_cols <- c(supp_cols, cols_found)
              }
            }
          }
          
          df <- df[, unique(c(actives_cols, supp_cols)), drop = FALSE]
          
          # Traitement des valeurs manquantes
          df[] <- lapply(df, function(col) {
            if (is.character(col) || is.factor(col)) {
              col[col %in% c("N/A", "null")] <- NA
              return(as.factor(col))
            } else col
          })
          
          # Gestion des valeurs manquantes selon le choix
          mode_na <- input[[ns("radio")]]
          
          if (mode_na == 2) {
            nb_avant <- nrow(df)
            df_clean <- na.omit(df)
            nb_supprimees <- nb_avant - nrow(df_clean)
            vals_acm$nb_lignes_supprimees <- nb_supprimees
            vals_acm$nb_lignes_initial <- nb_avant
            
            colnames(df_clean) <- rename_columns_for_display(colnames(df_clean))
            
            vals_acm$actives <- rename_columns_for_display(actives_cols)
            vals_acm$supplementaires <- rename_columns_for_display(supp_cols)
            return(df_clean)
          }
          if (mode_na == 3) df <- imputeMCA(df,ncp=3)$completeObs
          
          colnames(df) <- rename_columns_for_display(colnames(df))
          
          vals_acm$actives <- rename_columns_for_display(actives_cols)
          vals_acm$supplementaires <- rename_columns_for_display(supp_cols)
          
          return(df)
        })
        
        acm_resultats <- reactive({
          df <- donnees_modifiees()
          req(ncol(df) > 0, length(vals_acm$actives) > 0)
          
          MCA(df,
              quali.sup = if (length(vals_acm$supplementaires) > 0)
                which(colnames(df) %in% vals_acm$supplementaires)
              else NULL,
              graph = FALSE)
        })
        
        output[[ns("nb_lignes_supprimees_text")]] <- renderUI({
          HTML(paste0(
            "Nombre de lignes supprimées (valeurs manquantes) : ",
            "<span style='color:red;'>", vals_acm$nb_lignes_supprimees, "</span>",
            " sur ",
            "<span style='color:red;'>", vals_acm$nb_lignes_initial, "</span>"
          ))
        })
        
        # Graphe Profil 1
        output[[ns("graphe_profil1")]] <- renderPlotly({
          res <- acm_resultats()
          validate(
            need(!is.null(res), "Impossible de réaliser l'ACM : vérifiez vos sélections.")
          )
          
          var_coords_actives <- as.data.frame(res$var$coord)
          
          if (!is.null(res$quali.sup) && !is.null(res$quali.sup$coord) && nrow(res$quali.sup$coord) > 0) {
            var_coords_sup <- as.data.frame(res$quali.sup$coord)
            var_coords <- rbind(var_coords_actives, var_coords_sup)
            var_coords$type <- c(rep("Active", nrow(var_coords_actives)),
                                 rep("Supplémentaire", nrow(var_coords_sup)))
          } else {
            var_coords <- var_coords_actives
            var_coords$type <- "Active"
          }
          
          colnames(var_coords) <- gsub(" ", ".", colnames(var_coords))
          var_coords$label <- rownames(var_coords)
          var_coords$distance <- sqrt(var_coords$Dim.1^2 + var_coords$Dim.2^2)
          
          if (isTRUE(input[[ns("filtrer_distance")]])) {
            var_coords <- subset(var_coords, distance > 1)
          }
          
          validate(
            need(nrow(var_coords) > 0, "Aucune modalité avec une distance > 1.")
          )
          
          palette_couleur <- c(
            "Active" = "blue",
            "Supplémentaire" = "darkred"
          )
          
          p <- fviz_mca_biplot(res,
                               label = "none",
                               col.var = NA,
                               col.ind = "grey",
                               axes = c(1, 2),
                               repel = TRUE)
          
          p$layers <- Filter(function(layer) {
            !inherits(layer$geom, "GeomPoint") || !any(grepl("var", deparse(layer$mapping)))
          }, p$layers)
          
          p <- p +
            geom_point(data = var_coords, 
                       aes(x = Dim.1, y = Dim.2, color = type, shape = type),
                       size = 2) +
            geom_text(data = var_coords,
                      aes(x = Dim.1, y = Dim.2, label = label, color = type),
                      size = 3, vjust = -1.8) +
            scale_color_manual(values = palette_couleur) +
            scale_shape_manual(values = c("Active" = 16, "Supplémentaire" = 17)) +
            theme_minimal() +
            theme(legend.position = "right")
          
          ggplotly(p)
        })
        
        # Graphe Profil 2
        output[[ns("graphe_profil2")]] <- renderPlotly({
          res <- acm_resultats()
          validate(
            need(!is.null(res), "Impossible de réaliser l'ACM : vérifiez vos sélections.")
          )
          
          var_coords_actives <- as.data.frame(res$var$coord)
          
          if (!is.null(res$quali.sup) && !is.null(res$quali.sup$coord) && nrow(res$quali.sup$coord) > 0) {
            var_coords_sup <- as.data.frame(res$quali.sup$coord)
            var_coords <- rbind(var_coords_actives, var_coords_sup)
            var_coords$type <- c(rep("Active", nrow(var_coords_actives)),
                                 rep("Supplémentaire", nrow(var_coords_sup)))
          } else {
            var_coords <- var_coords_actives
            var_coords$type <- "Active"
          }
          
          colnames(var_coords) <- gsub(" ", ".", colnames(var_coords))
          var_coords$label <- rownames(var_coords)
          var_coords$distance <- sqrt(var_coords$Dim.2^2 + var_coords$Dim.3^2)
          
          if (isTRUE(input[[ns("filtrer_distance")]])) {
            var_coords <- subset(var_coords, distance > 1)
          }
          
          validate(
            need(nrow(var_coords) > 0, "Aucune modalité avec une distance > 1.")
          )
          
          palette_couleur <- c(
            "Active" = "blue",
            "Supplémentaire" = "darkred"
          )
          
          p <- fviz_mca_biplot(res,
                               label = "none",
                               col.var = NA,
                               col.ind = "grey",
                               axes = c(2, 3),
                               repel = TRUE)
          
          p$layers <- Filter(function(layer) {
            !inherits(layer$geom, "GeomPoint") || !any(grepl("var", deparse(layer$mapping)))
          }, p$layers)
          
          p <- p +
            geom_point(data = var_coords, 
                       aes(x = Dim.2, y = Dim.3, color = type, shape = type),
                       size = 2) +
            geom_text(data = var_coords,
                      aes(x = Dim.2, y = Dim.3, label = label, color = type),
                      size = 3, vjust = -1.8) +
            scale_color_manual(values = palette_couleur) +
            scale_shape_manual(values = c("Active" = 16, "Supplémentaire" = 17)) +
            theme_minimal() +
            theme(legend.position = "right")
          
          ggplotly(p)
        })
      })
    })
  })
}

