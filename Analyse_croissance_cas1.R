# Script analyse des résultats issu d'AraDEEPopsis, cas 1 : suivi de croissance, plusieurs lignées
# Author : Cécile Lecampion
# Institut : BIAM -LGBP
# Novembre 2021

# araDEEPopsis: From images to phenotypic traits using deep transfer learning. 
# Patrick Hüther, Niklas Schandry, Katharina Jandrasits, Ilja Bezrukov, Claude Becke - Gregor-Mendel-Institute - 
# Vienne https://www.biorxiv.org/content/10.1101/2020.04.01.018192v2 

# Pour le cas 1, on suit la croissance de plusieurs lignées dans le temps. Le nom des images est donc construit comme suit :
# Day_Line_index.jpg
  
###Partie du script à modifier##################################################################################################

DEBUG <- 1

if (DEBUG == 1) {
  # Le répertoire de travail
  setwd("/Volumes/Disk_4To")
  
  # Variable pour le script
  
  # les données :
  DATA <- "test_aradeep_script.csv" #(chemin complet vers le fichiers de resultat Aradeepopsis du type : aradeepopsis_traits.csv)
  
  # Le paramètre étudié
  VALUE <- 'plant_region_area'    # exemple : plant_region_area, class_norm_perimeter, class_senesc_area, 
  #class_senesc_perimeter, class_antho_area, class_antho_perimeter
  
  # La densité de la photo en pixel par cm
  PIXBYCM <- 122
  
  # Préciser si vous étudier une surface à convertir en cm2 ou un périmètre à convertir en cm
  
  IS_AREA <- TRUE # TRUE ou FALSE 
} else {
  # Le répertoire de travail
  setwd("PATH_TO_YOUR_DIRECTORY")
  
  # Variable pour le script
  
  # les données :
  DATA <- "results_File" #(Nom du fichier de resultat Aradeepopsis du type : aradeepopsis_traits.csv)
  
  # Le paramètre étudié
  VALUE <- 'le_nom_de_la_colonne_contenant_le_parametre'    # exemple : plant_region_area, class_norm_perimeter, class_senesc_area, 
  #class_senesc_perimeter, class_antho_area, class_antho_perimeter
  
  # La densité de la photo en pixel par cm
  PIXBYCM <- Valeur_numérique
  
  # Préciser si vous étudier une surface à convertir en cm2 ou un périmètre à convertir en cm
  
  IS_AREA <- Un_booléen # TRUE ou FALSE 
}

# Variables de personnalisation du graphique pour l'analyse statistique
#=======================================================================================================================================
# Définir la couleur des points
COULEUR <- "#d9c396"

# Palette de couleur pour les intervalles de confiance
PALETTE <- "Paired"

if (!require(ggplot2)) { install.packages("ggplot2") }
library(ggplot2)
# Orientationd des étiquettes de l'axe X.
# Pour des étiquettes horizontales : angle = 0, vjust = 0, hjust=0.
# Pour des étiquettes tournées de 90° : angle = 90, vjust = 0.5, hjust=1
orientation_xlabels <- theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))

# Position et aspect des marqueurs de groupe
# Les valeurs par défaut produise un graph ou les parties du graph sont séparées avec le facteur
# de groupement dans un cadre à l'intérieur du graph.
# On peut modifier ainsi :
# parties jointives : panel.spacing = unit(0, "lines")
# Pas de cadre autour du facteur de groupement : strip.background = element_rect(colour=NA, fill=NA
# facteur de groupement à l'extérieur du graph :strip.placement = "outside" 
strip_pos <- theme(panel.spacing = unit(0.3, "lines"), 
                   strip.background = element_rect(colour="black", fill=NA),
                   strip.placement = "inside")

# Variables de personnalisation du graphique pour la courbe de croissance
#=======================================================================================================================================
TITLE <- ""
Y_AXIS <- ""

################################################################################################################################
  
# Installation des pacakges

if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(devtools)) { install.packages("devtools") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(rcompanion)) { install.packages("rcompanion") }

################################################################################################################################

# Fonction utilisée dans le script

#=======================================================================================================================================
# Vérifie la normalité des données. Sort de la boucle si au moins un des groupes de données ne suit 
# pas une loi normale.
#Retourne TRUE si les données suivent une loi normale
#=======================================================================================================================================
check_normality <- function(shapiro_df) {
  # On suppose que les données sont normales
  flag_normal <- TRUE
  
  for (i in 1 : nrow(shapiro_df)) {
    if(shapiro_df[i, 4] > 0.05) {
      # print(paste0("les données ",shapiro_df$grouping_factor[i],"-", 
      # shapiro_df$plant_line[i], " suivent une loi normale"), quote = FALSE)
      
    } else {
      # print(paste0("les données ",shapiro_df$grouping_factor[i],"-", 
      #          shapiro_df$plant_line[i], " ne suivent pas une loi normale"), quote = FALSE)
      
      # En fait les données ne sont pas normales, pas besoin d'aller plus loin
      flag_normal <- FALSE
      break
    }
  }
  return(flag_normal)
}

#=======================================================================================================================================
# Prend les résultats du test Anova
# Retourne TRUE si au moins une moyenne est significativement différente
#=======================================================================================================================================
check_anova <- function(anova_results) {
  flag_anova <- FALSE
  
  for (i in 1 : nrow(anova_results)) {
    if (anova_results$p[i] < 0.05) {
      print (paste0("Le test Anova compare les moyennes, la pvalue pour le jour ", anova_results$Day[i] , " est < 0.05 ce qui indique qu’au moins 1 des moyennes est différentes des autres, on réalise un test post hoc de Tukey"))
      flag_anova <- TRUE
    } else {
      print (paste0("Le test Anova compare les moyennes, la pvalue pour le jour ", anova_results$Day[i] , " est > 0.05 ce qui indique qu’il n'y a pas de différence entre les moyennes"))
    }
  }    
  
  return(flag_anova)
}

#=======================================================================================================================================
# Fait le test de Kruskal-Wallis
# Retourne TRUE si au moins une médianes est significativement différente
#=======================================================================================================================================
check_kruskal <- function(kruskal_pval) {
  flag_kruskal <- FALSE
  
  for (i in 1 : nrow(kruskal_pval)) {
    if (kruskal_pval$p[i] < 0.05) {
      print (paste0("Le test de Kruskall Wallis compare les médianes, la pvalue pour le groupe ", kruskal_pval$Day[i] , " est < 0.05 ce qui indique qu’au moins 1 des médianes est différentes des autres, on réalise un test post hoc de Dunn"))
      flag_kruskal <- TRUE
    } else {
      print (paste0("Le test de Kruskall Wallis compare les médianes, la pvalue pour le groupe ", kruskal_pval$Day[i] , " est > 0.05 ce qui indique qu’il n'y a pas de différence entre les médianes"))
    }
  }    
  
  return(flag_kruskal)
}

#=======================================================================================================================================
# Fait le test de Dunn
# retourne les pvalue dans un dataframe
#=======================================================================================================================================
test_dunn <- function() {
  pval <- as.data.frame(df_data %>% group_by(Day) %>% dunn_test(mesure ~ Line, p.adjust.method = "BH"))
  print(df_data %>% group_by(Day) %>% dunn_test(mesure ~ Line, p.adjust.method = "BH"))
  return(pval)
}

#=======================================================================================================================================
# Réalise le graphique lorsque les données suivent une loi normale. L'intervalle de confiance est 
# construit autour de la moyenne
#=======================================================================================================================================
plot_normal <- function(df, my_colours, my_summary) {
  p <- ggplot(data=df, aes(x=Line, y=mesure)) +
    geom_quasirandom(dodge.width=0.8,alpha = 0.6, colour=COULEUR) +
    geom_pointrange(data=my_summary, 
                    aes(ymin=mesure - ci, ymax=mesure + ci, color=Line),
                    position=position_dodge(width=0.8)) + 
    scale_colour_manual(values=my_colours) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = VALUE,
                       expand = expansion(mult = c(.1, .1))) +    # échelle augmentée de 10% au dessus du point le plus haut et en dessous du point le plus bas
    facet_wrap(~ Day, strip.position = "bottom", scales = "free_y") +
    theme_classic() + 
    strip_pos +
    orientation_xlabels +
    theme(legend.title=element_blank())
  print(p)
}


#=======================================================================================================================================
# Réalise le graphique lorsque les données ne suivent pas une loi normale. L'intervalle de confiance est 
# construit autour de la médiane
#=======================================================================================================================================
plot_not_normal <- function(df, my_colours, conf_int) {
  # La colonne "median" doit porter le nom "mesured_value" pour le ggplot
  names(conf_int)[4] <- "mesure"
  
  p <- ggplot(data=df, aes(x=Line, y=mesure)) +
    geom_quasirandom(dodge.width=0.8, alpha = 0.6, colour=COULEUR) +
    geom_pointrange(data=conf_int, aes(ymin=Percentile.lower, ymax=Percentile.upper, 
                                       color=Line), position=position_dodge(width=0.8)) +
    scale_colour_manual(values=my_colours) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = VALUE,
                       expand = expansion(mult = c(.1, .1))) + # échelle augmentée de 10% au dessus du point le plus haut et en dessous du point le plus bas
    facet_wrap(~ Day, strip.position = "bottom", scales = "free") +
    theme_classic() +  
    strip_pos +
    orientation_xlabels +
    theme(legend.title=element_blank())
  print(p)
}

#=======================================================================================================================================
# permet d'ajouter les éléments de significativité contenu dans le resulat du test post hoc sur le ggplot 
#=======================================================================================================================================
add_pval <- function(i, df) {
  result <- geom_text(data = data.frame(x = i, y = summary_max$ymax[i] , 
                                        label = paste(df[i, 10] ) ), 
                      mapping = aes(x = x, y = y, label = label), 
                      inherit.aes = FALSE)
  return(result)
}

###Corps du script#######################################################################################################################

# s'assurer que les package rmisc et plyr ne sont pas chargés
# detach(package:Rmisc)
# detach(package:plyr)

# Chargement et préparation des données 

data <- read.csv(DATA, header = TRUE)

# A ce stade les 80 colonnes du fichiers de résultat sont chargées : il faut extraire la colonne contenant le parmètre d'interet
df_data <- data %>% select(file, VALUE)

# Faire de chaque éléments du nom des photo une colonne qui permettra de grouper les éléments.
df_data <- tidyr::separate(data = df_data, col = file, c("Day",  "Line", "Index"), sep = "_", remove=TRUE)

# Trier le data frame
df_data <- df_data[ with(df_data, order(Day, Line, Index)),]

colnames(df_data)[4] <- "mesure"
df_data$Day <- as.factor(df_data$Day)
df_data$Line <- as.factor(df_data$Line)

# Convertir en cm ou en cm2

if (IS_AREA == TRUE) {
  df_data <- df_data %>% mutate(mesure = mesure/PIXBYCM^2)
} else {
  df_data <- df_data %>% mutate(mesure = mesure/PIXBYCM)
}

#___________________________________________________________________________________________________________________________________
# Analyse statitique
#___________________________________________________________________________________________________________________________________

library(ggbeeswarm) # pour la fonction geom_quasirandom
library(RColorBrewer) # pour la définition des couleurs
library(rstatix)  # pour les tests statistiques
library(rcompanion) #pour le calcul de l'intervalle de confiance pour les données non paramétriques
library(dplyr) # pour les fonction %>%, group_by, summarise, select

# Define color
my_colours = brewer.pal(n = 9, PALETTE)[9:3]

# Determining data normality status
shapiro_df <- df_data %>%
  dplyr::group_by(Day, Line) %>%
  summarise(statistic = shapiro.test(mesure)$statistic, 
            p.value = shapiro.test(mesure)$p.value)

flag_normal <- check_normality(shapiro_df)

# Data treatement according to normality status
if(flag_normal == TRUE) {
  print("Les données suivent une loi normale")
  
  # Summary
  if (!require(Rmisc)) {install.packages("Rmisc")}
  library(plyr) # dépendence de rmisc
  library(Rmisc) # pour la commande summarySE
  my_summary <- summarySE(df_data, measurevar="mesure", groupvars=c("Day", "Line"))
  
  detach(package:Rmisc)
  detach(package:plyr)
  
  # Plot
  
  plot_normal(df_data, my_colours, my_summary)
  
  # Stats
  anova_results <- df_data %>% group_by(Day) %>%  anova_test(mesure ~ Line)
  flag_anova <- check_anova(anova_results)
  if (flag_anova == TRUE) {tukey_results <- df_data %>% group_by(Day) %>%  tukey_hsd(mesure ~ Line)
  }
  
  # Sauver les fichiers
  write.table(my_summary, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(anova_results, file = "Anova.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  if (flag_anova == TRUE) {
    write.table(tukey_results[, c(1,3,4, 9, 10)], file = "Tukey.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 
  #ggsave("Plot.svg", width=4, height=5)
  
  
} else {
  print("Les données ne suivent pas une loi normale")
  
  # Summary
  conf_int <- groupwiseMedian(data = df_data,
                                var = "mesure",
                                group = c("Day", "Line"),
                                conf       = 0.95,
                                R          = 5000,
                                percentile = TRUE,
                                bca        = FALSE,
                                digits     = 3)
  
  # Plot
  plot_not_normal(df_data, my_colours, conf_int)
  
  # Stats
  kruskal_pval <- (df_data %>% group_by(Day)%>%kruskal_test(mesure ~ Line)) %>% select(Day, p)
  
  flag_kruskal <- check_kruskal(kruskal_pval)
  if (flag_kruskal == TRUE) { pval_dunn <- test_dunn() }
  
  # Sauver les fichiers
  write.table(conf_int, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(kruskal_pval, file = "Kruskal.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  
  if (flag_kruskal == TRUE) {
    write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = "Dunn.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 
  #ggsave("Plot.svg", width=4, height=5)
}

#___________________________________________________________________________________________________________________________________
# Courbe de croissance
#___________________________________________________________________________________________________________________________________

# Claculer la moyenne et la SD
summary <- df_data %>% dplyr::group_by(Day, Line) %>%
  summarise(mean = mean(mesure), sd= sd(mesure))

# Préparer les donnée spour l'ajout de la significativité
summary_max <- summary %>% group_by(Day) %>% slice_max(mean)
summary_max$ymax <- (summary_max$mean+summary_max$sd)+ 0.6

# Faire la courbe

p2 <- ggplot(summary, aes(x = Day, y = mean, group = 1, color = Line)) + 
  geom_point() + 
  geom_line(aes(group = Line)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05)) +
  theme_classic() +
  scale_colour_manual(values=my_colours)+
  labs(title=TITLE, x="Jours", y = Y_AXIS, color = "Lignées")

# Ajoute les éléments de significativité sur la courbe pour chaque point
p3 <- p2

if(flag_normal == TRUE) {
  if(flag_anova == TRUE) {
    for (i in 1 : nrow(tukey_results)) {
      p3 <- p3 + add_pval(i, tukey_results)
    }
    print(p3)
  }else {
    print(p3)
    print("les données ne sont pas significativement différentes, le test de Tukey n'a pas été réalisé.")
  }
  
}else {
  if(flag_kruskal == TRUE) {
    for (i in 1 : nrow(pval_dunn)) {
      p3 <- p3 + add_pval(i, pval_dunn)
    }
    print(p3)
  }else {
    print(p3)
    print("les données ne sont pas significativement différentes, le test de Dunn n'a pas été réalisé.")
  }
}
  

# Sauver les fichiers
write.table(summary, file = "Summary_curve.txt", 
            quote = FALSE, row.names = FALSE, sep = '\t')

#Environnement
if (!require(devtools)) { install.packages("devtools") }

InfoSession <- devtools::session_info()

sink("InfoSession.txt")
print(InfoSession)
sink()
