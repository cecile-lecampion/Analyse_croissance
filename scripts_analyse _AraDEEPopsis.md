



# Utilisation des scripts pour l'analyse des résultats d'AraDEEPopsis

Les scripts permettent de faire l'analyse des données issues de l'outil AraDEEPopsis.

> araDEEPopsis: From images to phenotypic traits using deep transfer learning. 
>
> Patrick Hüther, Niklas Schandry, Katharina Jandrasits, Ilja Bezrukov, Claude Becke - Gregor-Mendel-Institute - Vienne https://www.biorxiv.org/content/10.1101/2020.04.01.018192v2 

Ces résultats sont sous la forme d'un fichier `.csv` de 80 colonnes et N lignes (nb de photos qui ont été analysées).

Les scripts permettent :

- de sélectionner le paramètre que vous souhaitez analyser
- de faire une analyse statistique des données en fonction de leur distribution, en calculant l'intervalle de confiance
- de tracer la courbe de croissance (moyenne du paramètre choisi) avec l'erreur standard
- faire un bar plot pour un jour donné avec marque de significativité sur le graph

## Choix du script à utiliser

Deux script sont disponibles pour analyser vos données :

`Analyse_croissance_cas1.R` : Ce script permet de faire l’analyse des données de suivi de croissance de plusieurs lignées dans une seule condition de croissance. 

Afin de permettre l’automatisation de l’analyse il faut respecter le motif suivant dans le nom des images :  `Day_Line_index.jpg`

`Analyse_croissance_cas2.R`  : Ce script permet de faire l’analyse des données de suivi de croissance d’une lignée dans plusieurs conditions de croissance. 

Afin de permettre l’automatisation de l’analyse il faut respecter le motif suivant dans le nom des images : `Day_Treatment_index.jpg`

Pourquoi ces contraintes ?

Dans le fichier de résultat d’AraDEEPopsis les deux première colonnes sont «file» qui contient le nom de l’image (ex : Day_Treatment_index) et  «format» qui contient l’extension (ex : jpg). Le script contient donc une commande qui permet de diviser la colonne «file» en 3 colonnes «Day», «Treatment» ou «Line» et «Index» qui seront utilisée pour grouper les données lors de l’analyse. 

exemple :

```R
df_data <- tidyr::separate(data = df_data, col = file, c("Day",  "Treatment", "Index"), sep = "_", remove=TRUE)
```

Si vous respecter les contraintes dans le nommage des photos le script pourra s’exécuter de manière automatique. Si ces contraintes ne sont pas respectées il vous faudra «tripatouiller» le code.



> Si vous avez un design de manip plus compliqué (par exemple plusieurs lignées et plusieurs conditions), venez me voir pour que l’on fasse un script qui réponde à vos besoin spécifiques.

## Le script pas à pas

Le script est composé de plusieurs partie : 	

- une partie à modifier qui permet de le personnaliser 
-  une partie regroupant les fonctions utilisées dans le script
- la partie principale : celle qui fait l'analyse

Les modifications que vous devez faire sont dans la première partie qui est commune au deux scripts.

```R
###Partie du script à modifier###########################################################

# Le répertoire de travail
setwd("PATH_TO_YOUR_DIRECTORY")
  
# Variable pour le script
  
 # les données :
 DATA <- "Path_to_results" #(Nom du fichier de resultat Aradeepopsis du type : aradeepopsis_traits.csv)
  
 # Le paramètre étudié
 VALUE <- 'le_nom_de_la_colonne_contenant_le_parametre'    # exemple : plant_region_area, class_norm_perimeter, class_senesc_area, 
  #class_senesc_perimeter, class_antho_area, class_antho_perimeter
  
 # La densité de la photo en pixel par cm
 PIXBYCM <- Valeur_numérique
  
 # Préciser si vous étudier une surface à convertir en cm2 ou un périmètre à convertir en cm
  
 IS_AREA <- Un_booléen # TRUE ou FALSE 

# Définir le jour pour la représentation des résultat en bar plot
GrowingDay <- Valeur_numérique

# Définir la lignée de référence
# Pour le cas 1
RefLine <- "nom_de_la_lignée_de_référence" # exemple : "col_0"

# Pour le cas 2
RefCond <- "nom_de_la_condition_de_référence" # exemple : "normal-light"

# Variables de personnalisation du graphique pour l'analyse statistique
#======================================================================================================================
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
#======================================================================================================================
TITLE <- ""
Y_AXIS <- ""

#######################################################################################################################

```

### 1. Les informations de bases : à modifier obligatoirement

#### a. Choix du répertoire de travail

```R
# Le répertoire de travail
setwd("PATH_TO_YOUR_DIRECTORY")
```

Il s’agit de donner au programme le répertoire dans lequel se trouve le fichier à analyser et ou vous allez enregistrer les résultats.

Comme pour les noms de fichiers, il est recommandé de ne pas utiliser d’accent, d’espace ou de caractère spéciaux autre que `-` ou `_`.

N’oubliez pas les quotes `" "`. En l’absence de ces dernière R n’identifirait pas ce qui est écrit comme le chemin vers votre répertoire mais comme un objet R, ce dernier n’existant pas il retournerait une erreur du type :

```R
setwd(PATH_TO_YOUR_DIRECTORY)
Error in setwd(PATH_TO_YOUR_DIRECTORY) : 
  object 'PATH_TO_YOUR_DIRECTORY' not found
```

Votre commande devra ressembler à ceci :

```R
setwd("~/partage/Data/test")
```

SI vous ne connaissait pas le chemin de votre répertoire vous pouvez utiliser l’interface graphique de R-studio. 

Dans le cadre en bas à droite, sélectionner l’onglet `Files`, parcourrez l’arborescence, entrez dans le répertoire de votre choix,  puis cliquez sur le bouton `More` avec la roue dentée bleue et sélectionnez `Set As Working Directory`

<img src=".images/scripts_analyse _AraDEEPopsis/image-20210422092326570.png" alt="image-20210422092326570" style="zoom:50%;" />

#### b. Les données

```R
# les données :
  DATA <- "results_File" #(Nom du fichier de resultat Aradeepopsis du type : aradeepopsis_traits.csv)
```

Il s’agit d’affecter à la variable `DATA` le nom du fichier qui contient les résultats. 

Si ce fichier se trouve dans le répertoire de travail, il suffit d’inscrire son nom entre quote, sinon il faut inscrire le chemin complet.

La commande devra ressembler à :

```R
DATA <- "aradeepopsis_traits.csv" 
```

#### c. Le paramètre à analyser

Le fichier de résultat d’AraDEEPopsis contient 80 colonnes et autant de ligne que de photos analysées.

Le script permet l’analyse d’un paramètre à la fois. Il faut donc sélectionner la colonne qui vous intéresse.  Le nom doit être **exactement** celui d’une colonne du fichier de résultat.

```R
# Le paramètre étudié
  VALUE <- 'le_nom_de_la_colonne_contenant_le_parametre'
```

Quelques exemples parmi les possibilités :

plant_region_area

class_norm_perimeter

class_senesc_area

class_senesc_perimeter

class_antho_area

class_antho_perimeter

```R
VALUE <- 'plant_region_area'
```

#### d. La densité en pixel de la photo

```R
# La densité de la photo en pixel par cm
  PIXBYCM <- Valeur_numérique
```

AraDEEPopsis rend des résultats en pixel. Afin de les convertir en cm il faut connaitre la densité de la photo en pixels/cm. 

Pour cela il vous faut une image, prise dans les mêmes conditions que vos photos de plantes, d’une surface dont vous connaissez les dimensions. A l’aide d’un logiciel d’analyse d’image vous devez déterminer le nombre de pixel dans cette surface et en déduire la densité de vos image. 

Si vous disposez de la valeur en inch, faite la conversion en cm (1 inch = 2.54 cm).

```R
PIXBYCM <- 122
```

#### e. Déclarer le type de paramètre que vous analysez

La formule permettant la conversion en cm n’est pas la même si vous analysez un paramètre de type périmètre, pour lequel on converti les pixels en cm ou un paramètre de surface pour lequel on converti en cm2.

Pour un périmètre : 
$$
périmètre = pixel/PIXBYCM
$$
Pour une surface :
$$
surface = pixel/PIXBYCM^2
$$
La variable `IS_AREA` est une variable de type Booléen, c’est à dire qu’elle ne peut prendre que deux valeurs, dans ce cas `TRUE` ou `FALSE`. SI vous analysez une surface vous devez affecter `TRUE` à la variable  `IS_AREA` , si vous analysez un périmètre alors elle prendra la valeur `FALSE`.

*NB : C’est [George Boole](https://fr.wikipedia.org/wiki/George_Boole) qui invente en 1854 l’algèbre binaire ou Booléenne qui connaitra de nombreuses applications en informatique, dont celle décrite ci dessus.*

### 2. Les paramètres de personnalisation des graphiques : les modifications sont optionnelles

#### a. Choix des couleurs 

-  <u>Pour les points</u> 

```R
COULEUR <- "#d9c396"
```

Le code hexadécimal des couleurs est utilisé. 

Pour modifier la couleur, taper le code (ex : "#d9c396") dans la barre de recherche Google. La page de résultat commence par un pavé interactif pour le choix des couleurs.

<img src=".images/scripts_analyse _AraDEEPopsis/image-20210517161736266.png" alt="image-20210517161736266" style="zoom:50%;" />

1 - Choisissez la nuance sur la barre "Arc en ciel"

2 - Déplacez le rond blanc sur la zone de votre choix

3 - Le code hexadécimal de la couleur apparait dans le cadre HEX

- <u>Pour les intervalles de confiance</u>

```R
PALETTE <- "Paired"
```

Ci dessous la liste des palettes compatibles avec les daltoniens;

<img src=".images/scripts_analyse _AraDEEPopsis/colopicker.png" alt="colopicker" style="zoom:50%;" />

Vous pouvez aussi faire apparaitre la liste en tapant dans la console (cadre en bas à gauche) :

```R
display.brewer.all(colorblindFriendly = TRUE)
```

1. **Palettes séquentielles** (première liste de couleurs), qui sont adaptées aux données ordonnées qui évoluent de bas en haut (gradient). Les noms des palettes sont : Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu YlOrBr, YlOrRd.
2. **Palettes qualitatives** (deuxième liste de couleurs), qui conviennent le mieux pour représenter des données nominales ou catégorielles. Ils n’impliquent pas de différences d’ampleur entre les groupes. Les noms des palettes sont : Dark2, Paired, Set2.
3. **Palettes divergentes** (troisième liste de couleurs), qui mettent autant l’accent sur les valeurs critiques du milieu et les extrêmes aux deux extrémités de la plage de données. Les palettes divergentes sont : BrBG, PiYG, PRGn, PuOr, RdBu, RdYlBu

#### b. Aspect du graphique représentant l’intervalle de confiance

- <u>Orientation des étiquettes de l'axe des abscisses</u>

```R
orientation_xlabels <- theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))
```

Les étiquettes de l'axe peuvent être orientée horizontalement ou tournées de 90°.

La valeur par défaut est : horizontale.

Pour modifier ce paramètre il faut modifier les valeur de la variable `orientation_xlabels` (ligne 18). 

Les valeurs par défaut sont : `angle = 0, vjust = 0, hjust=0`

Pour tourner les étiquettes : `angle = 90, vjust = 0.5, hjust=1`

- <u>Position et aspect des éléments de groupement</u>

    ```R
    strip_pos <- theme(panel.spacing = unit(0.3, "lines"), 
                      strip.background = element_rect(colour="black", fill=NA),
                      strip.placement = "inside")
    ```

    Par défaut le graph présente n (nombre de niveaux du facteur de groupement) panneaux séparés, avec mention du facteur de groupement correspondnat dans un cadre à l'intérieur du graph.

    Les valeurs par défaut sont : 

    - Pour l'espacement entre les panneaux

    `panel.spacing = unit(0.3, "lines")`

    La valeur peut être diminuée (0) ou augmentée selon que l'on souhaite plus ou moins espacer les panneaux

    - Pour l'aspect du "strip" contenant le facteur de groupement

    `strip.background = element_rect(colour="black", fill=NA)`

    Pour supprimer le cadre il faut remplacer `"black"` par `NA`

    - Pour la position du  "strip" contenant le facteur de groupement

    `strip.placement = "inside"`

    Pour positionner le strip à extérieur du graph remplacer `"inside"` par `"outside"`

#### c. Aspect de la courbe de croissance

Pour la courbe de croissance les couleurs sont les mêmes que pour les intervalles de confiance.

Par défaut, il n’y a pas de titre principal ni de nom pour l’axe y du graphique. Vous pouvez ajouter l’un ou l’autre ou les deux en complétant la commande suivante :

Pour le titre principal

```R
TITLE <- ""
```

Pour l’axe des y

```R
Y_AXIS <- ""
```

exemple :

```R
TITLE <- "Croissance de la lignée X"
Y_AXIS <- "Surface en cm2"
```

## Exécution du script

Pour exécuter le script, vous avez deux options : Dans le cadre en haut à gauche, vous disposez de deux boutons, `Run` et `Source`. 

Le bouton `Run` vous permet d’exécuter le script ligne par ligne

Le bouton `Source` exécute l’ensemble du script en un seul clic. 

![Capture d’écran 2022-06-20 à 14.28.37](.images/scripts_analyse _AraDEEPopsis/Capture d’écran 2022-06-20 à 14.28.37-5728299.png)

## Sauvegarde et export des fichiers

Les fichiers générés durant l’analyse sont automatiquement enregistrés dans votre répertoire de travail.

- Tableau de données pour l’intervalle de confiance : `Summary.txt`

Les résultats statistiques : 

- Les données du test anova :`Anova.txt`

ou

- Les données du test de Kruskal-Wallis : `Kruskal.txt`

Puis lorsqu’ils ont été réalisé (les tests anova ou Kruskal-Wallis montrent l’existence de différence) :

- Les résultats du test de Tukey : `Tukey.txt`

ou

- Les résultats du test de Dunn : `Dunn.txt`

Les informations sur la session : `InfoSession.txt` 

## Sauvegarde des graphiques

Les graphiques ne sont pas automatiquement sauvegarder. La sauvegarde manuelle vous permet de choisir le format (png, jpg, pdf, svg...) dans lequel vous voulez conserver votre graphique et aussi de choisir la taille.

Pour cela il faut utiliser le bouton `Export` du cadran en bas à droite (dans lequel le graph s’affiche), choisir `Save as image`

<img src=".images/scripts_analyse _AraDEEPopsis/Capture d’écran 2022-06-20 à 14.33.37.png" alt="Capture d’écran 2022-06-20 à 14.33.37" style="zoom:50%;" />



L’interface suivante apparait. Vous pouvez alors modifier le format, et la taille puis avoir une prévisualisation de votre graph.

<img src=".images/scripts_analyse _AraDEEPopsis/Capture d’écran 2022-06-20 à 14.35.28.png" alt="Capture d’écran 2022-06-20 à 14.35.28" style="zoom:50%;" />



## Citations

<u>Outils</u>

> Patrick Hüther, Niklas Schandry, Katharina Jandrasits, Ilja Bezrukov, Claude Becke. *araDEEPopsis: From images to phenotypic traits using deep transfer learning*.  - Gregor-Mendel-Institute - Vienne https://www.biorxiv.org/content/10.1101/2020.04.01.018192v2 



<u>R packages</u>

> Clarke, Erik, and Scott Sherrill-Mix. 2017. *Ggbeeswarm: Categorical Scatter (Violin Point) Plots*. https://CRAN.R-project.org/package=ggbeeswarm.

> Hope, Ryan M. 2013. *Rmisc: Rmisc: Ryan Miscellaneous*. https://CRAN.R-project.org/package=Rmisc.

> Kassambara, Alboukadel. 2021. *Rstatix: Pipe-Friendly Framework for Basic Statistical Tests*. https://CRAN.R-project.org/package=rstatix.

> Mangiafico, Salvatore. 2021. *Rcompanion: Functions to Support Extension Education Program Evaluation*. https://CRAN.R-project.org/package=rcompanion.

> Neuwirth, Erich. 2014. *RColorBrewer: ColorBrewer Palettes*. https://CRAN.R-project.org/package=RColorBrewer.

> R Core Team. 2021. *R: A Language and Environment for Statistical Computing*. Vienna, Austria: R Foundation for Statistical Computing. https://www.R-project.org/.

> Wickham, Hadley. 2011. “The Split-Apply-Combine Strategy for Data Analysis.” *Journal of Statistical Software* 40 (1): 1–29. http://www.jstatsoft.org/v40/i01/.

> Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York. https://ggplot2.tidyverse.org.

> Wickham, Hadley. 2021. *Tidyr: Tidy Messy Data*. https://CRAN.R-project.org/package=tidyr.

> Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019. “Welcome to the tidyverse.” *Journal of Open Source Software* 4 (43): 1686. https://doi.org/10.21105/joss.01686.

> Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2021. *Dplyr: A Grammar of Data Manipulation*. https://CRAN.R-project.org/package=dplyr.

> Wickham, Hadley, Jim Hester, and Winston Chang. 2021. *Devtools: Tools to Make Developing r Packages Easier*. https://CRAN.R-project.org/package=devtools.

<u>Script</u>

> Lecampion C : https://github.com/cecile-lecampion/Analyse_croissance
