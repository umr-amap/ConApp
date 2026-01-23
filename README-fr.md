# ConApp

> Une application web pour aider à mettre en œuvre des stratégies de conservation en Afrique

[**English**](README.md)

## Auteurs

**Nicolas Texier**<sup>1,2,3</sup>, **Gilles Dauby**<sup>4</sup>, **Tariq Stévart**<sup>1,3,5</sup>

<sup>1</sup> Missouri Botanical Garden, Africa & Madagascar Department, 4344 Shaw Blvd., St. Louis, MO 63110, USA
<sup>2</sup> Faculty of Sciences, Evolutionary Biology and Ecology, Université Libre de Bruxelles, CP160/12, 50 Av. F. Roosevelt, 1050 Brussels, Belgium
<sup>3</sup> Herbarium et Bibliothèque de Botanique africaine, CP 265, Université Libre de Bruxelles, Blvd. du Triomphe, 1050, Brussels, Belgium
<sup>4</sup> AMAP, Université Montpellier, IRD, CNRS, CIRAD, INRAE, Montpellier University, France
<sup>5</sup> Jardin botanique de Meise, Nieuwelaan 38, 1860, Meise, Belgium

**Contact :**
Nicolas Texier : ntexier@mobot.org
Gilles Dauby : gilles.dauby@ird.fr
Tariq Stévart : tstevart@mobot.org

## À propos

La planification de la conservation et de l'aménagement du territoire doit correctement prendre en compte les risques de conservation liés à la biodiversité. Cependant, il est souvent difficile pour les bureaux d'étude, les gestionnaires du territoire et les décideurs d'évaluer quelles espèces sont menacées sur un site et d'accéder à leurs données de distribution actualisées, en particulier pour les plantes. Par exemple, les données sur la distribution des plantes menacées en Afrique tropicale sont rarement disponibles dans la Liste Rouge de l'UICN, ou ne sont pas mises à jour, ce qui rend obsolètes les outils d'évaluation des risques tels que l'outil Integrated Biodiversity Assessment Tool (IBAT).

Nous présentons **ConApp**, une application web qui peut être utilisée pour :

1. **Identifier les espèces de plantes potentiellement menacées** dans une zone définie
2. **Obtenir des informations sur la distribution des espèces d'arbres menacées** au Gabon
3. **Conduire une évaluation préliminaire multi-espèces** du risque d'extinction selon le critère B de la Liste Rouge de l'UICN

Cette application permet d'interroger à la fois la base de données GBIF pour les plantes et une version régulièrement mise à jour de la base de données vérifiée RainBio, qui comprend des données non publiques provenant de parcelles et de transects en Afrique centrale, donnant ainsi accès aux données d'occurrence les plus récentes et/ou vérifiées pour les plantes d'Afrique tropicale.

L'évaluation préliminaire et automatique du risque d'extinction est basée sur l'aire de répartition géographique des espèces (critère B de la Liste Rouge) à l'aide d'une version améliorée du package ConR de R. L'application comprend des couches de données spatialisées sur les menaces et les zones protégées en Afrique tropicale, ce qui permet de calculer assez précisément le paramètre du nombre de localités.

Cette application est conçue pour planifier le développement éventuel d'un site en tenant compte de la présence d'espèces végétales potentiellement menacées d'extinction, ainsi que pour les scientifiques qui souhaitent évaluer le risque d'extinction d'un grand groupe d'espèces en utilisant les bases de données disponibles ou leur propre base de données d'occurrences.

**Mots-clés :** conservation, aménagement du territoire, risque d'extinction, Liste Rouge, évaluations automatiques

## Accès en ligne

Vous pouvez accéder à l'application directement en ligne sans installation à l'adresse :

**https://gdauby.shinyapps.io/conapp/**

⚠️ **Note :** La version en ligne est hébergée sur un serveur avec une RAM limitée. Les requêtes ou calculs volumineux peuvent provoquer un plantage de l'application. Pour une utilisation intensive ou de grands jeux de données, nous recommandons d'installer et d'exécuter l'application localement dans R (voir les instructions d'installation ci-dessous).

## Installation

ConApp est un package R qui fournit une application web Shiny. Installez-le directement depuis GitHub :

```r
# Installer remotes si nécessaire
install.packages("remotes")

# Installer la dépendance ConR
remotes::install_github("gdauby/ConR")

# Installer ConApp
remotes::install_github("umr-amap/ConApp", upgrade = FALSE)
```

## Utilisation

Lancer l'application :

```r
library(ConApp)

# Lancer en anglais (par défaut)
launch()

# Lancer en français
launch(lang = "fr")
```

L'application s'ouvrira dans votre navigateur par défaut sur le port 5791.

## Documentation

Pour les développeurs travaillant avec ce code, consultez [CLAUDE.md](CLAUDE.md) pour des informations détaillées sur l'architecture, les commandes de développement et les détails d'implémentation.

## Licence

GPL-2

## Remerciements

Cette application est basée sur le package ConR et intègre des données provenant de GBIF et de la base de données RainBio pour les occurrences de plantes d'Afrique centrale.
