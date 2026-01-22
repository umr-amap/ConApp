# ConApp

> A web application to help implement conservation strategies in Africa

[**Français**](README-fr.md)

## Authors

**Nicolas Texier**<sup>1,2,3</sup>, **Tariq Stévart**<sup>1,3,5</sup>, **Gilles Dauby**<sup>4</sup>

<sup>1</sup> Missouri Botanical Garden, Africa & Madagascar Department, 4344 Shaw Blvd., St. Louis, MO 63110, USA
<sup>2</sup> Faculty of Sciences, Evolutionary Biology and Ecology, Université Libre de Bruxelles, CP160/12, 50 Av. F. Roosevelt, 1050 Brussels, Belgium
<sup>3</sup> Herbarium et Bibliothèque de Botanique africaine, CP 265, Université Libre de Bruxelles, Blvd. du Triomphe, 1050, Brussels, Belgium
<sup>4</sup> AMAP, Université Montpellier, IRD, CNRS, CIRAD, INRAE, Montpellier University, France
<sup>5</sup> Jardin botanique de Meise, Nieuwelaan 38, 1860, Meise, Belgium

**Contact:**
Nicolas Texier: ntexier@mobot.org
Gilles Dauby: gilles.dauby@ird.fr
Tariq Stévart: tstevart@mobot.org

## About

Conservation and land-use planning needs to take proper account of conservation risk linked to biodiversity. However, it is often difficult for consultancies, land managers and decision-makers to assess which species are threatened on a site and to access their updated distribution data, particularly for plants. For example, data on distribution of threatened plants in tropical Africa are rarely available in the IUCN Red List, or are not updated, rendering risk screening tools such as the Integrated Biodiversity Assessment Tool (IBAT) obsolete.

We present **ConApp**, a web application that can be used to:

1. **Identify potentially threatened plant species** in a given area
2. **Obtain information on the distribution of threatened tree species** in Gabon
3. **Conduct a preliminary multi-species assessment** of extinction risk under criterion B of the IUCN Red List

This application allows querying both the GBIF database for plants and a regularly updated version of the verified RainBio database, which includes data from non-public plots and transects in Central Africa, thus providing access to the most recent and/or verified occurrence data for plants in tropical Africa.

The preliminary and automatic extinction risk assessment is based on the geographic range of species (criterion B of the Red List) using an improved version of the ConR package of R. The application includes layers of spatialised data on threats and protected areas in tropical Africa, allowing the parameter of the number of locations to be calculated fairly accurately.

This application is designed to plan the possible development of a site taking into account the presence of plant species potentially threatened by extinction, as well as for scientists who want to assess the extinction risk of a large group of species using available databases or their own occurrence database.

**Keywords:** conservation, land-use planning, extinction risk, Red List, automatic assessments

## Installation

ConApp is an R package that provides a Shiny web application. Install it directly from GitHub:

```r
# Install remotes if needed
install.packages("remotes")

# Install ConR dependency
remotes::install_github("gdauby/ConR")

# Install ConApp
remotes::install_github("umr-amap/ConApp", upgrade = FALSE)
```

## Usage

Launch the application:

```r
library(conrappli)

# Launch in English (default)
launch()

# Launch in French
launch(lang = "fr")
```

The application will open in your default web browser on port 5791.


## License

GPL-2

