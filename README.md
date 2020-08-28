# Datavisualisaties voor de KNVB

Set aan ggplot functies om de meest voorkomende datavisulaties voor de KNVB te maken.


## KNVB ggplot theme voor staafdiagrammen, tabellen en lege grafieken

    ```R
    source('~/datavis_knvb/ggplot_ThemeKNVB.R', echo=TRUE)
    
    ggplot_ThemeKNVB <- function(
        # Type grafiek
        type = "bar",
        # Grootte van de tekst
        text.size = 16,
        # As voor raster
        grid = "Y")
    
    ``` 
    
## KNVB kleuren voor ggplot fill & colors  

    ```R
    source('~/datavis_knvb/palette_ColorKNVB.R', echo=TRUE)
    
    palette <- function(colorpalette = "KNVB")
    
    ``` 