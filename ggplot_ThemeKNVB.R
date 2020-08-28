#' Een ggplot theme KNVB functie
#'
#' Functie om conform KNVB huisstijl grafieken te maken.
#' @param text.size Grootte van de tekst? Defaults to 16.
#' @param grid Assenstelsel? Defaults to Y.
#' @export theme
#' ggplot_ThemeKNVB()
# -----------------------------------------------------------------------------


    ggplot_ThemeKNVB <- function(
        # Type grafiek
        type = "bar",
        # Grootte van de tekst
        text.size = 16,
        # As voor raster
        grid = "Y") {
        
        theme <- if (type == "bar") {
            #Staafdiagram
            theme(
                # Achtergrond van plot
                plot.background = element_rect(fill = NA),
                panel.background =  element_rect(fill = NA),
                # Top, Rechts, Onder, Links
                plot.margin = unit(c(1.5, 1, 0.5, 0.5), "cm"),
                panel.spacing = grid::unit(2, "lines"),
                # Text overall
                text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    size = text.size
                ),
                # Text titel, subtitel, caption
                plot.title = element_text(
                    family = "Arial",
                    face = "bold",
                    colour = "#3C3C3C",
                    size = rel(2)
                ),
                plot.subtitle = element_text(size = rel(1.2),
                                             colour = "#A0A0A3"),
                plot.caption = element_text(colour = "#A0A0A3",
                                            size = rel(0.8)),
                # Text assen
                axis.text.x = element_text(size = rel(1)),
                axis.text.y = element_text(size = rel(1)),
                axis.title = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1.2),
                    hjust = 0.5
                ),
                # Lijn
                line = element_line(linetype = "solid", colour = "#505053"),
                # Lijn op assen
                axis.line = element_line(
                    linetype = "solid",
                    colour = "#505053",
                    size = 0.2
                ),
                # Aspunten
                axis.ticks = element_line(
                    linetype = "solid",
                    colour = "#505053",
                    size = 0.2
                ),
                axis.ticks.length = unit(8, "pt"),
                # Raster horizontale lijnen
                panel.grid.major.y = if (grid == "Y") {
                    element_line(colour = "#505053", size = 0.2)
                } else if (grid == "XY" | grid == "YX") {
                    element_line(colour = "#505053", size = 0.2)
                } else if (grid == " ") {
                    element_line(NA)
                } else {
                    element_line(NA)
                },
                # Raster verticale lijnen
                panel.grid.major.x = if (grid == "X") {
                    element_line(colour = "#505053", size = 0.2)
                } else if (grid == "XY" | grid == "YX") {
                    element_line(colour = "#505053", size = 0.2)
                } else if (grid == " ") {
                    element_line(NA)
                } else {
                    element_line(NA)
                },
                # Legenda
                legend.key.size = unit(10, "pt"),
                legend.text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    size = rel(1)
                ),
                legend.key = element_blank(),
                legend.title = element_text(
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1)
                ),
                legend.title.align = 10,
                legend.position = "top",
                legend.direction = "horizontal",
                
                strip.background = element_blank(),
                strip.placement = "outside",
                strip.text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1)
                )
            )
        } else if (type == "waffle") {
            # Lege theme
            theme(
                # Achtergrond van plot
                plot.background = element_rect(fill = NA),
                panel.background =  element_rect(fill = NA),
                panel.spacing = grid::unit(2, "lines"),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                # Top, Rechts, Onder, Links
                plot.margin = unit(c(1.5, 1, 0.5, 0.5), "cm"),
                panel.spacing = grid::unit(2, "lines"),
                # Text assen
                axis.text = element_blank(),
                axis.title = element_blank(),
                # Aspunten
                axis.ticks = element_blank(),
                # Lijn op assen
                axis.line = element_blank(),
                # Text overall
                text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    size = text.size
                ),
                # Text titel, subtitel, caption
                plot.title = element_text(
                    family = "Arial",
                    face = "bold",
                    colour = "#3C3C3C",
                    size = rel(2)
                ),
                plot.subtitle = element_text(size = rel(1.2),
                                             colour = "#A0A0A3"),
                plot.caption = element_text(colour = "#A0A0A3",
                                            size = rel(0.8)),
                # Legenda 
                legend.key.size = unit(15, "pt"),
                legend.text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    size = rel(1)
                ),
                legend.key = element_blank(),
                legend.title = element_text(
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1)
                ),
                legend.title.align = 10,
                legend.position = "top",
                legend.direction = "horizontal",
                
                strip.background = element_blank(),
                strip.placement = "outside",
                strip.text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1)
                )
            )
            
        } else if (type == "table") {
            # Tabel
            theme(
                # Rect
                plot.background = element_rect(fill = NA),
                # Top, Rechts, Onder, Links
                plot.margin = unit(c(1.5, 1, 0.5, 0.5), "cm"),
                panel.background =  element_rect(fill = NA),
                panel.spacing = grid::unit(5, "lines"),
                # Text overall
                text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    size = text.size
                ),
                # Text titel, subtitel, caption
                plot.title = element_text(
                    family = "Arial",
                    face = "bold",
                    colour = "#3C3C3C",
                    size = rel(2)
                ),
                plot.subtitle = element_text(size = rel(1.2),
                                             colour = "#A0A0A3"),
                plot.caption = element_text(colour = "#A0A0A3",
                                            size = rel(0.8)),
                # Axis tekst
                axis.text.x = element_text(size = rel(1.2), face = "bold"),
                axis.text.y = element_text(size = rel(1.2), face = "bold"),
                axis.title = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1.2),
                    hjust = 0.5
                ),
                # Lijn
                line = element_blank(),
                # Lijn op assen
                axis.line.y = element_blank(),
                axis.line.x = element_line(
                    linetype = "solid",
                    colour = "#505053",
                    size = 0.5
                ),
                panel.grid.major.y = element_line(NA),
                panel.grid.major.x = element_line(NA),
                # Aspunten
                axis.ticks = element_blank(),
                # Legenda
                legend.key.size = unit(10, "pt"),
                legend.text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    size = rel(1)
                ),
                legend.key = element_blank(),
                legend.title = element_text(
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1)
                ),
                legend.title.align = 10,
                legend.position = "top",
                legend.direction = "horizontal",
                
                strip.background = element_blank(),
                strip.placement = "outside",
                strip.text = element_text(
                    family = "Arial",
                    colour = "#3C3C3C",
                    face = "bold",
                    size = rel(1)
                )
            )
        }
    }

            
            

    

    
    