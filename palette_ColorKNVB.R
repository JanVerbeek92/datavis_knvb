#' Een KNVB color palettefunctie
#'
#' Functie om conform KNVB huisstijl grafieken te maken.
#' @param colorpalette Kleuren palet? Defaults to KNVB.
#' @export KNVB.theme
#' colorpalette()

        colorpalette <- function(colorpalette = "KNVB") {
                colors <-
                        if (colorpalette == "KNVB") {
                                c(
                                `donkeroranje`   = "#F36C21",
                                `donkeroranje_1` = "#bd4a0a",
                                `donkeroranje_5` = "#f79c6a",
                                `donkerblauw`    = "#253780",
                                `donkerblauw_1`  = "#C9D1F0",
                                `donkerblauw_5`  = "#131B40",
                                `turkoois`       = "#00AADB",
                                `turkoois_1`     = "#C5F2FF",
                                `turkoois_5`     = "#00556D",
                                `licht grijs`    = "#F2F2F2",
                                `donker grijs`   = "#7F7F7F")
                                } else {
                                c(
                                `groen` = '#008000',
                                `rood`  = '#ff0000'
                                )
                                }
                return(colors)
        }
