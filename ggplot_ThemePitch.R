#' Adds soccer pitch markings as a layer for use in a ggplot plot.
#'
#' @param colour Colour of pitch outline.
#' @param fill Colour of pitch fill
#' @param limits Whether to adjust the plot limits to display the whole pitch.
#' @param dimensions A list containing the pitch dimensions to draw. See `help(pitch_opta)`.
#'
#' @return list of ggplot geoms to be added to a ggplot plot
#'
#' @examples
#' library(ggplot2)
#'
#' shots_data <- data.frame(x = c(90, 85, 82, 78, 83),
#'                          y = c(43, 40, 52, 56, 44))
#'
#' ggplot() +
#' annotate_pitch(dimensions = pitch_custom,
#'               colour = "white",
#'               fill = "#9CB76D")  +
#'        theme_pitch(aspect_ratio = 68 / 105) +
#'        direction_label(x_label = 0,
#'                        y_label = 39) 
#'
#' @export

        # theme pitch (wrapper)
                # field annotations
                annotate_pitch <- function(dimensions = list(length = 105,
                                           width = 68,
                                           penalty_box_length = 16.5,
                                           penalty_box_width = 40.32,
                                           six_yard_box_length = 5.5,
                                           six_yard_box_width = 18.32,
                                           penalty_spot_distance = 11,
                                           goal_width = 7.32,
                                           origin_x = -52.5,
                                           origin_y = -34),
                                           limits = T,
                                           colour = "white",
                                           fill = "#9CB76D") {
                        
                        # Pitch components ---------------------------------------------
                        # Add markings for parts of a soccer pitch.
                        
                        annotate_base_pitch <- function(colour, fill, spec) {
                                midpoint <- pitch_center(spec)
                                centre_circle_radius <-
                                        4.6  # Not actually on the same scale as the pitch
                                
                                list(
                                        ggplot2::annotate(
                                                geom = "rect",
                                                xmin = spec$origin_x,
                                                xmax = spec$origin_x + spec$length,
                                                ymin = spec$origin_y,
                                                ymax = spec$origin_y + spec$width,
                                                colour = colour,
                                                fill = fill
                                        ),
                                        # Centre circle
                                        ggplot2::annotation_custom(
                                                grob = grid::circleGrob(
                                                        r  = grid::unit(1, "npc"),
                                                        gp = grid::gpar(
                                                                col  = colour,
                                                                fill = fill,
                                                                lwd = 2
                                                        )
                                                ),
                                                xmin = midpoint$x - centre_circle_radius,
                                                xmax = midpoint$x + centre_circle_radius,
                                                ymin = midpoint$y - centre_circle_radius,
                                                ymax = midpoint$y + centre_circle_radius
                                        ),
                                        # Centre spot
                                        ggplot2::annotate(
                                                geom = "point",
                                                x = midpoint$x,
                                                y = midpoint$y,
                                                colour = colour,
                                                fill = fill
                                        ),
                                        # Halfway line
                                        ggplot2::annotate(
                                                "segment",
                                                x = midpoint$x,
                                                xend = midpoint$x,
                                                y = spec$origin_y,
                                                yend = spec$origin_y + spec$width,
                                                colour = colour
                                        )
                                )
                        }
                        
                        annotate_penalty_box <- function(colour, fill, spec) {
                                midpoint <- pitch_center(spec)
                                penalty_radius <- 4.6
                                
                                list(
                                        # Right penalty area
                                        ggplot2::annotation_custom(
                                                grob = grid::circleGrob(
                                                        r  = grid::unit(1, "npc"),
                                                        gp = grid::gpar(
                                                                col  = colour,
                                                                fill = fill,
                                                                lwd = 2
                                                        )
                                                ),
                                                xmin = spec$origin_x + spec$length - spec$penalty_spot_distance - penalty_radius,
                                                xmax = spec$origin_x + spec$length - spec$penalty_spot_distance + penalty_radius,
                                                ymin = midpoint$y - penalty_radius,
                                                ymax = midpoint$y + penalty_radius
                                        ),
                                        ggplot2::annotate(
                                                geom = "rect",
                                                xmin = spec$origin_x + spec$length - spec$penalty_box_length,
                                                xmax = spec$origin_x + spec$length,
                                                ymin = midpoint$y - spec$penalty_box_width / 2,
                                                ymax = midpoint$y + spec$penalty_box_width / 2,
                                                colour = colour,
                                                fill = fill
                                        ),
                                        ## Penalty spot
                                        ggplot2::annotate(
                                                geom = "point",
                                                x = spec$origin_x + spec$length - spec$penalty_spot_distance,
                                                y = midpoint$y,
                                                colour = colour,
                                                fill = fill
                                        ),
                                        # Left penalty area
                                        ggplot2::annotation_custom(
                                                grob = grid::circleGrob(
                                                        r  = grid::unit(1, "npc"),
                                                        gp = grid::gpar(
                                                                col  = colour,
                                                                fill = fill,
                                                                lwd = 2
                                                        )
                                                ),
                                                xmin = spec$origin_x + spec$penalty_spot_distance - penalty_radius,
                                                xmax = spec$origin_x + spec$penalty_spot_distance + penalty_radius,
                                                ymin = midpoint$y - penalty_radius,
                                                ymax = midpoint$y + penalty_radius
                                        ),
                                        ggplot2::annotate(
                                                geom = "rect",
                                                xmin = spec$origin_x,
                                                xmax = spec$origin_x + spec$penalty_box_length,
                                                ymin = midpoint$y - spec$penalty_box_width / 2,
                                                ymax = midpoint$y + spec$penalty_box_width / 2,
                                                colour = colour,
                                                fill = fill
                                        ),
                                        ## Penalty spot
                                        ggplot2::annotate(
                                                geom = "point",
                                                x = spec$origin_x + spec$penalty_spot_distance,
                                                y = midpoint$y,
                                                colour = colour,
                                                fill = fill
                                        )
                                )
                        }
                        
                        annotate_six_yard_box <- function(colour, fill, spec) {
                                midpoint <- pitch_center(spec)
                                
                                list(
                                        ggplot2::annotate(
                                                geom = "rect",
                                                xmin = spec$origin_x + spec$length - spec$six_yard_box_length,
                                                xmax = spec$origin_x + spec$length,
                                                ymin = midpoint$y - spec$six_yard_box_width / 2,
                                                ymax = midpoint$y + spec$six_yard_box_width / 2,
                                                colour = colour,
                                                fill = fill
                                        ),
                                        ggplot2::annotate(
                                                geom = "rect",
                                                xmin = spec$origin_x,
                                                xmax = spec$origin_x + spec$six_yard_box_length,
                                                ymin = midpoint$y - spec$six_yard_box_width / 2,
                                                ymax = midpoint$y + spec$six_yard_box_width / 2,
                                                colour = colour,
                                                fill = fill
                                        )
                                )
                        }
                        
                        annotate_goal <- function(colour, fill, spec) {
                                midpoint <- pitch_center(spec)
                                goal_depth <- 2
                                
                                list(
                                        ggplot2::annotate(
                                                geom = "rect",
                                                xmin = spec$origin_x + spec$length,
                                                xmax = spec$origin_x + spec$length + goal_depth,
                                                ymin = midpoint$y - spec$goal_width / 2,
                                                ymax = midpoint$y + spec$goal_width / 2,
                                                colour = colour,
                                                fill = fill
                                        ),
                                        ggplot2::annotate(
                                                geom = "rect",
                                                xmin = spec$origin_x - goal_depth,
                                                xmax = spec$origin_x,
                                                ymin = midpoint$y - spec$goal_width / 2,
                                                ymax = midpoint$y + spec$goal_width / 2,
                                                colour = colour,
                                                fill = fill
                                        )
                                )
                        }
                        
                        pitch_center <- function(spec) {
                                list(x = spec$origin_x + spec$length/2,
                                     y = spec$origin_y + spec$width/2)
                        }
                        
                        marking_layers <- unlist(
                                list(
                                        annotate_base_pitch(colour, fill, dimensions),
                                        annotate_penalty_box(colour, fill, dimensions),
                                        annotate_six_yard_box(colour, fill, dimensions),
                                        annotate_goal(colour, fill, dimensions)
                                ),
                                recursive = FALSE
                        )
                        
                        if (!limits) {
                                return(marking_layers)
                        }
                        
                        # Leave room for full pitch + goals and direction_label by default
                        limit_layers <- list(
                                ggplot2::xlim(
                                        dimensions$origin_x - 10,
                                        dimensions$origin_x + dimensions$length + 10
                                ),
                                ggplot2::ylim(
                                        dimensions$origin_y - 15,
                                        dimensions$origin_y + dimensions$width + 10
                                )
                        )
                        
                        append(marking_layers,
                               limit_layers)
                }
                
                theme_pitch <- function(aspect_ratio = 68/105) {
                        
                        theme_basic <- ggplot2::theme(
                                panel.grid.major   = ggplot2::element_blank(),
                                panel.grid.minor   = ggplot2::element_blank(),
                                axis.title         = ggplot2::element_blank(),
                                axis.ticks         = ggplot2::element_blank(),
                                axis.text          = ggplot2::element_blank(),
                                axis.line          = ggplot2::element_blank(),
                                panel.background   = ggplot2::element_blank(),
                                panel.border       = ggplot2::element_blank(),
                                legend.text        = ggplot2::element_text(family = "Arial",colour = "#3C3C3C",size = 10),
                                legend.key         = ggplot2::element_blank(),
                                legend.title       = ggplot2::element_text(colour = "#3C3C3C",face = "bold",size = 10),
                                legend.title.align = 10,
                                legend.position    = "top", 
                                legend.direction   = "horizontal"
                        )
                        
                        if (!is.null(aspect_ratio)) {
                                return(list(theme_basic,
                                            ggplot2::theme(aspect.ratio = aspect_ratio)))
                        }
                        
                        list(theme_basic)
                }
                
                
                # direction
                direction_label <- function(x_label = 0,
                                            y_label = 39,
                                            label_length = 20,
                                            colour = "black",
                                            direction = "horizontal") {
                        
                        layer <- if (direction == "vertical") {
                                list(
                                        geom_segment(
                                                aes(
                                                        x = x_label - (label_length / 2),
                                                        y = y_label,
                                                        xend = x_label + (label_length / 2),
                                                        yend = y_label
                                                ),
                                                arrow = arrow(
                                                        length = unit(0.25, "cm"),
                                                        type = "closed"
                                                ),
                                                colour = colour
                                        ),
                                        annotate(
                                                "text",
                                                x = x_label,
                                                y = y_label + 3,
                                                label = c("Direction of attack"),
                                                vjust = 1.5,
                                                size = 3,
                                                colour = "black",
                                                angle = 90
                                        )
                                )
                        } else {
                                list(
                                        geom_segment(
                                                aes(
                                                        x = x_label - (label_length / 2),
                                                        y = y_label,
                                                        xend = x_label + (label_length / 2),
                                                        yend = y_label
                                                ),
                                                arrow = arrow(
                                                        length = unit(0.25, "cm"),
                                                        type = "closed"
                                                ),
                                                colour = colour
                                        ),
                                        annotate(
                                                "text",
                                                x = x_label,
                                                y = y_label + 3,
                                                label = c("Direction of attack"),
                                                vjust = 1.5,
                                                size = 3,
                                                colour = "black",
                                                angle = 0
                                        )
                                )
                        }
                        
                        return(layer)
                }
                
                
                
