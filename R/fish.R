# thank you ggbernie
# https://github.com/R-CoderDotCom/ggbernie

# fishes
# https://pngimg.com/image/67588
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Key fish
#'
#' @param data,params,size key stuff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_fish <-  function(data, params, size) {
  filename <- paste0(data$fish, ".png")
  # print(filename)
  img <- as.raster(png::readPNG(filename))
  aspect <- dim(img)[1]/dim(img)[2]
  # rasterGrob
  grid::rasterGrob(image = img)
}

# fishGrob
fishGrob <- function(x, y, size, fish = "goldfish",
                     angle = 0,
                     geom_key = list(goldfish = "goldfish.png",
                                     goldfish2 = "goldfish2.png",
                                     goldfish_shocked = "goldfish_shocked.png",
                                     bubble = "speech_bubble.png",
                                     head = "head.png",
                                     asking = "asking.png",
                                     young = "young.png",
                                     arms = "arms.png",
                                     eyebrows = "eyebrows.png")) {

  filename <- paste0("img/", fish,".png")
  img <- as.raster(png::readPNG(filename))

  # rasterGrob
  grid::rasterGrob(x             = x,
                   y             = y,
                   image         = img,
                   # only set height so that the width scales proportionally and so that the icon
                   # stays the same size regardless of the dimensions of the plot
                   height = size * ggplot2::unit(20, "mm"))
}

# Geomfish
Geomfish <- ggplot2::ggproto(`_class` = "Geomfish",
                             `_inherit` = ggplot2::Geom,
                             required_aes = c("x", "y"),
                             non_missing_aes = c("size", "fish"),
                             default_aes = ggplot2::aes(size = 1,
                                                        fish = "goldfish",
                                                        shape  = 19,
                                                        colour = "black",   fill   = NA,
                                                        alpha  = NA,
                                                        stroke =  0.5,
                                                        scale = 5,
                                                        image_filename = "goldfish"),

                             draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
                               coords <- coord$transform(data, panel_scales)
                               ggplot2:::ggname(prefix = "geom_fish",
                                                grob = fishGrob(x = coords$x,
                                                                y = coords$y,
                                                                size = coords$size,
                                                                fish = coords$fish))
                             },

                             draw_key = draw_key_fish)

#' @title fish layer
#' @description The geom is used to add fish Sanders to plots. See ?ggplot2::geom_points for more info.
#' @inheritParams ggplot2::geom_point
#' @examples
#'
#' # install.packages("ggplot2")
#'library(ggplot2)
#'
#' ggplot(mtcars) +
#'  geom_fish(aes(mpg, wt), fish = "goldfish") +
#'  theme_bw()
#'
#' ggplot(mtcars) +
#'  geom_fish(aes(mpg, wt), fish = "head") +
#'  theme_bw()
#'
#' @importFrom grDevices as.raster
#' @export
geom_fish <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {

  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = stat,
                 geom = Geomfish,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}



