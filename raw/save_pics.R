library(magick)
library(tidyverse)

img <- "gifs/pic.jpg" %>%
  image_read() %>%
  image_scale(100) %>%
  image_border(color = "white", geometry = "5x5")

image_append(c(img)) %>%
  image_write(path = "gifs/pic1.png", format = "png")

image_append(c(img, img)) %>%
  image_write(path = "gifs/pic2.png", format = "png")

image_append(c(img, img, img)) %>%
  image_write(path = "gifs/pic3.png", format = "png")

image_append(c(img, img, img, img)) %>%
  image_write(path = "gifs/pic4.png", format = "png")

image_append(c(img, img, img, img, img)) %>%
  image_write(path = "gifs/pic5.png", format = "png")
