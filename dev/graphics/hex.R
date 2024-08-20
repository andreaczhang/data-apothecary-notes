install.packages('hexSticker')
library(hexSticker)
library(ggplot2)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() #+ theme_transparent()
p

sticker(p, package="hexSticker", 
        p_size=20, 
        s_x=1, 
        s_y=.75, 
        s_width=1.3, 
        s_height=1,
        h_fill = 'white', 
        h_color = 'black',
        filename="./dev/graphics/hex.png")


# use my own image 
img <- '~/Documents/GitHub/data-apothecary-notes/dev/graphics/ggehr_1.png'

img <- system.file("figures/cat.png", package="hexSticker")

sticker(img, package="", 
        p_size=20, 
        s_x=1, 
        s_y=1, 
        s_width=0.6, 
        s_height=1,
        h_fill = 'white', 
        h_color = 'black',
        filename="./dev/graphics/hex_ggehr.png")





