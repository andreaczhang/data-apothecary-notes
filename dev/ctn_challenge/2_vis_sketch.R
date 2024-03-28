# visualization sketch

# one patient -----
# lollipop for one patient over time
# x axis is the time
# y axis is the treatment / drug pattern / detox 

# for the drug pattern, should also highlight different
# types with color: relapse, use decrease, abstience etc


# Libraries
library(ggplot2)

# Create data
data <- data.frame(
  x=LETTERS[1:4],
  y_left = 2 + c(rnorm(4, mean = 0, sd = 0.5)), 
  y_right = 4 + c(rnorm(4, mean = 0, sd = 0.5))
)


# Horizontal version
plt <- ggplot(data, aes(x=x, y=y_left)) 
plt <- plt + geom_segment(aes(x=x, xend=x, y=y_left, yend=y_right), 
                          color="skyblue")
# add point: left
plt <- plt + geom_point(aes(x =x, y = y_left), color="blue", size=4, alpha=0.6) 
plt <- plt + geom_point(aes(x =x, y = y_right), color="red", size=4, alpha=0.6) 

plt <- plt + theme_light() 
plt <- plt + coord_flip() 
plt

# add additional line data
point_data <- data.frame(x = 'B', y = 2)
point_data2 <- data.frame(x = c('B', 'B', 'C'), 
                          y = c(2, 3, 1))

plt + geom_point(data = point_data2, 
                 aes(x = x, y = y), color="yellow", size=4, alpha=0.6) 

# add additional segment data
segment_data <- data.frame(
  x=LETTERS[5],
  y_left = 2, 
  y_right = 4
)

plt + geom_segment(data = segment_data, 
                   aes(x=x, xend=x, y=y_left, yend=y_right), 
                   color="orange")




# multiple patients ----
# functionality to account for treatment type, summary
# ordering of groups








