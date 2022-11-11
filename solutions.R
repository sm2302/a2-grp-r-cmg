# Instruction to students: You may clear the code in this file and replace it
# with your own.

library(tidyverse)
library(ggforce)
theme_set(theme_void())

# Draw a random chord in a unit circle centred at origin -----------------------

# Coordinates of equilateral triangle
eqtri_df <- tibble(
  x    = c(0, sqrt(3) / 2, -sqrt(3) / 2),
  y    = c(1, -0.5, -0.5),
  xend = c(sqrt(3) / 2, -sqrt(3) / 2, 0),
  yend = c(-0.5, -0.5, 1)
)

# Coordinates of random chord
rdmchr_df <- tibble(
  x    = 0.93636368,
  y    = 0.35103142,
  xend = -0.9999991,
  yend = -0.001326758
)

# Plot
 p <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = rdmchr_df, aes(x = x, y = y, xend = xend, yend = yend),
               col = "red3") +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)


# Method A

# Centre of disk
ax = 0
ay = 0 

diameter = 10
r = diameter / 2
n = 2

eqtri_dfMethodA <- tibble(
  angleA = 2*pi*runif(n, min=0, max=1),
  pA = r*sqrt(runif(n, min=0, max=1)),
  qA = sqrt((r^2)-(pA^2)),
)


# Calculate Trig Values
sin_angleA = sin(eqtri_dfMethodA)
cos_angleA = cos(eqtri_dfMethodA)
 
# Calculate Chord endpoints
rdmchr_dfMethodA<- tibble(
  xA1 = (1.538834)*((0.9983508+4.757309)*(0.05740773)),
  yA1 = (4.191306)*((0.48852271-2.726344)*(-0.8725512)),
  xA2 = (1.538834)*((0.9983508-4.757309)*(0.05740773)),
  yA2 = (4.191306)*((0.48852271+2.726344)*(-0.8725512)),
)

# Calculate midpoints of chords
xA0 = (0.5084606+-0.33207)/2
yA0 = (8.184001+-11.75718)/2

# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(ax = 0, ay = 0, r = 5), col = "gray50") +
  geom_segment(data = eqtri_dfMethodA, aes(angleA=angleA, pA=pA, qA=qA)) +
  geom_segment(data = rdmchr_dfMethodA, aes(xA1=xA1, yA1=yA1, xA2=xA2, yA2=yA2),
               col = "red3") +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)


# Method B

# Centre of disk
ax = 0
ay = 0 

diameter = 10
r = diameter / 2
n = 2

eqtri_dfMethodB <- tibble(
  angleB = 2*pi*runif(n, min=0, max=1), # Choose angular component uniformly
  pB = r*runif(n, min=0, max=1), # Choose radial component uniformly
  qB = sqrt((r^2)-(pB^2)),
  qB2 = 2*sqrt((r^2)-(pB^2)), #Length of chord
)

# Calculate Trig Values
sin_angleB = sin(eqtri_dfMethodB)
cos_angleB = cos(eqtri_dfMethodB)

# Calculate Chord endpoints
rdmchr_dfMethodB<- tibble(
  xB1 = (3.349672)*((0.5985989+3.712101)*(-0.8010489)),
  yB1 = (4.137526)*((0.2548968-2.807291)*(-0.9669683)),
  xB2 = (3.349672)*((0.5985989-3.712101)*(-0.8010489)),
  yB2 = (4.137526)*((0.2548968+2.807291)*(-0.9669683)),
)

#Calculate midpoints of chords
xB0 = (-11.56669+8.354308)/2
yB0 = (10.21176-12.25137)/2

# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(ax = 0, ay = 0, r = 5), col = "gray50") +
  geom_segment(data = eqtri_dfMethodB, aes(angleB=angleB, pB=pB, qB=qB)) +
  geom_segment(data = rdmchr_dfMethodB, aes(xB1=xB1, yB1=yB1, xB2=xB2, yB2=yB2),
               col = "red3") +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)


# Method C

# Centre of disk
ax = 0
ay = 0 

diameter = 10
r = diameter / 2
n = 1

eqtri_dfMethodC <- tibble(
angleC1 = 2*pi*r*runif(n,min=0,max=1), # generate matrix with angular component uniformly
angleC2 = 2*pi*r*runif(n,min=0,max=1)  # generate matrix with component uniformly
)

# Chord endpoints calculation as follows
rdmchr_dfMethodC<- tibble(
  xC1 = 0 + r*cos(1.52831),
  yC1 = 0 + r*sin(1.52831),
  xC2 = 0 + r*cos(16.4373),
  yC2 = 0 + r*sin(16.4373)
)

# Calculate midpoints of chord
xC0 = (0.2123677 + (-3.728083)) / 2
yC0 = (4.995488 + (-3.331876)) / 2

# Statistics on chord lengths
lengthSide=r*sqrt(3) #length of triangle side

# Chord lengths

lengthA=sqrt((0.5084606+0.33207)^2+(8.184001+11.75718)^2) #Method A
lengthB=sqrt((-11.56669-8.354308)^2+(10.21176+12.25137)^2) #Method B
lengthC=sqrt((0.2123677+3.728083)^2+(4.995488+3.331876)^2) #Method C

#estimated probability of chord being longer than triangle side
probEstA=mean(lengthA>lengthSide) #Method A
probEstB=mean(lengthB>lengthSide) #Method B
probEstC=mean(lengthC>lengthSide) #Method C

  







# Plot
ggplot() +
  ggforce::geom_circle(aes(ax = 0, ay = 0, r = 5), col = "gray50") +
  geom_segment(data = ,aes(xC1 = xC1, yC1 = yC1, xC2 = xC2, yC2 = yC2)) +
  geom_segment(data = ,aes(xC0 = xC0, yC0 = yC0),
               col = "red3") 










lengthSide <- r*sqrt(3) # length of triangle side
lengthA <- hypotenuse((xC1 - xC2), (yC1 -yC2))

# estimated probability of chord being longer than triangle side
probEstA <- mean(lengthA > lengthSide) 

# create points for circle
t <- seq(0,2*pi,length = 200)
xp <- r*cos(t)
yp <- r*sin(t)


circleFun <- function(centre = c(0,0),diameter = 10, npoints = 200){
  r = diameter / 2
  t <- seq(0,2*pi,length = npoints)
  xx <- centre[1] + r * cos(tt)
  yy <- centre[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}    

dat<- circleFun(c(0,0), n=2)
ggplot(dat,aes(x,y)) + geom_path()

# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(cx = 0, cy = 0, r = 5), col = "gray50") +
  geom_segment(data = Z , aes(xA1 = xA1, yA1 = yA1, xA2 = xA2, yA2 = yA2)) +
  geom_segment(data = Mid_chrd, aes(xA0 = xA0, yA0= yA0),
               col = "red3") 
  





