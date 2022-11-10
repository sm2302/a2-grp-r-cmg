# Instruction to students: You may clear the code in this file and replace it
# with your own.

library(tidyverse)
library(ggforce)
library(ggplot2)
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
n = 1

angleA = 2*pi*runif(n, min=0, max=1)
pA = r*sqrt(runif(n, min=0, max=1))
qA = sqrt((r^2)-(pA^2))

# Calculate Trig Values
sin_angleA = sin(angleA)
cos_angleA = cos(angleA)
 
# Calculate Chord endpoints
xA1 = (ax+pA)*((cos_angleA+qA)*(sin_angleA))
yA1 = (ay+pA)*((sin_angleA-qA)*(cos_angleA))
xA2 = (ax+pA)*((cos_angleA-qA)*(sin_angleA))
yA2 = (ay+pA)*((sin_angleA+qA)*(cos_angleA))

# Calculate midpoints of chords
xA0 = (xA1+xA2)/2
yA0 = (yA1+yA2)/2


# Method B

# Centre of disk
ax = 0
ay = 0 

diameter = 10
r = diameter / 2
n = 1

angleB = 2*pi*runif(n, min=0, max=1) # Choose angular component uniformly
pB = r*runif(n, min=0, max=1) # Choose radial component uniformly
qB = sqrt((r^2)-(pB^2))
qB2 = 2*sqrt((r^2)-(pB^2)) #Length of chord

# Calculate Trig Values
sin_angleB = sin(angleB)
cos_angleB = cos(angleB)

# Calculate Chord endpoints
xB1 = (ax+pB)*((cos_angleB+qB)*(sin_angleB))
yB1 = (ay+pB)*((sin_angleB-qB)*(cos_angleB))
xB2 = (ax+pB)*((cos_angleB-qB)*(sin_angleB))
yB2 = (ay+pB)*((sin_angleB+qB)*(cos_angleB))

#Calculate midpoints of chords
xB0 = (xB1+xB2)/2
yB0 = (yB1+yB2)/2


# Method C

# Centre of disk
ax = 0
ay = 0 

diameter = 10
r = diameter / 2
n = 1

angleC1 = 2*pi*r*runif(n,min=0,max=1) # generate matrix with angular component uniformly
angleC2 = 2*pi*r*runif(n,min=0,max=1) # generate matrix with component uniformly

# Chord endpoints calculation as follows
xC1 = ax + r*cos(angleC1)
yC1 = ay + r*sin(angleC1)
xC2 = ax + r*cos(angleC2)
yC2 = ay + r*sin(angleC2) 

# Calculate midpoints of chord
xC0 = (xC1 + xC2) / 2
yC0 = (yC1 + yC2) / 2

# Statistics on chord lengths
lengthSide=r*sqrt(3) #length of triangle side

# Chord lengths

lengthA=hypot(xA1-xA2,yB1-yB2) #Method A
lengthB=hypot(xB1-xB2,yB1-yB2) #Method B
lengthC=hypotenuse(xC1-xC2,yC1-yC2) #Method C

%estimated probability of chord being longer than triangle side
probEstA=mean(lengthA>lengthSide) %Method A
probEstB=mean(lengthB>lengthSide) %Method B
probEstC=mean(lengthC>lengthSide) %Method C
%%%END Do some statistics on chord lengths END%%%
  







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
  





