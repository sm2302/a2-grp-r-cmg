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



# Method A----------------------------------------------------------------------

# Centre of disk
ax = 0
ay = 0 


r = 3 
n = 100


angleA = 2*pi*runif(n, min=-1, max=1)
pA = r*sqrt(runif(n, min=-1, max=1))
qA = sqrt((r^2)-(pA^2))

# Calculate Trig Values
sin_angleA = sin(angleA)
cos_angleA = cos(angleA)
 
# Calculate Chord endpoints
xA1 = (ax+pA)*(cos_angleA+qA)*(sin_angleA)
yA1 = (ay+pA)*(sin_angleA-qA)*(cos_angleA)
xA2 = (ax+pA)*(cos_angleA-qA)*(sin_angleA)
yA2 = (ay+pA)*(sin_angleA+qA)*(cos_angleA)


# Calculate midpoints of chords
xA0 = (xA1+xA2)/2
yA0 = (yA1+yA2)/2


# Method B----------------------------------------------------------------------

# Centre of disk
ax = 0
ay = 0 

diameter = 10
r = diameter / 2
n = 2

angleB = 2*pi*runif(n, min=0, max=1) # Choose angular component uniformly
pB = r*runif(n, min=0, max=1) # Choose radial component uniformly
qB = sqrt((r^2)-(pB^2))
qB2 = 2*sqrt((r^2)-(pB^2)) #Length of chord


# Calculate Trig Values
sin_angleB = sin(eqtri_dfMethodB)
cos_angleB = cos(eqtri_dfMethodB)

# Calculate Chord endpoints
xB1 = (3.349672)*((0.5985989+3.712101)*(-0.8010489))
yB1 = (4.137526)*((0.2548968-2.807291)*(-0.9669683))
xB2 = (3.349672)*((0.5985989-3.712101)*(-0.8010489))
yB2 = (4.137526)*((0.2548968+2.807291)*(-0.9669683))


#Calculate midpoints of chords
xB0 = (-11.56669+8.354308)/2
yB0 = (10.21176-12.25137)/2


# Method C----------------------------------------------------------------------

# Centre of disk
ax = 0
ay = 0 

diameter = 10
r = diameter / 2
n = 2

angleC1 = 2*pi*r*runif(n,min=-1,max=0.5) # choose angular component uniformly
angleC2 = 2*pi*r*runif(n,min=-1,max=0.5)  # choose angular component uniformly

# Chord endpoints calculation as follows
xC1 = 0 + r*cos(-0.4293923)
yC1 = 0 + r*sin(-23.0923336)
xC2 = 0 + r*cos(8.838027)
yC2 = 0 + r*sin(-16.521362)


# Calculate midpoints of chord
xC0 = (4.546095 + (-4.16372)) / 2
yC0 = (4.458721 + (3.633132)) / 2



# Statistics on chord lengths---------------------------------------------------
lengthSide=r*sqrt(3) #length of triangle side

# Chord lengths-----------------------------------------------------------------
lengthA=sqrt((xA1-xA2)^2+(yA1-yA2)^2) #Method A
lengthB=sqrt((-11.56669-8.354308)^2+(10.21176+12.25137)^2) #Method B
lengthC=sqrt((4.546095+4.16372)^2+(4.458721-3.633132)^2) #Method C

#estimated probability of chord being longer than triangle side-----------------
probEstA=mean(lengthA>lengthSide) #Method A
probEstB=mean(lengthB>lengthSide) #Method B
probEstC=mean(lengthC>lengthSide) #Method C

  


























