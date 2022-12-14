## Reference : 
## 10.2: Bertrand’s Paradox. (2020, May 5).Statistics LibreTexts. 
## Retrieved November 11, 2022,
## from https://stats.libretexts.org/Bookshelves/Probability_Theory/Probability_Mathematical_Statistics_and_Stochastic_Processes_(Siegrist)/10%3A_Geometric_Models/10.02%3A_Bertrand’s_Paradox

library(tidyverse)

# Method A----------------------------------------------------------------------

# Centre of disk
ax = 0
ay = 0 

r = 3 
n = 100

angleA = 2*pi*runif(n, min=0, max=1) # Choose angular component uniformly
pA = r*sqrt(runif(n, min=0, max=1)) # Choose radial component
qA = sqrt((r^2)-(pA^2)) # Distance to circle edge (along line)

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

r = 3
n = 100

angleB = 2*pi*runif(n, min=0, max=1) # Choose angular component uniformly
pB = r*runif(n, min=0, max=1) # Choose radial component uniformly
qB = sqrt((r^2)-(pB^2)) # Distance to circle edge (along line)
qB2 = 2*sqrt((r^2)-(pB^2)) # Length of chord

# Calculate Trig Values
sin_angleB = sin(angleB)
cos_angleB = cos(angleB)

# Calculate Chord endpoints
xB1 = (ax+pB)*(cos_angleB+qB)*(sin_angleB)
yB1 = (ay+pB)*(sin_angleB-qB)*(cos_angleB)
xB2 = (ax+pB)*(cos_angleB-qB)*(sin_angleB)
yB2 = (ay+pB)*(sin_angleB+qB)*(cos_angleB)

# Calculate midpoints of chords
xB0 = (xB1+xB2)/2
yB0 = (yB1+yB2)/2


# Method C----------------------------------------------------------------------

# Centre of disk
ax = 0
ay = 0 

r = 3
n = 100

angleC1 = 2*pi*r*runif(n,min=0,max=pi/2) # Choose angular component uniformly
angleC2 = 2*pi*r*runif(n,min=0,max=pi/2)  # Choose angular component uniformly

# Chord endpoints calculation as follows
xC1 = ax + r*cos(angleC1) 
yC1 = ay + r*sin(angleC1)
xC2 = ax + r*cos(angleC2)
yC2 = ay + r*sin(angleC2)

# Calculate midpoints of chord
xC0 = (xC1+xC2)/2
yC0 = (yC1+yC2)/2



# Statistics on chord lengths---------------------------------------------------
lengthSide=r*sqrt(3) # Length of triangle side


# Chord lengths-----------------------------------------------------------------
lengthA=sqrt((xA1-xA2)^2+(yA1-yA2)^2) # Method A
lengthB=sqrt((xB1-xB2)^2+(yB1-yB2)^2) # Method B
lengthC=sqrt((xC1-xC2)^2+(yC1-yC2)^2) # Method C


# Estimated probability of chord being longer than triangle side-----------------
probEstA=mean(lengthA>lengthSide) # Method A
probEstB=mean(lengthB>lengthSide) # Method B
probEstC=mean(lengthC>lengthSide) # Method C

  


























