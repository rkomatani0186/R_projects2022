#Effective spin
library(dplyr)

db <- connect_db("uiuc")
data <- dbGetQuery(db, "select * from fs_pitches where season = 2021")
dbDisconnect(db)


rho = 0.0750 #lb / ft^3
r = 0.121 #ft
A = pi * r^2
m = 5.125 #oz
K = (1 / 2) * rho * A / (m * 0.0625)
  
data <- data %>%
  mutate(yR = 60.5 - extension,
  tR = (- vy0 - sqrt((vy0)^2 - 2 * ay * (50 - yR))) / ay,
  vxR = vx0 + ax * tR,
  vyR = vy0 + ay * tR,
  vzR = vz0 + az * tR,
  tf = (-vyR - sqrt((vyR)^2 - 2 * ay * (yR - 17 / 12))) / ay,
  vxbar = (2 * vxR + ax * tf) / 2,
  vybar = (2 * vyR + ay * tf) / 2,
  vzbar = (2 * vzR + az * tf) / 2,
  vbar = sqrt(vxbar^2 + vybar^2 + vzbar^2),
  aDrag = - (ax * vxbar + ay * vybar + (az + 32.174) * (vzbar)) / vbar,
  Cd = aDrag / (K * vbar^2),
  aMx = ax + aDrag * vxbar / vbar,
  aMy = ay + aDrag * vybar / vbar,
  aMz = az + aDrag * vzbar / vbar + 32.174,
  aM = sqrt(aMx^2 + aMy^2 + aMz^2),
  Mx = 1 / 2 * ax * tf^2 * 12,
  Mz = 1 / 2 * az + tf^2 * 12,
  CL = aM / (K * vbar^2),
  S = 0.166 * log(0.336 / (0.336 - CL)),
  wT = 78.92 * S * vbar,
  spin_eff = wT / spin_rate)

