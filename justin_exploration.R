library(tidyverse)
library(spatstat)
library(lme4) 
library(mgcv) 
library(usmap) 

data = read_csv("1950-2024_actual_tornadoes.csv")

post07 = data %>% filter(yr >= 2007)

# categorize as weak (0 or 1) vs strong (2+) 
post07_clean = post07 %>% 
  filter(
    mag != -9, 
    between(elat, 25, 50), 
    between(elon, -125, -70), 
    between(slat, 25, 50), 
    between(slon, -125, -70)
  ) %>% 
  mutate(
    is_strong = factor(if_else(mag > 1, "strong", "weak")) 
  )

# starting longtitude, latitude 
# you can see shape of US
ggplot(post07_clean, aes(x=slon, y=slat)) + geom_point(alpha=0.01) 

# predicting strong storms? 
mag_mod = glm(is_strong ~ slat + slon, data = post07_clean, family="binomial") 

# try a GAM 
mag_mod_gam = gam(
  is_strong ~ s(slat) + s(slon), data = post07_clean, family="binomial"
)
plot(mag_mod_gam) # this makes more sense 

ggplot(post07_clean, aes(x=slon, y=slat, color=factor(is_strong))) + 
  geom_point(alpha=0.5) 

torn_ppp = ppp(
  x = post07_clean$slon, 
  y = post07_clean$slat, 
  window = owin(c(-125,-70), c(25,50)), 
  marks = factor(post07_clean$is_strong) 
) 
torn_ppp = unique(torn_ppp)

K = Kcross(torn_ppp, i="weak", j="strong") 
plot(K)

env_ws = envelope(
  torn_ppp,
  fun = Kcross,
  i = "weak", j = "strong",
  nsim = 100, # number of simulations
  simulate = expression(rlabel(torn_ppp)),
  correction = "border"
)
plot(env_ws)

plot(risk_map, main = "Estimate of Risky Hurricane Regions") 
