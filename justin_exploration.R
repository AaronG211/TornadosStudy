library(tidyverse)
library(spatstat)
library(lme4) 
library(mgcv) 
library(usmap) 
library(ggeffects) 

data = read_csv("1950-2024_actual_tornadoes_copy.csv")

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
summary(mag_mod)

glm_preds_lat = predict_response(mag_mod, terms = "slat [all]") 
glm_preds_lon = predict_response(mag_mod, terms = "slon [all]") 
plot(glm_preds_lat) 
plot(glm_preds_lon) 

# try a GAM 
mag_mod_gam = gam(
  is_strong ~ s(slat) + s(slon), data = post07_clean, family="binomial"
)

broom::tidy(mag_mod_gam)

gam_preds_lat = predict_response(mag_mod_gam, terms = "slat [all]") 
gam_preds_lon = predict_response(mag_mod_gam, terms = "slon [all]") 

plot(gam_preds_lat) + 
  geom_vline(xintercept = 30, linetype = "dashed", color = "red") + 
  geom_vline(xintercept = 35, linetype = "dashed", color = "red") + 
  labs(
    x = "Latitude", 
    y = "Probability of Weak Hurricane", 
    title = "Effect of Latitude on Weak Hurricanes (GAM)"
  )

plot(gam_preds_lon) + 
  geom_vline(xintercept = -85, linetype = "dashed", color = "red") + 
  geom_vline(xintercept = -95, linetype = "dashed", color = "red") + 
  labs(
    x = "Longitude", 
    y = "Probability of Weak Hurricane", 
    title = "Effect of Longitude on Weak Hurricanes (GAM)"
  )

  


ggplot(post07_clean, aes(x=slon, y=slat, color=factor(is_strong))) + 
  geom_point(alpha=0.5) + 
  labs(
    x = "Starting Longitude", 
    y = "Starting Latitude", 
    color = "Hurricane\nClassification", 
    title = "Location of Strong & Weak Hurricanes", 
    subtitle = "2007-Present"
  )

torn_ppp = ppp(
  x = post07_clean$slon, 
  y = post07_clean$slat, 
  window = owin(c(-125,-70), c(25,50)), 
  marks = factor(post07_clean$is_strong)
) 
torn_ppp = unique(torn_ppp)

K = Kcross(torn_ppp, i="weak", j="strong", correction="border") 
plot(K)
