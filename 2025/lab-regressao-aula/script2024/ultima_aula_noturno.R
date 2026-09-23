
# computar função logística

# x = -3
1/(1 + exp(3)) # 0.04742587

# x = -2

# x=  -1

# x = 0

# x= 1

#x= 2

# x= 3

## Importando os dados do latinobarômetro

library(here)
library(tidyverse)
library(haven)

lb <- readRDS(here("dados brutos 2024",
                   "Latinobarometro_2023_Esp_Rds_v1_0.rds"))

glimpse(lb)

lb <- lb %>%
  filter(idenpa == 76) %>% # filtra brasil
  mutate(aprova_governo = P15STGBS) %>% # variávei aprova_governo
  filter(aprova_governo %in% c(1,2)) %>% # filtrei não-sabe
  mutate(aprova_governo = factor(aprova_governo,
                                 levels = c(2,1),
                                 labels = c("No aprueba",
                                            "Aprueba"))) # transformei em factor
  
reg_logistica <- glm(aprova_governo ~ edad, data= lb, 
                     family = "binomial")
summary(reg_logistica)
# 20 anos
0.266941 + 0.009003*20
1/(1+ exp(-0.266941))

# 21 anos
0.266941 + 0.009003*21
1/(1+ exp(-0.456004))

library(marginaleffects)
comparisons(reg_logistica)

# gráfico da logística

library(arm)
curve(invlogit(coef(reg_logistica)[1] + coef(reg_logistica)[2]*x),
      from = -300, to=300)
curve(invlogit(coef(reg_logistica)[1] + coef(reg_logistica)[2]*x),
      from = 16, to=100, add=T, lwd=3)

# Logística com múltiplos preditores
lb <- lb %>%
  mutate(ideologia = P16ST,
         sexo = haven::as_factor(sexo)) %>%
  filter(ideologia %in% 0:10)

reg_logistica1 <- glm(aprova_governo ~ edad + sexo + ideologia , data= lb, 
                     family = "binomial")
summary(reg_logistica1)

# gráfico

minha_funcao <- function(x) {
  1/x
}
library(arm)

curve(invlogit(coef(reg_logistica1)[1] +
                 coef(reg_logistica1)[2]*mean(lb$edad) +
                 coef(reg_logistica1)[3]*1 +
                 coef(reg_logistica1)[4]*x),
      from = 0, to = 10, ylim = c(0,1))

curve(invlogit(coef(reg_logistica1)[1] +
                 coef(reg_logistica1)[2]*mean(lb$edad) +
                 coef(reg_logistica1)[3]*0 +
                 coef(reg_logistica1)[4]*x),
      from = 0, to = 10, ylim = c(0,1), add=TRUE,
      col = "blue")

curve(invlogit(coef(reg_logistica1)[1] +
                 coef(reg_logistica1)[2]*min(lb$edad) +
                 coef(reg_logistica1)[3]*0 +
                 coef(reg_logistica1)[4]*x),
      from = 0, to = 10, ylim = c(0,1), add=TRUE,
      col = "green")
