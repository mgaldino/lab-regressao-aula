# Diurno 26/11

# x = 5
1/(1 + exp(-5)) # 0.9933071

# x= 0
1/(1 + exp(0)) # 0.5

#x = 10
1/(1 + exp(-10)) # 0.9999546 
# x = -10
1/(1 + exp(10)) # 0.00004539787 
# x=1
1/(1 + exp(-1)) #  0.7310586
# x = -1
1/(1 + exp(1)) #   0.2689414

# importando banco de dados LB
library(here)
library(tidyverse)
library(haven)
lb <- readRDS(here("dados brutos 2024",
             "Latinobarometro_2023_Esp_Rds_v1_0.rds"))

# lb <- readRDS("C:\\Users\\fcslab122\\lab-regressao-aula\\lab-regressao-aula\\dados brutos 2024\\Latinobarometro_2023_Esp_Rds_v1_0.rds")
glimpse(lb)

lb <- lb %>%
  mutate(P3N = haven::as_factor(P3N),
         P3N = as.numeric(P3N),
         P3N_binaria = ifelse(P3N %in% c(6,7), 0,
                              ifelse(P3N %in% c(8,9), 1, NA)))

ln_bra <- lb %>%
  dplyr::filter(idenpa == 76)

reg <- lm(P3N_binaria ~ edad, data = ln_bra, weights = wt)
summary(reg)

reg_logistica <- glm(P3N_binaria ~ edad, data = ln_bra,
                     family = "binomial")
summary(reg_logistica)

# probabilidade de Y = 1 para idade == 18 anos: 0.6466323
1/(1 + exp(-(0.5676943 + 0.0020319*18)))

# probabilidade de Y = 1 para idade == 50 anos: 0.661344
1/(1 + exp(-(0.5676943 + 0.0020319*50)))

# probabilidade de Y = 1 para idade ==80 anos: 0.6748593
1/(1 + exp(-(0.5676943 + 0.0020319*80)))


library(arm)

n <- nrow(ln_bra)
idade_jitt <- ln_bra$edad + runif(n, -.2, .2)
mude_jitt <- ln_bra$P3N_binaria + ifelse(ln_bra$P3N_binaria==0, runif(n, .005, .05), runif(n, -.05, -.005))
curve(invlogit(reg_logistica$coef[1] + reg_logistica$coef[2]*x), from = -200,to=200, ylim=c(0,1), xlim=c(-200, 200), xaxt="n", xaxs="i", 
      ylab="Pr (quer mudar país)", xlab="Idade", lwd=.5, yaxs="i")
curve(invlogit(reg_logistica$coef[1] + reg_logistica$coef[2]*x), 18, 100, lwd=3, add=TRUE)
axis(1, seq(18, 100, by=5))
mtext("(left)", side=1, line=1.7, at=1, adj=.5)
mtext("(right)", 1, 1.7, at=11, adj=.5)
points(ideologia_jitt, vote_jitt, pch=20, cex=.1)

ln_bra <- ln_bra %>%
  mutate(ideologia = ifelse(P16ST %in%
                              c(-5,-4,-3,-2,-1,97,98,99),
                            NA, P16ST) )

ln_bra <- ln_bra %>%
  mutate(ideologia_sq = ideologia^2)
reg_logistica1 <- glm(P3N_binaria ~ edad + ideologia + ideologia_sq,
                      data=ln_bra)
summary(reg_logistica1)

curve(invlogit(reg_logistica$coef[1] + reg_logistica$coef[2]*x), from = -200,to=200, ylim=c(0,1), xlim=c(-200, 200), xaxt="n", xaxs="i", 
      ylab="Pr (quer mudar país)", xlab="Idade", lwd=.5, yaxs="i")

my_poli <- function(x) {
  1 + x - .1*x^2
}

arm::invlogit
ideologia <- ln_bra$ideologia
ideologia_sq <- ln_bra$ideologia_sq
edad <- ln_bra$edad
curve(invlogit(0.4875981 +
                 0.0024518*mean(edad) +
                 0.0300347*x -
                 0.0023686*x^2), from=0, to=10)

library(marginaleffects)
avg_predictions(reg_logistica1,
                variables="edad")
