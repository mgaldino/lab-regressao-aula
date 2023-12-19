library(here)
library(data.table)
library(tidyverse)
library(sjlabelled) # pra remover labelled variables
library(haven)
library(janitor)
library(lubridate)
library(knitr)
library(broom)

## dados
# https://www.latinobarometro.org/latContents.jsp

lat_bar23 <- haven::read_dta("Latinobarometro_2023_Eng_Stata_v1_0.dta") %>%
  mutate(S17 = as.Date(as.character(S17), "%Y%m%d")) %>%
  clean_names()

# get_label(lat_bar23)

lat_bar23 <- lat_bar23 %>%
  mutate(data_base = as.Date(paste(diareal, mesreal, "2023", sep="-"), "%d-%m-%Y"),
         idade = year(as.period(interval(s17,data_base))),
         econ_12_meses = ifelse(p6stgbs %in% c(1,2), "better", 
                                ifelse(p6stgbs == 8, NA, "other")),
         econ_12_meses = relevel(as.factor(econ_12_meses), ref = "other"),
         aprovacao_presidente = ifelse(p15stgbs == 0, NA, p15stgbs),
         ideologia = ifelse(p16st %in% c(97, 98, 99), NA, p16st),
         votaria_governo = ifelse(perpart == 4, NA,
                                  ifelse(perpart == 1, 1, 0)),
         genero = factor(sexo, labels = c("homem", "mulher")),
         evangelico = ifelse(s1 %in% c(0,98), NA,
                             ifelse(s1 %in% c(2,3,4,5), 1, 0))) # não considera adventista, testemunha Jeová, Mórmon

br_latbar_23 <- lat_bar23 %>%
  mutate(idenpa = remove_all_labels(idenpa)) %>% # haven_labelled problems
  filter(idenpa == 76) %>% ## seelciona brasil
  filter(!is.na(votaria_governo) & !is.na(evangelico) & !is.na(ideologia) & !is.na(econ_12_meses)) %>%
  filter(ideologia >= 0)

reg_logistica <- glm(votaria_governo ~ ideologia, data=br_latbar_23,
                     family=binomial(link = "logit"))

summary(reg_logistica)

reg_logistica %>%
  tidy() %>%
  kable()


# plotando # usando base R
# código adaptado de Regression and Other Stories (Gelman et. al.)
library(arm)

n <- nrow(br_latbar_23)
ideologia_jitt <- br_latbar_23$ideologia + runif(n, -.2, .2)
vote_jitt <- br_latbar_23$votaria_governo + 
  ifelse(br_latbar_23$votaria_governo==0, runif(n, .005, .05), runif(n, -.05, -.005))

curve(invlogit(reg_logistica$coef[1] + reg_logistica$coef[2]*x), from = -5,to=24, ylim=c(0,1), xlim=c(-5, 24), xaxt="n", xaxs="i", 
      ylab="Pr (voto governo)", xlab="Ideologia", lwd=.5, yaxs="i")
curve(invlogit(reg_logistica$coef[1] + reg_logistica$coef[2]*x), 0, 10, lwd=3, add=TRUE)
axis(1, 0:10)
mtext("(left)", side=0, line=1.7, at=1, adj=.5)
mtext("(right)", 0, 1.7, at=10, adj=.5)
points(ideologia_jitt, vote_jitt, pch=20, cex=.1)


# múltiplos preditores

reg_logistica1 <- glm(votaria_governo ~ ideologia + evangelico, data=br_latbar_23,
                      family=binomial(link= "logit"))

summary(reg_logistica1)

# interpretando os coefs
invlogit(reg_logistica1$coef[1] +
           reg_logistica1$coef[2]*9 +
           0*reg_logistica1$coef[3])


invlogit(reg_logistica1$coef[1] +
           reg_logistica1$coef[2]*10 +
           0*reg_logistica1$coef[3])

# evanéglica = 1

invlogit(reg_logistica1$coef[1] +
           reg_logistica1$coef[2]*9 +
           1*reg_logistica1$coef[3])


invlogit(reg_logistica1$coef[1] +
           reg_logistica1$coef[2]*10 +
           1*reg_logistica1$coef[3])

devtools::install_github('IQSS/Zelig')
library(Zelig)

fit_zelig <- zelig(votaria_governo ~ ideologia + 
                     evangelico,
                   model = "logit",
                   data=br_latbar_23)

summary(fit_zelig)

ideologia_10 <- setx(fit_zelig, ideologia = 10)
ideologia_9 <- setx(fit_zelig,evangelico = 9)
resultado <- sim(fit_zelig, x = ideologia_9, x1 = ideologia_10)
summary(resultado)
