# Aula 11 — Alternativa offline com zonas eleitorais sintéticas
# Copie e execute este bloco para criar a base. Ele usa somente o R básico.
# Cada linha é uma zona fictícia. Os percentuais são sobre os votos válidos.
set.seed(6183)
zonas <- data.frame(
  uf = factor(rep(c("RJ", "SP"), c(132, 315)), levels = c("RJ", "SP")),
  zona = c(1:132, 1:315)
)

# Primeiro turno: as distribuições aproximam as observadas em cada estado.
zonas$pct_lula_t1 <- c(17 + 48 * rbeta(132, 5.7, 5.9),
                       24 + 43 * rbeta(315, 2, 3.8))

# Segundo turno: em 40%, as retas partem de 42,59% (RJ) e 43,27% (SP).
# Por ponto a mais no primeiro turno, sobem 1,10 p.p. no RJ e 1,05 p.p. em SP.
x <- zonas$pct_lula_t1 - 40
media <- ifelse(zonas$uf == "RJ", 42.59 + 1.10*x, 43.27 + 1.05*x)
desvio <- ifelse(zonas$uf == "RJ", 0.95 + 0.06*pmax(x + 3, 0), 1.76)
# Ruído de média zero, com alguns desvios positivos maiores, como na amostra.
erro <- desvio * (rgamma(nrow(zonas), shape = 4, rate = 2) - 2)
zonas$pct_lula_t2 <- pmin(100, pmax(0, media + erro))

# Para salvar uma cópia, execute: write.csv(zonas, "zonas_sinteticas.csv", row.names = FALSE)

# Prática: execute os blocos abaixo com os pacotes da aula já instalados.
library(ggplot2)
library(sandwich)
library(lmtest)

# 1. Como se relacionam as votações simuladas nos dois turnos? ----
ggplot(zonas, aes(pct_lula_t1, pct_lula_t2, color = uf)) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_vline(xintercept = c(40, 50), linetype = "dotted") +
  labs(x = "Lula no primeiro turno (%)", y = "Lula no segundo turno (%)", color = "UF",
       caption = "Dados sintéticos. Cada ponto representa uma zona fictícia.")

# 2. A inclinação é a mesma nos dois estados? ----
zonas$x40 <- zonas$pct_lula_t1 - 40
aditivo <- lm(pct_lula_t2 ~ x40 + uf, data = zonas)
interacao <- lm(pct_lula_t2 ~ x40 * uf, data = zonas)
coef(aditivo)
coef(interacao)

# x40 é a inclinação no RJ. x40:ufSP é a diferença de inclinações (SP menos RJ).
# ufSP é a diferença prevista entre os estados quando o primeiro turno vale 40%.
V_hc3 <- vcovHC(interacao, type = "HC3")
coeftest(interacao, vcov. = V_hc3)

# O ponto do RJ é b1. O de SP é b1 + b3, com a covariância incluída no IC.
grafico_inclinacoes <- marginaleffects::plot_slopes(
  interacao, variables = "x40", condition = "uf",
  vcov = V_hc3, conf_level = 0.95, df = df.residual(interacao)
) +
  labs(title = "Inclinação da reta em cada estado",
       subtitle = "As barras mostram o IC de 95% por HC3",
       x = "Estado", y = "Variação no 2º turno (p.p.) por 1 p.p. no 1º turno",
       caption = "Dados sintéticos calibrados nas 447 zonas de RJ e SP dos slides.")
grafico_inclinacoes

# 3. Qual é a diferença prevista entre SP e RJ em 50%? ----
# São 10 p.p. acima de 40%. A diferença prevista é U + 10V.
b <- coef(interacao)
b["ufSP"] + 10 * b["x40:ufSP"]

# U é o coeficiente de ufSP e V é o de x40:ufSP.
# Var(U + 10V) = Var(U) + 100 Var(V) + 20 Cov(U, V).
ep_em_50 <- function(matriz) {
  sqrt(matriz["ufSP", "ufSP"] + 100 * matriz["x40:ufSP", "x40:ufSP"] +
         20 * matriz["ufSP", "x40:ufSP"])
}
c(convencional = ep_em_50(vcov(interacao)), HC3 = ep_em_50(V_hc3))

# 4. Como ler a mesma comparação diretamente na saída do R? ----
# Centrar em 50 faz ufSP responder à pergunta do bloco 3.
# A fórmula de McDermott (uf / x50) dá também uma inclinação por estado.
zonas$x50 <- zonas$pct_lula_t1 - 50
por_estado <- lm(pct_lula_t2 ~ uf / x50, data = zonas)
coef(summary(por_estado))[, c("Estimate", "Std. Error")]
V50_hc3 <- vcovHC(por_estado, type = "HC3")
coeftest(por_estado, vcov. = V50_hc3)

# ICs pontuais de 95% para a diferença prevista SP menos RJ, em p.p.
rbind("Em 40%" = coefci(interacao, vcov. = V_hc3)["ufSP", ],
      "Em 50%" = coefci(por_estado, vcov. = V50_hc3)["ufSP", ])
