# Execute neste diretório:
# Rscript --vanilla preparar_figuras_aula_11.R
# Rscript --vanilla -e 'rmarkdown::render("slides_aula_11_interacoes.Rmd")'
#
# O exemplo verificado do roteiro (09/11/2026) contém o recorte RJ–SP
# do treino de 2022 usado na Aula 9. Fontes dos dados eleitorais:
# ../../../data/eleicoes_2022_2026/docs/SOURCES.yaml

projeto <- file.path("..", "..", "..")
roteiro <- file.path(projeto, "roteiros", "aula_11", "2026-11-09")
exemplo <- readRDS(file.path(roteiro, "exemplo_verificado_v1.rds"))
treino <- readRDS(file.path(projeto, "output", "aula_09", "resultados_aula_09.rds"))$treino
original <- read.csv(file.path(projeto, "data", "eleicoes_2022_2026", "processed",
                               "presidente_2022_zonas.csv"))
dados <- exemplo$dados
referencia <- exemplo$numeros

conferir <- function(obtido, esperado) {
  stopifnot(isTRUE(all.equal(unname(obtido), unname(esperado), tolerance = 1e-9)))
}

chave_zona <- function(base) paste(base$uf, base$zona, sep = ":")
zonas_treino <- subset(treino, uf %in% c("RJ", "SP"))
posicao_original <- match(chave_zona(dados), chave_zona(original))
stopifnot(nrow(dados) == 447L, sum(dados$uf == "RJ") == 132L, sum(dados$uf == "SP") == 315L,
          anyDuplicated(chave_zona(dados)) == 0L, !anyNA(posicao_original),
          identical(sort(chave_zona(dados)), sort(chave_zona(zonas_treino))),
          all(dados$validos_t1 > 0), all(dados$validos_t2 > 0),
          all(dados$pct_lula_t1 >= 0 & dados$pct_lula_t1 <= 100),
          all(dados$pct_lula_t2 >= 0 & dados$pct_lula_t2 <= 100))
conferir(dados$pct_lula_t1, original$pct_lula_t1[posicao_original])
conferir(dados$pct_lula_t2, original$pct_lula_t2[posicao_original])
conferir(dados$pct_lula_t1, 100 * dados$lula_t1 / dados$validos_t1)
conferir(dados$pct_lula_t2, 100 * dados$lula_t2 / dados$validos_t2)

centro <- referencia$centro
pontos <- c(40, 50)
dados$uf <- factor(dados$uf, levels = c("RJ", "SP"))
dados$x_c <- dados$pct_lula_t1 - centro
dados$sp <- as.integer(dados$uf == "SP")

modelos <- list(
  Aditivo = lm(pct_lula_t2 ~ x_c + sp, data = dados),
  "Com interação" = lm(pct_lula_t2 ~ x_c * sp, data = dados)
)
interativo <- modelos[["Com interação"]]
bruto <- lm(pct_lula_t2 ~ pct_lula_t1 * sp, data = dados)
b <- coef(interativo)
C <- sandwich::vcovHC(interativo, type = "HC3")
gl <- df.residual(interativo)
critico <- qt(.975, df = gl)
stopifnot(nobs(modelos$Aditivo) == nrow(dados), nobs(interativo) == nrow(dados),
          gl == referencia$nu, centro == 40)
conferir(coef(modelos$Aditivo), coef(exemplo$aditivo))
conferir(b, coef(exemplo$centrado))
conferir(C, exemplo$covariancia_hc3)
conferir(fitted(bruto), fitted(interativo))
conferir(resid(bruto), resid(interativo))

faixas <- t(vapply(split(dados$pct_lula_t1, dados$uf), range, numeric(2)))
suporte_comum <- c(max(faixas[, 1]), min(faixas[, 2]))
conferir(suporte_comum, referencia$limites_suporte_comum)
stopifnot(all(pontos > suporte_comum[1] & pontos < suporte_comum[2]))

# T1 e T2 permanecem na escala de porcentagem de votos válidos.
ajuste <- function(modelo, x, sp) predict(modelo, data.frame(x_c = x - centro, sp = sp))
diferenca <- function(modelo, x) ajuste(modelo, x, 1) - ajuste(modelo, x, 0)
eixo_comum <- seq(suporte_comum[1], suporte_comum[2], length.out = 200)
contrastes <- function(modelo, x) {
  data.frame(t1 = x, estimativa = diferenca(modelo, x))
}
contrastes_hc3 <- function(x) {
  c <- x - centro
  delta <- b[["sp"]] + c * b[["x_c:sp"]]
  ep <- sqrt(C["sp", "sp"] + c^2 * C["x_c:sp", "x_c:sp"] + 2 * c * C["sp", "x_c:sp"])
  conferir(delta, diferenca(interativo, x))
  data.frame(t1 = x, estimativa = delta, ep_hc3 = ep,
             inferior = delta - critico * ep, superior = delta + critico * ep)
}
ic <- contrastes_hc3(eixo_comum)
ic_pontos <- contrastes_hc3(pontos)

numeros <- read.csv(file.path(roteiro, "resultados_exemplo_v1.csv"))
inclinacoes <- c(b[["x_c"]], b[["x_c"]] + b[["x_c:sp"]], b[["x_c:sp"]])
conferir(inclinacoes, numeros$estimativa[1:3])
conferir(2 * pt(-abs(b[["x_c:sp"]] / sqrt(C["x_c:sp", "x_c:sp"])), df = gl),
         numeros$valor_p[3])
alvos <- numeros[numeros$quantidade == "diferenca_niveis_SP_RJ", ]
conferir(as.matrix(ic_pontos[-1]), as.matrix(alvos[names(ic_pontos)[-1]]))

retas <- do.call(rbind, lapply(names(modelos), function(nome) {
  do.call(rbind, lapply(rownames(faixas), function(uf) {
    t1 <- seq(faixas[uf, 1], faixas[uf, 2], length.out = 100)
    data.frame(pct_lula_t1 = t1, pct_lula_t2 = ajuste(modelos[[nome]], t1, as.integer(uf == "SP")),
               uf = uf, modelo = nome)
  }))
}))
retas$modelo <- factor(retas$modelo, levels = names(modelos))
comparar_modelos <- function(x) {
  do.call(rbind, lapply(names(modelos), function(nome) {
    transform(contrastes(modelos[[nome]], x), modelo = nome)
  }))
}
curvas <- comparar_modelos(eixo_comum)
marcas <- comparar_modelos(pontos)

azul <- "#245F85"
cinza <- "#53616D"
tinta <- "#17212B"
cores_uf <- c(RJ = azul, SP = "#9A4E1C")
cortes_uf <- c(RJ = "dashed", SP = "solid")

tema <- ggplot2::theme_minimal(base_family = "sans", base_size = 10) +
  ggplot2::theme(
    text = ggplot2::element_text(colour = tinta),
    axis.text = ggplot2::element_text(colour = tinta, size = 9),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 3)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 4)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "#E6ECF0", linewidth = .24),
    panel.spacing.x = grid::unit(9, "pt"),
    strip.text = ggplot2::element_text(size = 11, face = "bold", hjust = 0),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = grid::unit(19, "pt"),
    legend.key.height = grid::unit(10, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.margin = ggplot2::margin(1, 0, 0, 0),
    plot.margin = ggplot2::margin(2, 5, 2, 1)
  )

linhas_t1 <- ggplot2::geom_vline(xintercept = pontos, linewidth = .35,
                                 colour = "#8B99A5", linetype = "dotted")
escalas_uf <- list(
  ggplot2::scale_colour_manual(values = cores_uf, name = NULL),
  ggplot2::scale_linetype_manual(values = cortes_uf, name = NULL)
)
rotulo_t1 <- "Votos de Lula no primeiro turno (%)"
rotulo_t2 <- "Lula no 2º turno (%)"
rotulo_diferenca <- "SP menos RJ (p.p.)"
rotulos_pp <- scales::label_number(decimal.mark = ",", accuracy = .1)

grafico_retas <- ggplot2::ggplot(retas, ggplot2::aes(pct_lula_t1, pct_lula_t2)) +
  linhas_t1 +
  ggplot2::geom_point(data = dados, ggplot2::aes(colour = uf), alpha = .24, size = .95,
                      show.legend = FALSE) +
  ggplot2::geom_line(ggplot2::aes(colour = uf, linetype = uf), linewidth = .72) +
  ggplot2::facet_wrap(~ modelo, nrow = 1) +
  escalas_uf +
  ggplot2::scale_x_continuous(breaks = seq(20, 60, 10)) +
  ggplot2::scale_y_continuous(breaks = seq(20, 70, 10)) +
  ggplot2::labs(x = rotulo_t1, y = rotulo_t2) +
  tema

grafico_comparacao <- ggplot2::ggplot(curvas, ggplot2::aes(t1, estimativa)) +
  ggplot2::geom_hline(yintercept = 0, colour = "#858F98", linewidth = .37) +
  linhas_t1 +
  ggplot2::geom_line(ggplot2::aes(colour = modelo, linetype = modelo), linewidth = .76) +
  ggplot2::geom_point(data = marcas, ggplot2::aes(colour = modelo), size = 2.1,
                      show.legend = FALSE) +
  ggplot2::scale_colour_manual(values = c(Aditivo = cinza, "Com interação" = azul), name = NULL) +
  ggplot2::scale_linetype_manual(values = c(Aditivo = "dashed", "Com interação" = "solid"),
                                 name = NULL) +
  ggplot2::scale_x_continuous(breaks = c(30, 40, 50, 60)) +
  ggplot2::scale_y_continuous(breaks = seq(-.5, 1.5, .5), labels = rotulos_pp) +
  ggplot2::labs(x = rotulo_t1, y = rotulo_diferenca) +
  tema

# Peso de cada zona na frequência percentual de sua própria UF.
dados$peso_histograma <- 100 / as.numeric(table(dados$uf)[dados$uf])
conferir(as.numeric(tapply(dados$peso_histograma, dados$uf, sum)), c(100, 100))
grafico_suporte <- ggplot2::ggplot(dados, ggplot2::aes(pct_lula_t1, weight = peso_histograma)) +
  ggplot2::geom_histogram(ggplot2::aes(fill = uf), binwidth = 2, boundary = 0,
                          colour = "white", linewidth = .2) +
  linhas_t1 +
  ggplot2::facet_wrap(~ uf, nrow = 1,
                      labeller = ggplot2::as_labeller(c(RJ = "RJ · 132 zonas", SP = "SP · 315 zonas"))) +
  ggplot2::scale_fill_manual(values = cores_uf, guide = "none") +
  ggplot2::scale_x_continuous(breaks = seq(20, 60, 10)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 25, 5),
                              expand = ggplot2::expansion(mult = c(0, .05))) +
  ggplot2::labs(x = rotulo_t1, y = "Zonas da UF (%)") +
  tema

# Escala superior é uma translação exata da escala inferior.
grafico_centro <- ggplot2::ggplot(subset(retas, modelo == "Com interação"),
                                  ggplot2::aes(pct_lula_t1, pct_lula_t2)) +
  linhas_t1 +
  ggplot2::geom_line(ggplot2::aes(colour = uf, linetype = uf), linewidth = .79) +
  escalas_uf +
  ggplot2::scale_x_continuous(
    breaks = seq(20, 60, 10),
    sec.axis = ggplot2::sec_axis(~ . - centro, name = "Primeiro turno centrado (p.p.)",
                                 breaks = seq(-20, 20, 10))
  ) +
  ggplot2::scale_y_continuous(breaks = seq(20, 70, 10)) +
  ggplot2::labs(x = rotulo_t1, y = rotulo_t2) +
  tema +
  ggplot2::theme(axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 3)))

grafico_ic <- ggplot2::ggplot(ic, ggplot2::aes(t1, estimativa)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = inferior, ymax = superior), fill = "#DAE8F2") +
  ggplot2::geom_hline(yintercept = 0, colour = "#788894", linewidth = .44, linetype = "dashed") +
  linhas_t1 +
  ggplot2::geom_line(linewidth = .84, colour = azul) +
  ggplot2::geom_errorbar(data = ic_pontos, ggplot2::aes(ymin = inferior, ymax = superior),
                         width = .65, colour = azul, linewidth = .66) +
  ggplot2::geom_point(data = ic_pontos, colour = azul, size = 2.2) +
  ggplot2::scale_x_continuous(breaks = c(30, 40, 50, 60)) +
  ggplot2::scale_y_continuous(labels = rotulos_pp) +
  ggplot2::labs(x = rotulo_t1, y = rotulo_diferenca) +
  tema

dir.create("figuras", showWarnings = FALSE)
salvar <- function(arquivo, figura, altura, largura = 5.55) {
  ggplot2::ggsave(file.path("figuras", arquivo), plot = figura, device = grDevices::pdf,
                  width = largura, height = altura, units = "in", bg = "white", useDingbats = FALSE)
}
salvar("01_ajustes_aditivo_interativo.pdf", grafico_retas, 2.10)
salvar("02_diferencas_nos_dois_ajustes.pdf", grafico_comparacao, 2.13)
salvar("03_distribuicao_primeiro_turno.pdf", grafico_suporte, 1.98)
salvar("04_mudanca_de_origem.pdf", grafico_centro, 2.12, largura = 3.24)
salvar("05_diferenca_e_ic_hc3.pdf", grafico_ic, 2.20)
cat("AULA11_PREVIA_NUMEROS_CONFERIDOS\n")
