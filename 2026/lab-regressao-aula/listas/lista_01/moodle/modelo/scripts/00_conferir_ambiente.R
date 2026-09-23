# Execute com o projeto Lista_01.Rproj aberto.
# Este script apenas confere o ambiente; não instala programas.
pacotes <- c("data.table", "dplyr", "ggplot2", "here", "rmarkdown", "knitr")
disponiveis <- vapply(pacotes, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
print(data.frame(pacote = pacotes, instalado = disponiveis))
if (!all(disponiveis)) {
  message("Pacotes ausentes: ", paste(pacotes[!disponiveis], collapse = ", "))
  message("Instale os pacotes ausentes pelo Console e execute esta conferência novamente.")
}
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  message("Pandoc disponível: ", rmarkdown::pandoc_available())
}
message("XeLaTeX encontrado no caminho: ", nzchar(Sys.which("xelatex")))
message("Base municipal encontrada: ", file.exists("dados/populacao_municipios_2020.csv"))
message("Base AGNU encontrada: ", file.exists("dados/brasil_convergencia_china_1997_2016.csv"))
message("Se uma base não foi encontrada, extraia o kit inteiro e abra Lista_01.Rproj.")
message("Versão do R: ", R.version.string)
