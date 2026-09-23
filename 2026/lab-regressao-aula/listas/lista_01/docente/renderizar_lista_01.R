# Execute na raiz de 2026/lab-regressao-aula.
# Cálculos e verificações ficam separados em validar_lista_01.R.
Sys.setenv(OMP_NUM_THREADS = "1")
dir.create("output/pdf", recursive = TRUE, showWarnings = FALSE)
rmarkdown::render(
  "listas/lista_01/lista_01_revisada.Rmd",
  output_file = "lista_01_revisada_2026.pdf",
  output_dir = normalizePath("output/pdf"),
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)
