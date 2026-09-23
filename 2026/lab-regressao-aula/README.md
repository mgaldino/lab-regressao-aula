# FLS 6183 - Métodos Quantitativos de Pesquisa II (2026)

Esta pasta reúne os materiais vigentes do curso em 2026.

- `syllabus.Rmd`: fonte editável do programa;
- `syllabus.pdf`: programa compilado;
- `projeto_agna/`: dados, scripts, roteiro e resultados do projeto aplicado cumulativo;
- `lab-regressao-aula-2026.Rproj`: raiz do projeto R.

## Reprodução

A partir desta pasta, prepare a base e execute o laboratório da Aula 2 com:

```bash
Rscript --vanilla projeto_agna/scripts/01_construir_painel_agna.R
Rscript --vanilla projeto_agna/scripts/02_analise_descritiva_aula2.R
Rscript --vanilla projeto_agna/scripts/03_validar_artefatos_aula2.R
```

Compile o syllabus com:

```bash
Rscript --vanilla -e 'rmarkdown::render("syllabus.Rmd", output_format = "pdf_document", clean = TRUE)'
```

Os materiais de 2025 permanecem na pasta correspondente como arquivo histórico.
