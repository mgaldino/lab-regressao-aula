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

## Laboratório da Aula 7

Atividade principal de 75 minutos: `projeto_agna/scripts/18_laboratorio_aula7_previsao_eleitoral.R`. Usa porcentagens de votos válidos por zona eleitoral, estima o modelo com UF em 2022, aplica-o ao primeiro turno de 2026 e executa bootstrap de pares dentro de UF. Os CSVs estão em `projeto_agna/data/eleicoes_2022_2026/processed/`. A documentação e a reprodução docente estão no README desse diretório de dados. O pacote para os alunos está em `output/laboratorio_aula_07_eleicoes_2026_alunos.zip`.

O laboratório sintético de Monte Carlo, `projeto_agna/scripts/14_laboratorio_aula7_monte_carlo_bootstrap.R`, permanece como atividade complementar. Os slides e suas figuras sintéticas seguem em `projeto_agna/laboratorios/` e `projeto_agna/output/aula_07/`.
