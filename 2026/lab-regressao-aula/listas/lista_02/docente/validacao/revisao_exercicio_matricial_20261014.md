# Lista 2: inclusão do exercício matricial

Revisão realizada em 14/10/2026, a pedido do professor, para exercitar as somas apresentadas no slide 11 da Aula 7.

O exercício 5 pede a transposta e as dimensões das matrizes, a multiplicação das quatro entradas de X'X e das duas entradas de X'Y. A demonstração deve mostrar os produtos escalares que originam cada soma, para uma regressão com intercepto e um preditor. Tempo sugerido: 15 minutos. A lista passou a ter cinco exercícios, quatro páginas e tempo total sugerido de 2h55.

## Arquivos atualizados

- Fonte: `listas/lista_02/lista_02_2026.Rmd`.
- PDF: `output/pdf/lista_02_2026.pdf`.
- Pacote discente: `output/lista_02_2026_alunos.zip`, com o PDF e os dois arquivos de dados/proveniência já distribuídos.

## Verificações executadas

A compilação com XeLaTeX foi concluída. As páginas 1 e 4 foram renderizadas e examinadas. O texto das páginas 2 e 3, com os exercícios 1 a 4, permaneceu idêntico ao PDF anterior. A conferência das coordenadas de todas as palavras encontrou zero palavras fora das páginas. O ZIP passou na conferência de integridade, e seus três arquivos foram comparados aos originais. O CSV e a proveniência permaneceram idênticos aos do pacote anterior. O diff passou sem espaços finais nas linhas alteradas.

Não houve alteração dos cálculos ou dos dados dos exercícios anteriores. O validador R existente não foi reexecutado, pois esta revisão acrescenta uma demonstração algébrica e atualiza os documentos de distribuição.

Reprodução do PDF, a partir de `2026/lab-regressao-aula`:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla -e 'rmarkdown::render("listas/lista_02/lista_02_2026.Rmd", output_format = "pdf_document", output_file = "lista_02_2026.pdf", output_dir = "output/pdf", clean = TRUE)'
```

SHA-256 do PDF: `eeef07d4c3340a52960da2817c7afd5c7594872718e77f9f081a484141e22691`.
