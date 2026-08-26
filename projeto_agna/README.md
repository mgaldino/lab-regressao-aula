# Projeto aplicado: Brasil, China e votações na AGNU

Este diretório contém o laboratório reprodutível da Aula 2. A atividade descreve a convergência direta dos votos de Brasil e China na Assembleia Geral das Nações Unidas entre 1997 e 2016, comparando 1997–2008 e 2009–2016. A comparação é estritamente descritiva.

## Pergunta da Aula 2

Como se distribui a convergência direta dos votos de Brasil e China entre 1997 e 2016, antes e depois de 2009?

Uma linha da base do laboratório representa uma votação nominal em que os votos de Brasil e China foram observados. `convergente` vale 1 quando os dois registros são iguais e 0 quando são diferentes.

## Universo autoritativo

A lista atual contém **96 unidades totais: Brasil e 95 países do donor pool**. Não há uma 96ª unidade do donor pool além desses 95 países. A China não pertence à lista de 96 unidades; seu voto é a referência usada para calcular a convergência.

Na janela 1997–2016, a grade completa possui 96 unidades, 1.813 votações e 174.048 linhas país × votação. A base exclusiva da Aula 2 contém 1.762 pares Brasil–China com ambos os votos observados.

## Fontes preservadas

- `data/raw/unvotes/unvotes_0.3.0.tar.gz`: versão local preservada do pacote `unvotes`, com votos, votações nominais e temas;
- `data/raw/donor_pool/synth_data.rds`: lista autoritativa de 96 unidades e painel país-ano de 1997 a 2016.

Os hashes SHA-256 das fontes e derivados estão em `data/processed/SHA256SUMS`. O dicionário está em `data/processed/dicionario_variaveis.csv` e a proveniência detalhada em `data/processed/manifesto_dados.csv`.

## Execução reprodutível

A compilação dos documentos e a geração dos checksums usam explicitamente o locale UTF-8 `pt_BR.UTF-8`, para preservar os acentos em português e impedir que avisos de locale sejam confundidos com hashes. A partir da raiz deste worktree, execute:

```bash
Rscript --vanilla projeto_agna/scripts/01_construir_painel_agna.R
Rscript --vanilla projeto_agna/scripts/02_analise_descritiva_aula2.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla -e \
  'rmarkdown::render("projeto_agna/laboratorios/aula_02_convergencia_brasil_china.Rmd", output_file = "laboratorio_aula_02_convergencia_brasil_china.pdf", output_dir = "projeto_agna/output/pdf")'
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla -e \
  'rmarkdown::render("projeto_agna/laboratorios/slides_aula_02_convergencia_brasil_china.Rmd", output_file = "slides_aula_02_convergencia_brasil_china.pdf", output_dir = "projeto_agna/output/pdf")'
Rscript --vanilla projeto_agna/scripts/03_validar_artefatos_aula2.R
```

## Principais produtos

- `data/processed/painel_pais_votacao_1997_2016.csv.gz`: base-mãe país × votação;
- `data/processed/painel_pais_ano_1997_2016.csv`: painel anual para extensões futuras;
- `data/processed/brasil_convergencia_china_1997_2016.csv`: base da Aula 2;
- `output/aula_02/`: tabelas e figuras numeradas, captions, resumo e manifesto;
- `laboratorios/aula_02_convergencia_brasil_china.Rmd`: roteiro detalhado;
- `output/pdf/laboratorio_aula_02_convergencia_brasil_china.pdf`: roteiro compilado;
- `laboratorios/slides_aula_02_convergencia_brasil_china.Rmd`: slides Beamer;
- `output/pdf/slides_aula_02_convergencia_brasil_china.pdf`: slides compilados.

## Resultados conferidos

- 1.762 pares válidos;
- 1.412 convergências e 350 divergências;
- convergência geral de 80,1%;
- 78,6% em 1997–2008 e 82,6% em 2009–2016;
- diferença descritiva de 3,9 pontos percentuais;
- correlação entre `convergente` e `pos_2009` de 0,048.

## Progressão futura

Depois da Aula 2, o projeto pode avançar por quatro etapas: cross-sections por ano relacionando comércio com a China e alinhamento de voto; painel de vários países; comparação com países do donor pool em que a China não se tornou o principal destino; e, somente como extensão futura do paper, uso do indicador “China = principal destino” como tratamento do desenho.

Os campos comerciais são preservados na base-mãe, mas não são usados na Aula 2. A escala transformada desses campos deve ser documentada antes de interpretação substantiva.
