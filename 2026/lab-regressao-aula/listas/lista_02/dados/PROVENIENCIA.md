# Proveniência dos dados da Lista 2 (2026)

Arquivo entregue: `convergencia_anual_1997_2016.csv`, gerado em 29 de setembro de 2026.

- Fonte local imediata: `projeto_agna/data/processed/brasil_convergencia_china_1997_2016.csv` (SHA-256 `6ccdceffde03684449dfb99cd593e65e0470dfafa770f7e408044c55fb2c2cc2`). A base preserva 1.762 votações nominais de 1997 a 2016 com votos válidos de Brasil e China. A construção anterior e as fontes brutas estão documentadas em `projeto_agna/data/processed/manifesto_dados.csv` e `projeto_agna/README.md`.
- Transformação: `listas/lista_02/docente/validar_lista_02.R` agrupa por `ano` e calcula `n_resolucoes_validas`, `n_convergentes` e `taxa_media_convergencia = n_convergentes / n_resolucoes_validas`.
- Unidade do arquivo entregue: ano; 20 linhas, uma por ano de 1997 a 2016. O denominador total é 1.762 votações válidas. A regressão pedida dá o mesmo peso a cada um dos 20 anos, cujos denominadores diferem.
- SHA-256 do arquivo entregue: `53fdcd792583aa748df8065d31e59c9b65a4bd6e85f1fbf3e95552954ead6a53`.

Para reproduzir a transformação, execute a partir de `2026/lab-regressao-aula`:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla listas/lista_02/docente/validar_lista_02.R
```

Os dados são observados; somente o exercício 4 solicita simulação. Os dados simulados não fazem parte deste arquivo.
