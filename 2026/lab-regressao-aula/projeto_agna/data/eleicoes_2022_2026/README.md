# Dados eleitorais da Aula 7

Uma linha representa uma zona eleitoral, identificada por **UF + número da zona**. Somam-se os votos de todas as seções e municípios abrangidos por ela antes de calcular as porcentagens. A versão original do laboratório cobre as 27 UFs brasileiras. Uma versão adicional de dados observados, preparada em 13/10/2026, inclui o exterior, identificado por ZZ.

## Versão de dados que inclui o exterior

| Arquivo | Linhas | Cobertura |
|---|---:|---|
| `processed/presidente_2022_zonas_com_exterior.csv` | 2.637 | Dois turnos de 2022, 27 UFs e ZZ |
| `processed/presidente_2026_t1_zonas_com_exterior.csv` | 2.641 | Primeiro turno de 2026, 27 UFs e ZZ |

Há uma zona do exterior por ano e turno nas bases. ZZ é uma categoria geográfica adicional, e não uma UF brasileira. Os percentuais usam todos os votos válidos do turno. Essa preparação não calcula modelos ou previsões.

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/17_preparar_dados_eleicoes.R --incluir-exterior
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/21_validar_dados_com_exterior.R
```

Detalhes da conferência: `docs/INCLUSAO_EXTERIOR.md`. Integridade dessa versão: `checksums_com_exterior.sha256`.

## Entradas prontas para o laboratório

| Arquivo | Linhas | Conteúdo |
|---|---:|---|
| `processed/presidente_2022_zonas.csv` | 2.636 | Lula e Jair Bolsonaro, votos e porcentagens nos dois turnos |
| `processed/presidente_2026_t1_zonas.csv` | 2.640 | Lula e Flávio Bolsonaro, votos e porcentagens no primeiro turno |

São aproximadamente 555 KiB de CSVs. O estudante precisa desses dois arquivos e do script 18. A coleta e a preparação já foram realizadas. Nenhum download é necessário durante a aula.

Cada porcentagem tem como denominador **todos os votos válidos do respectivo turno**. No primeiro turno, isso inclui os votos de candidatos que não disputam o segundo. Votos brancos e nulos não entram no denominador.

## Reprodução a partir dos arquivos preservados

Abra `lab-regressao-aula-2026.Rproj` ou execute os comandos a partir de `2026/lab-regressao-aula`:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/17_preparar_dados_eleicoes.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/18_laboratorio_aula7_previsao_eleitoral.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/19_validar_laboratorio_eleitoral.R
```

Dependências do estudante: R, `dplyr`, `ggplot2`, `here` e `scales`. A preparação e a validação docente também usam `arrow` e `jsonlite`.

Os três snapshots Parquet em `raw/` foram copiados, sem alterações, do projeto local `electoralFraud`, onde foram preparados em 05/10/2026 a partir dos arquivos públicos do TSE e de boletins de urna. Eles **já são extratos tratados por seção**, e não os arquivos ZIP originais do TSE. A cópia do script que os preparou está em `docs/origem_06b_section_votes.R`. Ele documenta a transformação anterior, mas sua execução completa depende dos arquivos daquele projeto. A reprodução da preparação das zonas aqui começa nos snapshots incluídos e dispensa o projeto externo.

O snapshot municipal preservado em `raw/painel_municipal_snapshot_20261005.parquet` foi consultado na exploração inicial. Não é entrada da versão final por zonas.

## Atualização das consultas oficiais

```sh
python3 projeto_agna/scripts/16_coletar_metadados_eleicoes.py
```

Esse comando consulta metadados, a totalização presidencial de 2026, totais de dois municípios e listas de seções de MG e SP. Salva JSONs sem alterações em um novo diretório UTC, com URLs, horários, tamanhos e SHA-256. Python usa apenas a biblioteca padrão. As consultas foram executadas em 13/10/2026. A execução padrão do script 17 usa o último JSON preservado de cada tipo. Se a totalização mudar e divergir dos snapshots, a validação interrompe a preparação. Uma nova coleta não atualiza os Parquets automaticamente.

## Conferência e completude

O arquivo de BUs de 2026 tinha 15 seções ausentes no Brasil. Foram recuperados 3.935 votos válidos na **soma das zonas** usando os totais municipais oficiais. As listas de seções mostram que todas as seções ausentes de cada um dos dois municípios pertencem à mesma zona. Assim, a alocação dos complementos não exige rateio ou imputação por seção. Detalhes em `docs/COLLECTION_LOG.md` e `docs/complementos_totais_de_zona_2026.csv`.

Os 12 totais de candidatos e o total de votos válidos, após os complementos e antes de excluir o exterior, coincidem com o JSON nacional oficial. Em 2022, as chaves de zona coincidem nos dois turnos. Entre 2022 e 2026 há cinco novas chaves e uma que deixa de aparecer. Elas não impedem a aplicação, pois o modelo usa a porcentagem do primeiro turno e a UF. O código não trata os dois anos como um painel de zonas com território fixo.

Fontes, licença e filtros: `docs/SOURCES.yaml`. Variáveis: `docs/DATA_DICTIONARY.md`. Integridade: `checksums.sha256` e `docs/manifesto_arquivos.csv`.

```sh
cd projeto_agna/data/eleicoes_2022_2026
shasum -a 256 -c checksums.sha256
```
