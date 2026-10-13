# Dicionário das entradas do laboratório

Os dois CSVs usam UTF-8, vírgula como separador e ponto decimal. `t1` designa primeiro turno e `t2` segundo turno. Uma linha representa UF + zona. Cada porcentagem é calculada depois de somar os votos de todas as seções da zona.

## Variáveis comuns

| Variável | Tipo | Descrição e unidade | Valores | Fonte |
|---|---|---|---|---|
| `uf` | texto | Sigla da unidade da Federação | 27 UFs, exclui ZZ | TSE |
| `zona` | inteiro | Número da zona dentro da UF | Positivo, identificador único apenas com UF | TSE |
| `lula_t1` | número inteiro de votos | Votos válidos de Lula no primeiro turno | Não negativo | `tse_2022_secoes_snapshot` ou `tse_2026_bu_snapshot` + `tse_2026_json_oficial` |
| `validos_t1` | número inteiro de votos | Soma dos votos válidos de todos os candidatos no primeiro turno | Positivo | Mesmas fontes |
| `brancos_t1`, `nulos_t1` | número inteiro de votos | Votos brancos e nulos, fora do denominador de porcentagens | Não negativos | Mesmas fontes |
| `secoes_t1` | inteiro | Número de seções receptoras na soma da zona | Positivo | Mesmas fontes |
| `municipios_t1` | inteiro | Número de códigos de município presentes na zona | Positivo | Mesmas fontes |
| `pct_lula_t1` | real | `100 * lula_t1 / validos_t1`, porcentagem de votos válidos | 0 a 100 | Cálculo a partir das fontes acima |

## `presidente_2022_zonas.csv`

Fonte: `tse_2022_secoes_snapshot`. Inclui as variáveis comuns e:

| Variável | Tipo | Descrição e unidade | Valores |
|---|---|---|---|
| `bolsonaro_t1` | número inteiro de votos | Votos válidos de Jair Bolsonaro no primeiro turno | Não negativo |
| `pct_bolsonaro_t1` | real | `100 * bolsonaro_t1 / validos_t1`, porcentagem | 0 a 100 |
| `lula_t2`, `bolsonaro_t2` | número inteiro de votos | Votos dos dois candidatos no segundo turno | Não negativos |
| `validos_t2` | número inteiro de votos | Total válido do segundo turno, igual à soma dos votos dos dois candidatos | Positivo |
| `brancos_t2`, `nulos_t2` | número inteiro de votos | Votos brancos e nulos no segundo turno | Não negativos |
| `secoes_t2`, `municipios_t2` | inteiro | Números de seções receptoras e de municípios na zona no segundo turno | Positivos |
| `pct_lula_t2`, `pct_bolsonaro_t2` | real | `100 * votos do candidato / validos_t2`, porcentagens que somam 100 | 0 a 100 |

## `presidente_2026_t1_zonas.csv`

Fontes: `tse_2026_bu_snapshot` e `tse_2026_json_oficial`. Inclui as variáveis comuns e:

| Variável | Tipo | Descrição e unidade | Valores |
|---|---|---|---|
| `flavio_t1` | número inteiro de votos | Votos válidos de Flávio Bolsonaro no primeiro turno | Não negativo |
| `pct_flavio_t1` | real | `100 * flavio_t1 / validos_t1`, porcentagem | 0 a 100 |
| `secoes_completadas_por_total_municipal_t1` | inteiro | Quantidade de seções cujo complemento de votos foi recuperado no total da zona por diferença com o total municipal oficial | 0, 3 ou 12 |

Este arquivo contém resultados observados do primeiro turno. Não contém resultados observados do segundo turno de 2026. As previsões são exportadas em outro diretório, `output/aula_07/laboratorio_eleitoral/`.

## Auditorias em `docs/`

`complementos_totais_de_zona_2026.csv`: `uf`, `cd_mun` (código municipal TSE), `zona`, `faltantes` (quantidade de seções), `cNN` (complemento de votos válidos do candidato número NN), `validos`, `brancos` e `nulos` (complementos em votos). Os valores são diferenças entre totais municipais oficiais e a soma de BUs disponíveis. São alocados integralmente na única zona com seções ausentes daquele município.

`zonas_com_chave_em_apenas_um_ano.csv`: `uf`, `zona`, `em_2022` e `em_2026` (lógicos). Registra cinco chaves novas em 2026 e uma presente apenas em 2022. Não informa a equivalência territorial das demais zonas.

`manifesto_arquivos.csv`: caminho relativo, tamanho em bytes e SHA-256 dos arquivos preservados e derivados.
