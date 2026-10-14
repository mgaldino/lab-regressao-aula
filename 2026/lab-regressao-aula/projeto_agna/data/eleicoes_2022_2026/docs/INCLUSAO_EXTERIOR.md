# Inclusão do exterior nas bases observadas

Preparação e conferência realizadas em 13/10/2026, após o pedido de inclusão do exterior. Foram produzidas duas bases adicionais a partir dos mesmos snapshots públicos do TSE preservados no curso. A diferença no filtro é a inclusão de `uf == "ZZ"`.

Uma linha continua representando UF + zona. A categoria ZZ reúne a Zona Eleitoral do Exterior. A base de 2022 passa a ter 2.637 linhas, com os dois turnos. A base do primeiro turno de 2026 passa a ter 2.641 linhas. Há 28 categorias geográficas: as 27 UFs e Exterior. Todos os votos das seções do exterior são somados antes de calcular as porcentagens sobre os votos válidos do respectivo turno.

O script 17 aceita `--incluir-exterior` e grava arquivos com o sufixo `_com_exterior`. A execução sem esse argumento continua preparando a versão original. O script 21 verifica apenas dados observados, sem estimar regressões nem gerar previsões.

## Verificações executadas

- Ausência de duplicidades de UF + zona e de valores ausentes.
- Presença das 27 UFs e ZZ nas duas bases.
- Denominadores positivos.
- Contagens de votos, seções e municípios do exterior iguais às somas dos snapshots por seção.
- Percentuais iguais às razões entre votos observados e votos válidos.
- Soma dos percentuais dos dois candidatos do segundo turno de 2022 igual a 100.
- Parte das 27 UFs idêntica à preparação anterior.
- Total de votos válidos do primeiro turno de 2026, incluindo exterior, igual ao JSON nacional oficial.

Resultado: `VALIDACAO_DADOS_COM_EXTERIOR_OK`. O relatório está em `validacao_dados_com_exterior.txt`. Os hashes estão em `checksums_com_exterior.sha256`, na raiz do diretório de dados.

Essa entrega é uma ampliação da cobertura dos dados observados. Os modelos, suas previsões e o ZIP do laboratório original não foram recalculados com essa versão.
