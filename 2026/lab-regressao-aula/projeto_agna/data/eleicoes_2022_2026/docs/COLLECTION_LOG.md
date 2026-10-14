# Relatório de coleta e preparação

Gerado automaticamente em 2026-10-14T15:31:22.512780+00:00.
Última consulta oficial registrada: 2026-10-13T23:15:37.727803+00:00.

## Cobertura obtida

- 2022: 2.637 zonas nas 27 UFs e Exterior (ZZ), com os dois turnos. Nenhuma chave perdida no cruzamento entre turnos.
- 2026: 2.641 zonas nas 27 UFs e Exterior (ZZ), com o primeiro turno. Todas recebem previsão.
- Entre anos: 5 chaves aparecem apenas em 2026 e 1 apenas em 2022.
- ZZ é uma zona agregada com uma linha em cada ano. Códigos iguais entre anos não asseguram território igual.
- CSVs em UTF-8. Votos são contagens não negativas, denominadores positivos e porcentagens em 0 a 100.
- Chaves UF + zona são únicas. As entradas do laboratório não têm valores ausentes.

## Diferença encontrada e resolvida em 2026

O snapshot por BU continha 119.296.853 votos válidos, incluindo exterior. O JSON nacional oficial registra 119.300.788. A diferença é 3.935 votos válidos. As listas oficiais identificam 3 seções ausentes em Betim (MG, zona 319) e 12 em Carapicuíba (SP, zona 388). São as 15 seções sem BU no Brasil registradas no recibo de cobertura.

Em cada município, todas as seções ausentes pertencem a uma única zona. Subtrai-se a soma dos BUs disponíveis do total municipal oficial e acrescenta-se a diferença à soma dessa zona. Essa identificação torna a alocação única. Não se estimam votos por seção nem se faz rateio entre zonas. O complemento de Betim é 631 votos válidos e o de Carapicuíba é 3.304. Brancos e nulos também são completados pelas diferenças oficiais.

Os 12 totais de candidatos e os votos válidos, após o complemento e incluindo ZZ, igualam a divulgação oficial nacional. A zona permanece a unidade do arquivo processado. A coluna de seções completadas e a tabela de complementos registram a transformação. Os Parquets originais não foram alterados.

O recibo registra ainda 41 seções sem BU no exterior. Os complementos brasileiros esgotam a diferença de votos válidos em relação ao total oficial nacional. A soma de votos válidos não tem diferença residual. Isso não estabelece a completude de todas as informações de eleitorado por seção.

## Fontes e filtros

2022: extratos tratados por seção de votação nominal e detalhe da apuração do TSE. 2026: extrato tratado de BUs da API do TSE, complementado com totais municipais oficiais. Preserva-se a atribuição da origem dos extratos ao projeto local electoralFraud. A reprodução desta preparação começa nos três Parquets preservados no curso. Os manifestos das consultas JSON registram URL, acesso UTC, tamanho e SHA-256. Os arquivos ZIP originais de todos os cargos não foram copiados para este pacote.

Filtros: cargo Presidente, dois turnos em 2022 e primeiro turno em 2026, 27 UFs e Exterior (ZZ). Primeiro somam-se contagens por categoria geográfica + zona. Depois calculam-se porcentagens sobre todos os votos válidos de cada turno. A porcentagem do primeiro turno não é renormalizada entre os dois candidatos finalistas.

## Verificações executadas

VALIDACAO_LAB_ELEITORAL_OK
Unidades, ausências, duplicidades e denominadores conferidos.
CSV de zonas, inclusive ZZ, igual à soma das seções, com os complementos documentados de 2026.
Inclinação e previsões conferidas por centralização dentro de UF/ZZ, sem lm()/predict().
Equações normais e agregações ponderadas de zonas e categorias conferidas.
500 sorteios: números de zonas por categoria preservados; ZZ fica fixa.
Maior diferença de inclinação bootstrap: 1.24345e-14
Maior diferença de previsão bootstrap: 4.547474e-13
Previsões de zona fora de 0-100: 0
Zonas 2026 fora da faixa de X de sua categoria em 2022: 94
Previsão didática nacional de Lula, 27 UFs + ZZ, pesos 2026 T1: 47.3076708
Estes checks validam dados e cálculos. Não testam a estabilidade da relação entre eleições.


A duração de 75 minutos é planejada. A adequação da relação de 2022 para prever o segundo turno de 2026 não foi validada fora do treino. O bootstrap condiciona no número de zonas por categoria e nas entradas de 2026; ZZ, com uma linha, não varia internamente. A agregação supõe pesos relativos de votos válidos iguais aos do primeiro turno de 2026.
