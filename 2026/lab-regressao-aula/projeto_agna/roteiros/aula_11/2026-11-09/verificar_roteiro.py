"""Conta a fala e verifica o orçamento conservador; não monta slides."""
import hashlib
import json
import math
import re
from pathlib import Path

PASTA = Path(__file__).resolve().parent
FONTE = PASTA / "roteiro_v1_revisado.md"

def mmss(minutos):
    segundos = math.floor(minutos * 60 + 0.5 + 1e-9)
    return f"{segundos // 60}:{segundos % 60:02d}"

def duracao(texto, categoria):
    achado = re.search(r"\*\*" + re.escape(categoria) + r":\*\*\s+([^\n]+)", texto)
    if achado is None:
        return 0
    valor = re.match(r"(?:(\d+)min(?:(\d+)s)?|(\d+) minutos?|(\d+) segundos?)", achado.group(1))
    if valor is None:
        raise ValueError(f"Duração não reconhecida: {achado.group(0)}")
    a, b, c, d = valor.groups()
    return 60 * int(a or c or 0) + int(b or d or 0)

def main():
    texto = FONTE.read_text(encoding="utf-8")
    encontrados = re.findall(r"^## B(\d+) — ([^\n]+)\n(.*?)(?=^## |\Z)", texto, re.M | re.S)
    assert len(encontrados) == 7
    assert texto.count("<!-- FALA -->") == texto.count("<!-- /FALA -->") == 7
    blocos = []
    acumulado = 0
    for numero, titulo, corpo in encontrados:
        fala = "\n".join(re.findall(r"<!-- FALA -->(.*?)<!-- /FALA -->", corpo, re.S)).strip()
        palavras = len(re.findall(r"\b\w+(?:[-’']\w+)*\b", fala))
        acoes = duracao(corpo, "Ação e pausa")
        interacao = duracao(corpo, "Interação")
        tempo_90 = palavras / 90 + (acoes + interacao) / 60
        acumulado += tempo_90
        blocos.append(dict(bloco=f"B{numero}", titulo=titulo, palavras=palavras,
                           acoes_segundos=acoes, interacao_segundos=interacao,
                           minutos_90=tempo_90, acumulado_90=acumulado))
    palavras = sum(b["palavras"] for b in blocos)
    acoes = sum(b["acoes_segundos"] for b in blocos) / 60
    interacao = sum(b["interacao_segundos"] for b in blocos) / 60
    folga = (50 - interacao) * .15
    teto = (50 - interacao) * .85
    exposicao_90 = palavras / 90 + acoes
    assert exposicao_90 <= teto, (exposicao_90, teto)
    totais = dict(disponivel_minutos=50, laboratorio_separado_minutos=75,
                  palavras_fala=palavras, acoes_minutos=acoes, interacao_minutos=interacao,
                  fala_minutos_90=palavras/90, fala_minutos_100=palavras/100,
                  teto_exposicao_minutos=teto, exposicao_90_minutos=exposicao_90,
                  folga_minutos=folga, total_90=exposicao_90+interacao+folga,
                  total_100=palavras/100+acoes+interacao+folga,
                  saldo_adicional_90=50-exposicao_90-interacao-folga)
    linhas = ["| Bloco | Palavras | Ações | Interação | Duração a 90 ppm | Acumulado |",
              "|------------------------------------------|--------:|------:|---------:|----------------:|----------:|"]
    for b in blocos:
        linhas.append(f"| {b['bloco']} — {b['titulo']} | {b['palavras']} | "
                       f"{mmss(b['acoes_segundos']/60)} | {mmss(b['interacao_segundos']/60)} | "
                       f"{mmss(b['minutos_90'])} | {mmss(b['acumulado_90'])} |")
    disponivel_texto = f"{50-interacao:g}".replace(".", ",")
    linhas += ["", f"Fala: **{palavras} palavras**. A 90 palavras/minuto: {mmss(palavras/90)} de fala + "
               f"{mmss(acoes)} de ações + {mmss(interacao)} de interação = {mmss(acumulado)} antes da folga.",
               "", f"Folga: {mmss(folga)}, correspondente a 15% dos {disponivel_texto} minutos de exposição disponíveis. "
               f"Total conservador com folga: **{mmss(totais['total_90'])}**. Saldo adicional: "
               f"{mmss(totais['saldo_adicional_90'])}. A 100 palavras/minuto, com as mesmas ações, interação e folga: "
               f"{mmss(totais['total_100'])}. Tempos estimados, sem ensaio; os segundos não expressam precisão empírica."]
    orcamento = "\n".join(linhas)
    texto = re.sub(r"(?<=<!-- ORCAMENTO_INICIO -->).*?(?=<!-- ORCAMENTO_FIM -->)",
                   "\n"+orcamento+"\n", texto, flags=re.S)
    FONTE.write_text(texto, encoding="utf-8")
    (PASTA/"tempo_v1.md").write_text("# Orçamento de tempo da Aula 11\n\n"+orcamento+"\n",encoding="utf-8")
    resultado = dict(versao="v1 revisada para aprovação em 09/11/2026", blocos=blocos, totais=totais,
                     criterio="Tokens lexicais Unicode somente dentro das marcações FALA",
                     calibracao="90 e 100 palavras/minuto são hipóteses, sem ensaio oral",
                     sha256_roteiro=hashlib.sha256(FONTE.read_bytes()).hexdigest())
    (PASTA/"tempo_v1.json").write_text(json.dumps(resultado,ensure_ascii=False,indent=2)+"\n",encoding="utf-8")
    print(json.dumps(totais,ensure_ascii=False,indent=2))

if __name__ == "__main__":
    main()
