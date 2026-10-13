#!/usr/bin/env python3
"""Registra fontes oficiais e totalização presidencial de 2026 para o curso.

Fontes: Portal de Dados Abertos do TSE e resultados.tse.jus.br.
Acesso: JSON público, sem credenciais. Acesso planejado em 13/10/2026.
Os arquivos brutos permanecem inalterados; cada coleta tem data e manifesto.
"""
import datetime as dt
import hashlib
import json
import logging
from pathlib import Path
import time
import urllib.request

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "data" / "eleicoes_2022_2026"
FONTES = {
    "metadados_tse_2022": "https://dadosabertos.tse.jus.br/api/3/action/package_show?id=resultados-2022",
    "metadados_tse_2026": "https://dadosabertos.tse.jus.br/api/3/action/package_show?id=resultados-2026",
    "presidente_nacional_2026_t1": "https://resultados.tse.jus.br/oficial/ele2026/6257/dados/br/br-c0001-e006257-u.json",
    "presidente_betim_2026_t1": "https://resultados.tse.jus.br/oficial/ele2026/6257/dados/mg/mg41335-c0001-e006257-u.json",
    "presidente_carapicuiba_2026_t1": "https://resultados.tse.jus.br/oficial/ele2026/6257/dados/sp/sp63134-c0001-e006257-u.json",
    "secoes_mg_2026": "https://resultados.tse.jus.br/oficial/ele2026/arquivo-urna/3220/config/mg/mg-p003220-cs.json",
    "secoes_sp_2026": "https://resultados.tse.jus.br/oficial/ele2026/arquivo-urna/3220/config/sp/sp-p003220-cs.json",
}


def baixar(url):
    for tentativa in range(4):
        try:
            pedido = urllib.request.Request(url, headers={
                "User-Agent": "Mozilla/5.0 (academic regression course; public TSE data)"})
            with urllib.request.urlopen(pedido, timeout=30) as resposta:
                return resposta.read()
        except Exception:
            if tentativa == 3:
                raise
            time.sleep(2 ** tentativa)


def main():
    agora = dt.datetime.now(dt.timezone.utc)
    destino = DATA / "raw" / agora.strftime("%Y%m%dT%H%M%SZ")
    destino.mkdir(parents=True, exist_ok=False)
    manifesto = []
    for nome, url in FONTES.items():
        logging.info("Consultando %s", nome)
        corpo = baixar(url)
        json.loads(corpo)
        arquivo = destino / (nome + ".json")
        arquivo.write_bytes(corpo)
        manifesto.append({"id": nome, "url": url, "arquivo": str(arquivo.relative_to(DATA)),
                          "acesso_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
                          "bytes": len(corpo), "sha256": hashlib.sha256(corpo).hexdigest()})
    (destino / "manifesto.json").write_text(
        json.dumps(manifesto, indent=2, ensure_ascii=False), encoding="utf-8")
    logging.info("Fontes registradas em %s", destino)


if __name__ == "__main__":
    main()
