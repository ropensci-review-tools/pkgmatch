#!/bin/bash

nohup ollama serve &
sleep 5

ollama pull jina/jina-embeddings-v2-base-en
ollama pull unclemusclez/jina-embeddings-v2-base-code

exec Rscript -e "pkgmatch::pkgmatch_update_data()"
