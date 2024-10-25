FROM ollama/ollama:latest
MAINTAINER Mark Padgham <mark@ropensci.org>

RUN nohup bash -c "ollama serve &" \
    && sleep 5 \
    && ollama pull jina/jina-embeddings-v2-base-en \
    && ollama pull ordis/jina-embeddings-v2-base-code

EXPOSE 11434

CMD ["serve"]
