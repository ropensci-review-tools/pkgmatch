FROM ubuntu
MAINTAINER Mark Padgham <mark@ropensci.org>

RUN apt-get update && apt-get install -y curl

RUN curl -fsSL https://ollama.com/install.sh | sh

RUN nohup bash -c "ollama serve &" \
    && sleep 5 \
    && ollama pull jina/jina-embeddings-v2-base-en \
    && ollama pull ordis/jina-embeddings-v2-base-code

EXPOSE 11434

CMD ["ollama", "serve"]
