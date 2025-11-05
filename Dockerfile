FROM ghcr.io/ropensci-review-tools/pkgcheck:latest
MAINTAINER Mark Padgham <mark@ropensci.org>

RUN apt-get update && apt-get install -y curl

RUN --mount=type=secret,id=GITHUB_PAT,env=GITHUB_PAT installGithub.r \
    ropensci-review-tools/pkgmatch
RUN Rscript -e 'pkgmatch::pkgmatch_update_cache()'

RUN curl -fsSL https://ollama.com/install.sh | sh

RUN nohup bash -c "ollama serve &" \
    && sleep 5 \
    && ollama pull jina/jina-embeddings-v2-base-en \
    && ollama pull unclemusclez/jina-embeddings-v2-base-code

EXPOSE 11434

ENTRYPOINT ["Rscript", "-e", "pkgmatch::pkgmatch_update_data()"]
