FROM eddelbuettel/r2u:24.04
LABEL org.opencontainers.image.authors="mark.padgham@email.com"

RUN apt-get update && apt-get install -y --no-install-recommends \
                sudo \
                r-cran-bspm \
        && echo "bspm::enable()" >> /etc/R/Rprofile.site \
        && echo "options(bspm.sudo=TRUE)" >> /etc/R/Rprofile.site \
        && echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends \
        && echo "docker ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/local-docker-user \
        && chmod 0440 /etc/sudoers.d/local-docker-user \
        && chgrp 1000 /usr/local/lib/R/site-library \
        && install.r remotes

RUN apt-get install -y curl libcurl4-openssl-dev

RUN install2.r \
    openssl \
    curl \
    xml2 \
    httr2

# Plus all 'Suggests' packages needed for update fn:
RUN install2.r \
    gert \
    hms \
    jsonlite \
    rappdirs \
    withr

RUN --mount=type=secret,id=GITHUB_PAT,env=GITHUB_PAT installGithub.r \
    ropensci-review-tools/pkgmatch

RUN curl -fsSL https://ollama.com/install.sh | sh

RUN nohup bash -c "ollama serve &" \
    && sleep 5 \
    && ollama pull jina/jina-embeddings-v2-base-en \
    && ollama pull unclemusclez/jina-embeddings-v2-base-code

EXPOSE 11434

ENTRYPOINT ["Rscript", "-e", "pkgmatch::pkgmatch_update_data()"]
