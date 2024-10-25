FROM eddelbuettel/r2u:22.04
MAINTAINER Mark Padgham <mark@ropensci.org>

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

# still need ubuntugis for gdal
RUN apt-get update -qq \
    && apt-get install -y software-properties-common gpg-agent \
    && apt-get update

RUN apt-get update -qq && apt-get install -y \
    curl \
    git

RUN curl -fsSL https://ollama.com/install.sh | sh \
    ollama serve & \
    ollama pull jina/jina-embeddings-v2-base-en \
    ollama pull ordis/jina-embeddings-v2-base-code

RUN install2.r \
    curl \
    httr \
    httr2 \
    rvest \
    xml2

RUN installGithub.r \
    ropensci-review-tools/pkgmatch

# Download package data:
RUN mkdir -p /home/.cache/R/pkgmatch \
    cd /home/.cache/R/pkgmatch \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/bm25-cran.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/bm25-ropensci-fns.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/bm25-ropensci.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/embeddings-cran.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/embeddings-fns.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/embeddings-ropensci.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/fn-calls-cran.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/fn-calls-ropensci.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/idfs-fn-calls-cran.Rds \
    curl -O https://github.com/ropensci-review-tools/pkgmatch/releases/download/v0.4.0/idfs-fn-calls-ropensci.Rds \
    cd ~
