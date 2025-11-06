FROM ghcr.io/ropensci-review-tools/pkgcheck:latest
MAINTAINER Mark Padgham <mark@ropensci.org>

RUN apt-get update && apt-get install -y curl

RUN --mount=type=secret,id=GITHUB_PAT,env=GITHUB_PAT installGithub.r \
    ropensci-review-tools/pkgmatch

RUN curl -fsSL https://ollama.com/install.sh | sh

COPY data-raw/start.sh /start.sh
RUN cmhod +x /start.sh

EXPOSE 11434

ENTRYPOINT ["/start.sh"]
