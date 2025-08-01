% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{get_ollama_url}
\alias{get_ollama_url}
\title{Get the URL for local ollama API}
\usage{
get_ollama_url()
}
\value{
The ollama API URL
}
\description{
Return the URL of the specified ollama API. Default is
"127.0.0.1:11434"
}
\examples{
u <- get_ollama_url ()

}
\seealso{
\code{set_ollama_url}

Other ollama: 
\code{\link{ollama_check}()},
\code{\link{set_ollama_url}()}
}
\concept{ollama}
