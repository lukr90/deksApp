FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev
RUN apt-get update -y
RUN apt-get install -y libpq-dev postgresql-client

RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('purrr')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('stringr')"
RUN Rscript -e "install.packages('rvest')"
RUN Rscript -e "install.packages('polite')"
RUN Rscript -e "install.packages('readr')"
RUN Rscript -e "install.packages('reactable')"
RUN Rscript -e "install.packages('shinyjs')"

COPY / /

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R', port = 3838, host = '0.0.0.0') "]