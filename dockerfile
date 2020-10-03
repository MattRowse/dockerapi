FROM rocker/r-ver:3.5.0

RUN apt-get update -qq && apt-get install -y \
      libssl-dev \
      libcurl4-gnutls-dev
      
RUN R -e "install.packages('plumber')"

COPY / /

EXPOSE 80

ENTRYPOINT ["Rscript", "main.R"]