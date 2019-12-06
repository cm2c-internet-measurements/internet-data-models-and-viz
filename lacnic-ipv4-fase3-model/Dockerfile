# FROM ubuntu:bionic
FROM rocker/r-ubuntu:18.04
ENV TZ=America/Montevideo
ARG DEBIAN_FRONTEND=noninteractive
ENV DEBIAN_FRONTEND=noninteractive
RUN apt -y update 
RUN apt -y install r-cran-rmarkdown
RUN apt -y install vim-tiny r-base less git curl libcurl4-openssl-dev
WORKDIR /root
# RUN git clone https://github.com/cm2c-internet-measurements/internet-data-models-and-viz.git models.git
# WORKDIR /root/models.git
