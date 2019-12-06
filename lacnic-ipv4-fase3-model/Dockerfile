FROM ubuntu:bionic
RUN apt -y update 
RUN apt -y install vim-tiny r-base 
WORKDIR /root
RUN git clone https://github.com/cm2c-internet-measurements/internet-data-models-and-viz.git models.git
WORKDIR /root/models.git
RUN ./render_report.sh
