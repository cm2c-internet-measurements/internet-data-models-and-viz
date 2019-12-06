#!/bin/sh

R -e \
   "rmarkdown::render('LACNIC_IPv4_F3_Report_v3.Rmd',output_file='ipv4_fase3.html')" \
    --args --title='Modelado IPv4 Fase 3' --author='Carlos Martinez <carlos@lacnic.net>'
