#

install.packages('curl')

rmarkdown::render('LACNIC_IPv4_F3_Report_v3.Rmd',output_file='ipv4_fase3.html')