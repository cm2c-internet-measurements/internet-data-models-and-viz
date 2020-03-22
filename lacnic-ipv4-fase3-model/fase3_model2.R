# testing new ipv4 dataset

ipv4data = read.csv2("http://trantor.labs.lacnic.net/carlos/ipv4avail_lacnic.csv", sep='|')


ipv4data <- cbind(ipv4data, nro=seq(1,318))

ipv4data <- cbind(ipv4data, assignable=ipv4data$available+ipv4data$reserved-2*65536)

# as.numeric(as.POSIXct("2013-09-06", format="%Y-%m-%d"))

ipv4.raw = data.frame(
  dates_free4 =  as.numeric(as.POSIXct(as.character(ipv4data$date), format="%Y%m%d", origin="19700101") ), 
  free4 = ipv4data$assignable
)

