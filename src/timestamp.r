# Format the timestamp without separator
timestamp <- Sys.time()
ftime <- format(timestamp, "%H%M%S")
fdate <- format(timestamp, "%Y%m%d")
# Format unique experiment identifier
PARAM$experimento <- paste0("KA4216-",fdate,"-",ftime)