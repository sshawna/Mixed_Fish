
to_jsdate <- function(date_){
  val = as.POSIXct(as.Date(date_),origin="1970-01-01")
  as.numeric(val)
}