# If fasttime is loaded, use fastPOSIXct, else use as.POSIXct
PosixFun <- if (use.fasttime) {
  function(x) {
    xx <- paste(paste(substring(x, 1, 4), substring(x, 5, 6), 
                      substring(x, 7, 8), sep="-"), substring(x, 10))
    fastPOSIXct(xx, "GMT")
  }
} else {
  function(x) {
    as.POSIXct(x, format="%Y%m%d %H:%M:%OS", tz="GMT")
  }
}

#' fast_POSIXct
#'
#' Quickly converts local timestamps to a POSIXct vector
#'
#' @param  x	timestamps (YYYY-mm-dd HH:MM:SS)
#' @param	tz	local timezone
#' @return	POSIXct vector
#' @export
fast_POSIXct <- function(x, tz) {
  require(fasttime)
  stopifnot(is.character(x))
  GMT <- fasttime::fastPOSIXct(x, tz='GMT')
  epoch <- as.numeric(GMT)
  t0 <- '1970-01-01'
  z <- as.POSIXct(epoch, tz=tz, origin=t0)
  adjusted <- z - as.POSIXlt(z)$isdst * 3600
  return(adjusted)
}

cat("making index for ", id, "\n")
idx <- PosixFun(fr[, 2])
obj <- xts(fr[, 3:4], idx, tzone="GMT")

tdata <- TYU12

# set our directory for getting the data
# we're going to convert this whole directory into minute xts data with VWAPs and sizes and closing prices
"C:/Users/gmonaie/xts_data/tick"

my.tickfiles = list.files("C:/Users/gmonaie/xts_data/tick")

for (tickfile in my.tickfiles[1])
{
  load(paste("C:/Users/gmonaie/xts_data/tick",tickfile, sep="/"))
  tdobject = as.POSIXct(paste(as.vector(get(tickfile)$DATE), as.vector(get(tickfile)$TIME)), 
                        format = "%m/%d/%Y %H:%M:%OS")
  tdata = xts(get(tickfile)[ ,3:5], order.by = tdobject) # truncate
  
  
  bardata <- merge(aggregatets(tdata$PRICE, FUN="previoustick", on="minutes", k=1),
                   aggregatets(tdata$SIZE, FUN="sumN", on="minutes", k=1),
                   aggregatets(tdata$PRICE, FUN="previoustick", on="minutes", k=1, weights=tdata$SIZE))

  colnames(bardata) <- c("PRICE", "SIZE", "VWAP")
  
  assign(tickfile, bardata)
  
  saveSymbols.common(tickfile, base_dir="C:/Users/gmonaie/xts_data/min/")
}


bardata2 <- merge(aggregatets(bardata$PRICE, FUN="previoustick", on="minutes", k=15),
                 aggregatets(bardata$SIZE, FUN="sumN", on="minutes", k=15),
                 aggregatets(bardata$VWAP, FUN="previoustick", on="minutes", k=15, weights=bardata$SIZE))

#load the tickdata from r
#convert the tickdata timestamps to POSIXct, these are all exchange time -- if we're doing fast conversion
#then we need to know the exchange time zone when we do this

tdobject = as.POSIXct(paste(as.vector(tdata$DATE), as.vector(tdata$TIME)), 
                      format = "%m/%d/%Y %H:%M:%OS")

tdata = xts(tdata[ ,3:5], order.by = tdobject)

system.time(
  foobar2 <- merge(aggregatets(tdata$PRICE, FUN="previoustick", on="minutes", k=15),
                   aggregatets(tdata$SIZE, FUN="sumN", on="minutes", k=15),
                   aggregatets(tdata$PRICE, FUN="previoustick", on="minutes", k=15, weights=tdata$SIZE))
  )

colnames(foobar2) <- c("PRICE", "SIZE", "VWAP")

# add a SYMBOL column
tdata$SYMBOL <- "FOO"

# after aggregating by trades we may end up with duplicate rows (trades at one price, closing at one price)
test <- aggregateTrades(tdata["2012-09-13"],on="seconds",k=1)


data("sample_tdata");
aggregate trade data to 5 minute frequency
x = aggregateTrades(sample_tdata,on="minutes",k=5)
head(x);