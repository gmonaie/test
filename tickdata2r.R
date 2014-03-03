require(highfrequency)
require(stringr)
require(doSNOW)
require(foreach)

use.fasttime <- if (require(fasttime)) {
  TRUE
} else {
  message("This would be faster if the fasttime package were installed")
  message("see http://www.rforge.net/fasttime/")
  FALSE
}

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

base_dir <- "~/xts_data"
base_dir <- "C:/Users/gmonaie/xts_data/"

# if base_dir doesn't end with a forward slash, add a forward slash at the end
if (substr(base_dir, nchar(base_dir), nchar(base_dir)) != "/") {
  base_dir <- paste0(base_dir, "/")
}

# Create base_dir if it doesn't already exist as well as 3 subdirectories
dir.create(base_dir, mode="0755", showWarnings=FALSE)
dir.create(archive_dir <- paste0(base_dir, "archive/"), mode="0755", 
           showWarnings=FALSE)
dir.create(tick_dir <- paste0(base_dir, "tick/"), mode="0755", 
           showWarnings=FALSE)
dir.create(sec_dir <- paste0(base_dir, "sec/"), mode="0755", 
           showWarnings=FALSE)
dir.create(min_dir <- paste0(base_dir, "min/"), mode="0755", 
           showWarnings=FALSE)

# set some options
if (is.null(getOption("digits.secs"))) options(digits.secs=6)
oldTZ <- Sys.getenv("TZ")
Sys.setenv(TZ='GMT') # data is stored in GMT
oldwd <- getwd()
setwd(archive_dir)

my.directory = "C:/FUT";
my.ticker = "TY"
my.tickerdir = paste(my.directory, substring(my.ticker,0,1), my.ticker, sep="/")
my.files = list.files(my.tickerdir, full.names=TRUE)

dir.create(tmp_dir <- paste0(base_dir, "tmp"), mode="0755", 
           showWarnings=FALSE)

# we're now going to default unzip and save files to the tmp_dir
setwd(tmp_dir)

# for each one of these files, unzip
for (fn in my.files) {
  cat("Unzipping ", fn, "\n")
  unzip(fn)
}

# sorted file list
my.filelist = sort(list.files())
unlink(my.filelist[grep(pattern="SUMMARY",my.filelist)])

my.filelist = sort(list.files())
my.symbols = unique(str_sub(my.filelist,0,5))

cl<-makeCluster(4) #change the 2 to your number of CPU cores, my ASUS N56VM has 4 cores
registerDoSNOW(cl)

foreach(symbol = my.symbols) %dopar%
{
  cat("Loading ", symbol, "...\n")
  my.files <- my.filelist[grep(symbol, my.filelist)]
  
  # pre-allocate an empty list of length files we will read in
  # this is faster than iteratively allocating space for files we read in
  my.list <- vector("list", length(my.files))
  
  for (i in seq_along(my.files)) {
    cat("  Reading ", my.files[i], "...\n")
    # truncate to the first 5 columns -- after 2005 there are extra condition columns
    my.list[[i]] <- read.delim(my.files[i], header=F, sep=",", dec = ".")[1:5]
    colnames(my.list[[i]]) <- c("DATE", "TIME", "PRICE", "SIZE", "EX")
  }
  
  # assign the tick data to this symbol variable name
  assign(symbol, do.call("rbind", lapply(my.list, data.frame, stringsAsFactors = FALSE)))
  
  # save it down to the tick directory
  save(list=symbol, file=paste0(path.expand(tick_dir), symbol))
  
  # remove the symbol from our local memory
  rm(symbol)
  rm(my.files, my.list)
}

stopCluster(cl)

rm(my.filelist)

# Restore previous settings
Sys.setenv(TZ=oldTZ)
setwd(oldwd)
unlink(path.expand(tmp_dir), recursive=T)