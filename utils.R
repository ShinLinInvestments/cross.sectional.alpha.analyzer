library(data.table)
library(ggplot2)
library(memoise)
library(futile.logger)

Sys.setlocale("LC_TIME","English")
setwd("H:/Goldhedge/Career/Opportunities/QuantFin/Coatue/")

utils.checkCond <- function(cond, err.message){
	if(all(cond)) return()
	stop(err.message)
}

utils.gen.bdays <- function(){
	all.trading.dates = seq(as.Date("2000-01-01"), Sys.Date(), by="1 day")
	all.trading.dates = all.trading.dates[!as.POSIXlt(all.trading.dates)$wday %in% c(0,6,7)]
	nyse.holidays = as.Date(c(
		'1999-01-01','1999-01-18','1999-02-15','1999-04-02','1999-05-31','1999-07-05','1999-09-06','1999-11-25','1999-12-24',
					 '2000-01-17','2000-02-21','2000-04-21','2000-05-29','2000-07-04','2000-09-04','2000-11-23','2000-12-25',
		'2001-01-01','2001-01-15','2001-02-19','2001-04-13','2001-05-28','2001-07-04','2001-09-03','2001-09-11','2001-09-12','2001-09-13','2001-09-14','2001-11-22','2001-12-25',
		'2002-01-01','2002-01-21','2002-02-18','2002-03-29','2002-05-27','2002-07-04','2002-09-02','2002-11-28','2002-12-25',
		'2003-01-01','2003-01-20','2003-02-17','2003-04-18','2003-05-26','2003-07-04','2003-09-01','2003-11-27','2003-12-25',
		'2004-01-01','2004-01-19','2004-02-16','2004-04-09','2004-05-31','2004-06-11','2004-07-05','2004-09-06','2004-11-25','2004-12-24',
					 '2005-01-17','2005-02-21','2005-03-25','2005-05-30','2005-07-04','2005-09-05','2005-11-24','2005-12-26',
		'2006-01-02','2006-01-16','2006-02-20','2006-04-14','2006-05-29','2006-07-04','2006-09-04','2006-11-23','2006-12-25',
		'2007-01-01','2007-01-02','2007-01-15','2007-02-19','2007-04-06','2007-05-28','2007-07-04','2007-09-03','2007-11-22','2007-12-25',
		'2008-01-01','2008-01-21','2008-02-18','2008-03-21','2008-05-26','2008-07-04','2008-09-01','2008-11-27','2008-12-25',
		'2009-01-01','2009-01-19','2009-02-16','2009-04-10','2009-05-25','2009-07-03','2009-09-07','2009-11-26','2009-12-25',
		'2010-01-01','2010-01-18','2010-02-15','2010-04-02','2010-05-31','2010-07-05','2010-09-06','2010-11-25','2010-12-24',
					 '2011-01-17','2011-02-21','2011-04-22','2011-05-30','2011-07-04','2011-09-05','2011-11-24','2011-12-26',
		'2012-01-02','2012-01-16','2012-02-20','2012-04-06','2012-05-28','2012-07-04','2012-09-03','2012-10-29','2012-10-30','2012-11-22','2012-12-25',
		'2013-01-01','2013-01-21','2013-02-18','2013-03-29','2013-05-27','2013-07-04','2013-09-02','2013-11-28','2013-12-25',
		'2014-01-01','2014-01-20','2014-02-17','2014-04-18','2014-05-26','2014-07-04','2014-09-01','2014-11-27','2014-12-25',
		'2015-01-01','2015-01-19','2015-02-16','2015-04-03','2015-05-25','2015-07-03','2015-09-07','2015-11-26','2015-12-25',
		'2016-01-01','2016-01-18','2016-02-15','2016-03-25','2016-05-30','2016-07-04','2016-09-05','2016-11-24','2016-12-25',
		'2017-01-02','2017-01-16','2017-02-20','2017-04-14','2017-05-29','2017-07-04','2017-09-04','2017-11-23','2017-12-25',
		'2018-01-01','2018-01-15','2018-01-19','2018-03-30','2018-05-28','2018-07-04','2018-09-03','2018-11-22','2018-12-25'))
	return(all.trading.dates[which(!all.trading.dates %in% nyse.holidays)])
}

utils.date2bday <- memoise( function(d){
	if(length(d) > 1){
		return(unlist(lapply(d, utils.date2bday)))
	}
	return(all.trading.dates[all.trading.dates > d][1])
} )

utils.diff.bday <- function(d1, d2){
	utils.checkCond((length(d1) == length(d2)) | (length(d1) == 1) | (length(d2) == 1), paste(deparse(sys.call()), "two input arguments have different length"))
	if((length(d1) == 1) & (length(d2) != 1)){
		return(utils.diff.bday(rep(d1, length(d2)), d2))
	} else if((length(d2) == 1) & (length(d1) != 1)){
		return(utils.diff.bday(d1, rep(d2, length(d1))))
	}
	if(length(d1) > 1){
		return(unlist(lapply(1:length(d1), function(i){ utils.diff.bday(d1[i], d2[i]) })))
	}
	if(d1 > d2) return(-utils.diff.bday(d2, d1))
	return(length(all.trading.dates[(all.trading.dates >= d1) & (all.trading.dates < d2)]))
}

utils.add.bday <- function(d, days){
	if(days == 0) return(d)
	if(length(d) > 1){
		return(unlist(lapply(d, function(d0){ utils.add.bday(d0, days) })))
	}
	curidx = which(all.trading.dates == d)
	if(length(curidx) == 0) curidx = min(which(all.trading.dates >= d))
	tgtidx = curidx + days
	if(tgtidx < 1) return(NA)
	return(all.trading.dates[tgtidx])
}

utils.read <- function(filepath, startDate = NA, endDate = NA){
	path = ifelse(grep(filepath, pattern = '/'), dirname(filepath), './')
	filename = basename(filepath)
	all.files = list.files(path = path, pattern = filename, include.dirs = FALSE)
	if(!is.na(startDate) && !is.na(endDate)){
		
	}
}

sumNA <- function(...) sum(..., na.rm = TRUE)
meanNA <- function(...) mean(..., na.rm = TRUE)
sdNA <- function(...) sd(..., na.rm = TRUE)

# Logger setting
flog.layout(layout.format('[~l] [~t] [~n.~f] ~m'))

# Source data preparation
if(!exists('company.data')){
	company.data = data.table(read.csv("source.data/company_data.csv", stringsAsFactors = FALSE))[,.(Ticker, Sector=Division.SIC, Industry=Major.SIC)]
}
if(!exists('financial.data')){
	financial.data = data.table(read.csv("source.data/financial_data.csv", stringsAsFactors = FALSE))
}
if(!exists('indice.data')){
	indice.data = data.table(read.csv("source.data/indice_data.csv", stringsAsFactors = FALSE))
}
if(!exists('market.data')){
	market.data = data.table(read.csv("source.data/market_data.csv", stringsAsFactors = FALSE))
}

all.trading.dates = utils.gen.bdays()
