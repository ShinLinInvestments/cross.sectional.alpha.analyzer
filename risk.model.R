library(data.table)

gen.size.loading <- function(market.data, halflife = 42, lookback = 504, startDate = "1999-01-01"){
	market.caps = market.data[,.(Date, Ticker, Market.Cap)]
	all.dates = market.caps[, sort(unique(Date))]
	all.dates = all.dates[which(all.dates >= startDate)]
	rbindlist( lapply( all.dates, function(d){
		market.cap.cur = market.caps[Date <= d][utils.add.bday(d, -lookback) <= Date][!is.na(Market.Cap)]
		cur.lookback.dates = market.cap.cur[, sort(unique(Date))]
		flog.info(paste("size loading generating for", d, "with", length(cur.lookback.dates), "dates"))
		bday.diff.map = data.table(Date=cur.lookback.dates, date.diff=utils.diff.bday(cur.lookback.dates, d))
		market.cap.cur = merge(market.cap.cur, bday.diff.map, by=c('Date'), all=T)
		market.cap.cur[, ewma := 0.5 ^ (date.diff / halflife)]
		res = market.cap.cur[,.(Date = d, mktcap.ewma = sumNA( ewma * Market.Cap ) / sum(ewma)), by=Ticker]
		res[, log.mktcap.ewma := log(mktcap.ewma)][, SIZE := (log.mktcap.ewma - meanNA(log.mktcap.ewma)) / sdNA(log.mktcap.ewma)]
		write.csv(res, paste("risk.loading/SIZE/SIZE.", d, '.csv', sep=''), quote = F, row.names = F)
		res
	} ) )
}

gen.size.loading(market.data[Date < "2018-01-01"][Date >= "2013-12-01"], startDate = "2016-02-08")
#gen.size.loading(market.data[Date < "2016-01-01"][Date >= "2011-12-01"], startDate = "2014-07-10")
#gen.size.loading(market.data[Date < "2014-01-01"][Date >= "2009-12-01"], startDate = "2012-03-01")
#gen.size.loading(market.data[Date < "2012-01-01"][Date >= "2007-12-01"], startDate = "2010-01-01")
#gen.size.loading(market.data[Date < "2010-01-01"][Date >= "2005-12-01"], startDate = "2008-01-01")
#gen.size.loading(market.data[Date < "2008-01-01"][Date >= "2003-12-01"], startDate = "2006-01-01")
#gen.size.loading(market.data[Date < "2006-01-01"][Date >= "2001-12-01"], startDate = "2004-01-01")

gen.mktbeta.loading <- function(market.data, indice.data, halflife = 63, lookback = 504, trim.pct = 1/21, startDate = "1999-01-01"){
	rets = market.data[,.(Date, Ticker, tret = Total.Return)]
	all.dates = sort(intersect(rets[, unique(Date)], indice.data[,unique(Date)]))
	all.dates = all.dates[which(all.dates >= startDate)]
	lapply( all.dates, function(d){
		rets.cur = rets[Date <= d][utils.add.bday(d, -lookback) <= Date][!is.na(tret)]
		idxs.cur = indice.data[Index == "SPY"][Date <= d][utils.add.bday(d, -lookback) <= Date][!is.na(Return)]
		idxs.cur = idxs.cur[, .(iret = mean(Return)), by=Date][order(Date)][, date.diff := ((nrow(idxs.cur)-1):0) / halflife]
		cur.lookback.dates = sort(intersect(rets.cur[, unique(Date)], idxs.cur[, unique(Date)]))
		secs = rets.cur[, sort(unique(Ticker))]
		flog.info(paste("mktbeta loading generating for", d, "with", length(cur.lookback.dates), "dates", length(secs), "securities"))
		res = rbindlist( lapply( secs, function(s){
			rets.sec = merge(rets.cur[Ticker == s], idxs.cur, by=c('Date'))[(!is.na(iret)) & (!is.na(tret))]
			if(nrow(rets.sec) <= 5) return(NULL)
			rets.sec[, `:=`(iret.wnsr = utils.winsorize(iret, trim.pct, 1 - trim.pct), tret.wnsr = utils.winsorize(tret, trim.pct/2, 1 - trim.pct/2))]
			data.table(Ticker = s, beta = rets.sec[, lm(tret.wnsr ~ 0 + iret.wnsr, weights = 0.5 ^ date.diff)]$coefficients[1])
		} ) )
		res[, MKTBETA := (beta - meanNA(beta)) / sdNA(beta)]
		write.csv(res, paste("risk.loading/MKTBETA/MKTBETA.", d, '.csv', sep=''), quote = F, row.names = F)
		NULL
	} )
}

gen.mktbeta.loading(market.data[Date < "2018-01-01"][Date >= "2013-12-01"], indice.data, startDate = "2016-01-01")
#gen.mktbeta.loading(market.data[Date < "2016-01-01"][Date >= "2011-12-01"], indice.data, startDate = "2014-01-01")
#gen.mktbeta.loading(market.data[Date < "2014-01-01"][Date >= "2009-12-01"], indice.data, startDate = "2012-01-01")
#gen.mktbeta.loading(market.data[Date < "2012-01-01"][Date >= "2007-12-01"], indice.data, startDate = "2010-01-01")
#gen.mktbeta.loading(market.data[Date < "2010-01-01"][Date >= "2005-12-01"], indice.data, startDate = "2008-01-01")
#gen.mktbeta.loading(market.data[Date < "2008-01-01"][Date >= "2003-12-01"], indice.data, startDate = "2006-01-01")
#gen.mktbeta.loading(market.data[Date < "2006-01-01"][Date >= "2001-12-01"], indice.data, startDate = "2004-02-01")

gen.industry.loading <- function(company.data){
	classified.companies = company.data[Sector != "NONCLASSIFIABLE",.(Ticker, Industry, Wgt=1)]
	industry.model = dcast(classified.companies, Ticker~Industry, fun.aggregate = sum, value.var = "Wgt")
}


