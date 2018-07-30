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

gen.value.loading <- function(financial.data, halflife = 63, lookback = 504, trim.pct = 0.02, startDate = "2004-02-01", endDate = "2018-01-01"){
	book.vals = financial.data[!is.na(Common.Equity), .(Ticker, Date=Fiscal.Period.End.Date, Common.Equity)]
	all.dates = all.trading.dates[which((all.trading.dates >= startDate) & (all.trading.dates <= endDate))]
	rbindlist( lapply( as.character(all.dates), function(d){
		book.vals.cur = book.vals[Date < d][utils.add.bday(d, -lookback) <= Date]
		cur.lookback.dates = book.vals.cur[, sort(unique(Date))]
		bday.diff.map = data.table(Date=cur.lookback.dates, date.diff=utils.diff.bday(cur.lookback.dates, d))
		book.vals.cur = merge(book.vals.cur, bday.diff.map, by=c('Date'), all=T)[, ewma := 0.5 ^ (date.diff / halflife)]
		res = book.vals.cur[,.(Date = d, book.value.ewma = sumNA( ewma * Common.Equity ) / sum(ewma)), by=Ticker]
		market.cap = utils.read("risk.loading/SIZE/SIZE.", startDate = d, endDate = d)[,.(Date, Ticker, mktcap.ewma)]
		res = merge(res, market.cap, by=c('Date','Ticker'))[, b.to.p := book.value.ewma / mktcap.ewma]
		res[, b.to.p.wnsr := utils.winsorize(b.to.p, trim.pct/2, 1-trim.pct/2, use.pct=TRUE)]
		res[, VALUE := (b.to.p.wnsr - meanNA(b.to.p.wnsr)) / sdNA(b.to.p.wnsr)]
		flog.info(paste("value loading generating for", d, ":", length(cur.lookback.dates), "earns, VALUE =", res[,paste(min(VALUE), "~", max(VALUE))]))
		write.csv(res, paste("risk.loading/VALUE/VALUE.", d, '.csv', sep=''), quote = F, row.names = F)
		NULL
	} ) )
}

gen.value.loading(financial.data)

gen.revenue.wgts.unclassified.tickers <- function(){
	ge.revenues = data.table(year = 2017:2004, Ticker = "GE",
							 TRANSPORTATION.EQUIPMENT=c(0.25,0.25,0.26,0.19,0.19,0.17,0.19,0.2,0.18,0.16,0.15,0.15,0.15,0.15),
							 INDUSTRIAL.AND.COMMERCIAL.MACHINERY.AND.COMPUTER.EQUIPMENT=c(0.37,0.37,0.3,0.22,0.22,0.24,0.21,0.22,0.24,0.2,0.16,0.16,0.16,0.16),
							 PETROLEUM.REFINING.AND.RELATED.INDUSTRIES=c(0.14,0.1,0.14,0.13,0.12,0.11,0.1,0.07,0.06,0.05,0.05,0.04,0.03,0.03),
							 MEASURING.ANALYZING.CONTROL.INSTR.PHOTO.MED.OPTIC.GDS.WATCHES.CLOCKS=c(0.17,0.19,0.22,0.18,0.18,0.18,0.17,0.16,0.17,0.15,0.16,0.17,0.18,0.18),
							 DEPOSITORY.INSTITUTIONS=c(0.07,0.09,0.09,0.28,0.3,0.31,0.33,0.36,0.36,0.44,0.48,0.48,0.48,0.48)
	)
	hon.revenues = data.table(year = 2017:2004, Ticker = "HON",
							  TRANSPORTATION.EQUIPMENT=c(0.36,0.38,0.39,0.39,0.4,0.41,0.42,0.43,0.44,0.47,0.5,0.5,0.54,0.55),
							  MEASURING.ANALYZING.CONTROL.INSTR.PHOTO.MED.OPTIC.GDS.WATCHES.CLOCKS=c(0.38,0.36,0.36,0.36,0.34,0.34,0.43,0.43,0.42,0.38,0.36,0.35,0.34,0.31),
							  CHEMICALS.AND.ALLIED.PRODUCTS=c(0.26,0.27,0.25,0.25,0.25,0.25,0.15,0.15,0.14,0.14,0.14,0.15,0.12,0.14)
	)
	seb.revenues = data.table(year = 2017:2004, Ticker = "SEB",
							  AGRICULTURAL.PRODUCTION.CROPS=c(0.82,0.81,0.81,0.84,0.82,0.8,0.82,0.78,0.76,0.74,0.71,0.69,0.73,0.79),
							  WATER.TRANSPORTATION=c(0.91,0.92,0.91,0.82,0.76,0.79,0.89,0.87,0.87,0.87,0.88,0.88,0.89,0.9),
							  ELECTRIC.GAS.AND.SANITARY.SERVICES=c(0.06,0.05,0.07,0.1,0.14,0.13,0.06,0.08,0.09,0.11,0.1,0.09,0.07,0.06)
	)
	brk.revenues = data.table(year = 2017:2004, Ticker = "BRK/B",
							  INSURANCE.CARRIERS=c(0.25,0.21,0.2,0.21,0.2,0.21,0.22,0.23,0.25,0.24,0.27,0.24,0.27,0.28),
							  BUSINESS.SERVICE=c(0.52,0.53,0.51,0.5,0.51,0.51,0.51,0.49,0.56,0.61,0.49,0.53,0.56,0.58),
							  RAILROAD.TRANSPORTATION=c(0.16,0.17,0.19,0.21,0.19,0.2,0.21,0.19,0.1,0.13,0.11,0.11,0,0),
							  HOLDING.AND.OTHER.INVESTMENT.OFFICES=c(0.06,0.09,0.11,0.08,0.1,0.07,0.06,0.09,0.09,0.02,0.13,0.12,0.17,0.14)
	)
	res = rbindlist(list(ge.revenues, hon.revenues, seb.revenues, brk.revenues), use.names = T, fill = T)
	res[is.na(res)] = 0
	res
}

gen.industry.loading <- function(company.data, startDate = "2004-01-01", endDate = "2018-01-01"){
	classified.companies = company.data[Sector != "NONCLASSIFIABLE",.(Ticker, Industry, Wgt=1)]
	industry.model = dcast(classified.companies, Ticker~Industry, fun.aggregate = sum, value.var = "Wgt")
	nonclassified.wgts = gen.revenue.wgts.unclassified.tickers()
	lapply( all.trading.dates[which((all.trading.dates >= startDate) & (all.trading.dates <= endDate))], function(d){
		nonclassified.cur = nonclassified.wgts[year == substr(d, 1, 4)]
		curres = rbindlist(list(industry.model, nonclassified.cur), use.names = T, fill = T)
		curres[is.na(curres)] = 0
		curres = data.table(Date = d, curres[, -c('year'), with=T])
		flog.info(paste("industry loading generating for", d, "with yr=", nonclassified.cur[, year[1]], " sd(Trans.Eq) =", nonclassified.cur[, sd(TRANSPORTATION.EQUIPMENT)]))
		write.csv(curres, paste("risk.loading/INDUSTRY/INDUSTRY.", d, '.csv', sep=''), quote = F, row.names = F)
	})
}

gen.industry.loading(company.data)
