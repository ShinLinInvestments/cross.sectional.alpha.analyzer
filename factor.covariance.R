library(data.table)

gen.capm.residual.vol.of.date <- function(market.data, indice.data, ymd, trim.pct = 1/21, lookback = 1260){
	rets = market.data[Date <= ymd, .(Date, Ticker, tret = Total.Return)][utils.add.bday(ymd, -lookback) <= Date][!is.na(tret)]
	spys = indice.data[Index == "SPY"][Date <= ymd][utils.add.bday(ymd, -lookback) <= Date][!is.na(Return), .(Date, iret=Return)]
	secs = rets[, unique(Ticker)]
	utils.checkCond(nrow(rets) > 0, paste("no market data found for", ymd, "with lookback =", lookback))
	res = rbindlist( lapply( secs, function(s){
		rets.sec = merge(rets[Ticker == s], spys, by=c('Date'))[(!is.na(iret)) & (!is.na(tret))]
		if(nrow(rets.sec) <= 5) return(NULL)
		rets.sec[, `:=`(iret.wnsr = utils.winsorize(iret, trim.pct, 1 - trim.pct), tret.wnsr = utils.winsorize(tret, trim.pct/2, 1 - trim.pct/2))]
		ret.lm = rets.sec[, lm(tret.wnsr ~ iret.wnsr)]
		data.table(Ticker = s, beta = ret.lm$coefficients[2], capm.residual.vol = sdNA(ret.lm$residuals)*sqrt(252))
	} ), use.names = T )
	if(nrow(res) > 1){
		res = data.table(Date = ymd, res)
		flog.info(paste("gen capm residual vol for", ymd, nrow(res), "secs vol", res[, paste(min(capm.residual.vol), "~", max(capm.residual.vol))]))
		write.csv(res, paste("risk.model.auxiliary/capm.residual.vol/capm.residual.vol.", ymd, ".csv", sep=''), quote = FALSE, row.names = FALSE)
	}
}

gen.capm.residual.vol <- function(market.data, indice.data, startDate, endDate, lookback = 1260, trim.pct = 1/21){
	market.data1 = market.data[Date <= endDate, .(Date, Ticker, Total.Return)][utils.add.bday(startDate, -lookback) <= Date][!is.na(Total.Return)]
	indice.data1 = indice.data[Index == "SPY"][Date <= endDate][utils.add.bday(startDate, -lookback) <= Date][!is.na(Return)]
	all.dates = all.trading.dates[which((all.trading.dates >= startDate) & (all.trading.dates <= endDate))]
	flog.info(paste("generating capm residual vol for", length(all.dates), "dates from", min(all.dates), "to", max(all.dates)))
	lapply(all.dates, gen.capm.residual.vol.of.date, market.data=market.data1, indice.data=indice.data1, trim.pct=trim.pct, lookback=lookback)
}

gen.capm.residual.vol(market.data, indice.data, startDate = "2004-01-01", endDate = "2005-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2005-01-01", endDate = "2006-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2006-01-01", endDate = "2007-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2007-01-01", endDate = "2008-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2008-01-01", endDate = "2009-01-01", trim.pct = 1/21)

gen.capm.residual.vol(market.data, indice.data, startDate = "2009-01-01", endDate = "2010-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2010-01-01", endDate = "2011-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2011-01-01", endDate = "2012-01-01", trim.pct = 1/21)

gen.capm.residual.vol(market.data, indice.data, startDate = "2012-01-01", endDate = "2013-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2013-01-01", endDate = "2014-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2014-01-01", endDate = "2015-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2015-01-01", endDate = "2016-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2016-01-01", endDate = "2017-01-01", trim.pct = 1/21)
gen.capm.residual.vol(market.data, indice.data, startDate = "2017-01-01", endDate = "2018-01-01", trim.pct = 1/21)

gen.factor.expos.on.date.filled.with.industry.median <- function(ymd){
	style.loading.names = c('SIZE','VALUE','MKTBETA')
	prevdate = utils.add.bday(ymd, -1)
	style.expo.dt = Reduce(utils.merge.all, lapply(style.loading.names, function(ln){
		expo.cur = utils.read(paste('risk.loading/', ln, '/', ln, '.', sep=''), startDate = prevdate, endDate = prevdate)
		expo.cur[, colnames(expo.cur)[grepl("[[:upper:]]",colnames(expo.cur))], with = FALSE]
	}) )
	industry.expos = utils.read(paste('risk.loading/INDUSTRY/INDUSTRY.', sep=''), startDate = prevdate, endDate = prevdate)
	
}
