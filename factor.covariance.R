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
	all.dates = utils.get.bday.range(startDate, endDate)
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
	univ = utils.read(paste("universe/universe.", ymd, sep=''), startDate = ymd, endDate = ymd)
	prevdate = utils.add.bday(ymd, -1)
	style.expo.dt = Reduce(utils.merge.all, lapply(style.loading.names, function(ln){
		expo.cur = utils.read(paste('risk.loading/', ln, '/', ln, '.', sep=''), startDate = prevdate, endDate = prevdate)
		expo.cur[, colnames(expo.cur)[grepl("[[:upper:]]",colnames(expo.cur))], with = FALSE]
	}) )
	style.expo.dt = merge(univ[,-c('Date'),with=T], style.expo.dt[,-c('Date'),with=T], by='Ticker', all.x=T)
	industry.expos = utils.read(paste('risk.loading/INDUSTRY/INDUSTRY.', sep=''), startDate = prevdate, endDate = prevdate)
	#Generate industry median for missing loadings
	all.indus = colnames(industry.expos)[grepl('^[A-Z.]+$', colnames(industry.expos))]
	style.expo.dt1 = Reduce(utils.merge.all, lapply(style.loading.names, function(ln){
		styles.cur = style.expo.dt[, .(Ticker, Loading = get(ln))]
		global.median = styles.cur[, medianNA(Loading)]
		styles.res = rbindlist(lapply(all.indus, function(ind){
			cur.indus = industry.expos[get(ind) != 0, .(Ticker, IndusWgt = get(ind))]
			cur = utils.merge(styles.cur[Ticker %in% cur.indus[, Ticker]], cur.indus)
			industry.median = cur[, medianNA(Loading)]
			if(is.na(industry.median)){
				flog.warn(paste("No valid", ln, "loadings on", ymd, "for", ind, "use global median", global.median))
				industry.median = global.median
			}
			flog.debug(paste("Industry median", ln, "loading =", round(industry.median, 2), "on", ymd, "for", ind))
			cur[is.na(Loading), Loading := industry.median]
		}), use.names = TRUE)
		styles.res = styles.res[, .(LoadingSum = sum(Loading * IndusWgt) / sum(IndusWgt)), by = Ticker]
		setnames(styles.res, 'LoadingSum', ln)
	}))
	#Combine with industry model
	res = utils.merge.all(style.expo.dt1, industry.expos[Ticker %in% univ[,unique(Ticker)], -c('Date'), with=F])
	write.csv(res, paste("risk.model/factor.expo/factor.expo.", ymd, ".csv", sep=''), quote = F, row.names = F)
}

lapply(all.trading.dates[which((all.trading.dates >= '2014-01-03') & (all.trading.dates <= '2014-02-02'))], gen.factor.expos.on.date.filled.with.industry.median)

gen.factor.spec.return.on.date <- function(ymd, market.data){
	style.loading.names = c('SIZE','VALUE','MKTBETA')
	univ = utils.read(paste("universe/universe.", ymd, sep=''), startDate = ymd, endDate = ymd)[,sort(Ticker)]
	# expo matrix
	expos = utils.read(paste("risk.model/factor.expo/factor.expo.", sep=''), startDate = ymd, endDate = ymd)
	factor.names = c(sort(colnames(expos)[colnames(expos) %in% style.loading.names]), sort(colnames(expos)[!colnames(expos) %in% c('Ticker',style.loading.names)]))
	expos = expos[, c('Ticker', factor.names), with=F][order(Ticker)]
	expos.mat = as.matrix(expos[,-1])
	rownames(expos.mat) = expos[,Ticker]
	# GLS weights
	gls.wgts = utils.read(paste("risk.model.auxiliary/capm.residual.vol/capm.residual.vol.", sep=''), startDate = ymd, endDate = ymd)
	gls.wgts = utils.merge(gls.wgts, data.table(Ticker=univ), all.y=T)[order(Ticker)]
	gls.wgts = gls.wgts[is.na(capm.residual.vol), capm.residual.vol := gls.wgts[,medianNA(capm.residual.vol)]]
	gls.wgts.mat = diag(gls.wgts[, capm.residual.vol^(-2)])
	rownames(gls.wgts.mat) = gls.wgts[, Ticker]; colnames(gls.wgts.mat) = gls.wgts[, Ticker]
	# rets
	rets = market.data[Date == ymd,.(Ticker, tret = Total.Return)]
	rets = utils.merge(rets, data.table(Ticker=univ), all.y=T)[is.na(tret), tret := 0][order(Ticker)]
	rets.mat = as.matrix(rets[, -'Ticker', with=F])
	rownames(rets.mat) = rets[, Ticker]
	flog.info(paste("tret on", ymd, "mean =", round(rets[,meanNA(tret)*100], 6), "% dispersion =", round(rets[,sdNA(tret)*100], 6), "%"))
	# f = (X'WX)^(-1)X'W * r
	utils.checkCond(ncol(expos.mat) == length(factor.names) & all(colnames(expos.mat) == factor.names), "expos matrix rownames are not factor names")
	utils.checkCond(nrow(expos.mat) == length(univ) & all(rownames(expos.mat) == univ), "expos matrix colnames are not ticker univ")
	utils.checkCond(nrow(gls.wgts.mat) == length(univ) & all(rownames(gls.wgts.mat) == univ), "gls weights matrix rownames are not ticker univ")
	utils.checkCond(ncol(gls.wgts.mat) == length(univ) & all(colnames(gls.wgts.mat) == univ), "gls weights matrix colnames are not ticker univ")
	utils.checkCond(nrow(rets.mat) == length(univ) & all(rownames(rets.mat) == univ), "tret vector rownames are not ticker univ")
	denominator = t(expos.mat) %*% gls.wgts.mat %*% expos.mat
	det.denominator = det(denominator)
	utils.checkCond(!is.na(det.denominator) & det.denominator != 0, "denominator is not invertible")
	fret.mat = solve(denominator) %*% t(expos.mat) %*% gls.wgts.mat %*% rets.mat
	fret.dt = data.table(Factor = rownames(fret.mat), fret = fret.mat[,1])
	write.csv(fret.dt[order(Factor)], paste("risk.model/factor.return/factor.return.", ymd, ".csv"), quote = F, row.names = F)
	flog.info(paste("fret on", ymd, "mean =", round(fret.dt[,meanNA(fret)*100], 6), "% dispersion =", round(fret.dt[,sdNA(fret)*100], 6), "%"))
	# Spec return
	sret.mat = rets.mat - expos.mat %*% fret.mat
	sret.dt = utils.merge.all(rets, data.table(Ticker = rownames(sret.mat), sret = sret.mat[,1]))
	write.csv(sret.dt[order(Ticker)], paste("risk.model/spec.return/spec.return.", ymd, ".csv"), quote = F, row.names = F)
	flog.info(paste("sret on", ymd, "mean =", round(sret.dt[,meanNA(sret)*100], 6), "% dispersion =", round(sret.dt[,sdNA(sret)*100], 6), "%"))
}

startDate = '2006-01-01'; endDate = '2008-01-01'
lapply(all.trading.dates[which((all.trading.dates >= startDate) & (all.trading.dates <= endDate))], gen.factor.spec.return.on.date, market.data = market.data[Date<=endDate][Date>=startDate])

gen.factor.covariance.matrix.on.date <- function(startDate, endDate, lookback = 504, halflife = 90, shrink.wgt = 0.8){
	frets = rbindlist(lapply(utils.get.bday.range(utils.add.bday(startDate, -lookback), endDate), function(d){
		data.table(Date = d, utils.read("risk.model/factor.return/factor.return.", d, d))
	}))
	lapply(utils.get.bday.range(startDate, endDate), function(d){
		dates.lb = utils.get.bday.range(utils.add.bday(d, -lookback), d)
		frets.cur = utils.merge(frets[(Date >= min(dates.lb)) & (Date <= max(dates.lb)),], data.table(Date=dates.lb, date.diff=utils.diff.bday(dates.lb, d)), all.y=T)
		frets.cur[, ewma := 0.5 ^ (date.diff / halflife)][, fret.ewma := fret * ewma]
		cov.raw = cov(dcast(frets.cur, Date ~ Factor, value.var = 'fret.ewma')[,-'Date',with=F])
		# Shrink cov matrix toward diagonal
		cov.prior = diag(diag(cov.raw))
		cov.res = cov.raw * shrink.wgt + cov.prior * (1 - shrink.wgt)
		det.cov.res = det(cov.res)
		flog.info(paste("Cov matrix of", d, "with det =", det.cov.res))
		utils.checkCond(!is.na(det.cov.res) & det.cov.res != 0, paste("Cov matrix of", d, "is singular!"))
		# Output
		write.csv(cov.res, paste("risk.model/factor.covariance/factor.covariance.", d, ".csv", sep=''), quote = F)
	})
}

gen.factor.covariance.matrix.on.date('2014-01-01','2018-01-01')
gen.factor.covariance.matrix.on.date('2010-01-01','2014-01-01')
gen.factor.covariance.matrix.on.date('2014-01-01','2018-01-01')
