library(data.table)

gen.forward.return <- function(startDate = "2004-02-02", endDate = "2017-09-29"){
	rets = utils.read(paste("risk.model/spec.return/spec.return", sep=''), startDate, utils.add.bday(endDate, 63))
	fwd.setting.list = list('01d' = 1, '02d' = 1:2, '03d' = 1:3, '04d' = 1:4, '05d' = 1:5, '10d' = 1:10, '15d' = 1:15, '21d' = 1:21, '42d' = 1:42, '63d' = 1:63)
	lapply(utils.get.bday.range(startDate, endDate), function(d){
		dates.lb = sort(unlist(lapply(1:63, function(lb) as.character(utils.add.bday(d, lb)))))
		rets.cur = rets[Date %in% dates.lb]
		res = Reduce(utils.merge.all, lapply(1:length(fwd.setting.list), function(idx){
			rets.cur1 = rets.cur[Date %in% dates.lb[fwd.setting.list[[idx]]], .(tret1 = prod(tret+1)-1, sret1 = prod(sret+1)-1), by=Ticker]
			setnames(rets.cur1, c('tret1','sret1'), paste(c('tret.','sret.'), names(fwd.setting.list)[idx], sep=''))
			rets.cur1
		}))
		names.ret = sort(colnames(res)[!colnames(res) %in% c('Date','Ticker')])
		res = data.table(Date = d, res[, c('Ticker', names.ret), with=F])
		flog.info(paste("gen fwd ret for", d, "with", nrow(res), "tickers"))
		write.csv(res, paste("signal.auxiliary/forward.return/forward.return.", d, ".csv", sep=''), quote = F, row.names = F)
	})
}

gen.forward.return(startDate = '2010-01-01', endDate = '2014-01-01')

expand.financial.data <- function(financial.data){
	financial.data[, qtr := quarter(Fiscal.Period.End.Date)]
	financial.data[, Gross.Profit := Quarterly.Sales - Cost.of.Goods.Sold][, EBITDA := Gross.Profit - Selling.General.and.Administrative]
	ticker.date.colnames = c('Ticker','qtr','Fiscal.Period.End.Date')
	metric.names = colnames(financial.data)[!colnames(financial.data) %in% ticker.date.colnames]
	# Padding metrics from last qtr and the same qtr last year
	utils.diff.metric(financial.data, metric.names, 'Ticker', 'Fiscal.Period.End.Date')
}

gen.signal.fundamentals <- function(financial.data, startDate, endDate, lookback = 504, halflife = 21){
	all.dates = utils.get.bday.range(startDate, endDate)
	rbindlist(lapply(all.dates, function(d){
		fin1 = fin0[Fiscal.Period.End.Date < d][Fiscal.Period.End.Date >= utils.add.bday(d, -lookback)]
		lookback.dates = fin1[, sort(unique(Fiscal.Period.End.Date))]
		fin1 = utils.merge(fin1, data.table(Fiscal.Period.End.Date = lookback.dates, date.diff = utils.diff.bday(lookback.dates, d)))
		fin1[, ewma := 0.5 ^ (date.diff / halflife)]
		flog.info(paste("gen signal for", d))
		fin1.cur = fin1[order(-Fiscal.Period.End.Date)][,.SD[1, .(ocf2a=Operating.Cash.Flow/Assets, accru2a=(Net.Income-Operating.Cash.Flow)/Assets)], by=Ticker]
		fin1.smo = fin1[, .(ebitda.d1y.2a = sumNA(ewma * (EBITDA-EBITDA.1y)/Assets)), by=Ticker]
		data.table(Date = d, utils.merge.all(fin1.cur, fin1.smo))
	}))
}

financial.data = expand.financial.data(financial.data)
sigs = gen.signal.fundamentals(financial.data, '2014-01-01', '2015-01-01')


