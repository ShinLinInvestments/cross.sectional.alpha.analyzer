library(data.table)

gen.forward.return <- function(startDate = "2004-02-02", endDate = "2017-09-29"){
	rets = utils.read(paste("risk.model/spec.return/spec.return", sep=''), startDate, utils.add.bday(endDate, 63))
	fwd.setting.list = list('1d' = 1, '2d' = 1:2, '3d' = 1:3, '4d' = 1:4, '1w' = 1:5, '2w' = 1:10, '3w' = 1:15, '1m' = 1:21, '2m' = 1:42, '1q' = 1:63)
	lapply(utils.get.bday.range(startDate, endDate), function(d){
		dates.lb = sort(unlist(lapply(1:63, function(lb) as.character(utils.add.bday(d, lb)))))
		rets.cur = rets[Date %in% dates.lb]
		res = Reduce(utils.merge.all, lapply(1:length(fwd.setting.list), function(idx){
			rets.cur1 = rets.cur[Date %in% dates.lb[fwd.setting.list[[idx]]], .(tret1 = prod(tret+1)-1, sret1 = prod(sret+1)-1), by=Ticker]
			setnames(rets.cur1, c('tret1','sret1'), paste(c('tret.','sret.'), names(fwd.setting.list)[idx], sep=''))
			rets.cur1
		}))
		res = data.table(Date = d, res)
		flog.info(paste("gen fwd ret for", d, "with", nrow(res), "tickers"))
		write.csv(res, paste("signal.auxiliary/forward.return/forward.return.", d, ".csv", sep=''), quote = F, row.names = F)
	})
}

gen.forward.return(startDate = '2014-01-01')

gen.signal.fundamentals <- function(financial.data, startDate, endDate, lookback = 504){
	financial.data[, qtr := quarter(Fiscal.Period.End.Date)]
	financial.data[, Gross.Profit := Quarterly.Sales - Cost.of.Goods.Sold][, EBITDA := Gross.Profit - Selling.General.and.Administrative]
	ticker.date.colnames = c('Ticker','qtr','Fiscal.Period.End.Date')
	metric.names = colnames(financial.data)[!colnames(financial.data) %in% ticker.date.colnames]
	# Padding metrics from last qtr and the same qtr last year
	fin0 = utils.diff.metric(financial.data, metric.names, 'Ticker', 'Fiscal.Period.End.Date')
	all.dates = utils.get.bday.range(startDate, endDate)
	lapply(all.dates, function(d){
		fin1 = fin0[Fiscal.Period.End.Date <= d][Fiscal.Period.End.Date >= utils.add.bday(d, -lookback)]
		lookback.dates = fin1[, sort(unique(Fiscal.Period.End.Date))]
		fin1 = utils.merge(fin1, data.table(Fiscal.Period.End.Date = lookback.dates, date.diff = utils.diff.bday(lookback.dates, d)))
		fin1[, ewma := 0.5 ^ (date.diff / halflife)]
		secs = fin1[, unique(Ticker)]
		res = rbindlist(lapply(secs, function(s){
			fin1 = Reduce(utils.merge.all, lapply(metric.names, utils.diff.metric, metric.dt = fin0[Ticker == s], col.date = 'Fiscal.Period.End.Date'))
			data.table(Ticker = s, fin1)
		}))
	})
}

