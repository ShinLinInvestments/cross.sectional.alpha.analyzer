library(data.table)

gen.universe <- function(market.data, company.data, startDate = "2004-01-01", endDate = "2018-01-01"){
	all.dates = all.trading.dates[which((all.trading.dates >= startDate) & (all.trading.dates <= endDate))]
	industry.secs = company.data[, unique(Ticker)]
	lapply(as.character(all.dates), function(d){
		mkt.secs = market.data[Date == d, unique(Ticker)]
		secs.missing.inudstry = setdiff(mkt.secs, industry.secs)
		flog.info(paste("gen universe on", d, "with", length(mkt.secs), "in mktData and", length(industry.secs), "from industry classification"))
		if(length(secs.missing.inudstry) > 0) flog.warn(paste("Missing industry classification:", paste(secs.missing.inudstry), "in universe"))
		res = data.table(Date = d, Ticker = sort(mkt.secs))
		write.csv(res, paste("universe/universe.", d, ".csv", sep=''), quote = FALSE, row.names = FALSE)
	})
}

gen.universe(market.data, company.data)
