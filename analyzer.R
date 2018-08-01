library(data.table)

analyze.ic <- function(sigs, startDate = sigs[,min(Date)], endDate = sigs[,max(Date)]){
	all.dates = utils.get.bday.range(startDate, endDate)
	# Get sigs & clean up Infs
	sigs1 = sigs[Date %in% all.dates]
	names.sig = colnames(sigs1)[!colnames(sigs1) %in% c('Date','Ticker')]
	eval(parse(text = print(paste('sigs1', paste('[is.infinite(', names.sig, '), ', names.sig, ' := ', NA, ']', sep='', collapse=''), sep=''))))
	# get rets
	rets = utils.read("signal.auxiliary/forward.return/forward.return.", startDate, endDate)
	names.ret = colnames(rets)[!colnames(rets) %in% c('Date','Ticker')]
	names.sret = names.ret[grepl('sret',names.ret)]
	names.tret = names.ret[grepl('tret',names.ret)]
	# get vols
	vols = utils.read("risk.model.auxiliary/capm.residual.vol/capm.residual.vol", startDate, endDate)
	vols[, vol := utils.winsorize(utils.winsorize(capm.residual.vol, 0.03, 0.97), 0.15, 0.45, use.pct = F), by = Date]
	sig.ret.dt = Reduce(utils.merge, list(rets, vols, sigs1))
	ic.dt = rbindlist(lapply(names.sig, function(s){
		rbindlist(lapply(names.sret, function(r){
			sig.ret.dt[, .(sig = s, item='IC', fwddays=as.numeric(substr(r, nchar(r)-2, nchar(r)-1)), value = corNA(get(s), get(r))), by=Date]
		}))
	}))
	ic.mean.dt = ic.dt[,.(value = meanNA(value)), by=.(sig, item, fwddays)]
}

