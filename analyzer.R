library(data.table)

analyze.ic <- function(sigs, startDate = sigs[,min(Date)], endDate = sigs[,max(Date)]){
	rets = utils.read("signal.auxiliary/forward.return/forward.return.", startDate, endDate)
	vols = utils.read("risk.model.auxiliary/capm.residual.vol/capm.residual.vol", startDate, endDate)
	vols[, vol := utils.winsorize(utils.winsorize(capm.residual.vol, 0.03, 0.97), 0.15, 0.45, use.pct = F), by = Date]
	names.sig = colnames(sigs)[!colnames(sigs) %in% c('Date','Ticker')]
	names.ret = colnames(rets)[!colnames(rets) %in% c('Date','Ticker')]
	names.sret = names.ret[grepl('sret',names.ret)]
	names.tret = names.ret[grepl('tret',names.ret)]
	all.dates = utils.get.bday.range(startDate, endDate)
	sig.ret.dt = Reduce(utils.merge, list(rets, vols, sigs))
	cmd.cor = paste('ic.dt = sig.ret.dt[,.(', paste(unlist(lapply(names.sig, function(s){
		names.sret.stat = paste('IC.', s, '.', substr(names.sret, nchar(names.sret)-2, nchar(names.sret)), sep='')
		names.tret.stat = paste('cor.', s, '.', names.tret, sep='')
		paste( paste(names.sret.stat, ' = corNA(', s, '/vol, ', names.sret, '/vol)', sep='', collapse = ', '),
			   paste(names.tret.stat, ' = corNA(', s, '/vol, ', names.tret, '/vol)', sep='', collapse = ', '), sep=', ')
	})), collapse = ',  '), '), by=Date]', sep='')
	eval(parse(text = print(cmd.cor)))
	ic.dt
}

