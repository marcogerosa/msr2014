library(RMySQL)
con <- dbConnect(MySQL(), dbname="msr2014", user='root', password='')

basedir <- "/Users/mauricioaniche/textos/msr2014/stats/"
projects <- c("metricminer", "gnarus", "tubaina")

# from http://stackoverflow.com/questions/9317948/how-to-label-histogram-bars-with-data-values-or-percents-in-r
histPercent <- function(x, ...) {
   H <- hist(x, plot = FALSE)
   H$density <- with(H, 100 * density* diff(breaks)[1])
   labs <- paste(round(H$density), "%", sep="")
   plot(H, freq = FALSE, labels = labs, ylim=c(0, 1.08*max(H$density)),...)
}


calculateAll = function(coverage, project, source, n_breaks) {

	correlation = cor.test(coverage$emma, coverage$heuristica, method = "spearman", paired=true)
	cat("p value ", correlation$p.value, " estimate ", correlation$estimate, file=paste(basedir, project, "-correlation-", source, ".txt", sep=""))

	diferenca <- coverage$heuristica - coverage$emma
	#diferenca_arredondada <- round(diferenca, 1)
	
	resumo <- summary(diferenca)
	cat(resumo, file=paste(basedir, project, "-summary-", source, ".txt", sep=""))

	png(filename=paste(basedir, project, "-histograma-", source, ".png", sep=""))
	histPercent(diferenca, col="gray", xlim=c(-1, 1), main=NULL, breaks=n_breaks, xlab=NULL, ylab=NULL)
	dev.off()

}

gelato = function(project) {	
	sql <- paste("select cobertura as heuristica, complexity_covered/(complexity_missed + complexity_covered) as emma from ", project, "_gelato gg join ", project, "_emma ge on gg.classe = ge.class and gg.pacote = ge.package", sep="")
	
	coverage <- dbGetQuery(con, sql)

	calculateAll(coverage, project, "gelato", 21)
	
}

aspectj = function(project) {

	sql <- paste("select x.pacote, x.classe, cc, testes, if(testes/cc > 1, 1, testes/cc) as heuristica, emma from (select distinct mc.pacote, mc.classe,(select sum(mc1.cc) from ", project, "_cc mc1 where mc1.classe = mc.classe and mc1.pacote = mc.pacote) as cc,(select sum(m.testes) from ", project, "_aj m where m.classe = mc.classe and m.pacote = mc.pacote) as testes,(select complexity_covered/(complexity_missed + complexity_covered) as emma from ", project, "_emma e where e.class = mc.classe and e.package = mc.pacote) as emma from ", project, "_cc mc) x", sep="")

	coverage <- dbGetQuery(con, sql)

	calculateAll(coverage, project, "aspectj", 21)

}

for(i in 1:length(projects)) {

	project <- projects[i]

	aspectj(project)
	gelato(project)
	
}
