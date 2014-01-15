library(RMySQL)
library(lattice)
con <- dbConnect(MySQL(), dbname="aspectj", user='root', password='')

basedir <- "/Users/mauricioaniche/textos/msr2014/"
projects <- c("metricminer", "gnarus", "tubaina")

project <- projects[1]

sql <- paste("select x.pacote, x.classe, cc, testes, if(testes/cc > 1, 1, testes/cc) as heuristica, emma from (select distinct mc.pacote, mc.classe,(select sum(mc1.cc) from ", project, "_cc mc1 where mc1.classe = mc.classe and mc1.pacote = mc.pacote) as cc,(select sum(m.testes) from ", project, "_aj m where m.classe = mc.classe and m.pacote = mc.pacote) as testes,(select complexity_covered/(complexity_missed + complexity_covered) as emma from ", project, "_emma e where e.class = mc.classe and e.package = mc.pacote) as emma from ", project, "_cc mc) x", sep="")

coverage <- dbGetQuery(con, sql)

correlation = cor.test(coverage$emma, coverage$heuristica, method = "spearman", paired=true)

diferenca <- coverage$heuristica - coverage$emma
summary(diferenca)

png(filename=paste(basedir, project, "-histograma.png", sep=""))
hist(diferenca)
axis(side=1, at=seq(-1,1,0.2))
dev.off()

png(filename=paste(basedir, project, "-histograma-pct.png", sep=""))
histogram(a)
dev.off()
