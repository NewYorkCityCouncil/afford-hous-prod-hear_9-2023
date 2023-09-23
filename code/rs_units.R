source("code/00_load_dependencies.R")

# explore data from nycdb/just fix repo
# https://github.com/NewYorkCityCouncil/afford-hous-prod-hear_9-2023/tree/main/code

rs_dt_17 <- fread("data/yearly_rs_2011-2017.csv")
rs_dt_17[, year := as.Date(year)]
setnames(rs_dt_17, "ucbbl", "bbl")

rs_dt_21 <- fread("data/yearly_rs_2018-2021.csv")
rsdt21_sub <- rs_dt_21[,.(bbl = ucbbl, uc2018, uc2019, uc2020, uc2021)]
longrs_21 <- unique(melt(rsdt21_sub, id.vars = "bbl"))
longrs_21[, year := as.Date(as.character(gsub("uc", "", variable)), format = "%Y")]
abats <- unique(rs_dt_17[!abatements %in% "", .(bbl, abatements)])
bbl = rep(abats$bbl, unlist(lapply(strsplit(abats$abatements, ","), length)))
abats2 <- unlist(strsplit(abats$abatements, ","))
abatsdt <- data.table(bbl, abats2)
rs_17_21 <- rbind(rs_dt_17[, .(bbl, year, unitcount)], longrs_21[, .(unitcount = value, bbl, year)])
mgd_rs <-unique(merge(rs_17_21, abatsdt, allow.cartesian = TRUE))
bbls <- unique(mgd_rs[, sum(is.na(unitcount)), by = .(bbl, year)][order(V1, decreasing = TRUE)][V1>1, .(bbl)])
# figure out which years these bbls are missing
subs <- unique(mgd_rs[bbl %in% bbls$bbl, .(bbl, unitcount, year)])
setorder(subs, "bbl", "year")
subs[, ord := order(year), by = "bbl"]
subs[!is.na(unitcount), beg := min(ord), by = "bbl"]
subs[!is.na(unitcount), end := max(ord), by = "bbl"]
subs[bbl %in% c(1000900017), ]
subs2 <- subs[!is.na(unitcount), ]

totals <- subs2[, sum(unitcount), by = "year"]

rs_time <- ggplot(totals, aes(x=year, y=V1)) + geom_line() + theme_bw() +
  ylab("total number of untis") + ggtitle("Number of Rent Stabilized Units Over Time")



