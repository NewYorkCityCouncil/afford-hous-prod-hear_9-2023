# are the current rs data reliable -- what if a bbl was on the 2017 but show's all nas for future years
# which(na_bbls$V1 %in% 1005690020)

source("code/utils/00_load_dependencies.R")
library(pdftools)

soa_scrape_fnx <- function(bbls) {

soa_list <- list()
i=1
  for(i in 1:nrow(bbls)){
    bbl <- 1006200054 
    soa_list[i] <- paste0('https://a836-edms.nyc.gov/dctm-rest/repositories/dofedmspts/StatementSearch?bbl=', bbl, '&stmtDate=20230819&stmtType=SOA')
  }
  
# re-org text 
soa_ocr_l <- list()


for (i in 1:length(soa_list)) {
  
  soa_ocr_l[[i]] <- pdf_text(soa_list[[i]])
  soa_ocr_l
  
  
}

soa_ocr2 <- lapply(soa_ocr_l, function(x) str_split(x, "\n"))

return(soa_ocr_l)

}
as.data.table(unlist(lapply(soa_ocr2, function(x)grep("Rent Stabilization", x))))



