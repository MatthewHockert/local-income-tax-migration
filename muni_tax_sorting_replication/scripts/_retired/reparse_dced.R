suppressMessages({library(readxl); library(data.table); library(stringr)})
xls_files <- list.files("data/pa_dced_raw", pattern = "^pa_taxes_[0-9]{4}\\.xls$",
                        full.names = TRUE)
cat("XLS files:", length(xls_files), "\n")
clean <- function(s) tolower(str_squish(as.character(s)))

read_one <- function(f) {
  yr <- as.integer(str_extract(basename(f), "[0-9]{4}"))
  d <- as.data.table(read_excel(f, .name_repair = "minimal"))
  setnames(d, c("Reporting Year","County","Municipality ID","Municipality",
                "School District ID","School District","Tax Name",
                "Municipal Tax Amount","School District Tax Amount","Total Tax Rate"),
           c("year","county","municipality_id","municipality",
             "sd_id","sd_name","tax_name",
             "muni_eit_rate","sd_eit_rate","total_eit_rate"),
           skip_absent = TRUE)
  d <- d[, .(year, county, municipality, municipality_id, sd_id, sd_name,
             muni_eit_rate, sd_eit_rate, total_eit_rate, tax_name)]
  d[, year           := as.integer(year)]
  d[, municipality_id := as.integer(municipality_id)]
  d[, sd_id          := as.integer(sd_id)]
  d[, muni_eit_rate  := as.numeric(muni_eit_rate)]
  d[, sd_eit_rate    := as.numeric(sd_eit_rate)]
  d[, total_eit_rate := as.numeric(total_eit_rate)]
  d[, tn := clean(tax_name)]
  d[, is_resident    := grepl("resident", tn) & !grepl("non.?resident", tn)]
  d[, is_nonresident := grepl("non.?resident", tn)]
  d[, tn := NULL]
  d
}

all <- rbindlist(lapply(xls_files, read_one), fill = TRUE)
fwrite(all, "data/pa_eit_rate_panel.csv")
cat("Rewrote panel:", nrow(all), "rows\n")
cat("Distinct sd_id:", uniqueN(all$sd_id), "\n")

mu <- all[is_resident & !is.na(sd_eit_rate),
          .(n = uniqueN(sd_id)), by = .(municipality_id, year)][n > 1]
cat("\nMulti-SD muni-years:", nrow(mu), "\n")
sid <- mu$municipality_id[1]; sy <- mu$year[1]
cat("\nSample multi-SD muni-year:\n")
print(all[municipality_id == sid & year == sy & is_resident,
          .(year, municipality, sd_id, sd_name, muni_eit_rate, sd_eit_rate)])
