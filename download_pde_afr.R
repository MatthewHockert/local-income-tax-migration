# Download PA School District AFR data from PDE
# Source: https://www.pa.gov/agencies/education/.../summary-of-annual-financial-report-data/
#
# Files:
#   - Expenditure detail (function-level, 2014-2024)
#   - Local revenue (Act 1/511 EIT breakdown, 2014-2024)
#   - Actual instruction expense (2008-2024)
#   - Debt information (2014-2024)
#
# Output: data/pde_afr/*.xlsx (raw), data/sd_fiscal_panel.rds (analysis-ready)

library(readxl)
library(data.table)
library(janitor)

data_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/data"
pde_dir <- file.path(data_dir, "pde_afr")
dir.create(pde_dir, showWarnings = FALSE)

base_url <- "https://www.pa.gov/content/dam/copapwp-pagov/en/education/documents/schools/grants-and-funding/school-finances/summary-of-afr-data"

# Files to download
files <- list(
  exp_detail = paste0(base_url, "/afr-data-detailed/finances%20afr%20expdetail%201415-2324.xlsx"),
  local_rev = paste0(base_url, "/afr-data-detailed/finances%20afr%20localrev%201415-2324.xlsx"),
  instruction = paste0(base_url, "/afr-data-detailed/finances%20aie%200809-2324.xlsx"),
  debt = paste0(base_url, "/afr-data-detailed/finances%20afr%20soin%201415-2324.xlsx")
)


# 1. Download ----
for (nm in names(files)) {
  outfile <- file.path(pde_dir, paste0(nm, ".xlsx"))
  if (file.exists(outfile)) {
    print(paste("Already downloaded:", nm))
    next
  }
  print(paste("Downloading:", nm))
  download.file(files[[nm]], outfile, method = "curl", extra = "-sL", quiet = TRUE)
  print(paste("  Saved:", round(file.size(outfile) / 1e6, 1), "MB"))
}


# 2. Parse expenditure detail ----
# Each sheet is one school year. Columns are function-level expenditure codes.
# We extract major categories and aggregate to district-year.

exp_file <- file.path(pde_dir, "exp_detail.xlsx")
exp_sheets <- excel_sheets(exp_file)

# Parse school year from sheet name (e.g., "2019-20" -> 2020)
parse_sy <- function(s) as.integer(paste0("20", sub(".*-", "", s)))

exp_list <- lapply(exp_sheets, function(sh) {
  d <- as.data.table(read_excel(exp_file, sheet = sh))

  # Only school districts (Ctg/Cat/Ctgy == 1)
  cat_col <- intersect(c("Ctg", "Cat", "Ctgy"), names(d))
  if (length(cat_col) > 0) {
    d <- d[get(cat_col[1]) == 1]
  }

  # Extract major categories using function code columns
  cols <- names(d)

  # Helper: find column by code pattern, sum if multiple match
  get_col <- function(pattern) {
    idx <- grep(pattern, cols)
    if (length(idx) == 0) return(rep(NA_real_, nrow(d)))
    if (length(idx) == 1) return(as.numeric(d[[idx]]))
    rowSums(sapply(d[, ..idx], as.numeric), na.rm = TRUE)
  }

  # LEA name column varies across sheets
  name_col <- intersect(c("LEA Name", "School District"), names(d))[1]

  data.table(
    aun = d$AUN,
    sd_name = if (!is.na(name_col)) d[[name_col]] else NA_character_,
    county = d$County,
    year = parse_sy(sh),
    total_exp = as.numeric(d$`Total Expenditures`),
    instruction = get_col("Instruction.*1000$"),
    regular_programs = get_col("Regular.*1100$"),
    special_programs = get_col("Special.*1200$"),
    vocational = get_col("Vocational.*1300$"),
    support_students = get_col("Students.*2100$"),
    support_instr_staff = get_col("Instructional Staff.*2200$"),
    support_admin = get_col("Administration.*2300$"),
    support_health = get_col("Health.*2400$"),
    support_business = get_col("Business.*2500$"),
    operations_maint = get_col("Maintenance.*2600$"),
    transportation = get_col("Transportation.*2700$"),
    support_central = get_col("Central.*2800$"),
    food_services = get_col("Food.*3100$"),
    student_activities = get_col("Activities.*3200$"),
    community_services = get_col("Community.*3300$"),
    facilities = get_col("Facilities.*4000$"),
    debt_service = get_col("Debt Service 5110$")
  )
})

sd_exp <- rbindlist(exp_list, fill = TRUE)
print(paste("Expenditure panel:", nrow(sd_exp), "district-years,",
  uniqueN(sd_exp$aun), "districts,", paste(range(sd_exp$year), collapse = "-")))


# 3. Parse local revenue ----
rev_file <- file.path(pde_dir, "local_rev.xlsx")
rev_sheets <- excel_sheets(rev_file)

rev_list <- lapply(rev_sheets, function(sh) {
  d <- as.data.table(read_excel(rev_file, sheet = sh))
  cat_col <- intersect(c("Ctg", "Cat", "Ctgy"), names(d))
  if (length(cat_col) > 0) d <- d[get(cat_col[1]) == 1]

  cols <- names(d)
  get_col <- function(pattern) {
    idx <- grep(pattern, cols)
    if (length(idx) == 0) return(rep(NA_real_, nrow(d)))
    as.numeric(d[[idx[1]]])
  }

  data.table(
    aun = d$AUN,
    year = parse_sy(sh),
    total_local_rev = get_col("Total Local Revenue"),
    re_tax = get_col("Current Real Estate Taxes"),
    act1_eit = get_col("Act 1 Earned Income Tax"),
    act511_eit = get_col("Act 511 Earned Income Taxes"),
    act511_lst = get_col("Local Services Taxes"),
    delinquent_taxes = get_col("Delinquent Taxes"),
    earnings_investments = get_col("Earnings on Investments")
  )
})

sd_rev <- rbindlist(rev_list, fill = TRUE)
print(paste("Revenue panel:", nrow(sd_rev), "district-years"))


# 4. Parse instruction expense (longer panel, 2008-2024) ----
instr_file <- file.path(pde_dir, "instruction.xlsx")
d_instr <- as.data.table(read_excel(instr_file, sheet = 1))
# Wide format: columns are years. Melt to long.
year_cols <- grep("^20", names(d_instr), value = TRUE)
sd_instr <- melt(d_instr, id.vars = c("AUN", "School District", "County"),
  measure.vars = year_cols, variable.name = "school_year", value.name = "instr_expense")
sd_instr[, year := parse_sy(as.character(school_year))]
sd_instr[, school_year := NULL]
setnames(sd_instr, c("AUN", "School District", "County"), c("aun", "sd_name", "county"))
print(paste("Instruction panel:", nrow(sd_instr), "district-years,",
  paste(range(sd_instr$year), collapse = "-")))


# 5. Merge into single SD fiscal panel ----
sd_fiscal <- merge(sd_exp, sd_rev[, .(aun, year, total_local_rev, re_tax,
  act1_eit, act511_eit)],
  by = c("aun", "year"), all.x = TRUE)

# Add long instruction panel for pre-2015 years
sd_instr_extra <- sd_instr[year < min(sd_exp$year), .(aun, sd_name, county, year,
  instruction = as.numeric(instr_expense))]
sd_fiscal <- rbindlist(list(sd_fiscal, sd_instr_extra), fill = TRUE)
sd_fiscal <- sd_fiscal[order(aun, year)]

# Compute shares
sd_fiscal[total_exp > 0, `:=`(
  sh_instruction = instruction / total_exp,
  sh_support_admin = support_admin / total_exp,
  sh_operations = operations_maint / total_exp,
  sh_transportation = transportation / total_exp,
  sh_debt = debt_service / total_exp,
  sh_facilities = facilities / total_exp,
  sh_student_support = support_students / total_exp
)]

# Total EIT revenue (Act 1 + Act 511)
sd_fiscal[, eit_rev := rowSums(cbind(
  ifelse(is.na(act1_eit), 0, act1_eit),
  ifelse(is.na(act511_eit), 0, act511_eit)
))]

# Log outcomes
for (v in c("instruction", "support_admin", "operations_maint", "transportation",
  "debt_service", "facilities", "support_students", "student_activities",
  "total_exp", "eit_rev", "re_tax")) {
  sd_fiscal[, paste0("log_", v) := ifelse(get(v) > 0, log(get(v)), NA_real_)]
}

saveRDS(sd_fiscal, file.path(data_dir, "sd_fiscal_panel.rds"))
print(paste("SD fiscal panel saved:", nrow(sd_fiscal), "rows,",
  uniqueN(sd_fiscal$aun), "districts,",
  paste(range(sd_fiscal$year, na.rm = TRUE), collapse = "-")))

# Summary: mean expenditure shares for all districts
mean_shares <- sd_fiscal[total_exp > 0 & year <= 2019, .(
  Instruction = round(mean(sh_instruction, na.rm = TRUE) * 100, 1),
  Admin = round(mean(sh_support_admin, na.rm = TRUE) * 100, 1),
  Operations = round(mean(sh_operations, na.rm = TRUE) * 100, 1),
  Transportation = round(mean(sh_transportation, na.rm = TRUE) * 100, 1),
  Debt = round(mean(sh_debt, na.rm = TRUE) * 100, 1),
  Facilities = round(mean(sh_facilities, na.rm = TRUE) * 100, 1),
  Student_Support = round(mean(sh_student_support, na.rm = TRUE) * 100, 1)
)]
print("Mean expenditure shares (%, 2015-2019):")
print(mean_shares)


# 6. Crosswalk: School District AUN → Municipality COUSUBFP ----
# Spatial join municipality centroids to SD boundaries, then name-match to PDE AUN.

library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

pa_cousub <- readRDS(file.path(data_dir, "pa_cousub.rds"))
pa_sds <- school_districts(state = "PA", type = "unified", year = 2020) %>%
  st_transform(4326)

muni_centroids <- st_centroid(pa_cousub)
muni_to_sd <- st_join(muni_centroids,
  pa_sds %>% select(sd_geoid = GEOID, sd_tiger_name = NAME),
  join = st_within) %>%
  st_drop_geometry() %>%
  select(cousub_fips = COUSUBFP, sd_geoid, sd_tiger_name) %>%
  as.data.table()

# Normalize names for matching
pde_sds <- unique(sd_fiscal[!is.na(sd_name) & grepl(" SD$", sd_name), .(aun, sd_name)])

muni_to_sd[, sd_clean := tolower(trimws(gsub(" School District$", "", sd_tiger_name)))]
pde_sds[, sd_clean := tolower(trimws(gsub(" SD$", "", sd_name)))]

# Fix known mismatches between TIGER and PDE names
muni_to_sd[sd_clean == "mount lebanon", sd_clean := "mt lebanon"]
muni_to_sd[sd_clean == "owen j. roberts", sd_clean := "owen j roberts"]
muni_to_sd[sd_clean == "st. clair area", sd_clean := "saint clair area"]
muni_to_sd[sd_clean == "st. marys area", sd_clean := "saint marys area"]

sd_xwalk <- merge(muni_to_sd, pde_sds[, .(aun, sd_clean)],
  by = "sd_clean", all.x = TRUE)
sd_xwalk <- sd_xwalk[!is.na(aun), .(cousub_fips, aun, sd_geoid)]
sd_xwalk <- unique(sd_xwalk)

saveRDS(sd_xwalk, file.path(data_dir, "muni_to_sd_xwalk.rds"))
print(paste("Municipality-to-SD crosswalk:", nrow(sd_xwalk), "municipalities matched to",
  uniqueN(sd_xwalk$aun), "school districts"))

beepr::beep()
