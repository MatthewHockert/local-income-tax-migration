# build_neighbors_rings.R
# Build ring-based contiguity neighbors for PA cousubs.
# Ring 1 = munis sharing a boundary (queen contiguity, st_touches).
# Ring 2 = neighbors of Ring 1 minus self minus Ring 1.
#
# Output: data/pa_neighbors_rings.rds with columns
#   muni, neighbor, ring (1 or 2)

suppressMessages({library(sf); library(data.table)})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")

cousub <- readRDS(file.path(data_dir, "pa_cousub.rds"))
# pa_panel and pa_rate_panel use COUSUBFP (5-digit muni code) as cousub_fips,
# not the full 10-digit GEOID. Match that convention.
cousub$cousub_fips <- cousub$COUSUBFP
stopifnot("cousub_fips" %in% names(cousub))
cat(sprintf("Loaded %d PA cousubs\n", nrow(cousub)))

# Make sure geometry is valid
cousub <- st_make_valid(cousub)

# Ring 1: queen contiguity (any boundary touch including a single point)
cat("Computing Ring 1 (queen contiguity)...\n")
nb1 <- st_touches(cousub, sparse = TRUE)
n_obs <- length(nb1)
ring1_dt <- rbindlist(lapply(seq_len(n_obs), function(i) {
  idx <- nb1[[i]]
  if (length(idx) == 0) return(NULL)
  data.table(muni     = cousub$cousub_fips[i],
             neighbor = cousub$cousub_fips[idx],
             ring     = 1L)
}))
cat(sprintf("Ring 1 pairs: %s\n", format(nrow(ring1_dt), big.mark = ",")))

# Ring 2: neighbors-of-Ring-1 minus self minus Ring 1
cat("Computing Ring 2...\n")
ring1_by_muni <- split(ring1_dt$neighbor, ring1_dt$muni)

ring2_dt <- rbindlist(lapply(seq_len(n_obs), function(i) {
  m <- cousub$cousub_fips[i]
  r1 <- ring1_by_muni[[m]]
  if (is.null(r1) || length(r1) == 0) return(NULL)
  # Neighbors of r1
  r2_candidates <- unique(unlist(ring1_by_muni[r1]))
  r2 <- setdiff(r2_candidates, c(m, r1))
  if (length(r2) == 0) return(NULL)
  data.table(muni = m, neighbor = r2, ring = 2L)
}))
cat(sprintf("Ring 2 pairs: %s\n", format(nrow(ring2_dt), big.mark = ",")))

rings <- rbind(ring1_dt, ring2_dt)
saveRDS(rings, file.path(data_dir, "pa_neighbors_rings.rds"))

# Summary stats
cat("\nSummary:\n")
nb_counts <- rings[, .N, by = .(muni, ring)]
nb_wide <- dcast(nb_counts, muni ~ ring, value.var = "N", fill = 0L)
setnames(nb_wide, c("muni","ring1","ring2"))
cat("Mean / median / max ring 1 size:",
    round(mean(nb_wide$ring1), 1), "/", median(nb_wide$ring1), "/", max(nb_wide$ring1), "\n")
cat("Mean / median / max ring 2 size:",
    round(mean(nb_wide$ring2), 1), "/", median(nb_wide$ring2), "/", max(nb_wide$ring2), "\n")
cat("Munis with no ring 1 neighbors (islands):", sum(nb_wide$ring1 == 0), "\n")

cat("Saved data/pa_neighbors_rings.rds\n")
