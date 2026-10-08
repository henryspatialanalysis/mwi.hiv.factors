## #######################################################################################
##
## DHS 01. PREPARE MALAWI DHS DATA FOR PROFILE-LEVEL INDICATORS
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-08
## PURPOSE: Ingest one Malawi DHS survey's household, women's, and men's recodes and the
##   cluster GPS points; harmonise wealth and work variables; and estimate, for each
##   cluster near a profiled catchment, the probability that its true (undisplaced)
##   location falls in each facility catchment.
##
##   Usage: Rscript 01_prepare_dhs.R <survey>, where <survey> is a name under
##   `dhs: surveys:` in config.yaml (default 'mwi_2024').
##
##   Outputs (dhs_prepared directory, in a subfolder named for the survey):
##   - households.csv: one row per household with design and wealth variables
##   - adults.csv: one row per woman or man aged 15-49 with design and work variables
##   - clusters.gpkg: displaced cluster points
##   - cluster_catchment_probabilities.csv: posterior location probabilities by catchment
##   - profile_catchments.csv: catchment-to-profile lookup for the final profiles
##   - prep_log.csv: counts and diagnostics recorded during preparation
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'sf', 'terra', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
dhs_settings <- config$get('dhs')
paper <- config$get('paper')
survey_id <- commandArgs(trailingOnly = TRUE)[1]
if(is.na(survey_id)) survey_id <- 'mwi_2024'
survey <- dhs_settings$surveys[[survey_id]]
if(is.null(survey)) stop("Unknown survey: ", survey_id)
message('Preparing ', survey$label)
raw_path <- function(key){
  file.path(config$get_dir_path('dhs_raw'), survey$raw_dir, survey[[key]])
}
out_path <- function(key) dhs_survey_path(config, 'dhs_prepared', key, survey_id)
set.seed(dhs_settings$seed)
sf::sf_use_s2(FALSE)

prep_log <- list()
log_item <- function(item, value){
  prep_log[[length(prep_log) + 1]] <<- data.table::data.table(
    item = item, value = as.character(value)
  )
  message(item, ': ', value)
}
min_age <- dhs_settings$age_range[[1]]
max_age <- dhs_settings$age_range[[2]]


## HOUSEHOLDS --------------------------------------------------------------------------->

hr_path <- raw_path('hr')
hr_names <- names(haven::read_dta(hr_path, n_max = 1))
roster_cols <- grep('^hv10[345]_[0-9]+$', hr_names, value = TRUE)
hr <- haven::read_dta(
  hr_path,
  col_select = dplyr::all_of(c(
    'hv001', 'hv002', 'hv005', 'hv009', 'hv012', 'hv013', 'hv021', 'hv022', 'hv024',
    'hv025', 'hv027', 'hv270', 'hv271', roster_cols,
    if(survey$district_source != 'ge') survey$district_source
  ))
)
# District of each cluster, when the GE file does not give it
if(survey$district_source != 'ge'){
  hr$district_label <- as.character(haven::as_factor(hr[[survey$district_source]]))
}
hr <- haven::zap_labels(hr) |> data.table::as.data.table()
cluster_districts <- if(survey$district_source != 'ge'){
  unique(hr[, .(cluster = hv001, district_label)])
} else NULL

# De facto women and men aged 15-49 per household, used to put the women's and men's
#  normalised weights on a common population scale
roster_long <- data.table::melt(
  hr[, c('hv001', 'hv002', roster_cols), with = FALSE],
  id.vars = c('hv001', 'hv002'),
  measure.vars = patterns(
    slept = '^hv103_', sex = '^hv104_', age = '^hv105_'
  )
)
eligible_counts <- roster_long[
  slept == 1 & age >= min_age & age <= max_age,
  .(n_women = sum(sex == 2), n_men = sum(sex == 1)),
  by = .(hv001, hv002)
]
hr <- merge(hr, eligible_counts, by = c('hv001', 'hv002'), all.x = TRUE)
hr[is.na(n_women), n_women := 0][is.na(n_men), n_men := 0]
pop_women <- hr[, sum(hv005 / 1e6 * n_women)]
pop_men <- hr[, sum(hv005 / 1e6 * n_men)]
log_item('Weighted de facto women 15-49 (HR, normalised units)', round(pop_women, 1))
log_item('Weighted de facto men 15-49 (HR, normalised units)', round(pop_men, 1))

log_item('Households in HR', nrow(hr))
log_item('Households missing wealth score', hr[is.na(hv271), .N])
missing_by_district <- hr[is.na(hv271), .N, by = hv024]
log_item(
  'Districts (hv024 codes) of households missing wealth score',
  paste0(missing_by_district$hv024, ' (', missing_by_district$N, ')', collapse = '; ')
)

households <- hr[
  !is.na(hv271) & hv012 > 0,
  .(
    cluster = hv001,
    household = hv002,
    psu = hv021,
    strata = hv022,
    urban = hv025 == 1,
    hh_weight = hv005 / 1e6,
    # Wealth indicators describe the de jure household population, as in DHS reports
    person_weight = hv005 / 1e6 * hv012,
    de_jure_members = hv012,
    wealth_quintile = hv270,
    wealth_score = hv271 / 1e5
  )
]
# Shift the score so the poorest household nationally scores zero; the Gini coefficient
#  needs non-negative values and depends on this choice of origin
households[, wealth_score_shifted := wealth_score - min(wealth_score)]
households[, bottom_40 := as.numeric(wealth_quintile %in% 1:2)]
for(qq in 1:5){
  households[, (paste0('quintile_', qq)) := as.numeric(wealth_quintile == qq)]
}
log_item('Households used for wealth indicators', nrow(households))


## WOMEN AND MEN ------------------------------------------------------------------------>

ir_dict <- read_cspro_dictionary(raw_path('ir_dcf'))
women <- read_cspro_items(
  dat_path = raw_path('ir_dat'),
  dictionary = ir_dict,
  item_names = c(
    'V001', 'V002', 'V003', 'V005', 'V012', 'V021', 'V022', 'V025', 'V714', 'V717',
    'V719', 'V731', 'V732', 'V741'
  ),
  id_item = 'CASEID'
)
data.table::setnames(women, 'caseid', 'id')
data.table::setnames(women, names(women), sub('^v', '', names(women)))
women[, sex := 'Women']

mr_dict <- read_cspro_dictionary(raw_path('mr_dcf'))
men <- read_cspro_items(
  dat_path = raw_path('mr_dat'),
  dictionary = mr_dict,
  item_names = c(
    'MV001', 'MV002', 'MV003', 'MV005', 'MV012', 'MV021', 'MV022', 'MV025', 'MV714',
    'MV717', 'MV719', 'MV731', 'MV732', 'MV741'
  ),
  id_item = 'MCASEID'
)
data.table::setnames(men, 'mcaseid', 'id')
data.table::setnames(men, names(men), sub('^mv', '', names(men)))
men[, sex := 'Men']
# Some surveys do not ask men who they work for; the item is then labelled "NA - ..."
men_employer_asked <- !grepl('^NA', mr_dict$items[name == 'MV719', label][1])
log_item(
  'Men: works for family/others/self (MV719) label',
  mr_dict$items[name == 'MV719', label][1]
)

# Completed interviews have a non-missing age; keep ages 15-49 for both sexes
log_item('Women records in IR', nrow(women))
log_item('Men records in MR', nrow(men))
adults <- data.table::rbindlist(list(women, men), use.names = TRUE, fill = TRUE)
adults <- adults[!is.na(`012`) & `012` >= min_age & `012` <= max_age & `005` > 0, ]
log_item('Women 15-49 with completed interviews', adults[sex == 'Women', .N])
log_item('Men 15-49 with completed interviews', adults[sex == 'Men', .N])

# Missing-value codes are 9 (single digit) and 98/99 (occupation)
na_code <- function(x, codes) data.table::fifelse(x %in% codes, NA_real_, x)
adults[, `:=` (
  `719` = na_code(`719`, 9),
  `731` = na_code(`731`, 9),
  `732` = na_code(`732`, 9),
  `741` = na_code(`741`, 9),
  `717` = na_code(`717`, c(98, 99))
)]
adults[, `:=` (
  cluster = `001`,
  psu = `021`,
  strata = `022`,
  urban = `025` == 1,
  age = `012`,
  weight_sex = `005` / 1e6,
  worked_12m = as.numeric(`731` %in% 1:3),
  self_or_family = as.numeric(`719` %in% c(1, 3)),
  no_cash_earnings = as.numeric(`741` %in% c(0, 3)),
  agriculture = as.numeric(`717` %in% 4:5),
  agri_self_employed = as.numeric(`717` == 4),
  seasonal_or_occasional = as.numeric(`732` %in% 2:3),
  seasonal = as.numeric(`732` == 2),
  occasional = as.numeric(`732` == 3)
)]
adults[is.na(`731`), worked_12m := NA_real_]
adults[, is_worker := worked_12m %in% 1]
# Work characteristics are defined only among those who worked in the last 12 months
for(vv in c('agriculture', 'agri_self_employed')){
  adults[is_worker == FALSE | is.na(`717`), (vv) := NA_real_]
}
for(vv in c('seasonal_or_occasional', 'seasonal', 'occasional')){
  adults[is_worker == FALSE | is.na(`732`), (vv) := NA_real_]
}
adults[is_worker == FALSE | is.na(`741`), no_cash_earnings := NA_real_]
adults[is_worker == FALSE | is.na(`719`), self_or_family := NA_real_]
if(!men_employer_asked) adults[sex == 'Men', self_or_family := NA_real_]

# Non-wage work: self-employed or working for a family member, or receiving no cash for
#  the work. Only defined for men when the men's questionnaire asks the employer question
adults[
  is_worker == TRUE & (sex == 'Women' | men_employer_asked),
  nonwage_work := as.numeric(self_or_family == 1 | no_cash_earnings == 1)
]
# Proxy available for both sexes, checked against the full definition among women:
#  no cash earnings, or self-employed in agriculture
adults[
  is_worker == TRUE,
  nonwage_proxy := as.numeric(no_cash_earnings == 1 | agri_self_employed == 1)
]
agree <- adults[
  sex == 'Women' & !is.na(nonwage_work) & !is.na(nonwage_proxy),
  .(
    agreement = sum(weight_sex * (nonwage_work == nonwage_proxy)) / sum(weight_sex),
    prev_full = sum(weight_sex * nonwage_work) / sum(weight_sex),
    prev_proxy = sum(weight_sex * nonwage_proxy) / sum(weight_sex),
    sensitivity = sum(weight_sex * (nonwage_proxy == 1 & nonwage_work == 1)) /
      sum(weight_sex * (nonwage_work == 1))
  )
]
log_item('Women: weighted agreement, non-wage vs proxy', round(agree$agreement, 3))
log_item('Women: weighted prevalence, non-wage work', round(agree$prev_full, 3))
log_item('Women: weighted prevalence, proxy', round(agree$prev_proxy, 3))
log_item('Women: proxy sensitivity for non-wage work', round(agree$sensitivity, 3))

# Pooled weights: rescale each sex's normalised weights to that sex's weighted
#  population of 15-49 year-olds from the household roster
adults[sex == 'Women', weight_pooled := weight_sex * pop_women / sum(weight_sex)]
adults[sex == 'Men', weight_pooled := weight_sex * pop_men / sum(weight_sex)]

adults <- adults[, .(
  id, sex, cluster, psu, strata, urban, age, weight_sex, weight_pooled, worked_12m,
  is_worker, nonwage_work, nonwage_proxy, self_or_family, no_cash_earnings, agriculture,
  agri_self_employed, seasonal_or_occasional, seasonal, occasional
)]


## CLUSTERS AND PROFILE CATCHMENTS ------------------------------------------------------>

clusters_sf <- read_dhs_clusters(raw_path('ge'))
if(!is.null(cluster_districts)){
  clusters_sf$dhs_district <- cluster_districts[
    match(clusters_sf$cluster, cluster), district_label
  ]
}
log_item('Clusters with GPS', nrow(clusters_sf))
log_item(
  'Clusters in HR without GPS',
  length(setdiff(unique(households$cluster), clusters_sf$cluster))
)

catchments_sf <- config$read('catchments', 'facility_catchments', quiet = TRUE)
admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)

vetted <- config$read(
  'analysis', 'vetted_profile_groupings', custom_version = paper$stage_versions$C
)
data.table::setnames(vetted, 1, 'catchment_id')
profile_order <- names(paper$profiles)
profile_levels <- paste0(seq_along(profile_order), '. ', unlist(paper$profiles))
profile_catchments <- vetted[
  !profile_color %in% paper$excluded_profiles,
  .(
    catchment_id,
    catchment_name,
    district,
    profile_color,
    profile = profile_levels[match(profile_color, profile_order)]
  )
]
log_item('Profiled catchments', nrow(profile_catchments))

# DHS restricts displacement to the survey's sampling districts; match these to Naomi
#  polygons at the configured level, harmonising names through `district_fixes`
districts_sf <- admin_bounds[
  admin_bounds$area_level == survey$restriction_level, c('area_name')
]
districts_sf$restrict_name <- normalize_district(
  districts_sf$area_name, survey$district_fixes
)
restrict_lookup <- data.table::data.table(
  restrict_name = sort(unique(districts_sf$restrict_name))
)[, restrict_code := .I]
districts_sf$restrict_code <- restrict_lookup[
  match(districts_sf$restrict_name, restrict_name), restrict_code
]
clusters_sf$restrict_name <- normalize_district(
  clusters_sf$dhs_district, survey$district_fixes
)
clusters_sf$restrict_code <- restrict_lookup[
  match(clusters_sf$restrict_name, restrict_name), restrict_code
]
unmatched <- unique(clusters_sf$dhs_district[is.na(clusters_sf$restrict_code)])
log_item('DHS districts unmatched to admin polygons', paste(unmatched, collapse = ', '))

# Candidate clusters: within the maximum displacement distance of a profiled catchment
profiled_union <- catchments_sf[
  catchments_sf$catchment_id %in% profile_catchments$catchment_id,
] |>
  sf::st_union()
max_dist <- dhs_settings$displacement$rural_far_max_m
near <- sf::st_is_within_distance(
  sf::st_transform(clusters_sf, 32736),
  sf::st_transform(profiled_union, 32736),
  dist = max_dist + 500,
  sparse = FALSE
)[, 1]
# Clusters with no positive-weight households (the Dzaleka camp clusters, which have no
#  wealth index and zero weights) cannot contribute to any estimate
candidates_sf <- clusters_sf[near & clusters_sf$cluster %in% households$cluster, ]
log_item(
  'Clusters within displacement range of a profiled catchment', nrow(candidates_sf)
)


## POPULATION PRIOR AND ZONE RASTERS ---------------------------------------------------->

pop_full <- terra::rast(
  file.path(config$get_dir_path('population_100m'), survey$population)
)
crop_ext <- sf::st_buffer(sf::st_transform(candidates_sf, 32736), max_dist + 2000) |>
  sf::st_transform(4326) |>
  terra::vect() |>
  terra::ext()
pop_100m <- terra::crop(pop_full, crop_ext)
pop_100m[is.na(pop_100m)] <- 0
zone_100m <- terra::rasterize(
  terra::vect(catchments_sf), pop_100m, field = 'catchment_id'
)
restrict_100m <- terra::rasterize(
  terra::vect(districts_sf), pop_100m, field = 'restrict_code'
)
coarse_template <- terra::aggregate(pop_100m, fact = 10)
restrict_1km <- terra::rasterize(
  terra::vect(districts_sf), coarse_template, field = 'restrict_code'
)
acc_codes <- unique(candidates_sf$restrict_code)
acceptance <- list(
  urban = dhs_restriction_acceptance(
    restrict_1km, codes = acc_codes, urban = TRUE, params = dhs_settings$displacement
  ),
  rural = dhs_restriction_acceptance(
    restrict_1km, codes = acc_codes, urban = FALSE, params = dhs_settings$displacement
  )
)


## LOCATION PROBABILITIES --------------------------------------------------------------->

cand_coords <- sf::st_coordinates(candidates_sf)
cand_dt <- data.table::data.table(
  cluster = candidates_sf$cluster,
  lon = cand_coords[, 1],
  lat = cand_coords[, 2],
  urban = candidates_sf$urban,
  restrict_code = candidates_sf$restrict_code
)
location_probs <- dhs_location_probabilities(
  clusters = cand_dt,
  pop_raster = pop_100m,
  zone_raster = zone_100m,
  restrict_raster = restrict_100m,
  acceptance = acceptance,
  params = dhs_settings$displacement,
  n_samples = dhs_settings$displacement$n_samples
)
data.table::setnames(location_probs, 'zone', 'catchment_id')
location_probs$profile <- profile_catchments$profile[
  match(location_probs$catchment_id, profile_catchments$catchment_id)
]
log_item(
  'Clusters with fallback weighting',
  location_probs[fallback != 'none', uniqueN(cluster)]
)
log_item('Minimum candidate ESS', round(min(location_probs$ess)))

# Catchment containing each displaced point, for the naive-assignment sensitivity check
naive <- sf::st_join(
  clusters_sf[, 'cluster'], catchments_sf[, 'catchment_id'], join = sf::st_within
) |>
  sf::st_drop_geometry() |>
  data.table::as.data.table()
naive <- naive[!duplicated(cluster), ]
clusters_sf$naive_catchment_id <- naive[match(clusters_sf$cluster, cluster), catchment_id]
clusters_sf$naive_profile <- profile_catchments[
  match(clusters_sf$naive_catchment_id, catchment_id), profile
]

# Expected clusters per profile, versus counts from the displaced points
expected <- location_probs[
  !is.na(profile), .(expected_clusters = sum(prob)), by = profile
]
naive_counts <- data.table::as.data.table(sf::st_drop_geometry(clusters_sf))[
  !is.na(naive_profile), .(naive_clusters = .N), by = .(profile = naive_profile)
]
cluster_summary <- merge(expected, naive_counts, by = 'profile', all = TRUE)
print(cluster_summary)


## SAVE --------------------------------------------------------------------------------->

data.table::fwrite(households, out_path('households'))
data.table::fwrite(adults, out_path('adults'))
sf::st_write(clusters_sf, out_path('clusters'), delete_dsn = TRUE, quiet = TRUE)
data.table::fwrite(location_probs, out_path('location_probs'))
data.table::fwrite(cluster_summary, out_path('cluster_summary'))
data.table::fwrite(profile_catchments, out_path('profile_catchments'))
data.table::fwrite(data.table::rbindlist(prep_log), out_path('prep_log'))
message('Done preparing ', survey$label, '.')
