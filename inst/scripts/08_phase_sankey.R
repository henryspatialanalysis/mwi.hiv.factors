## #######################################################################################
##
## 08. SANKEY DIAGRAM OF FACILITY GROUPINGS ACROSS ANALYSIS PHASES
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-11-23
## PURPOSE: Show how facilities moved between groups, from left to right:
##   1. District
##   2. Phase A: k-means groups from quantitative (GIS) indicators only
##   3. Phase B: k-means groups after adding qualitative (community) indicators
##   4. Phase C: final groups after expert review
##
##   Each facility is one lane, colored by its Phase C group. Groups are ordered within
##   each column to minimize the number of lane crossings. Phases correspond to stages A,
##   B, and C in the `paper` block of config.yaml, which also sets the analysis versions,
##   cluster counts, profile names, and colors.
##
##   Run 07_paper_figures.R first: it writes the Phase A groups used here.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## SETTINGS ----------------------------------------------------------------------------->

# Phase A groups. 'rerun': same GIS indicators as Phase B (written by 07_paper_figures.R).
#  'archived': original stage A run, which includes an indicator dropped from Phase B. Set
#  FIXED_ORDERS to NULL when switching, because the saved order is for 'rerun'.
PHASE_A_SOURCE <- 'rerun'

# If TRUE, number Phase A and B groups from top to bottom within each setting. If FALSE,
#  keep the original k-means cluster numbers, which match the archived analysis outputs.
#  The output CSV lists the original numbers (cluster_a, cluster_b) either way.
RENUMBER_GROUPS <- TRUE

# Top-to-bottom order of each column. Set a column to NULL to search for the order with
#  the fewest crossings (about 3 minutes with all four NULL). Phase A and B groups are
#  listed here by their original k-means numbers. The order below came from that search;
#  search again if the groupings change. Several orders tie for the fewest crossings: this
#  one was picked because it does not put the excluded facility at the top.
FIXED_ORDERS <- list(
  `District` = c(
    'Lilongwe', 'Dowa', 'Kasungu', 'Mulanje', 'Chikwawa', 'Nsanje', 'Karonga'
  ),
  `Phase A` = c(
    'Metro 2', 'Metro 1', 'Metro 3',
    'Non-metro 5', 'Non-metro 2', 'Non-metro 4', 'Non-metro 1', 'Non-metro 3'
  ),
  `Phase B` = c(
    'Metro 2', 'Metro 1', 'Metro 3',
    'Non-metro 2', 'Non-metro 3', 'Non-metro 5', 'Non-metro 4', 'Non-metro 1'
  ),
  `Phase C` = c('Teal', 'Excluded', 'Pink', 'Green', 'Grey', 'Brown', 'Yellow')
)

# Label for facilities with no final profile
EXCLUDED_LABEL <- 'Excluded'
EXCLUDED_COLOR <- '#A6A6A6'

# Order search
N_RESTARTS <- 40L           # Used only if a column above is NULL
SEED <- 2026L

# Dimensions, in inches
FIG_WIDTH <- 6.5
FIG_HEIGHT <- 5
TITLE_HEIGHT <- 0.3
MARGIN <- 0.03
NODE_GAP_MIN <- 0.09        # Smallest vertical gap between groups
LANE_FILL <- 0.88           # Share of each lane that is colored; the rest is white space
LABEL_WIDTH_LEFT <- 0.72    # Space for district labels
LABEL_WIDTH_RIGHT <- 1.27   # Space for Phase C labels
BAR_WIDTH <- 0.09           # Bars for District and Phase C
BOX_WIDTH <- 0.86           # Labelled boxes for Phases A and B
LABEL_WRAP <- 22L           # Characters per line in Phase C labels

# Text and colors
FONT_FAMILY <- 'Liberation Sans' # Metric-compatible with Arial
LABEL_PT <- 7.5
TITLE_PT <- 9
LINE_HEIGHT <- 0.9
INK <- '#171717'
NODE_FILL <- '#F0F0F0'
NODE_LINE <- '#7A7A7A'
DISTRICT_FILL <- '#7A7A7A'
LANE_ALPHA <- 0.75
DPI <- 600


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'ragg', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
paper <- config$get('paper')
dir.create(config$get_dir_path('paper'), showWarnings = FALSE, recursive = TRUE)

stages <- c('District', 'Phase A', 'Phase B', 'Phase C')


## LOAD GROUP MEMBERSHIPS --------------------------------------------------------------->

# Read k-means groups for one analysis stage. Results file names are templated by setting,
#  so build the paths directly.
read_kmeans_groups <- function(stage){
  lapply(names(paper$stage_k), function(setting_name){
    file.path(
      config$get_dir_path('analysis', custom_version = paper$stage_versions[[stage]]),
      setting_name, 'pca_kmeans_results.csv'
    ) |>
      data.table::fread() |>
      _[, .(
        catchment_id,
        setting = setting_name,
        cluster = get(paste0('kmeans_', paper$stage_k[[setting_name]]))
      )]
  }) |> data.table::rbindlist()
}

phase_a <- if(PHASE_A_SOURCE == 'rerun'){
  config$read('paper', 'stage_a_rerun')[
    , .(catchment_id, setting, cluster = stage_a_cluster)
  ]
} else {
  read_kmeans_groups('A')
}
phase_b <- read_kmeans_groups('B')

# The first column of the vetted file holds the catchment ID
profile_labels <- paste0(seq_along(paper$profiles), '. ', unlist(paper$profiles)) |>
  stats::setNames(names(paper$profiles))
phase_c <- config$read(
  'analysis', 'vetted_profile_groupings', custom_version = paper$stage_versions$C
)
data.table::setnames(phase_c, 1, 'catchment_id')
unknown_profiles <- setdiff(
  phase_c$profile_color, c(names(paper$profiles), paper$excluded_profiles)
)
if(length(unknown_profiles) > 0){
  stop('Profiles not in config: ', paste(unknown_profiles, collapse = ', '))
}
phase_c[, profile := data.table::fifelse(
  profile_color %in% paper$excluded_profiles,
  EXCLUDED_LABEL,
  profile_labels[profile_color]
)]

# One row per facility, with its group in each column of the diagram
facilities <- phase_c[order(catchment_id), .(
  catchment_id, catchment_name, District = district, `Phase C` = profile
)]
facilities[
  phase_a, `:=` (setting_a = i.setting, cluster_a = i.cluster), on = 'catchment_id'
][
  phase_b, `:=` (setting = i.setting, cluster_b = i.cluster), on = 'catchment_id'
]
stopifnot(
  anyDuplicated(facilities$catchment_id) == 0,
  setequal(facilities$catchment_id, phase_a$catchment_id),
  setequal(facilities$catchment_id, phase_b$catchment_id),
  !anyNA(facilities),
  facilities[, all(setting == setting_a)]
)
facilities[, `:=` (
  `Phase A` = paste(setting, cluster_a),
  `Phase B` = paste(setting, cluster_b),
  setting_a = NULL
)]


## ORDER GROUPS TO MINIMIZE CROSSINGS --------------------------------------------------->

# Rank facilities within one column: first by group, then so that facilities in the same
#  group leave toward the right in order. Ties are broken so that facilities sharing a
#  group in adjacent columns keep the same relative order.
within_group_keys <- list(
  `District` = c('Phase A', 'Phase B', 'Phase C'),
  `Phase A` = c('Phase B', 'Phase C', 'District'),
  `Phase B` = c('Phase C', 'Phase A', 'District'),
  `Phase C` = c('Phase B', 'Phase A', 'District')
)
rank_lanes <- function(orders){
  node_pos <- lapply(stages, function(s) match(facilities[[s]], orders[[s]]))
  names(node_pos) <- stages
  ranks <- matrix(0L, nrow(facilities), length(stages), dimnames = list(NULL, stages))
  for(s in stages){
    # Combine the sort keys into one number, as digits of decreasing importance
    keys <- node_pos[c(s, within_group_keys[[s]])]
    sort_key <- ((keys[[1]] * 100 + keys[[2]]) * 100 + keys[[3]]) * 100 + keys[[4]]
    ranks[order(sort_key), s] <- seq_len(nrow(facilities))
  }
  ranks
}

# Vertical center of each lane, with columns justified to the same total height
n_facilities <- nrow(facilities)
column_height <- FIG_HEIGHT - TITLE_HEIGHT - 2 * MARGIN
max_groups <- max(sapply(stages, function(s) data.table::uniqueN(facilities[[s]])))
lane_height <- (column_height - NODE_GAP_MIN * (max_groups - 1)) / n_facilities
lane_centers <- function(orders, ranks = rank_lanes(orders)){
  centers <- ranks
  for(s in stages){
    n_groups <- length(orders[[s]])
    gap <- (column_height - lane_height * n_facilities) / max(n_groups - 1, 1)
    node_pos <- match(facilities[[s]], orders[[s]])
    centers[, s] <- (
      MARGIN + column_height - (ranks[, s] - 0.5) * lane_height - (node_pos - 1) * gap
    )
  }
  centers
}

# Number of facility pairs that cross between adjacent columns
all_pairs <- utils::combn(n_facilities, 2)
pairs_first <- all_pairs[1, ]
pairs_second <- all_pairs[2, ]
count_crossings <- function(ranks){
  total <- 0
  for(j in seq_len(ncol(ranks) - 1)){
    # Lanes are ranked 1 to N, so crossings are inversions of the next column's ranks
    next_ranks <- ranks[order(ranks[, j]), j + 1]
    total <- total + sum(next_ranks[pairs_first] > next_ranks[pairs_second])
  }
  total
}
# Crossings decide the order; total vertical movement only breaks ties
layout_cost <- function(orders){
  ranks <- rank_lanes(orders)
  crossings <- count_crossings(ranks)
  centers <- lane_centers(orders, ranks)
  movement <- sum(abs(centers[, -1] - centers[, -ncol(centers)]))
  crossings + movement / (n_facilities * column_height * length(stages))
}

# Local search from random starts: move one group, or two adjacent groups, to every other
#  position in its column. Columns are searched in random order until no move helps.
list_moves <- function(x){
  moves <- list()
  for(len in seq_len(min(2, length(x) - 1))){
    for(from in seq_len(length(x) - len + 1)){
      run <- x[from + seq_len(len) - 1]
      others <- setdiff(x, run)
      moves <- c(moves, lapply(0:length(others), function(i) append(others, run, i)))
    }
  }
  setdiff(unique(moves), list(x))
}
search_orders <- function(start){
  best <- start
  best_cost <- layout_cost(best)
  free_stages <- stages[sapply(FIXED_ORDERS[stages], is.null)]
  improved <- TRUE
  while(improved){
    improved <- FALSE
    for(s in sample(free_stages)){
      candidates <- lapply(list_moves(best[[s]]), function(new_order){
        utils::modifyList(best, stats::setNames(list(new_order), s))
      })
      costs <- vapply(candidates, layout_cost, numeric(1))
      if(min(costs) < best_cost - 1e-9){
        best <- candidates[[which.min(costs)]]
        best_cost <- min(costs)
        improved <- TRUE
      }
    }
  }
  list(orders = best, cost = best_cost)
}

# Translate profile colors (the config keys) to profile labels
if(!is.null(FIXED_ORDERS$`Phase C`)){
  is_color <- FIXED_ORDERS$`Phase C` %in% names(profile_labels)
  FIXED_ORDERS$`Phase C`[is_color] <- profile_labels[FIXED_ORDERS$`Phase C`[is_color]]
}
for(s in stages[!sapply(FIXED_ORDERS[stages], is.null)]){
  if(!setequal(FIXED_ORDERS[[s]], unique(facilities[[s]]))){
    stop('FIXED_ORDERS for ', s, ' must list: ', toString(sort(unique(facilities[[s]]))))
  }
}
set.seed(SEED)
all_fixed <- !any(sapply(FIXED_ORDERS[stages], is.null))
searches <- lapply(seq_len(if(all_fixed) 1L else N_RESTARTS), function(i){
  start <- lapply(stages, function(s){
    if(is.null(FIXED_ORDERS[[s]])) sample(unique(facilities[[s]])) else FIXED_ORDERS[[s]]
  }) |> stats::setNames(stages)
  search_orders(start)
})
costs <- sapply(searches, `[[`, 'cost')
orders <- searches[[which.min(costs)]]$orders
message(
  'Layout has ', floor(min(costs)), ' crossings',
  if(!all_fixed) sprintf(
    ', reached by %d of %d starts', sum(floor(costs) == floor(min(costs))), length(costs)
  )
)

# A layout and its mirror image cross equally often: put the first profile in the top half
if(
  all(sapply(FIXED_ORDERS, is.null)) &&
  match(profile_labels[1], orders$`Phase C`) > length(orders$`Phase C`) / 2
) orders <- lapply(orders, rev)

if(RENUMBER_GROUPS){
  for(s in c('Phase A', 'Phase B')){
    group_settings <- sub(' [0-9]+$', '', orders[[s]])
    new_names <- paste(group_settings, data.table::rowid(group_settings))
    facilities[[s]] <- new_names[match(facilities[[s]], orders[[s]])]
    orders[[s]] <- new_names
  }
}
print(orders)   # Groups as displayed, top to bottom


## LAY OUT NODES AND LANES -------------------------------------------------------------->

# Left and right edges of each column of nodes
node_widths <- c(BAR_WIDTH, BOX_WIDTH, BOX_WIDTH, BAR_WIDTH)
flow_width <- (
  FIG_WIDTH - LABEL_WIDTH_LEFT - LABEL_WIDTH_RIGHT - sum(node_widths)
) / (length(stages) - 1)
columns <- data.table::data.table(stage = stages, width = node_widths)
columns[, xmin := LABEL_WIDTH_LEFT + cumsum(c(0, width[-.N] + flow_width))]
columns[, xmax := xmin + width]

ranks <- rank_lanes(orders)
centers <- lane_centers(orders, ranks)

nodes <- lapply(stages, function(s){
  data.table::data.table(stage = s, group = facilities[[s]], y = centers[, s])[
    , .(n = .N, ymin = min(y) - lane_height / 2, ymax = max(y) + lane_height / 2),
    by = .(stage, group)
  ]
}) |> data.table::rbindlist()
nodes[columns, `:=` (xmin = i.xmin, xmax = i.xmax), on = 'stage']
nodes[, y_mid := (ymin + ymax) / 2]

# Smooth ribbon for each facility between each pair of adjacent columns
smoothstep <- function(t) t^2 * (3 - 2 * t)
t_seq <- seq(0, 1, length.out = 60)
half_lane <- lane_height * LANE_FILL / 2
lanes <- lapply(seq_len(length(stages) - 1), function(j){
  x_seq <- columns$xmax[j] + t_seq * (columns$xmin[j + 1] - columns$xmax[j])
  lapply(seq_len(n_facilities), function(i){
    y_seq <- centers[i, j] + smoothstep(t_seq) * (centers[i, j + 1] - centers[i, j])
    data.table::data.table(
      lane_id = paste(i, j),
      profile = facilities$`Phase C`[i],
      x = c(x_seq, rev(x_seq)),
      y = c(y_seq + half_lane, rev(y_seq - half_lane))
    )
  }) |> data.table::rbindlist()
}) |> data.table::rbindlist()

profile_fills <- c(
  stats::setNames(unlist(paper$profile_colors)[names(paper$profiles)], profile_labels),
  stats::setNames(EXCLUDED_COLOR, EXCLUDED_LABEL)
)
nodes[, fill := NODE_FILL]
nodes[stage == 'District', fill := DISTRICT_FILL]
nodes[stage == 'Phase C', fill := profile_fills[group]]

nodes[, label := paste0(group, ' (', n, ')')]
# Wrap Phase C labels, keeping each count on the same line as the word before it
nodes[stage == 'Phase C', label := sapply(group, function(x){
  paste(strwrap(x, LABEL_WRAP + 1), collapse = '\n')
})][stage == 'Phase C', label := paste0(label, ' (', n, ')')]
label_pad <- 0.06
boxed <- c('Phase A', 'Phase B')


## DRAW --------------------------------------------------------------------------------->

# Plot coordinates are inches on the page
draw_text <- function(data, x, hjust){
  ggplot2::geom_text(
    data = data, mapping = ggplot2::aes(x = {{ x }}, y = y_mid, label = label),
    hjust = hjust, size = LABEL_PT, size.unit = 'pt', family = FONT_FAMILY,
    color = INK, lineheight = LINE_HEIGHT
  )
}
fig <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data = lanes, mapping = ggplot2::aes(x = x, y = y, group = lane_id, fill = profile),
    alpha = LANE_ALPHA
  ) +
  ggplot2::scale_fill_manual(values = profile_fills, guide = 'none') +
  ggplot2::geom_rect(
    data = nodes[stage %in% boxed, ],
    mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NODE_FILL, color = NODE_LINE, linewidth = 0.25
  ) +
  ggplot2::geom_rect(
    data = nodes[!stage %in% boxed, ],
    mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = nodes[!stage %in% boxed, fill]
  ) +
  draw_text(nodes[stage == 'District', ], x = xmin - label_pad, hjust = 1) +
  draw_text(nodes[stage %in% boxed, ], x = (xmin + xmax) / 2, hjust = 0.5) +
  draw_text(nodes[stage == 'Phase C', ], x = xmax + label_pad, hjust = 0) +
  ggplot2::geom_text(
    data = columns,
    mapping = ggplot2::aes(
      x = (xmin + xmax) / 2, y = FIG_HEIGHT - TITLE_HEIGHT / 2, label = stage
    ),
    size = TITLE_PT, size.unit = 'pt', family = FONT_FAMILY, fontface = 'bold',
    color = INK
  ) +
  ggplot2::scale_x_continuous(limits = c(0, FIG_WIDTH), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0, FIG_HEIGHT), expand = c(0, 0)) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

ggplot2::ggsave(
  filename = config$get_file_path('paper', 'fig_phase_sankey'), plot = fig,
  width = FIG_WIDTH, height = FIG_HEIGHT, units = 'in', dpi = DPI, bg = 'white',
  device = ragg::agg_png
)
ggplot2::ggsave(
  filename = config$get_file_path('paper', 'fig_phase_sankey_pdf'), plot = fig,
  width = FIG_WIDTH, height = FIG_HEIGHT, units = 'in', bg = 'white',
  device = grDevices::cairo_pdf
)
data.table::fwrite(
  facilities[
    do.call(order, lapply(stages[4:1], function(s) match(get(s), orders[[s]]))),
    c('catchment_id', 'catchment_name', 'setting', stages, 'cluster_a', 'cluster_b'),
    with = FALSE
  ],
  config$get_file_path('paper', 'phase_sankey_data')
)
message('Wrote Sankey diagram to: ', config$get_file_path('paper', 'fig_phase_sankey'))
