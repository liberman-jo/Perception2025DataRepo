suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fs)
})

# ==========================
# CONFIG
# ==========================
ROOT <- normalizePath(getwd(), winslash="/")

outer_zip <- file.path(ROOT, "AllFilesNeeded Final.zip")
box_centers_path <- file.path(ROOT, "box_centers.csv")
cond_map_xlsx <- file.path(ROOT, "Condition_map.xlsx")

work_dir <- file.path(ROOT, "_WORK_ALL5")
dir_create(work_dir, recurse = TRUE)

# Output zips
zip_out_mean <- file.path(ROOT, "final_plots_all_participants_mean_metric.zip")
zip_out_min  <- file.path(ROOT, "final_plots_all_participants_min_metric.zip")

# ==========================
# HELPERS
# ==========================
extract_zip <- function(zip_path, out_dir) {
  dir_create(out_dir, recurse = TRUE)
  unzip(zip_path, exdir = out_dir)
}

find_trc_header_row <- function(lines) {
  idx <- which(grepl("Frame\\s*#|Frame#", lines, ignore.case=TRUE) &
                 grepl("\\bTime\\b", lines, ignore.case=TRUE))
  if (length(idx)==0) return(NA_integer_)
  idx[1]
}

read_trc <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hdr_i <- find_trc_header_row(lines)
  if (is.na(hdr_i)) stop("Could not find TRC header row with Frame#/Time in: ", basename(path))

  df <- read.delim(path, header=TRUE, sep="\t", skip=hdr_i-1,
                   check.names=FALSE, stringsAsFactors=FALSE)

  # Fix blank/NA column names (this was your mutate() crash before)
  nms <- names(df)
  bad <- which(is.na(nms) | trimws(nms)=="")

  if (length(bad) > 0) {
    for (k in seq_along(bad)) {
      j <- bad[k]
      nms[j] <- paste0("NA_C", j)
    }
    names(df) <- nms
  }

  df
}

extract_trialnum <- function(filename) {
  # supports: P4V11.trc, P5V12.trc, P3_2_raw.csv, P1.2Trial1_raw.csv, etc.
  m <- str_match(filename, "(\\d+)(?:\\D*?)\\.(trc|csv)$")
  if (is.na(m[1,2])) return(NA_integer_)
  as.integer(m[1,2])
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

dist3 <- function(x,y,z, bx,by,bz) sqrt((x-bx)^2 + (y-by)^2 + (z-bz)^2)

# ==========================
# UNZIP OUTER PACKAGE
# ==========================
extract_zip(outer_zip, work_dir)

# Expected inner zips (your standard structure)
inner_p1 <- file.path(work_dir, "P1.2AllCSV.zip")
inner_p2 <- file.path(work_dir, "P2AllCSV.zip")
inner_p3 <- file.path(work_dir, "P3AllCSV.zip")
inner_p4 <- file.path(work_dir, "P4allTRCfiles.zip")
inner_p5 <- file.path(work_dir, "P5allTRC.zip")

p1_dir <- file.path(work_dir, "P1.2")
p2_dir <- file.path(work_dir, "P2")
p3_dir <- file.path(work_dir, "P3")
p4_dir <- file.path(work_dir, "P4")
p5_dir <- file.path(work_dir, "P5")

dir_create(c(p1_dir,p2_dir,p3_dir,p4_dir,p5_dir), recurse=TRUE)

extract_zip(inner_p1, p1_dir)
extract_zip(inner_p2, p2_dir)
extract_zip(inner_p3, p3_dir)
extract_zip(inner_p4, p4_dir)
extract_zip(inner_p5, p5_dir)

# ==========================
# READ CONDITION MAP
# ==========================
cm_raw <- read_excel(cond_map_xlsx, sheet=1, col_names=FALSE)

# rows: 4 = descriptions, 5 = keys, then participants
cond_desc <- as.character(cm_raw[4, 2:13] %>% unlist())
cond_keys <- as.character(cm_raw[5, 2:13] %>% unlist())

cond_tbl <- tibble(
  ConditionKey = cond_keys,
  ConditionDesc = cond_desc
) %>%
  mutate(
    Mode = if_else(str_starts(toupper(ConditionDesc), "AR"), "AR", "VR"),
    RealN = as.integer(str_extract(ConditionDesc, "(?<=\\s)\\d+(?=\\s+Real)")),
    VirtualN = as.integer(str_extract(ConditionDesc, "(?<=Real\\s)\\d+(?=\\s+Virtual)")),
    Collider = as.integer(str_extract(ConditionDesc, "(?<=,\\s)\\d+(?=\\s+Have)")),
    Composition = paste0(RealN,"R",VirtualN,"V"),
    ConditionID = as.integer(str_extract(ConditionKey, "\\d+"))
  )

parse_trials_cell <- function(x) {
  if (is.na(x) || is.null(x)) return(integer(0))
  as.integer(str_extract_all(as.character(x), "\\d+")[[1]])
}

participant_rows <- cm_raw[6:nrow(cm_raw), ]
trial_cond_map <- tibble()

for (r in 1:nrow(participant_rows)) {
  p <- participant_rows[r,1] %>% as.character() %>% str_trim()
  if (is.na(p) || p=="") next
  if (!(tolower(p) %in% c("p1.2","p2","p3","p4","p5"))) next

  part <- if (tolower(p)=="p1.2") "P1.2" else toupper(p)

  for (j in 1:12) {
    key <- cond_keys[j]
    trials <- parse_trials_cell(participant_rows[r, j+1] %>% unlist())

    if (length(trials)==0) next
    trial_cond_map <- bind_rows(
      trial_cond_map,
      tibble(Participant=part, TrialNum=trials, ConditionKey=key, TrialInBlock=seq_along(trials))
    )
  }
}

trial_cond_map <- trial_cond_map %>% left_join(cond_tbl, by="ConditionKey")

# ==========================
# READ BOX CENTERS
# ==========================
bc <- read_csv(box_centers_path, show_col_types = FALSE)
boxA <- bc %>% slice(1) %>% select(BoxA_X,BoxA_Y,BoxA_Z) %>% unlist() %>% as.numeric()
boxB <- bc %>% slice(1) %>% select(BoxB_X,BoxB_Y,BoxB_Z) %>% unlist() %>% as.numeric()

# ==========================
# MAKE RAW CSVs FOR P4/P5 FROM TRC
# ==========================
convert_trcs <- function(trc_dir, participant) {
  trcs <- dir_ls(trc_dir, recurse=TRUE, regexp="\\.trc$", type="file")
  out_dir <- file.path(trc_dir, "box_center_outputs", "csv_per_trial_raw_trc")
  dir_create(out_dir, recurse=TRUE)

  for (f in trcs) {
    tnum <- extract_trialnum(basename(f))
    if (is.na(tnum)) next
    df <- read_trc(f) %>%
      mutate(Participant=participant, TrialNum=tnum)
    write_csv(df, file.path(out_dir, sprintf("%s_%d_raw.csv", participant, tnum)))
  }
  out_dir
}

p4_raw_dir <- convert_trcs(p4_dir, "P4")
p5_raw_dir <- convert_trcs(p5_dir, "P5")

# P1–P3 raw csv dirs are inside their zips already; discover them:
find_raw_csvs <- function(dir, participant) {
  files <- dir_ls(dir, recurse=TRUE, regexp="\\.csv$", type="file")
  # keep only per-trial raw (heuristic)
  files <- files[str_detect(tolower(basename(files)), "raw")]
  tibble(
    Participant=participant,
    file=files,
    TrialNum=map_int(basename(files), extract_trialnum)
  ) %>% filter(!is.na(TrialNum))
}

p1_files <- find_raw_csvs(p1_dir, "P1.2")
p2_files <- find_raw_csvs(p2_dir, "P2")
p3_files <- find_raw_csvs(p3_dir, "P3")
p4_files <- find_raw_csvs(p4_dir, "P4")
p5_files <- find_raw_csvs(p5_dir, "P5")

all_files <- bind_rows(p1_files,p2_files,p3_files,p4_files,p5_files) %>%
  distinct(Participant, TrialNum, .keep_all=TRUE)

# ==========================
# FRAMEWISE DISTANCES (S2/C7 → Box A/B centers)
# ==========================
make_framewise <- function(path, participant, trialnum) {
  df <- read_csv(path, show_col_types = FALSE)

  # allow Frame or Frame#
  frame_col <- names(df)[str_detect(names(df), regex("^Frame", ignore_case=TRUE))][1]
  time_col  <- names(df)[str_detect(names(df), regex("^Time$", ignore_case=TRUE))][1]

  # required marker cols
  need <- c("S2_X","S2_Y","S2_Z","C7_X","C7_Y","C7_Z")
  if (!all(need %in% names(df))) {
    return(NULL)
  }

  df <- df %>%
    mutate(
      Frame = safe_num(.data[[frame_col]]),
      Time  = safe_num(.data[[time_col]]),
      S2_X = safe_num(S2_X), S2_Y=safe_num(S2_Y), S2_Z=safe_num(S2_Z),
      C7_X = safe_num(C7_X), C7_Y=safe_num(C7_Y), C7_Z=safe_num(C7_Z),

      distS2_A = dist3(S2_X,S2_Y,S2_Z, boxA[1],boxA[2],boxA[3]),
      distS2_B = dist3(S2_X,S2_Y,S2_Z, boxB[1],boxB[2],boxB[3]),
      distC7_A = dist3(C7_X,C7_Y,C7_Z, boxA[1],boxA[2],boxA[3]),
      distC7_B = dist3(C7_X,C7_Y,C7_Z, boxB[1],boxB[2],boxB[3]),

      meanAB_S2 = (distS2_A + distS2_B)/2,
      AminusB_S2 = distS2_A - distS2_B,

      Participant = participant,
      TrialNum = trialnum
    ) %>%
    select(Participant, TrialNum, Frame, Time,
           distS2_A, distS2_B, meanAB_S2, AminusB_S2,
           distC7_A, distC7_B)

  df
}

cat("[BUILD] Framewise for all participants...\n")
framewise_list <- vector("list", nrow(all_files))
for (i in 1:nrow(all_files)) {
  fw <- make_framewise(all_files$file[i], all_files$Participant[i], all_files$TrialNum[i])
  framewise_list[[i]] <- fw
}

framewise_all <- bind_rows(framewise_list)

# keep only trials that are mapped to conditions
framewise_all <- framewise_all %>%
  inner_join(trial_cond_map %>% select(Participant, TrialNum, ConditionKey, TrialInBlock, Mode, Composition, Collider, ConditionID),
             by=c("Participant","TrialNum"))

# ==========================
# SUMMARIES (MEAN metric vs MIN metric)
# ==========================
trial_summary_mean <- framewise_all %>%
  group_by(Participant, TrialNum, ConditionKey, TrialInBlock, Mode, Composition, Collider, ConditionID) %>%
  summarise(
    n_frames = n(),
    meanAB = mean(meanAB_S2, na.rm=TRUE),
    meanAminusB = mean(AminusB_S2, na.rm=TRUE),
    sdAB = sd(meanAB_S2, na.rm=TRUE),
    time_span = max(Time, na.rm=TRUE) - min(Time, na.rm=TRUE),
    .groups="drop"
  )

trial_summary_min <- framewise_all %>%
  group_by(Participant, TrialNum, ConditionKey, TrialInBlock, Mode, Composition, Collider, ConditionID) %>%
  summarise(
    n_frames = n(),
    # LOCKED DEFINITION:
    minA = min(distS2_A, na.rm=TRUE),
    minB = min(distS2_B, na.rm=TRUE),
    minAB = (minA + minB)/2,
    minAminusB = minA - minB,
    time_span = max(Time, na.rm=TRUE) - min(Time, na.rm=TRUE),
    .groups="drop"
  )

withinblock_delta <- function(trial_summary, value_col) {
  trial_summary %>%
    filter(TrialInBlock %in% c(1,3)) %>%
    select(Participant, ConditionKey, ConditionID, Mode, Composition, Collider, TrialInBlock, !!sym(value_col)) %>%
    pivot_wider(names_from=TrialInBlock, values_from=!!sym(value_col), names_prefix="Trial") %>%
    mutate(delta = Trial3 - Trial1) %>%
    arrange(Participant, ConditionID)
}

# ==========================
# PLOTTING / PACKAGE BUILDER
# ==========================
theme_pro <- function() {
  theme_bw(base_size=13) +
    theme(
      plot.title = element_text(face="bold"),
      axis.title = element_text(face="bold"),
      strip.text = element_text(face="bold"),
      panel.grid.minor = element_blank()
    )
}

save_png <- function(p, path, w=12, h=6) {
  ggsave(path, p, width=w, height=h, dpi=300)
}

make_package <- function(metric = c("mean","min")) {
  metric <- match.arg(metric)

  if (metric=="mean") {
    ts <- trial_summary_mean
    val <- "meanAB"
    ab  <- "meanAminusB"
    wb  <- withinblock_delta(ts, val) %>% rename(delta_meanAB = delta)
    wb_ab <- withinblock_delta(ts, ab) %>% rename(delta_AminusB = delta)
  } else {
    ts <- trial_summary_min %>% rename(meanAB = minAB, meanAminusB = minAminusB)
    val <- "meanAB"
    ab  <- "meanAminusB"
    wb  <- withinblock_delta(ts, val) %>% rename(delta_meanAB = delta)
    wb_ab <- withinblock_delta(ts, ab) %>% rename(delta_AminusB = delta)
  }

  # block means
  block_summary <- ts %>%
    group_by(Participant, ConditionKey, ConditionID, Mode, Composition, Collider) %>%
    summarise(
      n_trials = n_distinct(TrialNum),
      mean_AB = mean(.data[[val]], na.rm=TRUE),
      mean_AminusB = mean(.data[[ab]], na.rm=TRUE),
      .groups="drop"
    ) %>% arrange(Participant, ConditionID)

  # effect tests (consensus-level: all participants pooled)
  # NOTE: not "between participants" comparisons; just pooled effects by factor.
  cohens_d_onesample <- function(x, mu=0) {
    x <- x[is.finite(x)]
    if (length(x) < 2) return(NA_real_)
    (mean(x) - mu) / sd(x)
  }

  p_to_star <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    ""
  }

  # Collider effect on within-block delta (pooled)
  collider_eff <- wb %>%
    group_by(Collider) %>%
    summarise(
      n = sum(is.finite(delta_meanAB)),
      mean_delta = mean(delta_meanAB, na.rm=TRUE),
      sd = sd(delta_meanAB, na.rm=TRUE),
      se = sd/sqrt(n),
      p = {x <- delta_meanAB[is.finite(delta_meanAB)];
           if (length(x) < 2) NA_real_ else t.test(x, mu=0)$p.value},
      d = cohens_d_onesample(delta_meanAB, 0),
      star = p_to_star(p),
      .groups="drop"
    )

  # Composition effect on block means (pooled ANOVA)
  comp_anova_p <- tryCatch(anova(lm(mean_AB ~ Composition, data=block_summary))$`Pr(>F)`[1], error=function(e) NA_real_)
  comp_star <- p_to_star(comp_anova_p)

  # Mode effect (AR vs VR) on block means (pooled)
  mode_p <- tryCatch(t.test(mean_AB ~ Mode, data=block_summary)$p.value, error=function(e) NA_real_)
  mode_star <- p_to_star(mode_p)

  out_root <- file.path(work_dir, ifelse(metric=="mean","final_plots_all_participants_MEAN","final_plots_all_participants_MIN"))
  out_png  <- file.path(out_root, "PNGS")
  dir_create(out_png, recurse=TRUE)

  # --- PLOTS (11) ---
  # 01 composition meanAB
  p1 <- block_summary %>%
    group_by(Composition) %>%
    summarise(mean_val=mean(mean_AB, na.rm=TRUE),
              se=sd(mean_AB, na.rm=TRUE)/sqrt(sum(is.finite(mean_AB))),
              .groups="drop") %>%
    ggplot(aes(x=factor(Composition, levels=c("2R0V","1R1V","0R2V")),
               y=mean_val)) +
    geom_col(alpha=0.85) +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    labs(
      title=paste0("Composition effect (pooled) on block meanAB — metric=", metric),
      subtitle=paste0("ANOVA p=", signif(comp_anova_p,3), " ", comp_star),
      x="Composition", y="Block meanAB"
    ) + theme_pro()
  save_png(p1, file.path(out_root, "01_composition_meanAB_all_participants.png"), 12, 6)

  # 02 1R1V boxA vs boxB (pooled on mean_AminusB)
  p2 <- block_summary %>%
    filter(Composition=="1R1V") %>%
    ggplot(aes(x=factor(Collider), y=mean_AminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6) +
    labs(title=paste0("1R1V: BoxA−BoxB asymmetry by collider — metric=", metric),
         x="Collider", y="Block mean(A−B)") + theme_pro()
  save_png(p2, file.path(out_root, "02_1R1V_boxA_vs_boxB_all_participants.png"), 12, 6)

  # 03 collider effect withinblock delta
  p3 <- collider_eff %>%
    ggplot(aes(x=factor(Collider), y=mean_delta)) +
    geom_col(alpha=0.85) +
    geom_errorbar(aes(ymin=mean_delta-se, ymax=mean_delta+se), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_text(aes(label=star, y=mean_delta+se*1.2), fontface="bold", size=6) +
    labs(title=paste0("Collider effect on within-block Δ (Trial3−Trial1) — metric=", metric),
         x="Collider", y="Δ meanAB") + theme_pro()
  save_png(p3, file.path(out_root, "03_collider_effect_withinblock_delta_all.png"), 12, 6)

  # 04 mode effect on block means
  p4 <- block_summary %>%
    group_by(Mode) %>%
    summarise(mean_val=mean(mean_AB, na.rm=TRUE),
              se=sd(mean_AB, na.rm=TRUE)/sqrt(sum(is.finite(mean_AB))),
              .groups="drop") %>%
    ggplot(aes(x=factor(Mode, levels=c("AR","VR")), y=mean_val)) +
    geom_col(alpha=0.85) +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    labs(title=paste0("Mode effect (pooled) on block meanAB — metric=", metric),
         subtitle=paste0("t-test p=", signif(mode_p,3), " ", mode_star),
         x="Mode", y="Block meanAB") + theme_pro()
  save_png(p4, file.path(out_root, "04_mode_effect_blockmeans_meanAB_all.png"), 10, 6)

  # 05 mode effect withinblock delta
  p5 <- wb %>%
    left_join(cond_tbl %>% select(ConditionKey, Mode), by="ConditionKey") %>%
    group_by(Mode) %>%
    summarise(mean_val=mean(delta_meanAB, na.rm=TRUE),
              se=sd(delta_meanAB, na.rm=TRUE)/sqrt(sum(is.finite(delta_meanAB))),
              .groups="drop") %>%
    ggplot(aes(x=factor(Mode, levels=c("AR","VR")), y=mean_val)) +
    geom_col(alpha=0.85) +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(title=paste0("Mode effect (pooled) on within-block Δ — metric=", metric),
         x="Mode", y="Δ meanAB") + theme_pro()
  save_png(p5, file.path(out_root, "05_mode_effect_withinblock_delta_meanAB_all.png"), 10, 6)

  # 06 repro scatter withinblock delta (participant-to-consensus correlation)
  wb_wide <- wb %>% select(Participant, ConditionKey, delta_meanAB) %>%
    pivot_wider(names_from=Participant, values_from=delta_meanAB)
  wb_consensus <- wb %>% group_by(ConditionKey) %>%
    summarise(consensus=mean(delta_meanAB, na.rm=TRUE), .groups="drop")

  wb_long <- wb %>% left_join(wb_consensus, by="ConditionKey") %>%
    filter(is.finite(delta_meanAB), is.finite(consensus))

  p6 <- ggplot(wb_long, aes(x=consensus, y=delta_meanAB)) +
    geom_point(alpha=0.75) +
    geom_smooth(method="lm", se=TRUE) +
    labs(title=paste0("Repro: within-block Δ vs consensus (all points) — metric=", metric),
         x="Consensus Δ (condition mean)", y="Participant Δ") + theme_pro()
  save_png(p6, file.path(out_root, "06_repro_scatter_withinblock_delta_all.png"), 10, 7)

  # 07 repro scatter blockmeans
  bm_cons <- block_summary %>% group_by(ConditionKey) %>%
    summarise(consensus=mean(mean_AB, na.rm=TRUE), .groups="drop")
  bm_long <- block_summary %>% left_join(bm_cons, by="ConditionKey") %>%
    filter(is.finite(mean_AB), is.finite(consensus))

  p7 <- ggplot(bm_long, aes(x=consensus, y=mean_AB)) +
    geom_point(alpha=0.75) +
    geom_smooth(method="lm", se=TRUE) +
    labs(title=paste0("Repro: block meanAB vs consensus — metric=", metric),
         x="Consensus block meanAB", y="Participant block meanAB") + theme_pro()
  save_png(p7, file.path(out_root, "07_repro_scatter_blockmeans_all.png"), 10, 7)

  # 08 early vs late meanAB (block order)
  # define "early" = c1..c6, "late" = c7..c12 (by ConditionID)
  early_late <- block_summary %>%
    mutate(EarlyLate = if_else(ConditionID <= 6, "Early (c1–c6)", "Late (c7–c12)")) %>%
    group_by(EarlyLate) %>%
    summarise(mean_val=mean(mean_AB, na.rm=TRUE),
              se=sd(mean_AB, na.rm=TRUE)/sqrt(sum(is.finite(mean_AB))),
              .groups="drop")

  p8 <- ggplot(early_late, aes(x=EarlyLate, y=mean_val)) +
    geom_col(alpha=0.85) +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    labs(title=paste0("Early vs Late blocks (pooled) — metric=", metric),
         x="", y="Mean block meanAB") + theme_pro()
  save_png(p8, file.path(out_root, "08_early_vs_late_meanAB_all.png"), 10, 6)

  # 09 boxAminusB by collider (block means)
  p9 <- block_summary %>%
    ggplot(aes(x=factor(Collider), y=mean_AminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6) +
    labs(title=paste0("BoxA−BoxB by collider (block means) — metric=", metric),
         x="Collider", y="Block mean(A−B)") + theme_pro()
  save_png(p9, file.path(out_root, "09_boxAminusB_by_collider_blockmeans_all.png"), 12, 6)

  # 10 withinblock delta AminusB
  p10 <- wb_ab %>%
    ggplot(aes(x=factor(Collider), y=delta_AminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6) +
    labs(title=paste0("Within-block Δ(A−B) by collider — metric=", metric),
         x="Collider", y="Δ(A−B)") + theme_pro()
  save_png(p10, file.path(out_root, "10_withinblock_delta_AminusB_all.png"), 12, 6)

  # 11 block-to-block trajectory (TrialInBlock mean, by ConditionID)
  traj <- ts %>%
    group_by(ConditionID, TrialInBlock) %>%
    summarise(mean_val=mean(.data[[val]], na.rm=TRUE), .groups="drop")

  p11 <- ggplot(traj, aes(x=ConditionID, y=mean_val, group=TrialInBlock)) +
    geom_line(alpha=0.8) +
    geom_point(size=2) +
    labs(title=paste0("Block trajectory by trial-in-block (pooled) — metric=", metric),
         x="ConditionID (block order)", y="Trial metric value") + theme_pro()
  save_png(p11, file.path(out_root, "11_block_to_block_trajectory_meanAB_all.png"), 12, 6)

  # Write CSVs + sanity
  write_csv(block_summary, file.path(out_root, "block_summary.csv"))
  write_csv(wb %>% select(Participant,ConditionKey,ConditionID,Mode,Composition,Collider,delta_meanAB),
            file.path(out_root, "withinblock_delta_meanAB.csv"))
  write_csv(wb_ab %>% select(Participant,ConditionKey,ConditionID,Mode,Composition,Collider,delta_AminusB),
            file.path(out_root, "withinblock_delta_AminusB.csv"))

  sanity <- list(
    metric=metric,
    participants=sort(unique(ts$Participant)),
    n_framewise=nrow(framewise_all),
    n_trials=nrow(ts),
    n_blocks=nrow(block_summary)
  )
  writeLines(jsonlite::toJSON(sanity, pretty=TRUE, auto_unbox=TRUE),
             file.path(out_root, "sanity_checks.json"))

  out_root
}

out_mean_dir <- make_package("mean")
out_min_dir  <- make_package("min")

# ZIP BOTH PACKAGES
zip(zip_out_mean, files=dir_ls(out_mean_dir, recurse=TRUE), flags="-r9Xq")
zip(zip_out_min,  files=dir_ls(out_min_dir,  recurse=TRUE), flags="-r9Xq")

cat("\nDONE.\n")
cat("MEAN zip:", zip_out_mean, "\n")
cat("MIN  zip:", zip_out_min, "\n")
