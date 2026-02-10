suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fs)
})

# ==========================
# CONFIG
# ==========================
ROOT <- normalizePath(getwd(), winslash="/")
work_dir <- file.path(ROOT, "AllFilesNeeded Final")
box_centers_path <- file.path(work_dir, "box_centers.csv")
cond_map_xlsx <- file.path(work_dir, "Condition_map.xlsx")
output_dir <- file.path(ROOT, "_WORK_ALL5")
dir_create(output_dir, recurse = TRUE)

zip_out_mean <- file.path(ROOT, "final_plots_all_participants_mean_metric_WITH_P5.zip")
zip_out_min  <- file.path(ROOT, "final_plots_all_participants_min_metric_WITH_P5.zip")

# ==========================
# HELPERS
# ==========================
extract_zip <- function(zip_path, out_dir) {
  dir_create(out_dir, recurse = TRUE)
  tryCatch({
    unzip(zip_path, exdir = out_dir)
  }, error = function(e) {
    if (Sys.info()["sysname"] == "Windows") {
      ps_cmd <- sprintf('Expand-Archive -Path "%s" -DestinationPath "%s" -Force',
        normalizePath(zip_path, winslash = "\\"), normalizePath(out_dir, winslash = "\\"))
      system2("powershell", args = c("-Command", ps_cmd))
    }
  })
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
  if (is.na(hdr_i)) stop("Could not find TRC header row")

  df <- read.delim(path, header=TRUE, sep="\t", skip=hdr_i-1,
                   check.names=FALSE, stringsAsFactors=FALSE)

  # Fix column names - TRC has markers followed by blank columns for X,Y,Z
  nms <- names(df)
  for (i in seq_along(nms)) {
    if (i > 2 && (is.na(nms[i]) || trimws(nms[i]) == "")) {
      marker_idx <- max(which(!is.na(nms[1:(i-1)]) & trimws(nms[1:(i-1)]) != "" & 1:(i-1) > 2))
      if (length(marker_idx) > 0) {
        marker_name <- nms[marker_idx]
        coord_num <- i - marker_idx
        if (coord_num == 1) nms[i] <- paste0(marker_name, "_X")
        else if (coord_num == 2) nms[i] <- paste0(marker_name, "_Y")
        else if (coord_num == 3) nms[i] <- paste0(marker_name, "_Z")
        else nms[i] <- paste0("NA_C", i)
      } else {
        nms[i] <- paste0("NA_C", i)
      }
    }
  }
  names(df) <- nms
  df
}

extract_trialnum <- function(filename) {
  # Try V1 pattern (P4V11.trc -> 11, P5V110.trc -> 10)
  m <- str_match(filename, "V1(\\d+)\\.")
  if (!is.na(m[1,2])) {
    num <- as.integer(m[1,2])
    if (num >= 100) return(num %% 100)  # 110 -> 10
    return(num)
  }
  
  # Try Trial or _N_raw patterns
  m <- str_match(filename, "Trial(\\d+)|_(\\d+)_raw\\.")
  if (!is.na(m[1,1])) {
    for (i in 2:ncol(m)) {
      if (!is.na(m[1,i])) return(as.integer(m[1,i]))
    }
  }
  
  # Fallback
  m <- str_match(filename, "(\\d+)(?:\\D*?)\\.(trc|csv)$")
  if (is.na(m[1,2])) return(NA_integer_)
  num <- as.integer(m[1,2])
  if (num >= 100) return(num %% 100)
  num
}

safe_num <- function(x) suppressWarnings(as.numeric(x))
dist3 <- function(x,y,z, bx,by,bz) sqrt((x-bx)^2 + (y-by)^2 + (z-bz)^2)

# ==========================
# EXTRACT PARTICIPANT ZIPS
# ==========================
cat("Extracting participant ZIPs...\n")

inner_p1 <- file.path(work_dir, "P1.2AllCSV.zip")
inner_p2 <- file.path(work_dir, "P2AllCSV.zip")
inner_p3 <- file.path(work_dir, "P3AllCSV.zip")
inner_p4 <- file.path(work_dir, "P4allTRCfiles.zip")
inner_p5 <- file.path(work_dir, "P5allTRC.zip")

p1_dir <- file.path(output_dir, "P1.2")
p2_dir <- file.path(output_dir, "P2")
p3_dir <- file.path(output_dir, "P3")
p4_dir <- file.path(output_dir, "P4")
p5_dir <- file.path(output_dir, "P5")

dir_create(c(p1_dir,p2_dir,p3_dir,p4_dir,p5_dir), recurse=TRUE)

extract_zip(inner_p1, p1_dir)
extract_zip(inner_p2, p2_dir)
extract_zip(inner_p3, p3_dir)
extract_zip(inner_p4, p4_dir)
extract_zip(inner_p5, p5_dir)

# ==========================
# READ CONDITION MAP - FIXED ROW INDICES
# ==========================
cat("Reading condition map...\n")
cm_raw <- read_excel(cond_map_xlsx, sheet=1, col_names=FALSE)

# Row 1 = condition descriptions, Row 2 = condition keys, Rows 3-7 = participants
cond_desc <- as.character(cm_raw[1, 2:13] %>% unlist())
cond_keys <- as.character(cm_raw[2, 2:13] %>% unlist())

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
  x <- gsub("\\.", ",", as.character(x))  # Fix typos like "28. 29"
  as.integer(str_extract_all(x, "\\d+")[[1]])
}

participant_rows <- cm_raw[3:7, ]
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
    trial_cond_map <- bind_rows(trial_cond_map,
      tibble(Participant=part, TrialNum=trials, ConditionKey=key, TrialInBlock=seq_along(trials)))
  }
}

trial_cond_map <- trial_cond_map %>% left_join(cond_tbl, by="ConditionKey")

cat(sprintf("Loaded condition map: %d trial assignments across %d participants\n", 
            nrow(trial_cond_map), n_distinct(trial_cond_map$Participant)))

# ==========================
# READ BOX CENTERS
# ==========================
cat("Reading box centers...\n")
bc <- read_csv(box_centers_path, show_col_types = FALSE)
boxA_row <- bc %>% filter(Box == "A")
boxB_row <- bc %>% filter(Box == "B")
boxA <- c(boxA_row$X, boxA_row$Y, boxA_row$Z)
boxB <- c(boxB_row$X, boxB_row$Y, boxB_row$Z)

cat(sprintf("Box A: (%.2f, %.2f, %.2f)\n", boxA[1], boxA[2], boxA[3]))
cat(sprintf("Box B: (%.2f, %.2f, %.2f)\n", boxB[1], boxB[2], boxB[3]))

# ==========================
# CONVERT TRC FILES
# ==========================
convert_trcs <- function(trc_dir, participant) {
  cat(sprintf("Converting TRC files for %s...\n", participant))
  trcs <- dir_ls(trc_dir, recurse=TRUE, regexp="\\.trc$", type="file")
  out_dir <- file.path(trc_dir, "csv_converted")
  dir_create(out_dir, recurse=TRUE)

  converted <- 0
  for (f in trcs) {
    tnum <- extract_trialnum(basename(f))
    if (is.na(tnum)) next
    df <- read_trc(f) %>% mutate(Participant=participant, TrialNum=tnum)
    write_csv(df, file.path(out_dir, sprintf("%s_%d_raw.csv", participant, tnum)), progress=FALSE)
    converted <- converted + 1
  }
  cat(sprintf("  Converted %d TRC files\n", converted))
  out_dir
}

p4_raw_dir <- convert_trcs(p4_dir, "P4")
p5_raw_dir <- convert_trcs(p5_dir, "P5")

# ==========================
# DISCOVER ALL CSV FILES
# ==========================
find_raw_csvs <- function(dir, participant) {
  files <- dir_ls(dir, recurse=TRUE, regexp="\\.csv$", type="file")
  files <- files[str_detect(tolower(basename(files)), "raw")]
  
  tibble(
    Participant=participant,
    file=files,
    TrialNum=map_int(basename(files), ~extract_trialnum(.x))
  ) %>% filter(!is.na(TrialNum))
}

cat("\nDiscovering CSV files...\n")
all_files <- bind_rows(
  find_raw_csvs(p1_dir, "P1.2"),
  find_raw_csvs(p2_dir, "P2"),
  find_raw_csvs(p3_dir, "P3"),
  find_raw_csvs(p4_dir, "P4"),
  find_raw_csvs(p5_dir, "P5")
) %>% distinct(Participant, TrialNum, .keep_all=TRUE)

cat(sprintf("Total files: %d\n", nrow(all_files)))

all_files_mapped <- all_files %>%
  inner_join(trial_cond_map %>% select(Participant, TrialNum, ConditionKey), 
             by=c("Participant","TrialNum"))

cat(sprintf("Files with condition mappings: %d\n", nrow(all_files_mapped)))

# ==========================
# CALCULATE FRAMEWISE DISTANCES
# ==========================
cat("\nCalculating framewise distances...\n")

framewise_all <- tibble()

for (i in 1:nrow(all_files_mapped)) {
  if (i %% 30 == 0) cat(sprintf("  %d/%d\n", i, nrow(all_files_mapped)))
  
  f <- all_files_mapped$file[i]
  p <- all_files_mapped$Participant[i]
  tnum <- all_files_mapped$TrialNum[i]
  
  df <- tryCatch({
    read_csv(f, show_col_types = FALSE, progress = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(df)) next
  
  # Process S2 and C7 markers
  for (marker in c("S2", "C7")) {
    x_col <- paste0(marker, "_X")
    y_col <- paste0(marker, "_Y")
    z_col <- paste0(marker, "_Z")
    
    if (!all(c(x_col, y_col, z_col) %in% names(df))) next
    
    df_marker <- df %>%
      select(any_of(c("Frame", "Time", "Frame#")), all_of(c(x_col, y_col, z_col))) %>%
      rename(X = all_of(x_col), Y = all_of(y_col), Z = all_of(z_col)) %>%
      mutate(
        X = safe_num(X), Y = safe_num(Y), Z = safe_num(Z)
      ) %>%
      filter(is.finite(X) & is.finite(Y) & is.finite(Z)) %>%
      mutate(
        Participant = p,
        TrialNum = tnum,
        Marker = marker,
        dist_A = dist3(X, Y, Z, boxA[1], boxA[2], boxA[3]),
        dist_B = dist3(X, Y, Z, boxB[1], boxB[2], boxB[3])
      )
    
    framewise_all <- bind_rows(framewise_all, df_marker)
  }
}

cat(sprintf("Framewise data: %d rows\n", nrow(framewise_all)))

if (nrow(framewise_all) == 0) {
  stop("No framewise data generated!")
}

# Add condition metadata
framewise_all <- framewise_all %>%
  left_join(trial_cond_map, by=c("Participant","TrialNum"))

# ==========================
# COMPUTE TRIAL METRICS
# ==========================
cat("Computing trial-level metrics...\n")

trial_metrics <- framewise_all %>%
  group_by(Participant, TrialNum, ConditionKey, ConditionID, Mode, Composition, 
           Collider, TrialInBlock) %>%
  summarise(
    n_frames = n(),
    meanA = mean(dist_A, na.rm=TRUE),
    meanB = mean(dist_B, na.rm=TRUE),
    meanAB = (meanA + meanB) / 2,
    meanAminusB = meanA - meanB,
    minA = min(dist_A, na.rm=TRUE),
    minB = min(dist_B, na.rm=TRUE),
    minAB = (minA + minB) / 2,
    minAminusB = minA - minB,
    .groups="drop"
  )

cat(sprintf("Trial metrics: %d trials across %d participants\n", 
            nrow(trial_metrics), n_distinct(trial_metrics$Participant)))

# ==========================
# PLOTTING FUNCTIONS
# ==========================
theme_pro <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face="bold", size=14),
      axis.title = element_text(size=11),
      panel.grid.minor = element_blank()
    )
}

save_png <- function(p, path, w=10, h=6) {
  ggsave(path, p, width=w, height=h, dpi=150, bg="white")
}

p_to_star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  ""
}

cohens_d_onesample <- function(x, mu=0) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  (mean(x) - mu) / sd(x)
}

# ==========================
# ANALYSIS PACKAGE
# ==========================
make_package <- function(metric=c("meanAB","minAB")) {
  metric <- match.arg(metric)
  metric_name <- if_else(metric=="meanAB", "mean", "min")
  
  cat(sprintf("\n=== Creating package for %s metric ===\n", metric_name))
  
  val <- if (metric == "meanAB") "meanAB" else "minAB"
  val_AminusB <- if (metric == "meanAB") "meanAminusB" else "minAminusB"
  
  # Block summaries
  block_summary <- trial_metrics %>%
    group_by(Participant, ConditionKey, ConditionID, Mode, Composition, Collider) %>%
    summarise(
      n_trials = n(),
      metric = mean(.data[[val]], na.rm=TRUE),
      meanA = mean(meanA, na.rm=TRUE),
      meanB = mean(meanB, na.rm=TRUE),
      meanAminusB = mean(.data[[val_AminusB]], na.rm=TRUE),
      .groups="drop"
    )
  
  cat(sprintf("Blocks: %d across %d participants\n", 
              nrow(block_summary), n_distinct(block_summary$Participant)))
  
  # Within-block deltas
  wb <- trial_metrics %>%
    filter(TrialInBlock %in% c(1,3)) %>%
    arrange(Participant, ConditionKey, TrialInBlock) %>%
    group_by(Participant, ConditionKey) %>%
    filter(n() == 2) %>%
    summarise(
      delta_meanAB = diff(.data[[val]]),
      ConditionID = first(ConditionID),
      Mode = first(Mode),
      Composition = first(Composition),
      Collider = first(Collider),
      .groups="drop"
    )
  
  wb_ab <- trial_metrics %>%
    filter(TrialInBlock %in% c(1,3)) %>%
    arrange(Participant, ConditionKey, TrialInBlock) %>%
    group_by(Participant, ConditionKey) %>%
    filter(n() == 2) %>%
    summarise(
      delta_AminusB = diff(.data[[val_AminusB]]),
      ConditionID = first(ConditionID),
      Mode = first(Mode),
      Composition = first(Composition),
      Collider = first(Collider),
      .groups="drop"
    )
  
  # Statistics
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
  
  comp_anova_p <- tryCatch(
    anova(lm(metric ~ Composition, data=block_summary))$`Pr(>F)`[1], 
    error=function(e) NA_real_
  )
  comp_star <- p_to_star(comp_anova_p)
  
  mode_p <- tryCatch(
    t.test(metric ~ Mode, data=block_summary)$p.value, 
    error=function(e) NA_real_
  )
  mode_star <- p_to_star(mode_p)
  
  out_root <- file.path(output_dir, paste0("final_plots_all_participants_", metric_name))
  dir_create(out_root, recurse=TRUE)
  
  cat("Generating plots...\n")
  
  # 01: Composition
  p1 <- block_summary %>%
    group_by(Composition) %>%
    summarise(m=mean(metric, na.rm=TRUE),
              se=sd(metric, na.rm=TRUE)/sqrt(sum(is.finite(metric))), .groups="drop") %>%
    ggplot(aes(x=factor(Composition, levels=c("2R0V","1R1V","0R2V")), y=m)) +
    geom_col(alpha=0.85, fill="steelblue") +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste0("Composition effect on block meanAB — metric=", metric_name),
         subtitle=paste0("ANOVA p=", signif(comp_anova_p,3), " ", comp_star),
         x="Composition", y="Block meanAB") + theme_pro()
  save_png(p1, file.path(out_root, "01_composition_meanAB_all_participants.png"), 12, 6)
  
  # 02: 1R1V BoxA vs BoxB
  p2 <- block_summary %>%
    filter(Composition=="1R1V") %>%
    ggplot(aes(x=factor(Collider), y=meanAminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6, fill="coral") +
    labs(title=paste0("1R1V: BoxA−BoxB by collider — metric=", metric_name),
         x="Collider", y="Block mean(A−B)") + theme_pro()
  save_png(p2, file.path(out_root, "02_1R1V_boxA_vs_boxB_all_participants.png"), 12, 6)
  
  # 03: Collider effect
  p3 <- collider_eff %>%
    ggplot(aes(x=factor(Collider), y=mean_delta)) +
    geom_col(alpha=0.85, fill="darkgreen") +
    geom_errorbar(aes(ymin=mean_delta-se, ymax=mean_delta+se), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_text(aes(label=star, y=mean_delta+se*1.2), fontface="bold", size=6) +
    labs(title=paste0("Collider effect on Δ — metric=", metric_name),
         x="Collider", y="Δ meanAB") + theme_pro()
  save_png(p3, file.path(out_root, "03_collider_effect_withinblock_delta_all.png"), 12, 6)
  
  # 04: Mode effect
  p4 <- block_summary %>%
    group_by(Mode) %>%
    summarise(m=mean(metric, na.rm=TRUE),
              se=sd(metric, na.rm=TRUE)/sqrt(sum(is.finite(metric))), .groups="drop") %>%
    ggplot(aes(x=factor(Mode, levels=c("AR","VR")), y=m)) +
    geom_col(alpha=0.85, fill="purple") +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste0("Mode effect — metric=", metric_name),
         subtitle=paste0("t-test p=", signif(mode_p,3), " ", mode_star),
         x="Mode", y="Block meanAB") + theme_pro()
  save_png(p4, file.path(out_root, "04_mode_effect_blockmeans_meanAB_all.png"), 10, 6)
  
  # 05: Mode effect on delta
  p5 <- wb %>%
    group_by(Mode) %>%
    summarise(m=mean(delta_meanAB, na.rm=TRUE),
              se=sd(delta_meanAB, na.rm=TRUE)/sqrt(sum(is.finite(delta_meanAB))), .groups="drop") %>%
    ggplot(aes(x=factor(Mode, levels=c("AR","VR")), y=m)) +
    geom_col(alpha=0.85, fill="orange") +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(title=paste0("Mode effect on Δ — metric=", metric_name),
         x="Mode", y="Δ meanAB") + theme_pro()
  save_png(p5, file.path(out_root, "05_mode_effect_withinblock_delta_meanAB_all.png"), 10, 6)
  
  # 06: Reproducibility - deltas
  wb_consensus <- wb %>% group_by(ConditionKey) %>%
    summarise(consensus=mean(delta_meanAB, na.rm=TRUE), .groups="drop")
  wb_long <- wb %>% left_join(wb_consensus, by="ConditionKey") %>%
    filter(is.finite(delta_meanAB), is.finite(consensus))
  
  p6 <- ggplot(wb_long, aes(x=consensus, y=delta_meanAB)) +
    geom_point(alpha=0.75, color="steelblue") +
    geom_smooth(method="lm", se=TRUE, color="red") +
    labs(title=paste0("Repro: Δ vs consensus — metric=", metric_name),
         x="Consensus Δ", y="Participant Δ") + theme_pro()
  save_png(p6, file.path(out_root, "06_repro_scatter_withinblock_delta_all.png"), 10, 7)
  
  # 07: Reproducibility - blocks
  bm_cons <- block_summary %>% group_by(ConditionKey) %>%
    summarise(consensus=mean(metric, na.rm=TRUE), .groups="drop")
  bm_long <- block_summary %>% left_join(bm_cons, by="ConditionKey") %>%
    filter(is.finite(metric), is.finite(consensus))
  
  p7 <- ggplot(bm_long, aes(x=consensus, y=metric)) +
    geom_point(alpha=0.75, color="darkgreen") +
    geom_smooth(method="lm", se=TRUE, color="red") +
    labs(title=paste0("Repro: blocks — metric=", metric_name),
         x="Consensus", y="Participant block meanAB") + theme_pro()
  save_png(p7, file.path(out_root, "07_repro_scatter_blockmeans_all.png"), 10, 7)
  
  # 08: Early vs Late
  early_late <- block_summary %>%
    mutate(EarlyLate = if_else(ConditionID <= 6, "Early (c1–c6)", "Late (c7–c12)")) %>%
    group_by(EarlyLate) %>%
    summarise(m=mean(metric, na.rm=TRUE),
              se=sd(metric, na.rm=TRUE)/sqrt(sum(is.finite(metric))), .groups="drop")
  
  p8 <- ggplot(early_late, aes(x=EarlyLate, y=m)) +
    geom_col(alpha=0.85, fill="coral") +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste0("Early vs Late — metric=", metric_name),
         x="", y="Mean block meanAB") + theme_pro()
  save_png(p8, file.path(out_root, "08_early_vs_late_meanAB_all.png"), 10, 6)
  
  # 09: BoxA-BoxB by collider
  p9 <- block_summary %>%
    ggplot(aes(x=factor(Collider), y=meanAminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6, fill="purple") +
    labs(title=paste0("BoxA−BoxB by collider — metric=", metric_name),
         x="Collider", y="Block mean(A−B)") + theme_pro()
  save_png(p9, file.path(out_root, "09_boxAminusB_by_collider_blockmeans_all.png"), 12, 6)
  
  # 10: Delta A-B
  p10 <- wb_ab %>%
    ggplot(aes(x=factor(Collider), y=delta_AminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6, fill="orange") +
    labs(title=paste0("Δ(A−B) by collider — metric=", metric_name),
         x="Collider", y="Δ(A−B)") + theme_pro()
  save_png(p10, file.path(out_root, "10_withinblock_delta_AminusB_all.png"), 12, 6)
  
  # 11: Trajectory
  traj <- trial_metrics %>%
    group_by(ConditionID, TrialInBlock) %>%
    summarise(m=mean(.data[[val]], na.rm=TRUE), .groups="drop")
  
  p11 <- ggplot(traj, aes(x=ConditionID, y=m, color=factor(TrialInBlock))) +
    geom_line(alpha=0.8, size=1) +
    geom_point(size=2) +
    labs(title=paste0("Block trajectory — metric=", metric_name),
         x="ConditionID", y="Trial metric", color="Trial in Block") +
    theme_pro()
  save_png(p11, file.path(out_root, "11_block_to_block_trajectory_meanAB_all.png"), 12, 6)
  
  # Write CSVs
  write_csv(block_summary, file.path(out_root, "block_summary.csv"))
  write_csv(wb, file.path(out_root, "withinblock_delta_meanAB.csv"))
  write_csv(wb_ab, file.path(out_root, "withinblock_delta_AminusB.csv"))
  
  # Sanity check
  sanity <- list(
    metric = metric,
    participants = sort(unique(trial_metrics$Participant)),
    n_trials_per_participant = trial_metrics %>% 
      group_by(Participant) %>% 
      summarise(n=n(), .groups="drop") %>% 
      deframe() %>% 
      as.list(),
    missing_trials_per_participant = trial_cond_map %>%
      anti_join(trial_metrics, by=c("Participant", "TrialNum")) %>%
      group_by(Participant) %>%
      summarise(missing = list(TrialNum), .groups="drop") %>%
      deframe() %>%
      as.list()
  )
  
  writeLines(jsonlite::toJSON(sanity, pretty=TRUE, auto_unbox=FALSE),
             file.path(out_root, "sanity_checks.json"))
  
  cat(sprintf("✓ Package saved: %s\n", out_root))
  out_root
}

# ==========================
# RUN BOTH METRICS
# ==========================
out_mean_dir <- make_package("meanAB")
out_min_dir <- make_package("minAB")

# ZIP OUTPUTS
cat("\nCreating ZIP files...\n")
setwd(output_dir)
zip(zip_out_mean, files=dir_ls(out_mean_dir, recurse=TRUE), flags="-r9Xq")
zip(zip_out_min, files=dir_ls(out_min_dir, recurse=TRUE), flags="-r9Xq")
setwd(ROOT)

cat("\n========================================\n")
cat("COMPLETE! ALL 5 PARTICIPANTS INCLUDED!\n")
cat("========================================\n")
cat("Results:\n")
cat("  MEAN:", zip_out_mean, "\n")
cat("  MIN:", zip_out_min, "\n")
cat("\nParticipants: P1.2, P2, P3, P4, P5\n")
cat("Total trials:", nrow(trial_metrics), "\n")
