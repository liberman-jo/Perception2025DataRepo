suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fs)
})

# ==========================
# CONFIG - UPDATED FOR YOUR FOLDER
# ==========================
ROOT <- normalizePath(getwd(), winslash="/")

# FIXED: Use the actual folder name "AllFilesNeeded Final" (with space)
work_dir <- file.path(ROOT, "AllFilesNeeded Final")
box_centers_path <- file.path(work_dir, "box_centers.csv")
cond_map_xlsx <- file.path(work_dir, "Condition_map.xlsx")

output_dir <- file.path(ROOT, "_WORK_ALL5")
dir_create(output_dir, recurse = TRUE)

# Output zips
zip_out_mean <- file.path(ROOT, "final_plots_all_participants_mean_metric.zip")
zip_out_min  <- file.path(ROOT, "final_plots_all_participants_min_metric.zip")

# ==========================
# HELPERS
# ==========================
# Windows-friendly extraction
extract_zip <- function(zip_path, out_dir) {
  dir_create(out_dir, recurse = TRUE)
  
  tryCatch({
    unzip(zip_path, exdir = out_dir)
  }, error = function(e) {
    # Try PowerShell on Windows
    if (Sys.info()["sysname"] == "Windows") {
      ps_cmd <- sprintf(
        'Expand-Archive -Path "%s" -DestinationPath "%s" -Force',
        normalizePath(zip_path, winslash = "\\"),
        normalizePath(out_dir, winslash = "\\")
      )
      system2("powershell", args = c("-Command", ps_cmd))
    } else {
      stop(paste("Failed to extract:", basename(zip_path)))
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
  if (is.na(hdr_i)) stop("Could not find TRC header row with Frame#/Time in: ", basename(path))

  df <- read.delim(path, header=TRUE, sep="\t", skip=hdr_i-1,
                   check.names=FALSE, stringsAsFactors=FALSE)

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

extract_trialnum <- function(filename, participant = NULL) {
  m <- str_match(filename, "Trial(\\d+)|_(\\d+)_raw\\.|V1(\\d+)\\.")
  
  if (!is.na(m[1,1])) {
    for (i in 2:ncol(m)) {
      if (!is.na(m[1,i])) {
        return(as.integer(m[1,i]))
      }
    }
  }
  
  m <- str_match(filename, "(\\d+)(?:\\D*?)\\.(trc|csv)$")
  if (is.na(m[1,2])) return(NA_integer_)
  as.integer(m[1,2])
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
# READ CONDITION MAP
# ==========================
cat("Reading condition map...\n")
cm_raw <- read_excel(cond_map_xlsx, sheet=1, col_names=FALSE)

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
  x <- gsub("\\.", ",", as.character(x))
  as.integer(str_extract_all(x, "\\d+")[[1]])
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

cat(sprintf("Loaded condition map: %d trial assignments\n", nrow(trial_cond_map)))

# ==========================
# READ BOX CENTERS
# ==========================
cat("Reading box centers...\n")

bc <- read_csv(box_centers_path, show_col_types = FALSE)

boxA_row <- bc %>% filter(Box == "A")
boxB_row <- bc %>% filter(Box == "B")

boxA <- c(boxA_row$X, boxA_row$Y, boxA_row$Z)
boxB <- c(boxB_row$X, boxB_row$Y, boxB_row$Z)

cat(sprintf("Box A center: (%.2f, %.2f, %.2f)\n", boxA[1], boxA[2], boxA[3]))
cat(sprintf("Box B center: (%.2f, %.2f, %.2f)\n", boxB[1], boxB[2], boxB[3]))

# ==========================
# MAKE RAW CSVs FOR P4/P5 FROM TRC
# ==========================
convert_trcs <- function(trc_dir, participant) {
  cat(sprintf("Converting TRC files for %s...\n", participant))
  trcs <- dir_ls(trc_dir, recurse=TRUE, regexp="\\.trc$", type="file")
  out_dir <- file.path(trc_dir, "csv_converted")
  dir_create(out_dir, recurse=TRUE)

  converted_count <- 0
  for (f in trcs) {
    tnum <- extract_trialnum(basename(f), participant)
    if (is.na(tnum)) {
      cat(sprintf("  Warning: Could not extract trial number from %s\n", basename(f)))
      next
    }
    df <- read_trc(f) %>%
      mutate(Participant=participant, TrialNum=tnum)
    out_path <- file.path(out_dir, sprintf("%s_%d_raw.csv", participant, tnum))
    write_csv(df, out_path)
    converted_count <- converted_count + 1
  }
  cat(sprintf("  Converted %d TRC files\n", converted_count))
  out_dir
}

p4_raw_dir <- convert_trcs(p4_dir, "P4")
p5_raw_dir <- convert_trcs(p5_dir, "P5")

# Find all CSVs
find_raw_csvs <- function(dir, participant) {
  files <- dir_ls(dir, recurse=TRUE, regexp="\\.csv$", type="file")
  files <- files[str_detect(tolower(basename(files)), "raw")]
  
  result <- tibble(
    Participant=participant,
    file=files,
    TrialNum=map_int(basename(files), ~extract_trialnum(.x, participant))
  ) %>% filter(!is.na(TrialNum))
  
  cat(sprintf("Found %d CSV files for %s\n", nrow(result), participant))
  result
}

cat("\nDiscovering CSV files for each participant...\n")
p1_files <- find_raw_csvs(p1_dir, "P1.2")
p2_files <- find_raw_csvs(p2_dir, "P2")
p3_files <- find_raw_csvs(p3_dir, "P3")
p4_files <- find_raw_csvs(p4_dir, "P4")
p5_files <- find_raw_csvs(p5_dir, "P5")

all_files <- bind_rows(p1_files,p2_files,p3_files,p4_files,p5_files) %>%
  distinct(Participant, TrialNum, .keep_all=TRUE)

cat(sprintf("\nTotal files found: %d across %d participants\n", 
            nrow(all_files), n_distinct(all_files$Participant)))

# ==========================
# FRAMEWISE DISTANCES
# ==========================
cat("\nCalculating framewise distances...\n")

all_files_mapped <- all_files %>%
  inner_join(trial_cond_map %>% select(Participant, TrialNum, ConditionKey), 
             by=c("Participant","TrialNum"))

cat(sprintf("Processing %d trials with condition mappings\n", nrow(all_files_mapped)))

framewise_all <- tibble()

for (i in 1:nrow(all_files_mapped)) {
  f <- all_files_mapped$file[i]
  p <- all_files_mapped$Participant[i]
  tnum <- all_files_mapped$TrialNum[i]
  
  if (i %% 10 == 0) cat(sprintf("  Processing file %d/%d...\n", i, nrow(all_files_mapped)))
  
  df <- tryCatch({
    read_csv(f, show_col_types = FALSE)
  }, error = function(e) {
    cat(sprintf("  Error reading %s: %s\n", basename(f), e$message))
    return(NULL)
  })
  
  if (is.null(df)) next
  
  for (marker in c("S2", "C7")) {
    x_col <- paste0(marker, "_X")
    y_col <- paste0(marker, "_Y")
    z_col <- paste0(marker, "_Z")
    
    if (!all(c(x_col, y_col, z_col) %in% names(df))) next
    
    df_marker <- df %>%
      select(any_of(c("Frame", "Time")), all_of(c(x_col, y_col, z_col))) %>%
      rename(X = all_of(x_col), Y = all_of(y_col), Z = all_of(z_col)) %>%
      mutate(
        X = safe_num(X),
        Y = safe_num(Y),
        Z = safe_num(Z)
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
  stop("No framewise data generated. Check marker column names and file formats.")
}

framewise_all <- framewise_all %>%
  left_join(trial_cond_map, by=c("Participant","TrialNum"))

# ==========================
# TRIAL SUMMARIES
# ==========================
cat("Computing trial summaries...\n")

ts_mean <- framewise_all %>%
  group_by(Participant, TrialNum, Marker, ConditionKey) %>%
  summarise(
    mean_A = mean(dist_A, na.rm=TRUE),
    mean_B = mean(dist_B, na.rm=TRUE),
    mean_AB = (mean_A + mean_B) / 2,
    mean_AminusB = mean_A - mean_B,
    .groups="drop"
  ) %>%
  left_join(trial_cond_map, by=c("Participant","TrialNum","ConditionKey"))

ts_min <- framewise_all %>%
  group_by(Participant, TrialNum, Marker, ConditionKey) %>%
  summarise(
    min_A = min(dist_A, na.rm=TRUE),
    min_B = min(dist_B, na.rm=TRUE),
    min_AB = (min_A + min_B) / 2,
    min_AminusB = min_A - min_B,
    .groups="drop"
  ) %>%
  left_join(trial_cond_map, by=c("Participant","TrialNum","ConditionKey"))

cat(sprintf("Trial summaries: %d rows (mean), %d rows (min)\n", 
            nrow(ts_mean), nrow(ts_min)))

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
# MAIN ANALYSIS PACKAGE FUNCTION
# ==========================
make_package <- function(metric=c("mean","min")) {
  metric <- match.arg(metric)
  cat(sprintf("\n=== Creating analysis package for %s metric ===\n", metric))
  
  if (metric == "mean") {
    ts <- ts_mean
    val <- "mean_AB"
    val_AminusB <- "mean_AminusB"
  } else {
    ts <- ts_min
    val <- "min_AB"
    val_AminusB <- "min_AminusB"
  }
  
  block_summary <- ts %>%
    group_by(Participant, ConditionKey, ConditionID, Mode, Composition, Collider) %>%
    summarise(
      mean_AB = mean(.data[[val]], na.rm=TRUE),
      mean_AminusB = mean(.data[[val_AminusB]], na.rm=TRUE),
      .groups="drop"
    )
  
  cat(sprintf("Block summary: %d blocks\n", nrow(block_summary)))
  
  wb <- ts %>%
    arrange(Participant, ConditionKey, TrialInBlock) %>%
    group_by(Participant, ConditionKey) %>%
    filter(TrialInBlock %in% c(1,3)) %>%
    summarise(
      delta_meanAB = diff(.data[[val]][TrialInBlock %in% c(1,3)]),
      ConditionID = first(ConditionID),
      Mode = first(Mode),
      Composition = first(Composition),
      Collider = first(Collider),
      .groups="drop"
    )
  
  wb_ab <- ts %>%
    arrange(Participant, ConditionKey, TrialInBlock) %>%
    group_by(Participant, ConditionKey) %>%
    filter(TrialInBlock %in% c(1,3)) %>%
    summarise(
      delta_AminusB = diff(.data[[val_AminusB]][TrialInBlock %in% c(1,3)]),
      ConditionID = first(ConditionID),
      Mode = first(Mode),
      Composition = first(Composition),
      Collider = first(Collider),
      .groups="drop"
    )
  
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
    anova(lm(mean_AB ~ Composition, data=block_summary))$`Pr(>F)`[1], 
    error=function(e) NA_real_
  )
  comp_star <- p_to_star(comp_anova_p)
  
  mode_p <- tryCatch(
    t.test(mean_AB ~ Mode, data=block_summary)$p.value, 
    error=function(e) NA_real_
  )
  mode_star <- p_to_star(mode_p)
  
  out_root <- file.path(output_dir, ifelse(metric=="mean","final_plots_MEAN","final_plots_MIN"))
  dir_create(out_root, recurse=TRUE)
  
  cat("Generating plots...\n")
  
  # Plot 1
  p1 <- block_summary %>%
    group_by(Composition) %>%
    summarise(mean_val=mean(mean_AB, na.rm=TRUE),
              se=sd(mean_AB, na.rm=TRUE)/sqrt(sum(is.finite(mean_AB))),
              .groups="drop") %>%
    ggplot(aes(x=factor(Composition, levels=c("2R0V","1R1V","0R2V")),
               y=mean_val)) +
    geom_col(alpha=0.85, fill="steelblue") +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    labs(
      title=paste0("Composition effect — metric=", metric),
      subtitle=paste0("ANOVA p=", signif(comp_anova_p,3), " ", comp_star),
      x="Composition", y="Block meanAB"
    ) + theme_pro()
  save_png(p1, file.path(out_root, "01_composition_meanAB.png"), 12, 6)
  
  # Plot 2
  p2 <- block_summary %>%
    filter(Composition=="1R1V") %>%
    ggplot(aes(x=factor(Collider), y=mean_AminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6, fill="coral") +
    labs(title=paste0("1R1V: BoxA−BoxB by collider — metric=", metric),
         x="Collider", y="Block mean(A−B)") + theme_pro()
  save_png(p2, file.path(out_root, "02_1R1V_boxA_vs_boxB.png"), 12, 6)
  
  # Plot 3
  p3 <- collider_eff %>%
    ggplot(aes(x=factor(Collider), y=mean_delta)) +
    geom_col(alpha=0.85, fill="darkgreen") +
    geom_errorbar(aes(ymin=mean_delta-se, ymax=mean_delta+se), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_text(aes(label=star, y=mean_delta+se*1.2), fontface="bold", size=6) +
    labs(title=paste0("Collider effect on Δ (Trial3−Trial1) — metric=", metric),
         x="Collider", y="Δ meanAB") + theme_pro()
  save_png(p3, file.path(out_root, "03_collider_effect_delta.png"), 12, 6)
  
  # Plot 4
  p4 <- block_summary %>%
    group_by(Mode) %>%
    summarise(mean_val=mean(mean_AB, na.rm=TRUE),
              se=sd(mean_AB, na.rm=TRUE)/sqrt(sum(is.finite(mean_AB))),
              .groups="drop") %>%
    ggplot(aes(x=factor(Mode, levels=c("AR","VR")), y=mean_val)) +
    geom_col(alpha=0.85, fill="purple") +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    labs(title=paste0("Mode effect — metric=", metric),
         subtitle=paste0("t-test p=", signif(mode_p,3), " ", mode_star),
         x="Mode", y="Block meanAB") + theme_pro()
  save_png(p4, file.path(out_root, "04_mode_effect.png"), 10, 6)
  
  # Plot 5
  p5 <- wb %>%
    left_join(cond_tbl %>% select(ConditionKey, Mode), by="ConditionKey") %>%
    group_by(Mode) %>%
    summarise(mean_val=mean(delta_meanAB, na.rm=TRUE),
              se=sd(delta_meanAB, na.rm=TRUE)/sqrt(sum(is.finite(delta_meanAB))),
              .groups="drop") %>%
    ggplot(aes(x=factor(Mode, levels=c("AR","VR")), y=mean_val)) +
    geom_col(alpha=0.85, fill="orange") +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(title=paste0("Mode effect on Δ — metric=", metric),
         x="Mode", y="Δ meanAB") + theme_pro()
  save_png(p5, file.path(out_root, "05_mode_effect_delta.png"), 10, 6)
  
  # Plot 6
  wb_consensus <- wb %>% group_by(ConditionKey) %>%
    summarise(consensus=mean(delta_meanAB, na.rm=TRUE), .groups="drop")
  
  wb_long <- wb %>% left_join(wb_consensus, by="ConditionKey") %>%
    filter(is.finite(delta_meanAB), is.finite(consensus))
  
  p6 <- ggplot(wb_long, aes(x=consensus, y=delta_meanAB)) +
    geom_point(alpha=0.75, color="steelblue") +
    geom_smooth(method="lm", se=TRUE, color="red") +
    labs(title=paste0("Reproducibility: Δ vs consensus — metric=", metric),
         x="Consensus Δ", y="Participant Δ") + theme_pro()
  save_png(p6, file.path(out_root, "06_repro_delta.png"), 10, 7)
  
  # Plot 7
  bm_cons <- block_summary %>% group_by(ConditionKey) %>%
    summarise(consensus=mean(mean_AB, na.rm=TRUE), .groups="drop")
  bm_long <- block_summary %>% left_join(bm_cons, by="ConditionKey") %>%
    filter(is.finite(mean_AB), is.finite(consensus))
  
  p7 <- ggplot(bm_long, aes(x=consensus, y=mean_AB)) +
    geom_point(alpha=0.75, color="darkgreen") +
    geom_smooth(method="lm", se=TRUE, color="red") +
    labs(title=paste0("Reproducibility: block means — metric=", metric),
         x="Consensus block meanAB", y="Participant block meanAB") + theme_pro()
  save_png(p7, file.path(out_root, "07_repro_blocks.png"), 10, 7)
  
  # Plot 8
  early_late <- block_summary %>%
    mutate(EarlyLate = if_else(ConditionID <= 6, "Early (c1–c6)", "Late (c7–c12)")) %>%
    group_by(EarlyLate) %>%
    summarise(mean_val=mean(mean_AB, na.rm=TRUE),
              se=sd(mean_AB, na.rm=TRUE)/sqrt(sum(is.finite(mean_AB))),
              .groups="drop")
  
  p8 <- ggplot(early_late, aes(x=EarlyLate, y=mean_val)) +
    geom_col(alpha=0.85, fill="coral") +
    geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.2) +
    labs(title=paste0("Early vs Late blocks — metric=", metric),
         x="", y="Mean block meanAB") + theme_pro()
  save_png(p8, file.path(out_root, "08_early_vs_late.png"), 10, 6)
  
  # Plot 9
  p9 <- block_summary %>%
    ggplot(aes(x=factor(Collider), y=mean_AminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6, fill="purple") +
    labs(title=paste0("BoxA−BoxB by collider — metric=", metric),
         x="Collider", y="Block mean(A−B)") + theme_pro()
  save_png(p9, file.path(out_root, "09_boxAminusB_by_collider.png"), 12, 6)
  
  # Plot 10
  p10 <- wb_ab %>%
    ggplot(aes(x=factor(Collider), y=delta_AminusB)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(alpha=0.6, fill="orange") +
    labs(title=paste0("Δ(A−B) by collider — metric=", metric),
         x="Collider", y="Δ(A−B)") + theme_pro()
  save_png(p10, file.path(out_root, "10_delta_AminusB.png"), 12, 6)
  
  # Plot 11
  traj <- ts %>%
    group_by(ConditionID, TrialInBlock) %>%
    summarise(mean_val=mean(.data[[val]], na.rm=TRUE), .groups="drop")
  
  p11 <- ggplot(traj, aes(x=ConditionID, y=mean_val, color=factor(TrialInBlock))) +
    geom_line(alpha=0.8, size=1) +
    geom_point(size=2) +
    labs(title=paste0("Block trajectory — metric=", metric),
         x="ConditionID", y="Trial metric value", color="Trial in Block") +
    theme_pro()
  save_png(p11, file.path(out_root, "11_trajectory.png"), 12, 6)
  
  # Write CSVs
  cat("Writing summary CSVs...\n")
  write_csv(block_summary, file.path(out_root, "block_summary.csv"))
  write_csv(wb, file.path(out_root, "withinblock_delta.csv"))
  write_csv(wb_ab, file.path(out_root, "withinblock_delta_AminusB.csv"))
  
  sanity <- list(
    metric=metric,
    participants=sort(unique(ts$Participant)),
    n_framewise=nrow(framewise_all),
    n_trials=nrow(ts),
    n_blocks=nrow(block_summary)
  )
  writeLines(jsonlite::toJSON(sanity, pretty=TRUE, auto_unbox=TRUE),
             file.path(out_root, "sanity_checks.json"))
  
  cat(sprintf("Package created at: %s\n", out_root))
  out_root
}

# ==========================
# RUN ANALYSIS
# ==========================
out_mean_dir <- make_package("mean")
out_min_dir  <- make_package("min")

cat("\n========================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("Results saved in:", output_dir, "\n")
cat("  - Mean metric:", out_mean_dir, "\n")
cat("  - Min metric:", out_min_dir, "\n")