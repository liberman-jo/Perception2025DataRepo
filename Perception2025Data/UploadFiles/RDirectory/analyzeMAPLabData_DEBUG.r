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

# ==========================
# HELPERS
# ==========================
extract_zip <- function(zip_path, out_dir) {
  dir_create(out_dir, recurse = TRUE)
  tryCatch({
    unzip(zip_path, exdir = out_dir)
  }, error = function(e) {
    if (Sys.info()["sysname"] == "Windows") {
      ps_cmd <- sprintf(
        'Expand-Archive -Path "%s" -DestinationPath "%s" -Force',
        normalizePath(zip_path, winslash = "\\"),
        normalizePath(out_dir, winslash = "\\")
      )
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
  nms <- names(df)
  bad <- which(is.na(nms) | trimws(nms)=="")
  if (length(bad) > 0) {
    for (k in seq_along(bad)) {
      nms[bad[k]] <- paste0("NA_C", bad[k])
    }
    names(df) <- nms
  }
  df
}

extract_trialnum <- function(filename, participant = NULL) {
  m <- str_match(filename, "Trial(\\d+)|_(\\d+)_raw\\.|V1(\\d+)\\.")
  if (!is.na(m[1,1])) {
    for (i in 2:ncol(m)) {
      if (!is.na(m[1,i])) return(as.integer(m[1,i]))
    }
  }
  m <- str_match(filename, "(\\d+)(?:\\D*?)\\.(trc|csv)$")
  if (is.na(m[1,2])) return(NA_integer_)
  as.integer(m[1,2])
}

safe_num <- function(x) suppressWarnings(as.numeric(x))
dist3 <- function(x,y,z, bx,by,bz) sqrt((x-bx)^2 + (y-by)^2 + (z-bz)^2)

# ==========================
# EXTRACT ZIPS
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
# READ CONDITION MAP - PROPERLY
# ==========================
cat("Reading condition map...\n")

# Read raw excel
cm_raw <- read_excel(cond_map_xlsx, sheet=1, col_names=FALSE)

cat("Excel structure (first 10 rows):\n")
print(cm_raw[1:10, 1:5])

# Row 1 is blank
# Row 2 has headers
# Row 3 is blank
# Row 4 (index 4 in R, 1-indexed) has condition descriptions
# Row 5 (index 5) has condition keys  
# Rows 6-10 have participant data

cond_desc <- as.character(cm_raw[4, 2:13] %>% unlist())
cond_keys <- as.character(cm_raw[5, 2:13] %>% unlist())

cat("\nCondition keys:", paste(cond_keys, collapse=", "), "\n")
cat("Condition descriptions:", paste(head(cond_desc, 3), collapse=" | "), "...\n")

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

cat("\n✓ Parsed conditions:\n")
print(cond_tbl %>% select(ConditionKey, Mode, Composition, Collider))

parse_trials_cell <- function(x) {
  if (is.na(x) || is.null(x)) return(integer(0))
  x <- gsub("\\.", ",", as.character(x))
  as.integer(str_extract_all(x, "\\d+")[[1]])
}

# Read participant trial mappings
participant_rows <- cm_raw[6:10, ]
trial_cond_map <- tibble()

for (r in 1:nrow(participant_rows)) {
  p <- participant_rows[r,1] %>% as.character() %>% str_trim() %>% tolower()
  if (is.na(p) || p=="") next
  
  part <- case_when(
    p == "p1.2" ~ "P1.2",
    p == "p2" ~ "P2",
    p == "p3" ~ "P3",
    p == "p4" ~ "P4",
    p == "p5" ~ "P5",
    TRUE ~ NA_character_
  )
  
  if (is.na(part)) next
  
  cat(sprintf("\n%s: ", part))
  for (j in 1:12) {
    key <- cond_keys[j]
    trials <- parse_trials_cell(participant_rows[r, j+1] %>% unlist())
    if (length(trials)==0) next
    
    cat(sprintf("%s(%d) ", key, length(trials)))
    trial_cond_map <- bind_rows(
      trial_cond_map,
      tibble(Participant=part, TrialNum=trials, ConditionKey=key, 
             TrialInBlock=seq_along(trials))
    )
  }
}

cat("\n\n")

trial_cond_map <- trial_cond_map %>% 
  left_join(cond_tbl, by="ConditionKey")

cat(sprintf("✓ Mapped %d trials across %d participants\n", 
            nrow(trial_cond_map), n_distinct(trial_cond_map$Participant)))

# ==========================
# BOX CENTERS
# ==========================
cat("\nReading box centers...\n")
bc <- read_csv(box_centers_path, show_col_types = FALSE)
boxA_row <- bc %>% filter(Box == "A")
boxB_row <- bc %>% filter(Box == "B")
boxA <- c(boxA_row$X, boxA_row$Y, boxA_row$Z)
boxB <- c(boxB_row$X, boxB_row$Y, boxB_row$Z)
cat(sprintf("Box A: (%.1f, %.1f, %.1f)\n", boxA[1], boxA[2], boxA[3]))
cat(sprintf("Box B: (%.1f, %.1f, %.1f)\n", boxB[1], boxB[2], boxB[3]))

# ==========================
# CONVERT TRC
# ==========================
convert_trcs <- function(trc_dir, participant) {
  cat(sprintf("Converting TRC for %s...\n", participant))
  trcs <- dir_ls(trc_dir, recurse=TRUE, regexp="\\.trc$", type="file")
  out_dir <- file.path(trc_dir, "csv_converted")
  dir_create(out_dir, recurse=TRUE)
  for (f in trcs) {
    tnum <- extract_trialnum(basename(f), participant)
    if (is.na(tnum)) next
    df <- read_trc(f) %>% mutate(Participant=participant, TrialNum=tnum)
    write_csv(df, file.path(out_dir, sprintf("%s_%d_raw.csv", participant, tnum)), progress=FALSE)
  }
}

convert_trcs(p4_dir, "P4")
convert_trcs(p5_dir, "P5")

# ==========================
# FIND CSV FILES
# ==========================
find_raw_csvs <- function(dir, participant) {
  files <- dir_ls(dir, recurse=TRUE, regexp="\\.csv$", type="file")
  files <- files[str_detect(tolower(basename(files)), "raw")]
  tibble(
    Participant=participant,
    file=files,
    TrialNum=map_int(basename(files), ~extract_trialnum(.x, participant))
  ) %>% filter(!is.na(TrialNum))
}

cat("\nDiscovering files...\n")
all_files <- bind_rows(
  find_raw_csvs(p1_dir, "P1.2"),
  find_raw_csvs(p2_dir, "P2"),
  find_raw_csvs(p3_dir, "P3"),
  find_raw_csvs(p4_dir, "P4"),
  find_raw_csvs(p5_dir, "P5")
) %>% distinct(Participant, TrialNum, .keep_all=TRUE)

all_files_mapped <- all_files %>%
  inner_join(trial_cond_map, by=c("Participant","TrialNum"))

cat(sprintf("Files: %d total, %d mapped\n", nrow(all_files), nrow(all_files_mapped)))

# ==========================
# CALCULATE DISTANCES
# ==========================
cat("\nCalculating distances...\n")
framewise_all <- tibble()

for (i in 1:nrow(all_files_mapped)) {
  if (i %% 20 == 0) cat(sprintf("  %d/%d\n", i, nrow(all_files_mapped)))
  
  f <- all_files_mapped$file[i]
  df <- tryCatch(read_csv(f, show_col_types = FALSE, progress = FALSE), 
                 error = function(e) NULL)
  if (is.null(df)) next
  
  for (marker in c("S2", "C7")) {
    cols <- paste0(marker, c("_X", "_Y", "_Z"))
    if (!all(cols %in% names(df))) next
    
    df_marker <- df %>%
      select(any_of(c("Frame", "Time")), all_of(cols)) %>%
      rename(X = all_of(cols[1]), Y = all_of(cols[2]), Z = all_of(cols[3])) %>%
      mutate(
        X = safe_num(X), Y = safe_num(Y), Z = safe_num(Z),
        Participant = all_files_mapped$Participant[i],
        TrialNum = all_files_mapped$TrialNum[i],
        ConditionKey = all_files_mapped$ConditionKey[i],
        Marker = marker,
        dist_A = dist3(X, Y, Z, boxA[1], boxA[2], boxA[3]),
        dist_B = dist3(X, Y, Z, boxB[1], boxB[2], boxB[3])
      ) %>%
      filter(is.finite(dist_A) & is.finite(dist_B))
    
    framewise_all <- bind_rows(framewise_all, df_marker)
  }
}

cat(sprintf("✓ Framewise: %d rows\n", nrow(framewise_all)))

if (nrow(framewise_all) == 0) {
  cat("\n ERROR: No data! Check that CSV files have S2_X/Y/Z and C7_X/Y/Z columns\n")
  cat("Sample file columns:\n")
  sample_df <- read_csv(all_files_mapped$file[1], show_col_types = FALSE, n_max = 1)
  cat(paste(names(sample_df), collapse=", "), "\n")
  stop("No framewise data generated")
}

framewise_all <- framewise_all %>%
  left_join(cond_tbl, by="ConditionKey")

# ==========================
# SUMMARIES
# ==========================
cat("Computing summaries...\n")

ts_mean <- framewise_all %>%
  group_by(Participant, TrialNum, Marker, ConditionKey, Mode, Composition, Collider, ConditionID) %>%
  summarise(
    mean_A = mean(dist_A, na.rm=TRUE),
    mean_B = mean(dist_B, na.rm=TRUE),
    mean_AB = (mean_A + mean_B) / 2,
    mean_AminusB = mean_A - mean_B,
    .groups="drop"
  ) %>%
  left_join(trial_cond_map %>% select(Participant, TrialNum, ConditionKey, TrialInBlock), 
            by = c("Participant", "TrialNum", "ConditionKey"))

ts_min <- framewise_all %>%
  group_by(Participant, TrialNum, Marker, ConditionKey, Mode, Composition, Collider, ConditionID) %>%
  summarise(
    min_A = min(dist_A, na.rm=TRUE),
    min_B = min(dist_B, na.rm=TRUE),
    min_AB = (min_A + min_B) / 2,
    min_AminusB = min_A - min_B,
    .groups="drop"
  ) %>%
  left_join(trial_cond_map %>% select(Participant, TrialNum, ConditionKey, TrialInBlock), 
            by = c("Participant", "TrialNum", "ConditionKey"))

cat(sprintf("✓ Trials: %d (mean), %d (min)\n\n", nrow(ts_mean), nrow(ts_min)))

# ==========================
# ANALYSIS & PLOTS
# ==========================
theme_pro <- function() {
  theme_minimal() +
    theme(plot.title = element_text(face="bold", size=14),
          axis.title = element_text(size=11),
          panel.grid.minor = element_blank())
}

save_png <- function(p, path, w=10, h=6) {
  ggsave(path, p, width=w, height=h, dpi=150, bg="white")
}

make_package <- function(metric=c("mean","min")) {
  metric <- match.arg(metric)
  cat(sprintf("=== %s METRIC ===\n", toupper(metric)))
  
  ts <- if(metric=="mean") ts_mean else ts_min
  val <- if(metric=="mean") "mean_AB" else "min_AB"
  val_AminusB <- if(metric=="mean") "mean_AminusB" else "min_AminusB"
  
  block_summary <- ts %>%
    group_by(Participant, ConditionKey, ConditionID, Mode, Composition, Collider) %>%
    summarise(mean_AB = mean(.data[[val]], na.rm=TRUE),
              mean_AminusB = mean(.data[[val_AminusB]], na.rm=TRUE),
              .groups="drop")
  
  cat(sprintf("Blocks: %d\n", nrow(block_summary)))
  
  out_root <- file.path(output_dir, paste0("Results_", toupper(metric)))
  dir_create(out_root, recurse=TRUE)
  
  # Plot 1: Composition
  p1 <- block_summary %>%
    group_by(Composition) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=factor(Composition, levels=c("2R0V","1R1V","0R2V")), y=m)) +
    geom_col(fill="steelblue", alpha=0.8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("Composition Effect -", metric), x="Composition", y="Distance (mm)") +
    theme_pro()
  save_png(p1, file.path(out_root, "01_composition.png"))
  
  # Plot 2: Mode
  p2 <- block_summary %>%
    group_by(Mode) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=Mode, y=m)) +
    geom_col(fill="coral", alpha=0.8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("AR vs VR -", metric), x="Mode", y="Distance (mm)") +
    theme_pro()
  save_png(p2, file.path(out_root, "02_mode.png"))
  
  # Plot 3: Collider
  p3 <- block_summary %>%
    group_by(Collider) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=factor(Collider), y=m)) +
    geom_col(fill="darkgreen", alpha=0.8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("Collider Effect -", metric), x="Collider", y="Distance (mm)") +
    theme_pro()
  save_png(p3, file.path(out_root, "03_collider.png"))
  
  # Plot 4: Participants
  p4 <- block_summary %>%
    group_by(Participant) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=Participant, y=m)) +
    geom_col(fill="purple", alpha=0.8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("By Participant -", metric), x="Participant", y="Distance (mm)") +
    theme_pro()
  save_png(p4, file.path(out_root, "04_participants.png"))
  
  write_csv(block_summary, file.path(out_root, "block_summary.csv"))
  write_csv(ts, file.path(out_root, "trial_summary.csv"))
  
  cat(sprintf("✓ Saved to: %s\n\n", out_root))
  out_root
}

out_mean <- make_package("mean")
out_min <- make_package("min")

cat("========================================\n")
cat("COMPLETE!\n")
cat("========================================\n")