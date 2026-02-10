suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fs)
})

ROOT <- normalizePath(getwd(), winslash="/")
work_dir <- file.path(ROOT, "AllFilesNeeded Final")
box_centers_path <- file.path(work_dir, "box_centers.csv")
cond_map_xlsx <- file.path(work_dir, "Condition_map.xlsx")
output_dir <- file.path(ROOT, "_WORK_ALL5")
dir_create(output_dir, recurse = TRUE)

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
  
  # Read with the marker names
  df <- read.delim(path, header=TRUE, sep="\t", skip=hdr_i-1,
                   check.names=FALSE, stringsAsFactors=FALSE)
  
  # The TRC format has: Frame#, Time, Marker1, , , Marker2, , , ...
  # Where each marker is followed by 3 blank columns for X, Y, Z
  # We need to rename those blank columns
  
  nms <- names(df)
  for (i in seq_along(nms)) {
    if (i > 2 && (is.na(nms[i]) || trimws(nms[i]) == "")) {
      # Find the last non-empty marker name before this position
      marker_idx <- max(which(!is.na(nms[1:(i-1)]) & trimws(nms[1:(i-1)]) != "")[1:2 != 1:length(nms[1:(i-1)])])
      if (length(marker_idx) > 0 && marker_idx > 2) {
        marker_name <- nms[marker_idx]
        # Count how many columns after this marker
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

cat("Reading condition map...\n")
cm_raw <- read_excel(cond_map_xlsx, sheet=1, col_names=FALSE)

# FIXED: Excel rows 1-3 are headers/blank, so:
# R row 1 = Excel row 4 (condition descriptions)
# R row 2 = Excel row 5 (condition keys)
# R rows 3-7 = Excel rows 6-10 (participants)

cond_desc <- as.character(cm_raw[1, 2:13] %>% unlist())  # FIXED: row 1 not row 4
cond_keys <- as.character(cm_raw[2, 2:13] %>% unlist())  # FIXED: row 2 not row 5

cat("Conditions:", paste(head(cond_keys, 4), collapse=", "), "...\n")

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

participant_rows <- cm_raw[3:7, ]  # FIXED: rows 3-7 not 6-10
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
  
  for (j in 1:12) {
    key <- cond_keys[j]
    trials <- parse_trials_cell(participant_rows[r, j+1] %>% unlist())
    if (length(trials)==0) next
    trial_cond_map <- bind_rows(trial_cond_map,
      tibble(Participant=part, TrialNum=trials, ConditionKey=key, TrialInBlock=seq_along(trials)))
  }
}

trial_cond_map <- trial_cond_map %>% left_join(cond_tbl, by="ConditionKey")
cat(sprintf("✓ %d trials mapped\n", nrow(trial_cond_map)))

cat("Reading box centers...\n")
bc <- read_csv(box_centers_path, show_col_types = FALSE)
boxA <- bc %>% filter(Box == "A") %>% select(X, Y, Z) %>% as.numeric()
boxB <- bc %>% filter(Box == "B") %>% select(X, Y, Z) %>% as.numeric()

cat("Converting TRC files...\n")
convert_trcs <- function(trc_dir, participant) {
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

find_raw_csvs <- function(dir, participant) {
  files <- dir_ls(dir, recurse=TRUE, regexp="\\.csv$", type="file")
  files <- files[str_detect(tolower(basename(files)), "raw")]
  tibble(Participant=participant, file=files,
         TrialNum=map_int(basename(files), ~extract_trialnum(.x, participant))) %>% 
    filter(!is.na(TrialNum))
}

cat("Finding files...\n")
all_files <- bind_rows(
  find_raw_csvs(p1_dir, "P1.2"),
  find_raw_csvs(p2_dir, "P2"),
  find_raw_csvs(p3_dir, "P3"),
  find_raw_csvs(p4_dir, "P4"),
  find_raw_csvs(p5_dir, "P5")
) %>% distinct(Participant, TrialNum, .keep_all=TRUE)

all_files_mapped <- all_files %>% inner_join(trial_cond_map, by=c("Participant","TrialNum"))
cat(sprintf("✓ %d files mapped\n", nrow(all_files_mapped)))

cat("Calculating distances...\n")
framewise_all <- tibble()

for (i in 1:nrow(all_files_mapped)) {
  if (i %% 20 == 0) cat(sprintf("  %d/%d\n", i, nrow(all_files_mapped)))
  
  df <- tryCatch(read_csv(all_files_mapped$file[i], show_col_types = FALSE, progress = FALSE), 
                 error = function(e) NULL)
  if (is.null(df)) next
  
  for (marker in c("S2", "C7")) {
    cols <- paste0(marker, c("_X", "_Y", "_Z"))
    if (!all(cols %in% names(df))) next
    
    df_marker <- df %>%
      select(any_of(c("Frame", "Time", "Frame#")), all_of(cols)) %>%
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

cat(sprintf("✓ %d framewise rows\n", nrow(framewise_all)))
if (nrow(framewise_all) == 0) stop("No data generated!")

framewise_all <- framewise_all %>% left_join(cond_tbl, by="ConditionKey")

cat("Computing summaries...\n")
ts_mean <- framewise_all %>%
  group_by(Participant, TrialNum, Marker, ConditionKey, Mode, Composition, Collider, ConditionID) %>%
  summarise(mean_A = mean(dist_A, na.rm=TRUE), mean_B = mean(dist_B, na.rm=TRUE),
            mean_AB = (mean_A + mean_B) / 2, mean_AminusB = mean_A - mean_B, .groups="drop") %>%
  left_join(trial_cond_map %>% select(Participant, TrialNum, ConditionKey, TrialInBlock), 
            by = c("Participant", "TrialNum", "ConditionKey"))

ts_min <- framewise_all %>%
  group_by(Participant, TrialNum, Marker, ConditionKey, Mode, Composition, Collider, ConditionID) %>%
  summarise(min_A = min(dist_A, na.rm=TRUE), min_B = min(dist_B, na.rm=TRUE),
            min_AB = (min_A + min_B) / 2, min_AminusB = min_A - min_B, .groups="drop") %>%
  left_join(trial_cond_map %>% select(Participant, TrialNum, ConditionKey, TrialInBlock), 
            by = c("Participant", "TrialNum", "ConditionKey"))

theme_pro <- function() {
  theme_minimal() + theme(plot.title = element_text(face="bold", size=14),
                          axis.title = element_text(size=11), panel.grid.minor = element_blank())
}

save_png <- function(p, path, w=10, h=6) {
  ggsave(path, p, width=w, height=h, dpi=150, bg="white")
}

make_package <- function(metric) {
  cat(sprintf("\n=== %s ===\n", toupper(metric)))
  ts <- if(metric=="mean") ts_mean else ts_min
  val <- if(metric=="mean") "mean_AB" else "min_AB"
  
  block_summary <- ts %>%
    group_by(Participant, ConditionKey, ConditionID, Mode, Composition, Collider) %>%
    summarise(mean_AB = mean(.data[[val]], na.rm=TRUE), .groups="drop")
  
  out_root <- file.path(output_dir, paste0("Results_", toupper(metric)))
  dir_create(out_root, recurse=TRUE)
  
  p1 <- block_summary %>% group_by(Composition) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=factor(Composition, levels=c("2R0V","1R1V","0R2V")), y=m)) +
    geom_col(fill="steelblue", alpha=0.8) + geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("Composition -", metric), x="", y="Distance (mm)") + theme_pro()
  save_png(p1, file.path(out_root, "01_composition.png"))
  
  p2 <- block_summary %>% group_by(Mode) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=Mode, y=m)) + geom_col(fill="coral", alpha=0.8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("AR vs VR -", metric), x="", y="Distance (mm)") + theme_pro()
  save_png(p2, file.path(out_root, "02_mode.png"))
  
  p3 <- block_summary %>% group_by(Collider) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=factor(Collider), y=m)) + geom_col(fill="darkgreen", alpha=0.8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("Collider -", metric), x="", y="Distance (mm)") + theme_pro()
  save_png(p3, file.path(out_root, "03_collider.png"))
  
  p4 <- block_summary %>% group_by(Participant) %>%
    summarise(m=mean(mean_AB, na.rm=TRUE), se=sd(mean_AB, na.rm=TRUE)/sqrt(n()), .groups="drop") %>%
    ggplot(aes(x=Participant, y=m)) + geom_col(fill="purple", alpha=0.8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(title=paste("Participants -", metric), x="", y="Distance (mm)") + theme_pro()
  save_png(p4, file.path(out_root, "04_participants.png"))
  
  write_csv(block_summary, file.path(out_root, "block_summary.csv"))
  write_csv(ts, file.path(out_root, "trial_summary.csv"))
  
  cat(sprintf("✓ Saved: %s\n", out_root))
  out_root
}

out_mean <- make_package("mean")
out_min <- make_package("min")

cat("\n========================================\n")
cat("COMPLETE! Results saved to:\n")
cat(" ", out_mean, "\n")
cat(" ", out_min, "\n")
cat("========================================\n")