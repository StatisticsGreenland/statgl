#' Convert a Word document to Quarto (QMD)
#'
#' Uses Quarto's Pandoc to convert a selected .docx to a .qmd in the same folder,
#' extracts media, fixes absolute image paths, converts EMF to PNG when possible,
#' extracts header metadata to YAML, and removes PUBLSLUT.
#'
#' @export
word_to_qmd_addin <- function() {
  in_file <- file.choose()
  if (!nzchar(in_file) || !file.exists(in_file)) {
    return(invisible(NULL))
  }
  word_to_qmd(in_file)
}

#' Convert a Word document to Quarto (QMD) (non-interactive)
#' @param in_file Path to .docx
#' @export
word_to_qmd <- function(in_file) {
  ext <- tolower(tools::file_ext(in_file))
  if (ext != "docx") {
    stop("Please choose a .docx file")
  }
  if (!file.exists(in_file)) {
    stop("Input file not found: ", in_file)
  }

  out_dir <- dirname(in_file)

  # Keep original stem for human-facing title, even if it contains æøå
  stem_raw <- tools::file_path_sans_ext(basename(in_file))

  # Safe stem for filenames/paths we create
  stem_safe <- make_safe_stem(stem_raw)

  out_file <- file.path(out_dir, paste0(stem_safe, ".qmd"))
  media_dir_rel <- paste0(stem_safe, "_files")

  front_matter <- c(
    "---",
    paste0('title: "', escape_yaml_string(stem_raw), '"'),
    paste0('slug: "', stem_safe, '"'),
    "---",
    ""
  )

  quarto_bin <- Sys.which("quarto")
  if (quarto_bin == "") {
    stop("Quarto CLI not found on PATH.")
  }

  # Pandoc can fail to open files with æøå in path on some systems.
  safe <- pandoc_safe_docx_copy(in_file)
  on.exit(unlink(safe$tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  in_file_for_pandoc <- safe$tmp_docx

  # Run pandoc from output directory so extracted-media links are relative
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(out_dir)

  res <- system2(
    quarto_bin,
    c(
      "pandoc",
      shQuote(in_file_for_pandoc),
      "-t",
      "markdown",
      "-o",
      shQuote(out_file),
      "--extract-media",
      shQuote(media_dir_rel),
      "--wrap=none"
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    stop("Conversion failed.\n\n", paste(res, collapse = "\n"))
  }

  body <- readLines(out_file, warn = FALSE)
  if (length(body) == 0 || trimws(body[1]) != "---") {
    writeLines(c(front_matter, body), out_file, useBytes = TRUE)
  }

  post_process_qmd(out_file, stem_safe)

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::navigateToFile(out_file)
  }

  message("Wrote: ", out_file)
  invisible(out_file)
}

make_safe_stem <- function(x) {
  if (requireNamespace("janitor", quietly = TRUE)) {
    out <- janitor::make_clean_names(x)
  } else {
    out <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
    out <- tolower(out)
    out <- gsub("[^a-z0-9]+", "_", out)
    out <- gsub("^_+|_+$", "", out)
    out <- gsub("_+", "_", out)
  }
  if (!nzchar(out)) {
    out <- "doc"
  }
  out
}

escape_yaml_string <- function(x) gsub('"', '\\"', x, fixed = TRUE)

post_process_qmd <- function(qmd_path, stem_safe) {
  media_dir_rel <- paste0(stem_safe, "_files")
  media_media_abs <- file.path(dirname(qmd_path), media_dir_rel, "media")

  rewrite_media_links(qmd_path, media_dir_rel)

  if (dir.exists(media_media_abs)) {
    convert_emf_to_png(media_media_abs)
  }

  txt <- readLines(qmd_path, warn = FALSE)
  txt <- gsub("\\\\", "/", txt)
  txt <- gsub("(?i)\\.emf\\)", ".png)", txt, perl = TRUE)
  writeLines(txt, qmd_path, useBytes = TRUE)

  tidy_word_qmd(qmd_path) # header->YAML + remove PUBLSLUT + remove empty headings
  invisible(qmd_path)
}

rewrite_media_links <- function(qmd_path, media_dir_rel) {
  txt <- readLines(qmd_path, warn = FALSE)
  pat <- paste0("\\((?:[A-Za-z]:[/\\\\].*?[/\\\\])", media_dir_rel, "[/\\\\]")
  txt <- gsub(pat, paste0("(", media_dir_rel, "/"), txt, perl = TRUE)
  txt <- gsub("\\\\", "/", txt)
  writeLines(txt, qmd_path, useBytes = TRUE)
  invisible(qmd_path)
}

convert_emf_to_png <- function(media_dir) {
  emf_files <- list.files(
    media_dir,
    pattern = "\\.emf$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(emf_files)) {
    return(invisible(NULL))
  }

  if (!requireNamespace("magick", quietly = TRUE)) {
    warning(
      "Found EMF images but {magick} is not installed. ",
      "Install.packages('magick') + ImageMagick to convert EMF -> PNG."
    )
    return(invisible(emf_files))
  }

  for (f in emf_files) {
    out <- sub("(?i)\\.emf$", ".png", f, perl = TRUE)
    img <- magick::image_read(f)
    magick::image_write(img, out, format = "png")
  }

  invisible(emf_files)
}

tidy_word_qmd <- function(qmd_path) {
  lines <- readLines(qmd_path, warn = FALSE)
  yaml_start <- which(trimws(lines) == "---")[1]
  yaml_end <- which(trimws(lines) == "---")[-1][1]
  if (is.na(yaml_start) || is.na(yaml_end) || yaml_end <= yaml_start) {
    return(invisible(FALSE))
  }

  yaml <- lines[yaml_start:yaml_end]
  body <- lines[(yaml_end + 1):length(lines)]

  add_yaml_field <- function(key, value) {
    if (is.null(value) || !nzchar(value)) {
      return()
    }
    if (any(grepl(paste0("^", key, ":"), yaml))) {
      return()
    }
    yaml <<- append(
      yaml,
      paste0(key, ': "', escape_yaml_string(value), '"'),
      after = length(yaml) - 1
    )
  }

  # remove empty headings (# on its own)
  body <- body[!grepl("^#+\\s*$", body)]

  # header line: "##### ![](path){...}Title"
  i_logo <- which(grepl("^#{4,6}\\s+!\\[\\]\\(", body))[1]
  if (!is.na(i_logo)) {
    line <- body[i_logo]
    header_logo <- sub("^#{4,6}\\s+!\\[\\]\\(([^)]+)\\).*", "\\1", line)
    header_title <- trimws(sub(
      "^#{4,6}\\s+!\\[\\]\\([^)]+\\)\\{[^}]*\\}\\s*",
      "",
      line
    ))
    add_yaml_field("header_logo", header_logo)
    add_yaml_field("header_title", header_title)
    body <- body[-i_logo]
  }

  # subject + date in first chunk
  top <- trimws(body[1:min(40, length(body))])

  subj_idx <- which(
    nzchar(top) & !grepl("^#+\\s", top) & !grepl("^!\\[\\]", top)
  )[1]
  if (!is.na(subj_idx)) {
    add_yaml_field("header_subject", top[subj_idx])
    j <- which(trimws(body) == top[subj_idx])[1]
    if (!is.na(j) && j <= 25) body <- body[-j]
  }

  fix_date <- function(x) {
    x <- gsub("\\s*/\\.\\s*", ". ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  date_idx <- which(grepl(
    "^\\d{1,2}([./]\\s*|\\.\\s*|/\\.\\s*)[A-Za-zæøåÆØÅ]+\\s+\\d{4}$",
    top
  ))[1]
  if (!is.na(date_idx) && !any(grepl("^date:", yaml))) {
    add_yaml_field("date", fix_date(top[date_idx]))
    # remove date line early in body
    j <- which(fix_date(trimws(body)) == fix_date(top[date_idx]))[1]
    if (!is.na(j) && j <= 25) body <- body[-j]
  }

  # remove PUBLSLUT section entirely
  i_pub <- which(grepl(
    "^#\\s*PUBLSLUT\\s*$",
    trimws(body),
    ignore.case = TRUE
  ))[1]
  if (!is.na(i_pub)) {
    body <- body[1:(i_pub - 1)]
  }

  writeLines(c(yaml, body), qmd_path, useBytes = TRUE)
  invisible(TRUE)
}


convert_emf_to_png <- function(media_dir) {
  emf_files <- list.files(
    media_dir,
    pattern = "\\.emf$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(emf_files)) {
    return(invisible(NULL))
  }

  if (!requireNamespace("magick", quietly = TRUE)) {
    warning(
      "Found EMF images but {magick} is not installed; skipping EMF -> PNG."
    )
    return(invisible(emf_files))
  }

  failed <- character(0)

  for (f in emf_files) {
    out <- sub("(?i)\\.emf$", ".png", f, perl = TRUE)
    ok <- tryCatch(
      {
        img <- magick::image_read(f)
        magick::image_write(img, out, format = "png")
        TRUE
      },
      error = function(e) {
        failed <<- c(failed, f)
        FALSE
      }
    )
  }

  if (length(failed)) {
    warning(
      "Skipped ",
      length(failed),
      " EMF file(s) (ImageMagick has no EMF delegate).\n"
    )
  }

  invisible(emf_files)
}

pandoc_safe_docx_copy <- function(in_file) {
  tmp_dir <- tempfile("statgl_docx_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  tmp_docx <- file.path(tmp_dir, "input.docx")
  ok <- file.copy(in_file, tmp_docx, overwrite = TRUE)
  if (!ok) {
    stop("Could not copy input docx to temp dir: ", tmp_docx)
  }

  list(tmp_dir = tmp_dir, tmp_docx = tmp_docx)
}
