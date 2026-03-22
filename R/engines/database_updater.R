# =========================================================
# R/engines/database_updater.R -- Database Update System
#
# Checks a remote manifest for newer database versions and
# downloads updates to the user data folder.
# =========================================================

# Default manifest URL (points to the public release repo)
DB_MANIFEST_URL <- "https://raw.githubusercontent.com/shihj12/MSTerp/main/database-manifest.json"

#' Get the user data directory for database storage
#'
#' Checks MSTERP_USER_DATA env var (set by Electron), falls back to
#' a platform-appropriate location.
#' @return Path to user data directory (created if needed)
db_get_user_data_dir <- function() {
  base <- Sys.getenv("MSTERP_USER_DATA", unset = "")
  if (!nzchar(base)) {
    base <- file.path(Sys.getenv("APPDATA", unset = tempdir()), "MSTerp")
  }
  db_dir <- file.path(base, "databases")
  if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE)
  db_dir
}

#' Get path to local version tracking file
db_versions_path <- function() {
  file.path(db_get_user_data_dir(), "db-versions.json")
}

#' Read local version info
db_read_local_versions <- function() {
  path <- db_versions_path()
  if (file.exists(path)) {
    tryCatch(
      jsonlite::fromJSON(path, simplifyVector = FALSE),
      error = function(e) list()
    )
  } else {
    list()
  }
}

#' Save local version info
db_save_local_versions <- function(versions) {
  path <- db_versions_path()
  writeLines(jsonlite::toJSON(versions, auto_unbox = TRUE, pretty = TRUE), path)
}

#' Check for database updates
#'
#' Fetches the remote manifest and compares against local versions.
#' @param manifest_url URL to the manifest JSON
#' @return data.frame with columns: id, name, folder, file, local_version, remote_version, size_mb, update_available
db_check_updates <- function(manifest_url = DB_MANIFEST_URL) {
  manifest <- tryCatch({
    resp <- httr::GET(manifest_url, httr::timeout(10))
    httr::stop_for_status(resp)
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(manifest) || is.null(manifest$databases)) {
    return(data.frame(
      id = character(0), name = character(0), folder = character(0),
      file = character(0), local_version = character(0),
      remote_version = character(0), size_mb = numeric(0),
      update_available = logical(0), stringsAsFactors = FALSE
    ))
  }

  local_versions <- db_read_local_versions()

  results <- lapply(manifest$databases, function(db) {
    local_ver <- local_versions[[db$id]] %||% "not installed"
    remote_ver <- db$version %||% "unknown"
    data.frame(
      id = db$id,
      name = db$name,
      folder = db$folder,
      file = db$file,
      local_version = local_ver,
      remote_version = remote_ver,
      size_mb = db$size_mb %||% 0,
      url = db$url %||% "",
      update_available = (local_ver != remote_ver),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

#' Download a database update
#'
#' @param id Database ID from manifest
#' @param url Download URL
#' @param folder Subfolder name (terpbase, metabobase, complexbase)
#' @param file Filename
#' @param version Version string to record
#' @param progress_callback Optional function(pct) for progress updates
#' @return list(ok, message)
db_download_update <- function(id, url, folder, file, version,
                               progress_callback = NULL) {
  user_dir <- db_get_user_data_dir()
  dest_dir <- file.path(user_dir, folder)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  dest_file <- file.path(dest_dir, file)

  tryCatch({
    if (!is.null(progress_callback)) progress_callback(0)

    resp <- httr::GET(
      url,
      httr::write_disk(dest_file, overwrite = TRUE),
      httr::progress(),
      httr::timeout(300)
    )
    httr::stop_for_status(resp)

    if (!is.null(progress_callback)) progress_callback(100)

    # Validate the downloaded file is a valid RDS
    tryCatch(readRDS(dest_file), error = function(e) {
      file.remove(dest_file)
      stop("Downloaded file is not a valid RDS: ", e$message)
    })

    # Update version tracking
    versions <- db_read_local_versions()
    versions[[id]] <- version
    db_save_local_versions(versions)

    list(ok = TRUE, message = sprintf("Updated %s to version %s", file, version))
  }, error = function(e) {
    list(ok = FALSE, message = sprintf("Failed to download %s: %s", file, e$message))
  })
}

#' Resolve a database file path, checking user data folder first
#'
#' @param folder Subfolder name (terpbase, metabobase, complexbase)
#' @param file Filename
#' @param bundled_dir Path to the bundled database directory
#' @return Full path to the file (user-updated version preferred)
db_resolve_path <- function(folder, file, bundled_dir = NULL) {
  # Check user data folder first
  user_path <- file.path(db_get_user_data_dir(), folder, file)
  if (file.exists(user_path)) return(user_path)

  # Fall back to bundled
  if (!is.null(bundled_dir)) {
    bundled_path <- file.path(bundled_dir, file)
    if (file.exists(bundled_path)) return(bundled_path)
  }

  # Try relative path as last resort
  relative_path <- file.path(folder, file)
  if (file.exists(relative_path)) return(relative_path)

  NULL
}
