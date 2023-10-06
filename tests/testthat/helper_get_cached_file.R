#' Get Files from Cache or Download then
#'
#' This is a wrapper function around [`download.file()`](utils::download.file),
#' that will copy files from a local cache instead of downloading them, if the
#' [md5sum](tools::md5sum) of the cached file matches the one provided by the
#' upstream file repository.
#'
#' @md
#' @param url,destfile,quiet,mode Arguments passed along to
#'     [`download.file()`](utils::download.file).
#' @param cache Location of the local file cache for the `remind2` package.
#'     Defaults to the `remind2.cache` option.  If `NULL` (i.e. not set by the
#'     user), caching is ignored.  Must be a directory the user has write access
#'     to otherwise.
#'
#' @details
#' Files are copied from the local cache instead of downloaded if:
#' - the upstream repository provides a checksums,
#' - there is a checksum for this file,
#' - there is a file with the same name in the local cache,
#' - and the checksum of the local file matches that of the remote file.
#'
#' In all other cases the file is downloaded and added to the local cache.
#'
#' To use file caching:
#' - Set the `remind2.cache` option to a directory the user has write access to
#'   _before_ the `remind2` package is attached, e.g. in either the site or user
#'   `.Rprofile` file using
#'   ```
#'   options('remind2.cache' = '/some/directory/')
#'   ```
#' - Place a MD5 `checksums` file in the upstream repository from where files
#'   are downloaded, usually by invoking
#'   ```
#'   $ md5sum * > checksums
#'   ```
#' The function will then fill the local cache with the downloaded files,
#' replacing them if their checksums are updated in the upstream file
#' repository.
#'
#' Checksums are downloaded only once from each remote repository and kept in
#' memory for the duration of the R session.  To affect an update of remote
#' checksums, the `remind2` package has to be attached again.
#'
#' @return The function does not return a meaningful value.

#' @export
get_cached_file <- (function() {
  # checksum cache ----
  checksum_cache <- new.env(hash = FALSE, parent = emptyenv())

  assign('checksums', list(), envir = checksum_cache, inherits = FALSE)

  get_checksums <- function() {
    get('checksums', envir = checksum_cache, inherits = FALSE)
  }

  append_checksums <- function(x) {
    assign('checksums', append(get_checksums(), x), envir = checksum_cache,
           inherits = FALSE)
  }

  update_checksums <- function(x) {
    assign('checksums', x, envir = checksum_cache, inherits = FALSE)
  }

  # download function ----
  function(url, destfile, quiet = FALSE, mode = 'w',
           cache = getOption('remind2.cache')) {
    # guardians ----
    # if no cache is set, just download the file
    if (is.null(cache))
      return(utils::download.file(url, destfile, quiet = quiet, mode = mode))

    # if cache is not a single writeable directory
    if (any(1 < length(cache),
            isFALSE(file.info(cache, extra_cols = FALSE)[['isdir']]),
            0 != file.access(cache, 2)))
      stop('`cache` needs to be either NULL or a single writable directory')

    if (length(url) != length(destfile))
      stop('lengths of `url` and `destfile` must match')

    # download checksums ----
    messages <- character(0)
    for (u in unique(dirname(url))) {
      if (u %in% names(get_checksums()))
        next

      checksums_file <- tempfile('remind2_checksums')

      tmp <- tryCatch(
        suppressWarnings(
          utils::download.file(paste0(u, '/checksums'), checksums_file,
                               quiet = TRUE)),

        error = function(e) { e }
      )

      if (rlang::is_error(tmp)) {
        if (grepl('^cannot open URL', tmp[['message']])) {
          messages <- append(messages,
                             paste('Cannot download `checksums` file from ', u))
          append_checksums(setNames(list(NA), u))
        }
        else {
          stop(tmp)
        }
      }
      else {
        tmp <- unlist(strsplit(readLines(checksums_file), ' +'))
        tmp <- setNames(as.list(tmp[seq(1, length(tmp), 2)]),
                        tmp[seq(2, length(tmp), 2)])
        append_checksums(setNames(list(tmp), u))
      }

      unlink(checksums_file)
    }

    if (0 < length(messages))
      message(paste(c(messages, 'Those downloads will not be cached.'),
                    collapse = '\n'))

    checksums <- get_checksums()

    # get cached files ----
    cached_files <- as.list(tools::md5sum(list.files(cache, full.names = TRUE)))

    for (i in seq_along(url)) {
      u <- url[[i]]
      download <- FALSE

      # does the repository provide checksums?
      if (is.na(checksums[dirname(u)]))
        download <- TRUE

      # is there a checksum for this file?
      if (!basename(u) %in% names(checksums[[dirname(u)]]))
        download <- TRUE

      # is file with that name in the cache?
      if (!basename(u) %in% basename(names(cached_files)))
        download <- TRUE

      # do the checksums match?
      if (!download) {
        cache_checksum <- cached_files[[file.path(cache, basename(u))]]
        repository_checksum <- checksums[[dirname(u)]][[basename(u)]]

        if (cache_checksum != repository_checksum)
          download <- TRUE
      }

      if (download) {
        utils::download.file(u, destfile[[i]], quiet = quiet, mode = mode)
        file.copy(destfile[[i]], file.path(cache, basename(u)))

        # add/update checksum in the cache
        if (!isTRUE(is.na(checksums[[dirname(u)]]))) {
          checksums[[dirname(u)]][[basename(u)]] <-
            unname(tools::md5sum(destfile[[i]]))
          update_checksums(checksums)
        }
      }
      else {
        file.copy(file.path(cache, basename(u)), destfile[[i]])
      }
    }
  }
})()
