#' @title Download example data from GitHub repository
#'
#' @description
#' This function downloads files stored in the package GitHub repository
#' using the raw GitHub URL (raw.githubusercontent.com).
#'
#' @param files \code{character} vector with the names of available files. If
#' \code{NULL}, the list of available files will be showed.
#' @param destdir \code{character}. Local directory where the file will be saved.
#' @param repo \code{character}. GitHub repository in the form "user/repo".
#' @param ref \code{character}. Branch, tag, or commit (default: "main").
#' @param overwrite \code{logical}. Whether to overwrite existing files.
#' @param quiet If \code{TRUE}, suppress downloading status messages (if any), 
#' and the progress bar.
#'
#' @return A \code{list} with paths to the downloaded files.
#' @export
#' 
#' @examples
#' download_gts_data()
#' 
#' download_gts_data(files = c("bathymetry.nc", "shelfbreak.nc"))
download_gts_data <- function(
    files = NULL,
    destdir = ".",
    repo = "LuisLauM/gts-raw-data",
    ref = "main",
    overwrite = TRUE,
    quiet = FALSE
  ){
  
  if(is.null(files)){
    
    repo <- unlist(strsplit(x = repo, split = "/"))
    
    repoFiles <- list_github_files(
      user = repo[1], repo = repo[2], 
      ref = ref, pattern = "\\.nc$"
    )
    
    cli_text(sprintf(fmt = "List of files of %s", paste(repo, collapse = "/")))
    
    cli_ul(items = repoFiles)
    
    cli_text(
      sprintf(
        fmt = "{.run c(%s)}", 
        paste(sprintf(fmt = "\"%s\"", repoFiles), collapse = ", ")
      )  
    )
    
    return(invisible(repoFiles))
  }
  
  # Create destination directory if needed
  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }
  
  destfiles <- setNames(
    object = character(length = length(files)), 
    nm = names(files)
  )
  for(i in seq_along(files)){
    file <- files[i]
    
    # Build raw GitHub URL
    url <- sprintf(
      "https://raw.githubusercontent.com/%s/%s/%s",
      repo, ref, file
    )
    
    # Destination file path
    destfiles[i] <- file.path(destdir, basename(file))
    
    # Check overwrite
    if(file.exists(destfiles[i]) && !overwrite){
      message("File already exists: ", destfiles[i])
      next
    }
    
    # Download
    download.file(
      url = url,
      destfile = destfiles[i],
      mode = "wb",
      quiet = quiet
    )
  }
  
  invisible(as.list(destfiles))
}

list_github_files <- function(user, repo, ref = "master", pattern = ".", ...){
  
  # Build API URL
  url <- sprintf(
    "https://api.github.com/repos/%s/%s/git/trees/%s?recursive=1",
    user, repo, ref
  )
  
  # Read JSON response
  res <- fromJSON(url)
  
  # Check for API errors
  if(is.null(res) || "message" %in% names(res)){
    cli_abort(sprintf(fmt = "Error accessing GitHub API: %s", res$message))
  }
  
  # Extract file names (exclude directories if desired)
  res$tree$path[grepl(x = res$tree$path, pattern = pattern, ...)]
}
