# install.packages("miniCRAN")
# devtools::install_github("metacran/crandb")
# devtools::install_github("muschellij2/ghtravis")

# Declare global variables to get rid of the Travis R CMD check NOTEs
globalVariables(c("biocinstallRepos"))

library(miniCRAN)
library(ghtravis)
library(desc)


#' Read the Bioconductor package data
#' to be used with miniCRAN:pkgDep
#'
read_bioc <- function() {
  bioc <- local({
    env <- new.env()
    on.exit(rm(env))
    evalq(source("http://bioconductor.org/biocLite.R", local=TRUE), env)
    biocinstallRepos()
  })

  return(bioc)
}

#' Gets dependency tree for a package
#'
#' \code{get_dependency_tree} will build and return
#' a dependency tree. This is done recursively
#' for the import/depends fields and non-recursively
#' for the suggests field.
#'
#' @param pkg character vector of packages
#' @param cran search CRAN
#' @param bioc search Bioconductor
#' @param URLs URLs for third party package DBs
#'
#' @return a list of unique package names in the dependency
#'    tree
#'
#' @importFrom miniCRAN pkgDep
#'
#' @examples
#' \dontrun{get_dependency_tree('ggplot2',bioc=FALSE)}
#' \dontrun{get_dependency_tree('fslr',URLs='http://neuroconductor.org:8080')}
#'
#' @export
#'
get_dependency_tree <- function(pkg, cran=TRUE, bioc=TRUE, URLs=NULL) {
  search_params <- c(URLs)
  if(cran) {
    search_params <- c(search_params,'http://cran.us.r-project.org')
  }
  if(bioc) {
    bioc_db = read_bioc()
    search_params <- c(search_params,bioc_db)
  }
  pkgdep_tree <- tryCatch(
    {
      pkg_tree <- miniCRAN::pkgDep(pkg, repos = search_params)
      unique(pkg_tree)
    },
    error=function(cond) {
      message('No dependencies found')
      return(NULL)
    }
  )
  return(pkgdep_tree)
}

#' Gets the import/depends/suggest list from
#' a GitHub repo
#'
#' \code{get_gh_repo_dependencies} will parse
#'  a remote GitHub repo's DESCRIPTION file
#'  and it will return a list of unique
#'  dependencies (from Imports/Dependes/Suggests)
#'
#' @param slug is the GitHub repository name
#'   in the form of username/repo
#'
#' @return a list of unique package names that
#'   \code{slug} repo depends on, from the
#'   DESCRIPTION file. This is a combination
#'   of the Imports, Depends and Suggests
#'   tags.
#'
#' @importFrom ghtravis cat_desc_elements_remote
#' @importFrom utils capture.output
#'
#' @examples
#' \dontrun{get_gh_repo_dependencies('neuroconductor/fslr')}
#'
#' @export
#'
get_gh_repo_dependencies <- function(slug) {
  gh_desc <- tryCatch(
    {
      utils::capture.output(ghtravis::cat_desc_elements_remote(slug))
    },
    error=function(cond) {
      message('GitHub repo not found')
      return(NULL)
    }
  )
  if(!is.null(gh_desc)) {
    gh_deps = c(gh_desc[grep("Name", gh_desc)],gh_desc[grep("Imports", gh_desc)],gh_desc[grep("Depends", gh_desc)],gh_desc[grep("Suggests", gh_desc)])
    gh_deps = lapply(gh_deps, function(x) gsub("\\w*: ","", x))
    gh_deps = lapply(gh_deps, function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x)))
    gh_deps = lapply(gh_deps, function(x) gsub(" ","", x))
    gh_deps = paste(gh_deps,collapse=",")
    gh_deps = unique(strsplit(gh_deps,","))
    gh_deps = unlist(gh_deps)
    return(gh_deps)
  }
  else {return(NULL)}
}

#' Get repo SystemRequirements
#'
#' \code{get_single_pkg_sysreqs} returns the
#'  system requirements for a package by
#'  cross referencing the package name with
#'  the Neuroconductor maintained list of
#'  formatted system requirements
#'
#' @param pkg is the package name to be
#'  cross referenced with the Neuroconductor
#'  list of formatted system requirements, it
#'  can also be a repo slug
#' @param sysreqs_db is the data.frame containing
#'  the Google sheet system requirements list
#'
#' @return a list of formatted system requirements
#'  if the package is in the Neuroconductor system
#'  requirements list or the original system requirements
#'  for the DESCRIPTION file (NA if none)
#'
#' @importFrom ghtravis get_remote_package_dcf
#' @importFrom desc description
#'
#' @examples
#' \dontrun{get_gh_repo_sysreqs('neuroconductor/ANTsR')}
#' \dontrun{get_gh_repo_sysreqs('fslr')}
#'
get_single_pkg_sysreqs <- function(pkg, sysreqs_db) {
  if(grepl('/',pkg)) {
    pkg_name = strsplit(pkg,"/")[[1]][2]
    sysreq = format_sysreq(pkg_name, sysreqs_db)
    if(is.na(sysreq)) {
      path = get_remote_package_dcf(remotes = pkg)
      if(!is.na(path))
      {
        desc = description$new(file = path)
        sysreq = unname(desc$get('SystemRequirements'))
      }
    }
  }
  else {
    sysreq = format_sysreq(pkg, sysreqs_db)
  }
  return(sysreq)
}

#' Cross reference the \code{pkg} against
#' a custom Google sheet to get formatted
#' system requirements
#'
#' @return a list of formatted system requirements
#'  if the package is in the Neuroconductor system
#'  requirements list
#'
#' @param pkg is the package name to be
#'  cross referenced with the Neuroconductor
#'  list of formatted system requirements, it
#'  can also be a repo slug
#' @param sysreqs_db is the data.frame containing
#'  the Google sheet system requirements list
#'
format_sysreq <- function(pkg, sysreqs_db) {
  rownames(sysreqs_db) <- sysreqs_db$Package
  if (pkg %in% sysreqs_db$Package) {
    if(sysreqs_db[pkg,]$Recommended.System.Requirements != "") {
      new_sysreq = sysreqs_db[pkg,]$Recommended.System.Requirements
      return(new_sysreq)
    }
  }
  return(NA)
}

#' Get a list of all system requirements for a package
#' using the recursive dependencies tree
#'
#' @param pkg repo slug
#' @return a list with all system dependencies for \code{pkg}
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{get_all_sysreqs('neuroconductor/ANTsR')}
#'
#' @export
#'
get_all_sysreqs <- function(pkg) {
  sysreqs_db = read.csv('https://goo.gl/x7rcCD',stringsAsFactors = FALSE)
  if(grepl('/',pkg)) {
    pkg_name = strsplit(pkg,"/")[[1]][2]
    sysreq_list <- get_gh_repo_dependencies(pkg)
  }
  options(warn=-1)
  sysreq_all <- get_dependency_tree(sysreq_list,URLs='http://neuroconductor.org:8080')
  options(warn=0)
  sysreq_all <- lapply(sysreq_all, function(x) get_single_pkg_sysreqs(x,sysreqs_db))
  sysreq_all <- sysreq_all[!is.na(sysreq_all)]
  sysreq_all <- lapply(sysreq_all, function(x) gsub(" ","", x))
  sysreq_all <- paste(sysreq_all,collapse=",")
  sysreq_all <- unique(strsplit(sysreq_all,","))
  sysreq_all <- unlist(sysreq_all)
  return(sysreq_all)
}


