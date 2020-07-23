#' Load Package Information
#' @param repo CRAN repository
#' @importFrom utils available.packages
#' @return a list contains archive, current and available package information on
#'     current repository, and bioconductor package information containing
#'     Software, AnnotatonData, ExperimentData and Workflow from the current
#'     release version. see \code{\link[wherepackage]{where}}
#'
#' @export
#'
loadData <- function(repo = getOption("repos")){
    message()
    message('---------- Loading data')
    message()

    # ----------------------------------------archive
    message('    CRAN: Archive packages')
    url=paste0(do::Trim(repo,'/'),'/src/contrib/Meta/archive.rds')
    archive <- rio::import(url)
    archive <- lapply(archive, function(i) i[,c('size','mtime')])

    # --------------------------------------------current
    message('    CRAN: Current packages')
    url=paste0(do::Trim(repo,'/'),'/src/contrib/Meta/current.rds')
    current <- rio::import(url)
    current <- current[,c('size','mtime')]

    # ---------------------------------------------available
    message('    CRAN: Available packages')
    available=available.packages(repos = repo)

    # ---------------------------------------------aliases
    message('    CRAN: Aliases')
    url=paste0(do::Trim(repo,'/'),'/src/contrib/Meta/aliases.rds')
    aliases <- rio::import(url)

    # --------------------------------------detailed info for search
    message('    CRAN: detailed info for search')
    url=paste0(do::Trim(repo,'/'),'/web/packages/packages.rds')
    search <- rio::import(url)

    # bioconductor
    message('    Bioc: Software')
    bioc=available.packages(repos = 'https://bioconductor.org/packages/release/bioc')
    message('    Bioc: Annotation')
    annotate=available.packages(repos = 'https://bioconductor.org/packages/release/data/annotation')
    message('    Bioc: Experiment')
    experiment=available.packages(repos = 'https://bioconductor.org/packages/release/data/experiment')
    message('    Bioc: Wordflow')
    workflows=available.packages(repos = 'https://bioconductor.org/packages/release/workflows')
    bioc=rbind(bioc,annotate,experiment,workflows)
    message()
    message('---------- OK')
    message()
    list(current=current,
         archive=archive,
         available=available,
         aliases=aliases,
         search=search,
         bioconductor=bioc)
}
