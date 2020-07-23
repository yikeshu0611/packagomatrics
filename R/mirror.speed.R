#' Test speed of mirror
#'
#' @param min.second the minium second time to visit the mirror web page
#' @param cran logical, whether to test CRAN mirrors. Default is TRUE
#' @param bioc logical, whether to test bioconductor mirrors. Default is TRUE
#'
#' @return repositories which visiting time is minus the minium second.
#' @export
#'
#' @examples
#' # test cran and bioconductor mirrors
#' mirror.speed()
#'
#' # test bioconductor mirrors only
#' mirror.speed(cran=FALSE)
mirror.speed <- function(min.second=0.2,
                         cran=TRUE,
                         bioc=TRUE){
    res=list()
    # CRAN
    if (cran){
        cat(crayon::bgWhite('\n---test CRAN mirror---'))
        message('\nLoading CRAN mirrors')
        repo.test=read.csv(paste0(getOption('repos'),'CRAN_mirrors.csv'),stringsAsFactors = FALSE)$URL
        message('test ',length(repo.test),' mirrors')
        for (i in 1:length(repo.test)) {
            if (i==1) mirror=time.all=c()
            time=system.time({
                f=tryCatch(
                    httr::GET(url=repo.test[i],
                              httr::timeout(min.second)),
                    error=function(e) paste0(i,',')
                )
            })
            if (is.list(f)){
                mirror=c(mirror,repo.test[i])
                time.all=c(time.all,time[3])
                message('\n',repo.test[i],' ---',round(time[3],4),'s')
            }else{
                cat(f)
            }
        }
        res$cran=if(!is.null(time.all)) mirror[order(time.all)]
    }
    # bioc
    if (bioc){
        cat(crayon::bgWhite('\n\n---test bioconductor mirror---'))
        message('\nLoading bioconductor mirrors')
        repo.test=read.csv('https://bioconductor.org/BioC_mirrors.csv',stringsAsFactors = FALSE)$URL
        message('test ',length(repo.test),' mirrors')
        for (i in 1:length(repo.test)) {
            if (i==1) mirror=time.all=c()
            time=system.time({
                f=tryCatch(
                    httr::GET(url=repo.test[i],
                              httr::timeout(min.second)),
                    error=function(e) paste0(i,',')
                )
            })
            if (is.list(f)){
                mirror=c(mirror,repo.test[i])
                time.all=c(time.all,time[3])
                message('\n',repo.test[i],' ---',round(time[3],4),'s')
            }else{
                cat(f)
            }
        }
        res$bioc=if(!is.null(time.all)) mirror[order(time.all)]
    }
    invisible(res)
}
