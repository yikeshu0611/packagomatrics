#' Imports packages of one package
#'
#' @param data data of function loadData()
#'
#' @return a dataframe contains Package, Imports and modified time
#' @export
#'
#' @examples
#' \donttest{
#' d <- loadData()
#' i <- imports(d)
#' head(i)
#' }
imports <- function(data){
    i <- paste0(data$available[,"Depends"],',',data$available[,"Imports"])
    i <- strsplit(i,' {0,}, {0,}(\n){0,}')
    i <- lapply(i,function(i) gsub(' {0,}\\(.*','',i))
    i <- lapply(i,function(i) gsub(' ','',i))
    i <- lapply(i,function(i) i[! i %in% c('R','NA')])
    names(i)=rownames(data$available)
    i=i[sapply(i, function(i) length(i))>0]
    i=lapply(1:length(i), function(j) cbind(names(i[j]),i[[j]]))
    i=do.call(rbind,i)
    i=data.frame(i,stringsAsFactors = FALSE)
    names(i)=c('Package','Imports')

    i$mtime = gsub(' .*','',data$current[i$Package,'mtime'])
    i$year = gsub('-.*','',i$mtime)
    i
}

