#' Find package by query words
#' @description Find package from package description field.
#' @param data results of functin loadData()
#' @param query one or more query words
#'
#' @return relative packages
#' @export
#'
#' @examples
#' \donttest{
#' d <- loadData()
#' findpkg(d,c('table','one'))
#' }
findpkg <- function(data,query){
    x=data$search[,"Description"]
    pkg=data$search[,"Package"]
    x2=lapply(query, function(i) as.numeric(grepl(i,x)))
    for (i in 1:length(query)) {
        if (i==1){
            res=x2[[i]]
        }else{
            res=res+x2[[i]]
        }
    }
    pkg[res==length(query)]
}