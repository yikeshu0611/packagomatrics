#' Mostly imported packages
#'
#' @param imports results of function imports()
#' @param n the most frequency number
#' @param year logical, default is FALSE. Whether to include years
#'
#' @return a data.frame contains package, frequency or year
#' @export
#'
#' @examples
#' \donttest{
#'     d <- loadData()
#'     i <- imports(d)
#'     imt <- imports.most(imports = i,20)
#'
#'     library(ggplot2)
#'     ggplot(imt,aes(Imports,Freq))+
#'         geom_col()+
#'         coord_flip()+
#'         theme(axis.text = element_text(size=16))
#'
#'
#'     imt <- imports.most(i,10,T)
#'     imt <- imt[imt$year >= 2011 & imt$year <= 2020,]
#'     # the latest 10 years
#'     library(ggplot2)
#'     library(tidytext)
#'
#'     ggplot(imt,aes(reorder_within(Imports,Freq,year),Freq))+
#'         geom_col()+
#'         scale_x_reordered() +
#'         facet_wrap(~year,scales="free")+
#'         coord_flip()+
#'         theme(
#'             axis.text = element_text(size=16),
#'             strip.text.x = element_text(size = 18,
#'                                         colour = "red")
#'         )+
#'         xlab(NULL)+ylab(NULL)
#' }
imports.most <- function(imports,n=10,year=FALSE){
    if (year){
        df=data.frame(table(imports$Imports,imports$year),stringsAsFactors = FALSE)
        colnames(df)[1:2]=c('Imports','year')
        df=df[nchar(as.character(df$Imports))>0,]
        df=df[order(df$Freq,decreasing = TRUE),]
        year=as.character(unique(df$year))
        df=do.call(rbind,lapply(year,function(i) head(df[df$year==i,],n)))
        rownames(df)=NULL
        df[,1] = factor(df[,1],levels = rev(unique(df[,1])))
        df$year=as.numeric(as.character(df$year))
        df
    }else{
        tb=table(imports$Imports)
        df=data.frame(head(tb[order(tb,decreasing = TRUE)],n),stringsAsFactors = FALSE)
        df[,1] = factor(df[,1],levels = rev(df[,1]))
        colnames(df)[1]='Imports'
        df
    }
}
