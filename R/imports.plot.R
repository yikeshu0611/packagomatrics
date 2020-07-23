#' Plot for imports frequency data
#'
#' @param imports.most results of function imports.most()
#' @importFrom ggplot2 ggplot geom_col aes facet_wrap coord_flip
#' @importFrom ggplot2 theme xlab ylab element_text
#' @importFrom tidytext reorder_within scale_x_reordered
#' @return a ggplot2 style plot
#' @export
#'
imports.plot <- function(imports.most){
    Imports=Freq=year=0
    if ('year' %in% colnames(imports.most)){
        ggplot(imports.most,
               aes(reorder_within(Imports,Freq,year),Freq))+
            geom_col()+
            scale_x_reordered() +
            facet_wrap(~year,scales="free")+
            coord_flip()+
            theme(
                axis.text = element_text(size=16),
                strip.text.x = element_text(size = 18,
                                            colour = "red")
            )+
            xlab(NULL)+ylab(NULL)
    }else{
        ggplot(imports.most,aes(Imports,Freq))+
            geom_col()+
            coord_flip()+
            theme(axis.text = element_text(size=16))+
            xlab(NULL)+ylab(NULL)
    }

}
