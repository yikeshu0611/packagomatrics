#' Plot sankey picture
#'
#' @param imports.most results of function imports.most()
#' @importFrom ggalluvial geom_stratum geom_alluvium StatStratum
#' @importFrom ggplot2 geom_text scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 theme_bw element_blank
#' @importFrom grid unit
#' @return a sankey plot
#' @export
#'
imports.sankey <- function(imports.most){
    for (i in unique(imports.most$year)) {
        imports.most[imports.most$year==i,'Freq']=((imports.most$Freq)[imports.most$year==i])/(sum((imports.most$Freq)[imports.most$year==i]))
    }
    year=Freq=Imports=0
    ggplot(imports.most,
           aes(x = as.factor(year),
               y = Freq,
               weight=Freq,
               alluvium = Imports,
               stratum = Imports,
               fill = Imports,
               label = Imports))+
        geom_stratum(alpha = .5,decreasing=F)+
        geom_alluvium(decreasing=F)+
        geom_text(stat = "stratum", size = 5,decreasing=F)+
        xlab(NULL)+ylab(NULL)+
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0))+
        theme_bw() +
        theme(panel.grid = element_blank(),
              panel.border = element_blank(),
              legend.position="none",
              axis.text = element_text(size=15),
              plot.background = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(1,1,1,1),'cm')
        )
}
