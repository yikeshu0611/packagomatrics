
as.squared <- function(A){
    # row
    A
    rowname = row.names(A)
    colname = colnames(A)
    check= !colname %in% rowname
    A1=matrix(rep(0,ncol(A)*(sum(check))),ncol=ncol(A),
              dimnames = list(colname[check],colname))
    A1
    A2=rbind(A,A1)
    A2
    # col
    rowname = row.names(A2)
    colname = colnames(A2)
    check= !rowname %in% colname
    A3=matrix(rep(0,nrow(A2)*(sum(check))),nrow=nrow(A2),
              dimnames = list(rowname,rowname[check]))
    A3
    cbind(A2,A3)
}
