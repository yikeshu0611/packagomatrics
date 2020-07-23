adjacent.matrix <-function(data,n.imports=100,n.pkg=6){
    im <- imports(data)
    im=im[nchar(im$Imports)>0,]
    pkg = unique(im$Package)
    x=lapply(pkg, function(i) t(table(im[im$Package==i,'Imports'])))
    x2=do.call(plyr::rbind.fill.matrix,x)
    x2=ifelse(is.na(x2),0,1)
    rownames(x2)=pkg
    cols=colSums(x2)
    x3=x2[,cols>=n.imports]
    rows=rowSums(x3)
    x4=x3[rows>=n.pkg,]
    as.matrix(x4)
}

