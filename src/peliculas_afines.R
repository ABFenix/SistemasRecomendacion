peliculas_afines <- function (d_titulo, d_kn) {
  
  if (!require(data.table)) install.packages("data.table")
  
  pelis <- as.data.table(read.table("../dat/ml-100k/u.data", sep = "\t"))
  colnames(pelis) <- c("user", "item", "rating", "timestamp")
  pelis <- within(pelis, rm(timestamp))
  
  titulos_pelis <- as.data.table(read.table("../dat/ml-100k/u.item", sep = "|", quote=""))
  titulos_pelis <- titulos_pelis[,.(V1,V2)]
  colnames(titulos_pelis) <- c("id","titulo")
  
  id_peli <- titulos_pelis[toupper(titulo) == toupper(d_titulo), id]
  
  if (length(id_peli) == 1) {
    val_peli <- pelis[pelis$item == id_peli,]
    val_otherpelis <- pelis[pelis$item != id_peli,]
    tmp <- merge(val_peli, val_otherpelis, by = "user")
    tmp <- tmp[order(tmp$item.y),]
    
    result <- tmp[, `:=`(n=length(user), cosine.dist=cor(rating.x, rating.y)), by=item.y]
    result <- unique(result[,.(item.y, n, cosine.dist)])
    result <- merge(result, titulos_pelis, all.x=TRUE, by.x="item.y", by.y="id")
    result <- result[order(-result$cosine.dist),]
    result <- result[result$n>=10,]
    return (head(result$titulo, d_kn))
  }
  else {
    print ("La pelicula no se encuentra entre las valoradas por los usuarios")
    return (c(""))
  }
}