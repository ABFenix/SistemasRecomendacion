cold_start <- function (d_titulo, d_generos) {
  
  if (!require(data.table)) install.packages("data.table")
  if (!require(plyr)) install.packages("dplyr")
  if (!require(reshape2)) install.packages("reshape2")
  
  val_generos <- read.table("../dat/ml-100k/u.val_gen", sep = "|", header = TRUE)
  
  pelis <- read.table("../dat/ml-100k/u.item", sep = "|", quote = "")
  
  titulos_pelis <- pelis[,1:2]
  colnames(titulos_pelis) <- c("id","titulo")
  titulos_pelis <- data.table(titulos_pelis)
  
  id_peli <- titulos_pelis[toupper(titulo) == toupper(d_titulo), id]
  
  if (length(id_peli) >= 1) {
    print ("La pelicula se encuentra entre las valoradas por los usuarios")
  } 
  else {
    val_select <- melt(val_generos, id.vars=("user"))
    val_select <- val_select[val_select$variable %in% unlist(d_generos),]
    colnames(val_select) <- c("user", "genero", "mean_rating")
    val_select <- dcast(val_select, user ~ ., fun=mean)
    colnames(val_select) <- c("user", "mean_generos")
    top_clients <- val_select[order(-val_select$mean_generos),]
    return(head(top_clients$user,10))
  }
}