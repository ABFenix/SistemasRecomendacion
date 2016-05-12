usuarios_afines <- function (d_user, d_kn, d_nitems) {
  
  if (!require(data.table)) install.packages("data.table")
  if (!require(plyr)) install.packages("dplyr")
  
  new_pelis <- as.data.table(read.table("../dat/ml-100k/u.new_data", sep = "\t"))
  colnames(new_pelis) <- c("user", "item", "rating")
  
  val_user <- new_pelis[new_pelis$user == d_user,]
  val_otherusers <- new_pelis[new_pelis$user != d_user,]
  
  tmp <- merge(val_user, val_otherusers, by = "item")
  tmp <- tmp[order(tmp$user.y),]
  
  result <- ddply(tmp, .(user.y), summarize, n = length(item), cosine.dist = cor(rating.x, rating.y))
  result <- result[order(-abs(result$cosine.dist)),]
  result <- result[result$n>=d_nitems,]
  colnames(result) <- c("user", "n_peliculas", "dist")
  return (head(result, d_kn))
}