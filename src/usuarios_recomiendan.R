usuarios_recomiendan <- function (d_users, d_user_base, d_kn) {
  
  if (!require(data.table)) install.packages("data.table")
  if (!require(plyr)) install.packages("dplyr")
  
  new_pelis <- as.data.table(read.table("../dat/ml-100k/u.new_data", sep = "\t"))
  colnames(new_pelis) <- c("user", "item", "rating")
  
  titulos_pelis <- as.data.table(read.table("../dat/ml-100k/u.item", sep = "|", quote = ""))
  titulos_pelis <- titulos_pelis[,.(V1,V2)]
  colnames(titulos_pelis) <- c("id","titulo")
  
  colnames(d_users) <- c("user", "afinidad")
  
  pelis_vistas <- new_pelis[user==d_user_base, item]
  new_pelis <- new_pelis[user %in% unlist(d_users[,1, with=FALSE]),]
  pelis_no_vistas <- new_pelis[!(item %in% unlist(pelis_vistas)),]
  
  if (nrow(new_pelis) > 0) {
    if (nrow(pelis_no_vistas) > 0) {
      # cambiamos la valoracion de los usuarios que tienen gustos opuestos al usuario_base
      usuarios_opuestos <- d_users[afinidad==-1, user]
      
      new_val <- pelis_no_vistas[user %in% unlist(usuarios_opuestos),]
      new_val$new_rating  <- (5 + 1) - new_val$rating
      
      same_val <- pelis_no_vistas[!(user %in% unlist(usuarios_opuestos)),]
      same_val$new_rating <- same_val$rating
      
      new_val <- rbind(new_val, same_val)
      colnames(new_val)
      
      pelis_recom <- ddply(new_val, .(item, new_rating), summarize, n = length(user))
      pelis_recom <- data.table(pelis_recom)[(n>=d_kn & new_rating > 3), ][order(-new_rating), item]
      
      titulos_recom <- titulos_pelis[id %in% unlist(pelis_recom), titulo]
      
      return (titulos_recom)
    }
    else {
      print("Los usuarios afines a ti no han visto peliculas diferentes a las tuyas")
    }
  }
  else {
    print ("Los usuarios facilitados no han valorado peliculas")
    return (c(""))
  }
}

