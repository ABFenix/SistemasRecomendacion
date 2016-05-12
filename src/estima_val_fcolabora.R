estima_val_fcolabora <- function (d_user, d_peli) {
  if (!require(data.table)) install.packages("data.table")
  
  valoraciones <- as.data.table(read.table("../dat/ml-100k/u.new_data", sep = "\t"))
  colnames(valoraciones) <- c("user", "item", "rating")
  
  if (nrow(valoraciones[user==d_user & item== d_peli,])==1) {
    print ("El usuario indicado ya ha valorado esa película")
  }
  else {
    source("usuarios_afines.R")
    
    # identifico los usuarios afines con el signo de afinidad (positiva o negativa)
    afin_users <- as.data.table(usuarios_afines(d_user, 5, 10))
    afin_users$afinidad <- sign(afin_users$dist)
    afin_users <- afin_users[,.(user, afinidad)]
    
    # para los usuarios afines busco si han valorado la película en cuestión
    # si la han valorado saco el valor medio de las valoraciones
    # si no la han valorado, saco las valoraciones medias de los géneros asociados a la película
    
    val_users_afines <- valoraciones[(user %in% unlist(afin_users$user) & item== d_peli), ]
    val_users_afines[(user %in% unlist(afin_users[afinidad==-1, user])), rating:=6-rating]
    
    if (nrow(val_users_afines) > 0) {
      return (round(mean(val_users_afines$rating), digits=0))
    }
    else {
      val_generos <- as.data.table(read.table("../dat/ml-100k/u.val_gen", sep = "|", header = TRUE))
      val_generos <- val_generos[user %in% unlist(afin_users$user),]
      
      generos <- as.data.table(read.table("../dat/ml-100k/u.item", sep = "|", quote=""))
      colnames(generos) <- c("movieid",
                             "movietitle",
                             "releasedate",
                             "videoreleasedate",
                             "IMDbURL",               
                             "unknown",
                             "Action",
                             "Adventure",
                             "Animation",
                             "Children",
                             "Comedy",
                             "Crime",
                             "Documentary",
                             "Drama",
                             "Fantasy",
                             "Film.Noir",  
                             "Horror",
                             "Musical",
                             "Mystery",
                             "Romance",
                             "Sci.Fi",
                             "Thriller",
                             "War",
                             "Western")
      generos$movietitle <- generos$releasedate <- generos$videoreleasedate <- generos$IMDbURL <- generos$date <- NULL
      
      generos <- generos[movieid==d_peli,]
      lista_generos <- unlist(melt(generos, id.vars=c("movieid"))[value==1,variable])
      
      val_generos <- melt(val_generos, id.vars = c("user"))
      return(round(mean(val_generos[val_generos$variable %in% unlist(lista_generos),value]), digits=0))
    }
  }
}