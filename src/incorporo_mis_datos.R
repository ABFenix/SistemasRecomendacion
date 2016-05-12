if (!require(data.table)) install.packages("data.table")
if (!require(plyr)) install.packages("dplyr")
if (!require(reshape2)) install.packages("reshape2")

valoraciones <- as.data.table(read.table("../dat/ml-100k/u.data", sep = "\t"))
colnames(valoraciones) <- c("user", "movieid", "rating", "timestamp")
valoraciones <- within(valoraciones, rm(timestamp))

dt_myval = data.table(rep(1000, 21), 
                      c(1, 2, 22, 50, 56, 69, 73, 89, 98, 249, 250, 449, 450, 548, 557, 587, 300, 5, 1221, 1240, 1423 ),
                      c(3, 3, 4, 4, 5, 5, 2, 4, 1, 1, 4, 4, 4, 5, 1, 1, 3, 2, 5, 1, 1)) 
colnames(dt_myval)=c("user", "movieid", "rating")

valoraciones <- rbind(valoraciones, dt_myval)
write.table(valoraciones, file="../dat/ml-100k/u.new_data", sep = "\t", 
            row.names = FALSE, col.names = FALSE)

usuarios <- as.data.table(read.table("../dat/ml-100k/u.user", sep = "|", quote=""))
usuarios <- within(usuarios, rm(V5))
colnames(usuarios) <- c("user", "edad", "sexo", "ocupacion")

usuarios <- rbind(usuarios, list(1000, 43, "F", "other"))
write.table(usuarios, file="../dat/ml-100k/u.new_user", sep = "|", 
            row.names = FALSE, col.names = FALSE)

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
                       "Film-Noir",  
                       "Horror",
                       "Musical",
                       "Mystery",
                       "Romance",
                       "Sci-Fi",
                       "Thriller",
                       "War",
                       "Western")

#-------------------------------------

tmp <- merge(valoraciones, generos, by.x="movieid", by.y="movieid")
tmp$movietitle <- tmp$releasedate <- tmp$videoreleasedate <- tmp$IMDbURL <- tmp$date <- NULL

tmp <- melt(tmp, id.vars = c("user", "movieid", "rating"))
tmp <- tmp[tmp$value == 1,]

tmp <- ddply(tmp, .(user, variable), summarize, rating = mean(rating))

val_generos <- dcast(tmp, user ~ variable)

val_generos[is.na(val_generos)] <- 0
write.table(val_generos, file="../dat/ml-100k/u.val_gen", sep = "|", 
            row.names = FALSE)
