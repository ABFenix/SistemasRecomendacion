usuarios_semejantes <- function (d_user) {
  
  if (!require(data.table)) install.packages("data.table")
  
  usuarios <- as.data.table(read.table("../dat/ml-100k/u.new_user", sep = "|", quote=""))
  colnames(usuarios) <- c("user", "edad", "sexo", "ocupacion")
  datos_user <- usuarios[user == d_user,]
  
  parecidos <- usuarios[edad %in% c((datos_user$edad-5):(datos_user$edad+5)) &
                              sexo==datos_user$sexo & 
                              ocupacion==datos_user$ocupacion &
                              user != d_user, ]
  return (parecidos)
}