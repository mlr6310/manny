varmatch_generic <- function(var_vector, df_key){
  print("vartable is vector; first df_key col is uncorrected var")
  neworder <- df_key[match(var_vector, df_key[,1]),2]
  neworder
}