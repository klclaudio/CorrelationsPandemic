#
# 
#
type_blood_f <- function (aux_mat) { 
   
   type_blood <- c()
   type_blood <- rbind( type_blood, t(aux_mat[, 1:9] %*% c(0, 1, 0, 0, 0, 1, 0, 0, 0)) ) # O
   type_blood <- rbind( type_blood, t(aux_mat[, 1:9] %*% c(0, 0, 1, 0, 0, 0, 1, 0, 0)) ) # A
   type_blood <- rbind( type_blood, t(aux_mat[, 1:9] %*% c(0, 0, 0, 1, 0, 0, 0, 1, 0)) ) # B
   type_blood <- rbind( type_blood, t(aux_mat[, 1:9] %*% c(0, 0, 0, 0, 1, 0, 0, 0, 1)) ) # AB
   type_blood <- rbind( type_blood, t(aux_mat[, 2:5] %*% c(1, 1, 1 ,1)) )                # Rh +
   type_blood <- rbind( type_blood, t(aux_mat[, 6:9] %*% c(1, 1, 1, 1)) )                # Rh -

   return(type_blood)
} #
