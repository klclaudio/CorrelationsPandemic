#
# Test and create directories
#

create_dir_f <- function( path_name ) {
  
  if (dir.exists(path_name) == FALSE) {
    
       dir.create( path_name );
    
    }

  
return()  
}# End createdir