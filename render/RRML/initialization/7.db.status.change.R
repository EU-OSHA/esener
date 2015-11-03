# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Mark instance as an available one

mark.as.available <- function(){
  system(paste("cd ", RRML.db.location, ";", php.command, " -f mark.as.available.php ", r.instance.screen.name,  sep = ""))
}

mark.service.as.available <- function(){
   system(paste("cd ", RRML.db.location, ";", php.command, " -f mark.service.as.available.php",  sep = ""))
}

mark.as.available()