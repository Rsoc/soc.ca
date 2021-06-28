files <- list.files(path = "R/", full.names = T)
l.f   <- lapply(files, readLines)
f     <- unlist(l.f)
writeLines(f, con = "extra/soc_ca_all_scripts.R")
