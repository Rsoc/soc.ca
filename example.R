# A working example
data 		   <- read.csv("data.csv", sep ="|", encoding="latin1")
idento 		 <- as.factor(1:nrow(data))
data 		   <- data.frame(data, idento)

rm(idento)
attach(data)

inaktive 	 <- c("MISSING", "Missing", "missing", "Irrelevant", "8888", "Uoplyst", "Ved ikke om har været udstationeret som embedsperson", "stillingfoerUdlandet")
identifier <- idento

analyse 	 <- data.frame(ekstra_udd, vidart, uni_underv, lederfoer, alder_ved_foerst_admin, numiniaar, stillingfoer,
		         antal_stillinger, ejendomsvaerdi_cat, ordenfin, offentlig_bestyrelse, medie, boeger, education_type)

sup 		   <- data.frame(udstationeret, phd_udd, koen, titel, ministerium_rangeret, foedeby, alder_kat5)

