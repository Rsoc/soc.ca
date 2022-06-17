library(soc.ca)
data(taste)

colnames(taste)
data(directors)

attach(directors)

Career     <- data.frame(careerprofile_maclean_cat, years_between_edu_dir_cat,
                         time_in_corp_before_ceo_cat, age_as_ceo_cat, career_changes_cat2, mba, hd, abroad)
Education  <- data.frame(phd, education, author, careerfoundation_maclean_cat)

Family     <- data.frame(placeofbirth, familyclass_bourdieu, partnersfamily_in_whoswho, "Family in Who's Who" = family_in_whoswho, check.names = FALSE)

sup       	<- data.frame(size_prestige, ownership_cat_2, sector, location)

id          <- navn
detach(directors)

active     <- list(Career = Career, Education = Education, Family = Family)

options(passive = c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))

object <- soc.mca(active)
object$headings