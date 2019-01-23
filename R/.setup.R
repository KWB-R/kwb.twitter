# devtools::install_github("KWB-R/kwb.orcid")
#
# remotes::install_github("KWB-R/kwb.pkgbuild", build = TRUE, build_opts = c(
#   "--no-resave-data", "--no-manual"
# ))
library(magrittr)

package <- "kwb.twitter"

fs::file_move(path = file.path(package_dir, "DESCRIPTION"), 
              new_path = file.path(package_dir, ".DESCRIPTION")
              )

desc::desc_get_field("Title",file = ".DESCRIPTION")

# Delete the original DESCRIPTION file


author <- list(
  name = "Hauke Sonnenberg",
  orcid = kwb.orcid::get_kwb_orcids()["Hauke Sonnenberg"],
  url = "https://github.com/hsonne"
)


desc_file <- ".DESCRIPTION"

deps <- list(depends = desc::desc_get_field("Depends",
                                            file = desc_file)#, 
            # depends = desc::desc_get_field("Depends",
            #                                file = desc_file), 
            # suggests = desc::desc_get_field("Suggests",
            #                                file = desc_file)
            )
stringr::str_match_all(deps$depends, pattern = "^kwb")
description <- list(
  name = package,
  title = desc::desc_get_field("Title", file = desc_file),
  desc  = desc::desc_get_field("Description", file = desc_file)
)


setwd(package_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)

desc::desc_set("Imports" = deps$depends)



deps$remotes <- stringr::str_split(deps$depends, ", ", simplify = TRUE) %>% 
  as.character() %>% stringr::str_subset("^kwb")
desc::desc_add_remotes(stringr::str_c("github::", deps$remotes))



usethis::use_r("function")

pkg_dependencies <- c("digest", "kwb.fakin", "kwb.utils", "yaml")

sapply(pkg_dependencies, usethis::use_package)

# And now, let's do the first commit and upload everything to GitHub
# (manually)...