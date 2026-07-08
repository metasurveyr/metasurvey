# Ver RFC-MODULARIZACION.md · metapaquete metasurvey
# Mensaje de attach estilo tidyverse: informa que se cargo el ecosistema.

.onAttach <- function(libname, pkgname) {
  pkgs <- c(
    "metasurvey.core",
    "metasurvey.fromstata",
    "metasurvey.anda",
    "metasurvey.explorer.backend"
  )
  versions <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
  }, character(1))
  lines <- paste0("  ", format(pkgs), "  ", versions)
  packageStartupMessage(
    "-- Attaching metasurvey ecosystem ------------------------------\n",
    paste(lines, collapse = "\n"),
    "\n",
    "Shiny explorer (optional): metasurvey.explorer.frontend::explore_recipes()"
  )
}
