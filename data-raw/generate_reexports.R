# Regenera R/reexports.R (asignaciones) y R/reexports-namespace.R (directivas
# NAMESPACE via @rawNamespace) para el metapaquete metasurvey.
# Ejecutar tras cambios en las APIs de los sub-paquetes:
#   Rscript data-raw/generate_reexports.R
# metasurvey.explorer.frontend NO se reexporta (shiny es Suggests).

pkgs <- c(
  "metasurvey.core",
  "metasurvey.explorer.backend",
  "metasurvey.anda",
  "metasurvey.fromstata"
)

seen <- character(0)
assigns <- character(0)
raw <- character(0)
for (p in pkgs) {
  ex <- sort(getNamespaceExports(p))
  for (fn in ex) {
    if (fn %in% seen) next
    seen <- c(seen, fn)
    is_op <- grepl("[^A-Za-z0-9._]", fn)
    nm <- if (is_op) paste0("`", fn, "`") else fn
    assigns <- c(assigns, paste0(nm, " <- ", p, "::", nm))
    raw <- c(
      raw,
      paste0("importFrom(", p, ",", if (is_op) paste0('"', fn, '"') else fn, ")"),
      paste0("export(", if (is_op) paste0('"', fn, '"') else fn, ")")
    )
  }
}

# R/reexports.R — solo las asignaciones (sin roxygen, para no confundir a roxygen2
# con los generadores R6)
con <- file("R/reexports.R", "w")
writeLines(c(
  "# Ver RFC-MODULARIZACION.md . metapaquete metasurvey",
  "# Reexporta la superficie publica del ecosistema (estilo tidyverse).",
  "# GENERADO por data-raw/generate_reexports.R: no editar a mano.",
  "# Las directivas NAMESPACE se inyectan desde R/reexports-namespace.R.",
  "",
  assigns
), con)
close(con)

# R/reexports-namespace.R — inyecta export()/importFrom() via @rawNamespace
con <- file("R/reexports-namespace.R", "w")
writeLines(c(
  "# GENERADO por data-raw/generate_reexports.R: no editar a mano.",
  "",
  "#' @rawNamespace",
  paste0("#' ", raw),
  "NULL"
), con)
close(con)

cat("regenerado:", length(seen), "simbolos\n")

# --- R/reexports-doc.R: stub de documentación (silencia "undocumented") --------
aliases <- seen[grepl("^[A-Za-z.][A-Za-z0-9._]*$", seen)]
con <- file("R/reexports-doc.R", "w")
writeLines(c(
  "# GENERADO por data-raw/generate_reexports.R: no editar a mano.",
  "",
  "#' Objects re-exported from the metasurvey ecosystem",
  "#'",
  "#' These objects are imported from other packages of the metasurvey",
  "#' ecosystem and re-exported here so a single library(metasurvey) makes",
  "#' them available. See their documentation in the source packages:",
  "#' metasurvey.core, metasurvey.fromstata, metasurvey.anda and",
  "#' metasurvey.explorer.backend.",
  "#'",
  "#' @name reexports",
  paste0("#' @aliases ", paste(aliases, collapse = " ")),
  "#' @keywords internal",
  "NULL"
), con)
close(con)
cat("reexports-doc.R con", length(aliases), "aliases\n")
