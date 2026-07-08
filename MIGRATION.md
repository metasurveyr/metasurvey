# Migración: metasurvey (metapaquete)

Metapaquete "paraguas" del ecosistema, publicado en el repo
[`metasurveyr/metasurvey`](https://github.com/metasurveyr/metasurvey) (el monolito
original fue renombrado a
[`metasurvey-legacy`](https://github.com/metasurveyr/metasurvey-legacy)). Parte de la
modularización sugerida por la revisión de rOpenSci. Ver `RFC-MODULARIZACION.md` en el
repo legacy.

Responsabilidad: un solo `library(metasurvey)` carga y reexporta todo el ecosistema
(estilo tidyverse), y provee la orquestación de alto nivel que cruza varios paquetes.

## Qué reexporta

`R/reexports.R` reexporta la superficie pública (128 símbolos) de:

- **metasurvey.core** (94) — Survey/Step/Recipe/Workflow, pipeline, backend local.
- **metasurvey.explorer.backend** (27) — cliente API `api_*`, `configure_api`.
- **metasurvey.anda** (2) — `anda_download_microdata`, `anda_variables`.
- **metasurvey.fromstata** (5) — `transpile_stata`, etc.

`metasurvey.explorer.frontend` **no** se reexporta: la Shiny es opcional (deps pesadas),
se usa vía `metasurvey.explorer.frontend::explore_recipes()`.

Los 3 archivos de reexport (`reexports.R` asignaciones, `reexports-namespace.R`
directivas `@rawNamespace`, `reexports-doc.R` stub de documentación) se generan con
`data-raw/generate_reexports.R`. Regenerar tras cambios en las APIs de los sub-paquetes.

## Orquestación (corte C3)

`R/reproduce.R` contiene las 2 funciones que cruzan paquetes y por eso viven aquí:

- `reproduce_workflow()` — resuelve pesos, descarga datos de ANDA, trae recipes del
  backend y devuelve un `Survey` listo para `workflow()`. Usa `metasurvey.anda::`,
  `metasurvey.core::` (get_backend/load_survey).
- `resolve_weight_spec()` — convierte el `weight_spec` portable en config de pesos;
  descarga replicados de ANDA si hace falta. Ambas migradas de
  `metasurvey-legacy/R/utils.R`.

## Versión

`0.3.0` (el legacy quedó en `0.2.0`; el metapaquete arranca por encima para dejar claro
que es la continuación del nombre `metasurvey` en CRAN).

## Dependencias

- **Depends**: `metasurvey.core`.
- **Imports**: `metasurvey.anda`, `metasurvey.fromstata`, `metasurvey.explorer.backend`.
- **Suggests**: `metasurvey.explorer.frontend` + los paquetes que las funciones
  reexportadas tocan transitivamente (cli, data.table, gt, httr2, ...), declarados para
  satisfacer `R CMD check`.

R CMD check: 0 errors, 0 warnings, 0 notes. Tests: 14 pass.
