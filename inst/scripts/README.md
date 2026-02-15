# Database Scripts

Scripts para configurar y poblar la base de datos MongoDB del ecosistema metasurvey.

## Prerequisitos

- [mongosh](https://www.mongodb.com/docs/mongodb-shell/) (MongoDB Shell)
- R con paquetes: `jsonlite`, `mongolite`, `xml2` (para ANDA), `httr`
- Variable de entorno `METASURVEY_MONGO_URI` con la connection string

## Archivos

```
inst/
├── scripts/
│   ├── setup_mongodb.js        # Crea colecciones + schema + indexes
│   ├── seed_ech_recipes.R      # Pobla recetas/workflows/users desde JSON
│   └── seed_anda_metadata.R    # Descarga DDI de ANDA5, parsea y seedea variables
└── seed-data/                  # Fuente unica de verdad
    ├── recipes.json            # 13 recetas ECH
    ├── workflows.json          # 4 workflows ECH
    ├── users.json              # Usuarios seed (autores de recetas)
    └── anda_variables.json     # ~616 variables ECH (backup del DDI)
```

## Setup inicial (base nueva)

```bash
# 1. Crear colecciones con schema validation e indexes
mongosh "$METASURVEY_MONGO_URI" inst/scripts/setup_mongodb.js

# 2. Poblar recetas, workflows, usuarios
METASURVEY_MONGO_URI="..." Rscript inst/scripts/seed_ech_recipes.R

# 3. Poblar metadatos ANDA (descarga DDI XML de INE)
METASURVEY_MONGO_URI="..." Rscript inst/scripts/seed_anda_metadata.R
```

## Migracion de schema (base existente)

MongoDB Atlas free tier no soporta `collMod`, asi que para actualizar
el schema hay que dropear y recrear las colecciones:

```bash
# 1. Dropear colecciones (el seed las repobla)
mongosh "$METASURVEY_MONGO_URI" --eval '
  db.recipes.drop();
  db.workflows.drop();
  db.anda_variables.drop();
'

# 2. Recrear con schema actualizado
mongosh "$METASURVEY_MONGO_URI" inst/scripts/setup_mongodb.js

# 3. Re-seedear
METASURVEY_MONGO_URI="..." Rscript inst/scripts/seed_ech_recipes.R
METASURVEY_MONGO_URI="..." Rscript inst/scripts/seed_anda_metadata.R
```

Los usuarios NO se dropean; el seed script hace upsert por email.

## ANDA Metadata

El script `seed_anda_metadata.R` descarga el DDI XML del catalogo
[ANDA5 de INE Uruguay](https://www4.ine.gub.uy/Anda5/) y extrae
metadatos de cada variable: nombre, label, tipo, value labels.

Por defecto usa el catalogo ECH 2024 (ID 767). Para otra edicion:

```bash
ANDA_CATALOG_ID=735 METASURVEY_MONGO_URI="..." Rscript inst/scripts/seed_anda_metadata.R
```

IDs conocidos: 2021→716, 2022→730, 2023→735, 2024→767

El script tambien guarda un backup JSON en `inst/seed-data/anda_variables.json`.

## Seed data (JSON)

Los archivos en `inst/seed-data/` son la **fuente unica de verdad**.
Para agregar o modificar recetas/workflows:

1. Editar el JSON correspondiente en `inst/seed-data/`
2. Re-correr el seed script

El seed script limpia solo los IDs que conoce (los que estan en el JSON),
no borra datos creados por usuarios via la API.

### Campos clave

**recipes.json:**
- `id`: Identificador unico (ej: `ech_employment_001`)
- `edition`: String o array de strings (ej: `["2015", ..., "2024"]`)
- `depends_on_recipes`: Array de IDs de recetas prerequisito
- `data_source`: Objeto con `s3_bucket`, `s3_prefix`, `file_pattern`, `provider`
- `doc`: Pipeline documentado con `input_variables`, `output_variables`, steps

**workflows.json:**
- `id`: Identificador unico (ej: `ech_wf_labor`)
- `edition`: String o array de strings
- `recipe_ids`: Recetas que debe aplicar antes de estimar
- `calls`: Expresiones `survey::svymean(...)` como strings
- `call_metadata`: Descripcion estructurada de cada estimacion

## Colecciones MongoDB

| Coleccion | Docs | Descripcion |
|-----------|------|-------------|
| `users` | ~7 | Usuarios registrados |
| `recipes` | 13 | Recetas ECH |
| `workflows` | 4 | Workflows ECH |
| `anda_variables` | ~616 | Metadatos de variables ECH (del DDI) |

## Variables de entorno

| Variable | Default | Descripcion |
|----------|---------|-------------|
| `METASURVEY_MONGO_URI` | — | Connection string MongoDB (requerida) |
| `METASURVEY_DB` | `metasurvey` | Nombre de la base de datos |
| `ANDA_CATALOG_ID` | `767` | ID del catalogo ANDA (solo para seed_anda) |
