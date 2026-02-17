# ==============================================================================
# metasurvey Data API — plumber + mongolite
#
# Endpoints:
#   POST /auth/register    — Register a new user
#   POST /auth/login       — Login, returns JWT
#   GET  /auth/me          — Get current user profile (requires JWT)
#
#   GET  /recipes          — List/search/filter recipes
#   GET  /recipes/:id      — Get single recipe by ID
#   POST /recipes          — Publish a recipe (requires JWT)
#   POST /recipes/:id/download — Increment download counter
#
#   GET  /workflows          — List/search/filter workflows
#   GET  /workflows/:id      — Get single workflow by ID
#   POST /workflows          — Publish a workflow (requires JWT)
#   POST /workflows/:id/download — Increment download counter
#
#   GET  /indicators              — List/search/filter indicators
#   GET  /indicators/:id          — Get single indicator
#   GET  /indicators/:id/recipe   — Get recipe that produced it
#   GET  /indicators/:id/workflow — Get workflow (estimation + design)
#   POST /indicators              — Publish an indicator (requires JWT)
#   POST /indicators/compute      — Request on-demand computation (requires JWT)
#
# Config (env vars):
#   METASURVEY_MONGO_URI           — MongoDB connection string (required)
#   METASURVEY_DB                  — Database name (default: metasurvey)
#   METASURVEY_JWT_SECRET          — JWT signing secret
#   METASURVEY_WORKER_URL          — Internal worker URL (e.g. http://worker:8788)
#   METASURVEY_ENABLE_INDICATORS   — "1" (default) or "0" to disable /indicators
#   METASURVEY_ENABLE_WORKER       — "0" (default) or "1" to enable POST /compute
#
# Launch:
#   METASURVEY_MONGO_URI="mongodb+srv://user:pass@cluster.mongodb.net" \
#     Rscript -e 'plumber::plumb("inst/api/plumber.R")$run(port=8787)'
# ==============================================================================

library(plumber)
library(jsonlite)
library(mongolite)
library(digest)
library(jose)

# -- Global serializer (auto_unbox so scalars are JSON scalars, not arrays) ----

#* @plumber
function(pr) {
  pr$setSerializer(serializer_json(auto_unbox = TRUE))

  # Enhance OpenAPI spec with security scheme and API info
  spec <- pr$getApiSpec()
  spec$info <- list(
    title = "metasurvey API",
    description = paste(
      "REST API for sharing survey recipes, estimation workflows,",
      "published indicators with traceability, and",
      "ANDA variable metadata. Built with plumber + MongoDB.",
      "\n\nAuthentication: POST /auth/login returns a JWT token.",
      "Include it as `Authorization: Bearer <token>` in requests",
      "that require authentication."
    ),
    version = "2.0.0",
    contact = list(
      name = "metasurvey",
      url = "https://metasurveyr.github.io/metasurvey"
    )
  )
  spec$components$securitySchemes <- list(
    bearerAuth = list(
      type = "http",
      scheme = "bearer",
      bearerFormat = "JWT",
      description = paste0(
        "JWT token from /auth/login (24h)",
        " or /auth/generate-token (90 days)"
      )
    )
  )
  pr$setApiSpec(spec)
}

# -- Config -------------------------------------------------------------------

MONGO_URI <- Sys.getenv("METASURVEY_MONGO_URI", "")
DATABASE <- Sys.getenv("METASURVEY_DB", "metasurvey")
JWT_SECRET <- Sys.getenv(
  "METASURVEY_JWT_SECRET",
  "metasurvey-dev-secret-change-me"
)
ADMIN_EMAIL <- Sys.getenv("METASURVEY_ADMIN_EMAIL", "")
WORKER_URL <- Sys.getenv("METASURVEY_WORKER_URL", "")

# Feature flags — disable modules not available in this deployment
ENABLE_INDICATORS <- tolower(Sys.getenv(
  "METASURVEY_ENABLE_INDICATORS", "1"
)) %in% c("1", "true", "yes")
ENABLE_WORKER <- tolower(Sys.getenv(
  "METASURVEY_ENABLE_WORKER", "0"
)) %in% c("1", "true", "yes")

if (!nzchar(MONGO_URI)) {
  stop(
    "METASURVEY_MONGO_URI environment variable is required. ",
    "Set it to your MongoDB connection string, e.g.:\n",
    paste0(
      "  export METASURVEY_MONGO_URI=",
      "'mongodb+srv://user:pass",
      "@cluster.mongodb.net'"
    )
  )
}

# -- mongolite connections (one per collection, reused) -----------------------

db_users <- mongolite::mongo(
  collection = "users",
  db = DATABASE, url = MONGO_URI
)
db_recipes <- mongolite::mongo(
  collection = "recipes",
  db = DATABASE, url = MONGO_URI
)
db_workflows <- mongolite::mongo(
  collection = "workflows",
  db = DATABASE, url = MONGO_URI
)
db_anda <- mongolite::mongo(
  collection = "anda_variables",
  db = DATABASE, url = MONGO_URI
)
db_stars <- mongolite::mongo(
  collection = "stars",
  db = DATABASE, url = MONGO_URI
)
db_comments <- mongolite::mongo(
  collection = "comments",
  db = DATABASE, url = MONGO_URI
)
if (ENABLE_INDICATORS) {
  db_indicators <- mongolite::mongo(
    collection = "indicators",
    db = DATABASE, url = MONGO_URI
  )
}

message(sprintf(
  "[metasurvey-api] Connected to MongoDB: %s (db: %s)",
  sub("://.*@", "://***@", MONGO_URI),
  DATABASE
))
message(sprintf(
  paste0(
    "[metasurvey-api] Collections",
    " — users: %d, recipes: %d,",
    " workflows: %d, anda: %d"
  ),
  db_users$count(), db_recipes$count(),
  db_workflows$count(), db_anda$count()
))
if (ENABLE_INDICATORS) {
  message(sprintf(
    "[metasurvey-api] Indicators: %d",
    db_indicators$count()
  ))
}
message(sprintf(
  "[metasurvey-api] Features — indicators: %s, worker: %s",
  if (ENABLE_INDICATORS) "ON" else "OFF",
  if (ENABLE_WORKER) "ON" else "OFF"
))

# -- JWT helpers --------------------------------------------------------------

jwt_encode <- function(payload) {
  payload$iat <- as.integer(Sys.time())
  payload$exp <- as.integer(Sys.time()) + 86400L # 24h
  jwt_claim <- jose::jwt_claim(
    iss = "metasurvey-api",
    sub = payload$email,
    name = payload$name,
    user_type = payload$user_type,
    exp = payload$exp,
    iat = payload$iat
  )
  jose::jwt_encode_hmac(jwt_claim, charToRaw(JWT_SECRET))
}

jwt_decode <- function(token) {
  tryCatch(
    jose::jwt_decode_hmac(token, charToRaw(JWT_SECRET)),
    error = function(e) NULL
  )
}

hash_password <- function(pw) {
  digest::digest(pw, algo = "sha256", serialize = FALSE)
}

# -- Auth middleware -----------------------------------------------------------

get_user_from_request <- function(req) {
  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header)) {
    return(NULL)
  }
  token <- sub("^Bearer\\s+", "", auth_header)
  if (!nzchar(token)) {
    return(NULL)
  }
  jwt_decode(token)
}

require_auth <- function(req, res) {
  user <- get_user_from_request(req)
  if (is.null(user)) {
    res$status <- 401L
    return(list(
      error = paste0(
        "Authentication required.",
        " Send Authorization:",
        " Bearer <token>"
      )
    ))
  }
  user
}

require_admin <- function(req, res) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }
  if (!nzchar(ADMIN_EMAIL) || user$sub != ADMIN_EMAIL) {
    res$status <- 403L
    return(list(error = "Admin access required"))
  }
  user
}

# -- CORS ---------------------------------------------------------------------

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200L
    return(list())
  }
  plumber::forward()
}

# ==============================================================================
# AUTH
# ==============================================================================

#* Register a new user. Individual accounts
#* are auto-approved. Institutional accounts
#* require admin review.
#* @tag Auth
#* @post /auth/register
#* @param name:str User display name
#* @param email:str Email address (must be unique)
#* @param password:str Password (hashed with SHA-256 before storage)
#* @param user_type:str Account type:
#*   individual, institutional_member,
#*   or institution
#* @param institution:str Institution name (required for institutional_member)
#* @response 201 Account created.
#*   Returns {ok, token, user} for individual;
#*   {ok, pending, message, user} for
#*   institutional.
#* @response 400 Missing required fields or invalid user_type.
#* @response 409 Email already registered.
function(req, res, name, email, password,
         user_type = "individual",
         institution = NULL) {
  if (missing(name) || missing(email) || missing(password)) {
    res$status <- 400L
    return(list(error = "name, email, and password are required"))
  }

  valid_types <- c("individual", "institutional_member", "institution")
  if (!user_type %in% valid_types) {
    res$status <- 400L
    return(list(error = paste(
      "user_type must be one of:",
      paste(valid_types, collapse = ", ")
    )))
  }

  if (user_type == "institutional_member" &&
    (is.null(institution) ||
      !nzchar(institution))) {
    res$status <- 400L
    return(list(error = "institution is required for institutional_member"))
  }

  # Check duplicate
  existing <- db_users$find(
    query = toJSON(list(email = email), auto_unbox = TRUE),
    limit = 1
  )
  if (nrow(existing) > 0) {
    res$status <- 409L
    return(list(error = "Email already registered"))
  }

  # Institutional accounts require admin review
  needs_review <- user_type %in% c("institution", "institutional_member")
  review_status <- if (needs_review) "pending" else "approved"

  doc <- toJSON(list(
    name = name,
    email = email,
    password_hash = hash_password(password),
    user_type = user_type,
    institution = if (is.null(institution)) NULL else institution,
    verified = FALSE,
    review_status = review_status,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  ), auto_unbox = TRUE, null = "null")

  tryCatch(
    {
      db_users$insert(doc)

      if (needs_review) {
        res$status <- 201L
        list(
          ok = TRUE,
          pending = TRUE,
          message = paste0(
            paste0(
              "Account created.",
              " Institutional accounts",
              " require admin review",
              " before activation. "
            ),
            "You will be able to login once your account is approved."
          ),
          user = list(
            name = name,
            email = email,
            user_type = user_type,
            review_status = "pending"
          )
        )
      } else {
        token <- jwt_encode(list(
          email = email,
          name = name,
          user_type = user_type
        ))
        res$status <- 201L
        list(
          ok = TRUE,
          token = token,
          user = list(
            name = name,
            email = email,
            user_type = user_type,
            review_status = "approved"
          )
        )
      }
    },
    error = function(e) {
      res$status <- 500L
      list(error = paste("Registration failed:", e$message))
    }
  )
}

#* Login and receive a JWT token (24h expiry).
#* Use the token in Authorization:
#* Bearer <token> header.
#* @tag Auth
#* @post /auth/login
#* @param email:str Registered email address
#* @param password:str Account password
#* @response 200 Returns {ok, token, user}.
#* @response 401 Invalid email or password.
#* @response 403 Account pending review or rejected.
function(req, res, email, password) {
  if (missing(email) || missing(password)) {
    res$status <- 400L
    return(list(error = "email and password are required"))
  }

  query <- toJSON(
    list(
      email = email,
      password_hash = hash_password(password)
    ),
    auto_unbox = TRUE
  )
  result <- db_users$find(query = query, limit = 1)

  if (nrow(result) == 0) {
    res$status <- 401L
    return(list(error = "Invalid email or password"))
  }

  doc <- as.list(result[1, ])

  # Check review status for institutional accounts
  rs <- doc$review_status %||% "approved"
  if (rs == "pending") {
    res$status <- 403L
    return(list(
      error = "Account pending review",
      review_status = "pending",
      message = paste0(
        "Your institutional account is",
        " awaiting admin approval.",
        " You will be notified once",
        " reviewed."
      )
    ))
  }
  if (rs == "rejected") {
    res$status <- 403L
    return(list(
      error = "Account not approved",
      review_status = "rejected",
      message = paste0(
        "Your institutional account",
        " request was not approved.",
        " Contact the admin for details."
      )
    ))
  }

  token <- jwt_encode(list(
    email = doc$email,
    name = doc$name,
    user_type = doc$user_type
  ))
  list(
    ok = TRUE,
    token = token,
    user = list(
      name = doc$name,
      email = doc$email,
      user_type = doc$user_type,
      institution = doc$institution,
      verified = isTRUE(doc$verified),
      review_status = rs
    )
  )
}

#* Get current user profile. Requires JWT authentication.
#* @tag Auth
#* @get /auth/me
#* @response 200 Returns {name, email,
#*   user_type, institution, verified,
#*   created_at}.
#* @response 401 Authentication required.
function(req, res) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  result <- db_users$find(
    query = toJSON(list(email = user$sub), auto_unbox = TRUE),
    limit = 1
  )

  if (nrow(result) == 0) {
    res$status <- 404L
    return(list(error = "User not found"))
  }

  d <- as.list(result[1, ])
  list(
    name = d$name,
    email = d$email,
    user_type = d$user_type,
    institution = d$institution,
    verified = isTRUE(d$verified),
    created_at = d$created_at
  )
}

#* Refresh JWT token. Issue a new 24h token. Requires valid (non-expired) JWT.
#* @tag Auth
#* @post /auth/refresh
#* @response 200 Returns {ok, token}.
function(req, res) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  # Issue a new token with fresh expiry
  token <- jwt_encode(list(
    email = user$sub,
    name = user$name,
    user_type = user$user_type
  ))
  list(ok = TRUE, token = token)
}

#* Generate a long-lived API token (90 days)
#* for R scripting. Requires JWT authentication.
#* @tag Auth
#* @post /auth/generate-token
#* @response 200 Returns {ok, token, expires_in, expires_at}.
function(req, res) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  payload <- list(
    email = user$sub,
    name = user$name,
    user_type = user$user_type
  )
  payload$iat <- as.integer(Sys.time())
  payload$exp <- as.integer(Sys.time()) + 90L * 86400L # 90 days

  jwt_claim <- jose::jwt_claim(
    iss = "metasurvey-api",
    sub = payload$email,
    name = payload$name,
    user_type = payload$user_type,
    exp = payload$exp,
    iat = payload$iat
  )
  token <- jose::jwt_encode_hmac(jwt_claim, charToRaw(JWT_SECRET))

  list(
    ok = TRUE,
    token = token,
    expires_in = "90 days",
    expires_at = format(Sys.time() + 90 * 86400, "%Y-%m-%dT%H:%M:%SZ")
  )
}

# ==============================================================================
# ADMIN — Institution review
# ==============================================================================

#* List institutional accounts pending admin review. Requires admin JWT.
#* @tag Admin
#* @get /admin/pending-users
#* @response 200 Returns {ok, count, users}.
#* @response 403 Admin access required.
function(req, res) {
  user <- require_admin(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  tryCatch(
    {
      result <- db_users$find(
        query = '{"review_status": "pending"}',
        fields = '{"password_hash": 0}'
      )
      if (nrow(result) == 0) {
        return(list(ok = TRUE, count = 0L, users = list()))
      }
      users <- lapply(seq_len(nrow(result)), function(i) as.list(result[i, ]))
      list(ok = TRUE, count = length(users), users = users)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Approve an institutional account.
#* Sets review_status to "approved".
#* Requires admin JWT.
#* @tag Admin
#* @post /admin/approve/<email>
#* @response 200 Returns {ok, message}.
function(req, res, email) {
  user <- require_admin(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  tryCatch(
    {
      db_users$update(
        query = toJSON(list(email = email), auto_unbox = TRUE),
        update = toJSON(list(`$set` = list(
          review_status = "approved",
          reviewed_by = user$sub,
          reviewed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        )), auto_unbox = TRUE)
      )
      list(ok = TRUE, message = paste("Account approved:", email))
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Reject an institutional account.
#* Sets review_status to "rejected".
#* Requires admin JWT.
#* @tag Admin
#* @post /admin/reject/<email>
#* @response 200 Returns {ok, message}.
function(req, res, email) {
  user <- require_admin(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  tryCatch(
    {
      db_users$update(
        query = toJSON(list(email = email), auto_unbox = TRUE),
        update = toJSON(list(`$set` = list(
          review_status = "rejected",
          reviewed_by = user$sub,
          reviewed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        )), auto_unbox = TRUE)
      )
      list(ok = TRUE, message = paste("Account rejected:", email))
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

# ==============================================================================
# RECIPES
# ==============================================================================

#* List recipes with optional search,
#* filtering, and pagination.
#* Sorted by downloads descending.
#* @tag Recipes
#* @get /recipes
#* @param search:str Regex search on recipe name
#* @param survey_type:str Filter by survey type: ech, eaii, eph, eai
#* @param topic:str Filter by topic:
#*   labor_market, income, education,
#*   health, demographics, housing
#* @param certification:str Filter by
#*   certification level: community,
#*   reviewed, official
#* @param user:str Filter by author email
#* @param limit:int Max results (default 50)
#* @param offset:int Skip N results for pagination (default 0)
#* @response 200 Returns {ok, count,
#*   recipes: [{id, name, user,
#*   survey_type, edition, description,
#*   topic, version, downloads, steps,
#*   depends_on, certification,
#*   user_info, doc, ...}]}.
function(req, res, search = NULL, survey_type = NULL, topic = NULL,
         certification = NULL, user = NULL, limit = 50, offset = 0) {
  filter <- list()
  if (!is.null(survey_type) && nzchar(survey_type)) {
    filter$survey_type <- survey_type
  }
  if (!is.null(topic) && nzchar(topic)) {
    filter$topic <- topic
  }
  if (!is.null(certification) && nzchar(certification)) {
    filter[["certification.level"]] <- certification
  }
  if (!is.null(user) && nzchar(user)) {
    filter$user <- user
  }

  if (!is.null(search) && nzchar(search)) {
    filter$name <- list(`$regex` = search, `$options` = "i")
  }

  query_json <- if (length(filter) == 0) {
    "{}"
  } else {
    toJSON(filter, auto_unbox = TRUE)
  }

  tryCatch(
    {
      # mongolite $find returns a data.frame;
      # use $iterate for list-of-lists
      iter <- db_recipes$iterate(
        query = query_json,
        sort = '{"downloads": -1}',
        skip = as.integer(offset),
        limit = as.integer(limit)
      )

      docs <- list()
      while (!is.null(doc <- iter$one())) {
        doc[["_id"]] <- NULL
        docs[[length(docs) + 1L]] <- doc
      }

      list(ok = TRUE, count = length(docs), recipes = docs)
    },
    error = function(e) {
      res$status <- 500L
      list(error = paste("Failed to fetch recipes:", e$message))
    }
  )
}

#* Get a single recipe by its unique ID.
#* @tag Recipes
#* @get /recipes/<id>
#* @response 200 Returns {ok, recipe: {id, name, user, survey_type, ...}}.
#* @response 404 Recipe not found.
function(req, res, id) {
  tryCatch(
    {
      iter <- db_recipes$iterate(
        query = toJSON(list(id = id), auto_unbox = TRUE),
        limit = 1
      )
      doc <- iter$one()

      if (is.null(doc)) {
        res$status <- 404L
        return(list(error = "Recipe not found"))
      }

      doc[["_id"]] <- NULL
      list(ok = TRUE, recipe = doc)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Publish a new recipe. Requires JWT
#* authentication. Send the recipe as
#* JSON body with required fields: name,
#* survey_type, edition. Optional:
#* description, topic, steps, depends_on,
#* categories, version, doc.
#* @tag Recipes
#* @post /recipes
#* @response 201 Returns {ok, id}.
#* @response 400 Missing required fields.
#* @response 401 Authentication required.
function(req, res) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  body <- req$body
  if (is.null(body)) {
    res$status <- 400L
    return(list(error = "Request body is required"))
  }

  # Validate required fields
  required <- c("name", "survey_type", "edition")
  missing_fields <- required[!required %in% names(body)]
  if (length(missing_fields) > 0) {
    res$status <- 400L
    return(list(error = paste(
      "Missing required fields:",
      paste(missing_fields, collapse = ", ")
    )))
  }

  # Set metadata
  body$user <- user$sub # email from JWT
  body$downloads <- 0L
  body$created_at <- format(
    Sys.time(), "%Y-%m-%dT%H:%M:%SZ"
  )
  if (is.null(body$id)) {
    body$id <- paste0(
      "r_", as.integer(Sys.time()),
      "_", sample.int(999, 1)
    )
  }
  if (is.null(body$version)) body$version <- "1.0.0"
  if (is.null(body$certification)) {
    body$certification <- list(
      level = "community",
      certified_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    )
  }
  if (is.null(body$user_info)) {
    body$user_info <- list(
      name = user$name,
      user_type = user$user_type,
      email = user$sub
    )
  }

  tryCatch(
    {
      db_recipes$insert(toJSON(body, auto_unbox = TRUE, null = "null"))
      res$status <- 201L
      list(ok = TRUE, id = body$id)
    },
    error = function(e) {
      res$status <- 500L
      list(error = paste("Failed to publish recipe:", e$message))
    }
  )
}

#* Increment recipe download counter. No authentication required.
#* @tag Recipes
#* @post /recipes/<id>/download
#* @response 200 Returns {ok}.
function(req, res, id) {
  tryCatch(
    {
      db_recipes$update(
        query = toJSON(list(id = id), auto_unbox = TRUE),
        update = '{"$inc": {"downloads": 1}}'
      )
      list(ok = TRUE)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Rate a recipe (1-5). Requires authentication.
#* Creates or updates the user's rating for this recipe.
#* @tag Recipes
#* @put /recipes/<id>/star
#* @response 200 Returns {ok, value}.
#* @response 401 Unauthorized.
function(req, res, id) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) return(user)

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  value <- as.integer(body$value)
  if (is.na(value) || value < 1L || value > 5L) {
    res$status <- 400L
    return(list(error = "value must be an integer between 1 and 5"))
  }

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  tryCatch(
    {
      db_stars$update(
        query = toJSON(list(
          user = user$sub,
          target_type = "recipe",
          target_id = id
        ), auto_unbox = TRUE),
        update = toJSON(list(
          `$set` = list(value = value, updated_at = now),
          `$setOnInsert` = list(
            user = user$sub,
            target_type = "recipe",
            target_id = id,
            created_at = now
          )
        ), auto_unbox = TRUE),
        upsert = TRUE
      )
      list(ok = TRUE, value = value)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Get star summary for a recipe.
#* Returns average rating, count, and the current
#* user's rating if authenticated.
#* @tag Recipes
#* @get /recipes/<id>/stars
#* @response 200 Returns {average, count, user_value}.
function(req, res, id) {
  tryCatch(
    {
      query <- toJSON(list(
        target_type = "recipe", target_id = id
      ), auto_unbox = TRUE)
      all_stars <- db_stars$find(query, fields = '{"value": 1, "user": 1, "_id": 0}')
      count <- nrow(all_stars)
      average <- if (count > 0) round(mean(all_stars$value), 2) else 0

      user_value <- NULL
      token <- sub("^Bearer\\s+", "", req$HTTP_AUTHORIZATION %||% "")
      if (nzchar(token)) {
        user <- tryCatch(decode_token(token), error = function(e) NULL)
        if (!is.null(user)) {
          user_row <- all_stars[all_stars$user == user$sub, ]
          if (nrow(user_row) > 0) user_value <- user_row$value[1]
        }
      }

      list(average = average, count = count, user_value = user_value)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Add a comment to a recipe. Requires authentication.
#* @tag Recipes
#* @post /recipes/<id>/comments
#* @response 200 Returns {ok, comment}.
#* @response 401 Unauthorized.
function(req, res, id) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) return(user)

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  text <- trimws(body$text %||% "")
  if (!nzchar(text)) {
    res$status <- 400L
    return(list(error = "text is required"))
  }
  if (nchar(text) > 2000L) {
    res$status <- 400L
    return(list(error = "text must be 2000 characters or fewer"))
  }

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  comment_id <- paste0("c_", as.integer(Sys.time()), "_", sample(999, 1))

  comment <- list(
    id = comment_id,
    user = user$sub,
    user_name = user$name,
    target_type = "recipe",
    target_id = id,
    text = text,
    created_at = now
  )

  tryCatch(
    {
      db_comments$insert(toJSON(comment, auto_unbox = TRUE, null = "null"))
      list(ok = TRUE, comment = comment)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* List comments for a recipe, sorted by date ascending.
#* @tag Recipes
#* @get /recipes/<id>/comments
#* @response 200 Returns {comments}.
function(req, res, id) {
  tryCatch(
    {
      query <- toJSON(list(
        target_type = "recipe", target_id = id
      ), auto_unbox = TRUE)
      comments <- db_comments$find(
        query,
        sort = '{"created_at": 1}',
        fields = '{"_id": 0}'
      )
      list(comments = comments)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Get recipes that depend on this recipe (backlinks).
#* @tag Recipes
#* @get /recipes/<id>/dependents
#* @response 200 Returns {dependents}.
function(req, res, id) {
  tryCatch(
    {
      query <- toJSON(list(
        depends_on_recipes = id
      ), auto_unbox = TRUE)
      deps <- db_recipes$find(
        query,
        fields = '{"_id": 0, "id": 1, "name": 1, "user": 1, "edition": 1, "survey_type": 1}'
      )
      list(dependents = deps)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

# ==============================================================================
# WORKFLOWS
# ==============================================================================

#* List workflows with optional search,
#* filtering, and pagination.
#* Sorted by downloads descending.
#* @tag Workflows
#* @get /workflows
#* @param search:str Regex search on workflow name
#* @param survey_type:str Filter by survey type: ech, eaii, eph, eai
#* @param recipe_id:str Filter by referenced recipe ID
#* @param user:str Filter by author email
#* @param limit:int Max results (default 50)
#* @param offset:int Skip N results for pagination (default 0)
#* @response 200 Returns {ok, count,
#*   workflows: [{id, name, user,
#*   survey_type, edition,
#*   estimation_type, recipe_ids,
#*   calls, ...}]}.
function(req, res, search = NULL, survey_type = NULL, recipe_id = NULL,
         user = NULL, limit = 50, offset = 0) {
  filter <- list()
  if (!is.null(survey_type) && nzchar(survey_type)) {
    filter$survey_type <- survey_type
  }
  if (!is.null(recipe_id) && nzchar(recipe_id)) {
    filter$recipe_ids <- recipe_id
  }
  if (!is.null(user) && nzchar(user)) {
    filter$user <- user
  }
  if (!is.null(search) && nzchar(search)) {
    filter$name <- list(`$regex` = search, `$options` = "i")
  }

  query_json <- if (length(filter) == 0) {
    "{}"
  } else {
    toJSON(filter, auto_unbox = TRUE)
  }

  tryCatch(
    {
      iter <- db_workflows$iterate(
        query = query_json,
        sort = '{"downloads": -1}',
        skip = as.integer(offset),
        limit = as.integer(limit)
      )

      docs <- list()
      while (!is.null(doc <- iter$one())) {
        doc[["_id"]] <- NULL
        docs[[length(docs) + 1L]] <- doc
      }

      list(ok = TRUE, count = length(docs), workflows = docs)
    },
    error = function(e) {
      res$status <- 500L
      list(error = paste("Failed to fetch workflows:", e$message))
    }
  )
}

#* Get a single workflow by its unique ID.
#* @tag Workflows
#* @get /workflows/<id>
#* @response 200 Returns {ok, workflow: {id, name, user, survey_type, ...}}.
#* @response 404 Workflow not found.
function(req, res, id) {
  tryCatch(
    {
      iter <- db_workflows$iterate(
        query = toJSON(list(id = id), auto_unbox = TRUE),
        limit = 1
      )
      doc <- iter$one()

      if (is.null(doc)) {
        res$status <- 404L
        return(list(error = "Workflow not found"))
      }

      doc[["_id"]] <- NULL
      list(ok = TRUE, workflow = doc)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Publish a new workflow. Requires JWT
#* authentication. Send the workflow as
#* JSON body with required fields: name,
#* survey_type, edition. Optional:
#* description, estimation_type,
#* recipe_ids, calls, call_metadata,
#* categories, version.
#* @tag Workflows
#* @post /workflows
#* @response 201 Returns {ok, id}.
#* @response 400 Missing required fields.
#* @response 401 Authentication required.
function(req, res) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  body <- req$body
  if (is.null(body)) {
    res$status <- 400L
    return(list(error = "Request body is required"))
  }

  required <- c("name", "survey_type", "edition")
  missing_fields <- required[!required %in% names(body)]
  if (length(missing_fields) > 0) {
    res$status <- 400L
    return(list(error = paste(
      "Missing required fields:",
      paste(missing_fields, collapse = ", ")
    )))
  }

  body$user <- user$sub
  body$downloads <- 0L
  body$created_at <- format(
    Sys.time(), "%Y-%m-%dT%H:%M:%SZ"
  )
  if (is.null(body$id)) {
    body$id <- paste0(
      "wf_", as.integer(Sys.time()),
      "_", sample.int(999, 1)
    )
  }
  if (is.null(body$version)) body$version <- "1.0.0"
  if (is.null(body$certification)) {
    body$certification <- list(
      level = "community",
      certified_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    )
  }
  if (is.null(body$user_info)) {
    body$user_info <- list(
      name = user$name,
      user_type = user$user_type,
      email = user$sub
    )
  }

  tryCatch(
    {
      db_workflows$insert(toJSON(body, auto_unbox = TRUE, null = "null"))
      res$status <- 201L
      list(ok = TRUE, id = body$id)
    },
    error = function(e) {
      res$status <- 500L
      list(error = paste("Failed to publish workflow:", e$message))
    }
  )
}

#* Increment workflow download counter. No authentication required.
#* @tag Workflows
#* @post /workflows/<id>/download
#* @response 200 Returns {ok}.
function(req, res, id) {
  tryCatch(
    {
      db_workflows$update(
        query = toJSON(list(id = id), auto_unbox = TRUE),
        update = '{"$inc": {"downloads": 1}}'
      )
      list(ok = TRUE)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Rate a workflow (1-5). Requires authentication.
#* @tag Workflows
#* @put /workflows/<id>/star
#* @response 200 Returns {ok, value}.
function(req, res, id) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) return(user)

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  value <- as.integer(body$value)
  if (is.na(value) || value < 1L || value > 5L) {
    res$status <- 400L
    return(list(error = "value must be an integer between 1 and 5"))
  }

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  tryCatch(
    {
      db_stars$update(
        query = toJSON(list(
          user = user$sub,
          target_type = "workflow",
          target_id = id
        ), auto_unbox = TRUE),
        update = toJSON(list(
          `$set` = list(value = value, updated_at = now),
          `$setOnInsert` = list(
            user = user$sub,
            target_type = "workflow",
            target_id = id,
            created_at = now
          )
        ), auto_unbox = TRUE),
        upsert = TRUE
      )
      list(ok = TRUE, value = value)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Get star summary for a workflow.
#* @tag Workflows
#* @get /workflows/<id>/stars
#* @response 200 Returns {average, count, user_value}.
function(req, res, id) {
  tryCatch(
    {
      query <- toJSON(list(
        target_type = "workflow", target_id = id
      ), auto_unbox = TRUE)
      all_stars <- db_stars$find(query, fields = '{"value": 1, "user": 1, "_id": 0}')
      count <- nrow(all_stars)
      average <- if (count > 0) round(mean(all_stars$value), 2) else 0

      user_value <- NULL
      token <- sub("^Bearer\\s+", "", req$HTTP_AUTHORIZATION %||% "")
      if (nzchar(token)) {
        user <- tryCatch(decode_token(token), error = function(e) NULL)
        if (!is.null(user)) {
          user_row <- all_stars[all_stars$user == user$sub, ]
          if (nrow(user_row) > 0) user_value <- user_row$value[1]
        }
      }

      list(average = average, count = count, user_value = user_value)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Add a comment to a workflow. Requires authentication.
#* @tag Workflows
#* @post /workflows/<id>/comments
#* @response 200 Returns {ok, comment}.
function(req, res, id) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) return(user)

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  text <- trimws(body$text %||% "")
  if (!nzchar(text)) {
    res$status <- 400L
    return(list(error = "text is required"))
  }
  if (nchar(text) > 2000L) {
    res$status <- 400L
    return(list(error = "text must be 2000 characters or fewer"))
  }

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  comment_id <- paste0("c_", as.integer(Sys.time()), "_", sample(999, 1))

  comment <- list(
    id = comment_id,
    user = user$sub,
    user_name = user$name,
    target_type = "workflow",
    target_id = id,
    text = text,
    created_at = now
  )

  tryCatch(
    {
      db_comments$insert(toJSON(comment, auto_unbox = TRUE, null = "null"))
      list(ok = TRUE, comment = comment)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* List comments for a workflow, sorted by date ascending.
#* @tag Workflows
#* @get /workflows/<id>/comments
#* @response 200 Returns {comments}.
function(req, res, id) {
  tryCatch(
    {
      query <- toJSON(list(
        target_type = "workflow", target_id = id
      ), auto_unbox = TRUE)
      comments <- db_comments$find(
        query,
        sort = '{"created_at": 1}',
        fields = '{"_id": 0}'
      )
      list(comments = comments)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

# ==============================================================================
# COMMENTS (cross-type)
# ==============================================================================

#* Delete a comment. Only the author can delete their own comments.
#* @tag Comments
#* @delete /comments/<comment_id>
#* @response 200 Returns {ok}.
#* @response 401 Unauthorized.
#* @response 403 Forbidden — not the comment author.
function(req, res, comment_id) {
  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) return(user)

  tryCatch(
    {
      query <- toJSON(list(id = comment_id), auto_unbox = TRUE)
      existing <- db_comments$find(query, fields = '{"user": 1, "_id": 0}')
      if (nrow(existing) == 0) {
        res$status <- 404L
        return(list(error = "Comment not found"))
      }
      if (existing$user[1] != user$sub) {
        res$status <- 403L
        return(list(error = "You can only delete your own comments"))
      }
      db_comments$remove(query)
      list(ok = TRUE)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

# ==============================================================================
# INDICATORS
# Feature-gated: set METASURVEY_ENABLE_INDICATORS=0 to disable
# ==============================================================================

require_indicators <- function(res) {
  if (!ENABLE_INDICATORS) {
    res$status <- 404L
    return(list(error = "Indicators module is disabled"))
  }
  NULL
}

require_worker <- function(res) {
  if (!ENABLE_WORKER || !nzchar(WORKER_URL)) {
    res$status <- 404L
    return(list(error = "Worker module is disabled"))
  }
  NULL
}

#* List published indicators with optional
#* search, filtering, and pagination.
#* Sorted by published_at descending.
#* No authentication required.
#* @tag Indicators
#* @get /indicators
#* @param search:str Regex search on indicator name
#* @param survey_type:str Filter by survey type
#* @param recipe_id:str Filter by recipe ID
#* @param workflow_id:str Filter by workflow ID
#* @param edition:str Filter by survey edition
#* @param limit:int Max results (default 50)
#* @param offset:int Skip N results (default 0)
#* @response 200 Returns {ok, count,
#*   indicators: [{id, name, value, se,
#*   cv, recipe_id, workflow_id, ...}]}.
function(req, res, search = NULL,
         survey_type = NULL,
         recipe_id = NULL,
         workflow_id = NULL,
         edition = NULL,
         limit = 50, offset = 0) {
  guard <- require_indicators(res)
  if (!is.null(guard)) return(guard)

  filter <- list()
  if (!is.null(survey_type) && nzchar(survey_type)) {
    filter$survey_type <- survey_type
  }
  if (!is.null(recipe_id) && nzchar(recipe_id)) {
    filter$recipe_id <- recipe_id
  }
  if (!is.null(workflow_id) && nzchar(workflow_id)) {
    filter$workflow_id <- workflow_id
  }
  if (!is.null(edition) && nzchar(edition)) {
    filter$edition <- edition
  }
  if (!is.null(search) && nzchar(search)) {
    filter$name <- list(
      `$regex` = search,
      `$options` = "i"
    )
  }

  query_json <- if (length(filter) == 0) {
    "{}"
  } else {
    toJSON(filter, auto_unbox = TRUE)
  }

  tryCatch(
    {
      iter <- db_indicators$iterate(
        query = query_json,
        sort = '{"published_at": -1}',
        skip = as.integer(offset),
        limit = as.integer(limit)
      )

      docs <- list()
      while (!is.null(doc <- iter$one())) {
        doc[["_id"]] <- NULL
        docs[[length(docs) + 1L]] <- doc
      }

      list(ok = TRUE, count = length(docs), indicators = docs)
    },
    error = function(e) {
      res$status <- 500L
      list(error = paste(
        "Failed to fetch indicators:",
        e$message
      ))
    }
  )
}

#* Get a single indicator by its unique ID.
#* No authentication required.
#* @tag Indicators
#* @get /indicators/<id>
#* @response 200 Returns {ok, indicator:
#*   {id, name, value, se, cv,
#*   confint_lower, confint_upper,
#*   recipe_id, workflow_id, ...}}.
#* @response 404 Indicator not found.
function(req, res, id) {
  guard <- require_indicators(res)
  if (!is.null(guard)) return(guard)

  tryCatch(
    {
      iter <- db_indicators$iterate(
        query = toJSON(
          list(id = id),
          auto_unbox = TRUE
        ),
        limit = 1
      )
      doc <- iter$one()

      if (is.null(doc)) {
        res$status <- 404L
        return(list(error = "Indicator not found"))
      }

      doc[["_id"]] <- NULL
      list(ok = TRUE, indicator = doc)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Get the recipe that produced an indicator.
#* Shows the data transformation steps
#* (input variables, recodes, computations).
#* No authentication required.
#* @tag Indicators
#* @get /indicators/<id>/recipe
#* @response 200 Returns {ok, indicator_id,
#*   recipe: {id, name, steps, doc, ...}}.
#* @response 404 Indicator not found.
function(req, res, id) {
  guard <- require_indicators(res)
  if (!is.null(guard)) return(guard)

  tryCatch(
    {
      iter <- db_indicators$iterate(
        query = toJSON(
          list(id = id),
          auto_unbox = TRUE
        ),
        limit = 1
      )
      ind <- iter$one()

      if (is.null(ind)) {
        res$status <- 404L
        return(list(error = "Indicator not found"))
      }

      rid <- ind$recipe_id
      if (is.null(rid) || !nzchar(rid)) {
        return(list(
          ok = TRUE,
          indicator_id = id,
          recipe = NULL,
          message = "No recipe linked to this indicator"
        ))
      }

      riter <- db_recipes$iterate(
        query = toJSON(
          list(id = rid),
          auto_unbox = TRUE
        ),
        limit = 1
      )
      rdoc <- riter$one()
      if (!is.null(rdoc)) rdoc[["_id"]] <- NULL

      list(ok = TRUE, indicator_id = id, recipe = rdoc)
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Get the workflow that produced an indicator.
#* Shows the estimation calls (svymean,
#* svytotal, svyby), design type,
#* and referenced recipe IDs.
#* No authentication required.
#* @tag Indicators
#* @get /indicators/<id>/workflow
#* @response 200 Returns {ok, indicator_id,
#*   workflow: {id, name, estimation_type,
#*   calls, call_metadata,
#*   recipe_ids, ...}}.
#* @response 404 Indicator not found.
function(req, res, id) {
  guard <- require_indicators(res)
  if (!is.null(guard)) return(guard)

  tryCatch(
    {
      iter <- db_indicators$iterate(
        query = toJSON(
          list(id = id),
          auto_unbox = TRUE
        ),
        limit = 1
      )
      ind <- iter$one()

      if (is.null(ind)) {
        res$status <- 404L
        return(list(error = "Indicator not found"))
      }

      wid <- ind$workflow_id
      if (is.null(wid) || !nzchar(wid)) {
        return(list(
          ok = TRUE,
          indicator_id = id,
          workflow = NULL,
          message = "No workflow linked"
        ))
      }

      witer <- db_workflows$iterate(
        query = toJSON(
          list(id = wid),
          auto_unbox = TRUE
        ),
        limit = 1
      )
      wdoc <- witer$one()
      if (!is.null(wdoc)) wdoc[["_id"]] <- NULL

      list(
        ok = TRUE,
        indicator_id = id,
        workflow = wdoc
      )
    },
    error = function(e) {
      res$status <- 500L
      list(error = e$message)
    }
  )
}

#* Publish a new indicator. Requires JWT
#* authentication. Required fields: name,
#* workflow_id, value. Optional: recipe_id,
#* description, survey_type, edition,
#* estimation_type, stat, se, cv,
#* confint_lower, confint_upper, metadata.
#* @tag Indicators
#* @post /indicators
#* @response 201 Returns {ok, id}.
#* @response 400 Missing required fields.
#* @response 401 Authentication required.
function(req, res) {
  guard <- require_indicators(res)
  if (!is.null(guard)) return(guard)

  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  body <- req$body
  if (is.null(body)) {
    res$status <- 400L
    return(list(error = "Request body is required"))
  }

  required <- c("name", "workflow_id", "value")
  missing_fields <- required[
    !required %in% names(body)
  ]
  if (length(missing_fields) > 0) {
    res$status <- 400L
    return(list(error = paste(
      "Missing required fields:",
      paste(missing_fields, collapse = ", ")
    )))
  }

  body$user <- user$sub
  body$published_at <- format(
    Sys.time(), "%Y-%m-%dT%H:%M:%SZ"
  )
  body$metasurvey_version <- "0.0.16"
  if (is.null(body$id)) {
    body$id <- paste0(
      "ind_", as.integer(Sys.time()),
      "_", sample.int(999, 1)
    )
  }
  if (is.null(body$user_info)) {
    body$user_info <- list(
      name = user$name,
      user_type = user$user_type,
      email = user$sub
    )
  }

  tryCatch(
    {
      db_indicators$insert(toJSON(
        body,
        auto_unbox = TRUE,
        null = "null"
      ))
      res$status <- 201L
      list(ok = TRUE, id = body$id)
    },
    error = function(e) {
      res$status <- 500L
      list(error = paste(
        "Failed to publish indicator:",
        e$message
      ))
    }
  )
}

#* Request an on-demand computation from the
#* worker. Proxies the request to the internal
#* compute worker, stores the result as an
#* indicator, and returns it. Requires JWT.
#*
#* The frontend can use this to request
#* estimations with specific filters
#* (by variable, domain subset).
#*
#* @tag Indicators
#* @post /indicators/compute
#* @response 200 Returns {ok, indicator_id,
#*   results: [{stat, value, se, cv, ...}]}.
#* @response 400 Missing fields or no worker.
#* @response 401 Authentication required.
#* @response 502 Worker unavailable or error.
function(req, res) {
  guard <- require_indicators(res)
  if (!is.null(guard)) return(guard)
  guard <- require_worker(res)
  if (!is.null(guard)) return(guard)

  user <- require_auth(req, res)
  if (is.list(user) && !is.null(user$error)) {
    return(user)
  }

  body <- req$body
  if (is.null(body)) {
    res$status <- 400L
    return(list(error = "Request body is required"))
  }

  required <- c("workflow_id", "survey_type", "edition")
  missing_fields <- required[
    !required %in% names(body)
  ]
  if (length(missing_fields) > 0) {
    res$status <- 400L
    return(list(error = paste(
      "Missing required fields:",
      paste(missing_fields, collapse = ", ")
    )))
  }

  tryCatch(
    {
      # Forward to worker
      worker_resp <- httr2::request(
        paste0(WORKER_URL, "/compute")
      ) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(300) |>
        httr2::req_perform()

      worker_result <- httr2::resp_body_json(
        worker_resp
      )

      if (!isTRUE(worker_result$ok)) {
        res$status <- 502L
        return(list(
          error = "Worker computation failed",
          detail = worker_result$error
        ))
      }

      # Store each result as an indicator
      indicator_ids <- character(0)
      results <- worker_result$results

      for (r in results) {
        ind_id <- paste0(
          "ind_", as.integer(Sys.time()),
          "_", sample.int(9999, 1)
        )

        filters_meta <- body$filters
        ind_doc <- list(
          id = ind_id,
          name = r$stat %||% "Computed indicator",
          workflow_id = body$workflow_id,
          recipe_id = if (length(body$recipe_ids) > 0) {
            body$recipe_ids[[1]]
          } else {
            NULL
          },
          survey_type = body$survey_type,
          edition = body$edition,
          estimation_type = body$estimation_type %||%
            "annual",
          stat = r$stat,
          value = r$value,
          se = r$se,
          cv = r$cv,
          confint_lower = r$confint_lower,
          confint_upper = r$confint_upper,
          metadata = list(
            call = r$call,
            by_group = r$by_group,
            filters = filters_meta,
            computed_on_demand = TRUE
          ),
          user = user$sub,
          user_info = list(
            name = user$name,
            user_type = user$user_type,
            email = user$sub
          ),
          published_at = format(
            Sys.time(),
            "%Y-%m-%dT%H:%M:%SZ"
          ),
          metasurvey_version = "0.0.16"
        )

        db_indicators$insert(toJSON(
          ind_doc,
          auto_unbox = TRUE,
          null = "null"
        ))
        indicator_ids <- c(indicator_ids, ind_id)
      }

      list(
        ok = TRUE,
        indicator_ids = indicator_ids,
        count = length(results),
        results = results
      )
    },
    error = function(e) {
      res$status <- 502L
      list(error = paste(
        "Worker request failed:",
        e$message
      ))
    }
  )
}

# ==============================================================================
# ANDA METADATA
# ==============================================================================

#* Get ANDA variable metadata from INE
#* Uruguay's data catalog. Returns variable
#* labels, types, and value categories.
#* @tag ANDA
#* @param survey_type:str Survey type (default "ech")
#* @param names:str Comma-separated variable names (returns all if empty)
#* @get /anda/variables
#* @response 200 Returns {ok, count,
#*   variables: [{survey_type, name,
#*   label, type, value_labels,
#*   description, source_edition,
#*   source_catalog_id}]}.
function(survey_type = "ech", names = "") {
  query <- list(survey_type = survey_type)

  if (nzchar(names)) {
    var_names <- trimws(strsplit(names, ",")[[1]])
    var_names <- tolower(var_names)
    query$name <- list(`$in` = var_names)
  }

  query_json <- jsonlite::toJSON(query, auto_unbox = TRUE)
  docs <- db_anda$find(query_json, fields = '{"_id": 0}')

  list(ok = TRUE, count = nrow(docs), variables = docs)
}

# ==============================================================================
# HEALTH
# ==============================================================================

#* API health check. Returns service status
#* and MongoDB connection state.
#* No authentication required.
#* @tag System
#* @get /health
#* @response 200 Returns {status, service,
#*   version, database, mongodb, timestamp}.
function() {
  # Quick connectivity check
  ok <- tryCatch(
    {
      db_users$count()
      TRUE
    },
    error = function(e) FALSE
  )

  list(
    status = if (ok) "ok" else "degraded",
    service = "metasurvey-api",
    version = "2.0.0",
    database = DATABASE,
    mongodb = if (ok) "connected" else "disconnected",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
}
