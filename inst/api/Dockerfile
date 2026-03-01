# metasurvey Plumber API
# Build: docker build -t metasurvey-api .
# Run:   docker run -p 8787:8787 -e METASURVEY_MONGO_URI="mongodb+srv://..." metasurvey-api

FROM rocker/r-ver:4.4.0

# System dependencies for mongolite, jose/openssl, plumber/httpuv, sodium
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libmongoc-dev \
    libbson-dev \
    libsodium-dev \
    libhiredis-dev \
    pkg-config \
    make \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install R packages (openssl explicitly as jose dependency)
RUN R -e "install.packages(c('plumber', 'mongolite', 'jsonlite', 'openssl', 'jose', 'digest', 'sodium', 'redux'), repos='https://cloud.r-project.org')"

# Verify all packages load correctly
RUN R -e "library(plumber); library(mongolite); library(jsonlite); library(jose); library(digest); library(redux); cat('All packages loaded OK\n')"

# Copy API code
COPY plumber.R /app/plumber.R

# Healthcheck — hit /health every 30s
HEALTHCHECK --interval=30s --timeout=5s --retries=3 --start-period=30s \
  CMD curl -f http://localhost:8787/health || exit 1

EXPOSE 8787

# Run plumber on 0.0.0.0. PORT env var is set by Railway/Render.
CMD ["R", "-e", "port <- as.integer(Sys.getenv('PORT', '8787')); plumber::plumb('/app/plumber.R')$run(host='0.0.0.0', port=port)"]
