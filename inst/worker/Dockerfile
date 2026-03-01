# metasurvey Compute Worker
# Internal service — NOT exposed to the public internet.
# Has metasurvey installed + access to private microdata.
#
# Build: docker build -t metasurvey-worker .
# Run:   docker run -p 8788:8788 \
#          -e METASURVEY_MONGO_URI="mongodb://..." \
#          -v /path/to/microdata:/data/surveys \
#          metasurvey-worker

FROM rocker/r-ver:4.4.0

# System dependencies
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
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages: plumber + mongolite + survey ecosystem
RUN R -e "install.packages(c( \
    'plumber', 'mongolite', 'jsonlite', \
    'data.table', 'survey', 'R6', \
    'cli', 'glue', 'lifecycle', 'httr2', \
    'remotes', 'haven', 'redux', 'callr' \
  ), repos='https://cloud.r-project.org')"

# Install metasurvey from GitHub
ARG METASURVEY_REF=main
RUN R -e "remotes::install_github( \
    'metasurveyr/metasurvey@${METASURVEY_REF}', \
    dependencies = FALSE \
  )"

# Verify
RUN R -e "library(metasurvey); library(plumber); library(mongolite); library(survey); library(redux); library(callr); cat('All packages OK\n')"

# Copy worker code
COPY plumber.R /app/plumber.R

# Create data directory (will be mounted as volume)
RUN mkdir -p /data/surveys

HEALTHCHECK --interval=30s --timeout=5s --retries=3 --start-period=60s \
  CMD curl -f http://localhost:8788/health || exit 1

EXPOSE 8788

CMD ["R", "-e", "port <- as.integer(Sys.getenv('PORT', '8788')); plumber::plumb('/app/plumber.R')$run(host='0.0.0.0', port=port)"]
