# metasurvey Recipe Explorer — Shiny App
# Build: docker build -t metasurvey-shiny .
# Run:   docker run -p 3838:3838 -e METASURVEY_API_URL="https://metasurvey-api-production.up.railway.app" metasurvey-shiny

FROM rocker/r-ver:4.4.0

# System dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    zlib1g-dev \
    pkg-config \
    make \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'bslib', 'bsicons', 'htmltools', 'httr', 'jsonlite', 'digest', 'remotes', 'jose', 'data.table', 'crayon', 'bit64', 'survey', 'glue', 'lifecycle', 'R6'), repos='https://cloud.r-project.org')"

# Optional: visNetwork for pipeline graphs
RUN R -e "install.packages('visNetwork', repos='https://cloud.r-project.org')"

# Install metasurvey from GitHub (main branch)
ARG METASURVEY_REF=main
RUN R -e "remotes::install_github('metaSurveyR/metasurvey@${METASURVEY_REF}', dependencies = FALSE)"

# Copy Shiny app
COPY app.R /app/app.R
COPY R/ /app/R/

EXPOSE 3838

# Railway/Render set PORT env var. Default to 3838.
CMD ["R", "-e", "port <- as.integer(Sys.getenv('PORT', '3838')); shiny::runApp('/app', host='0.0.0.0', port=port, launch.browser=FALSE)"]
