# metasurvey — Developer Makefile
# Run `make help` to see available targets.

.DEFAULT_GOAL := help
SHELL := /bin/bash

# ── Dev Setup ────────────────────────────────────────────────
.PHONY: dev-setup
dev-setup: ## Install R deps, pre-commit hooks, and validate setup
	@echo "Installing R dependencies..."
	Rscript -e 'if (!requireNamespace("devtools", quietly=TRUE)) install.packages("devtools")'
	Rscript -e 'devtools::install_deps(dependencies = TRUE)'
	@echo "Installing pre-commit hooks..."
	pip3 install pre-commit --quiet 2>/dev/null || brew install pre-commit 2>/dev/null || true
	pre-commit install
	@echo "Dev setup complete."

.PHONY: dev-clean-hooks
dev-clean-hooks: ## Nuke pre-commit cache and reinstall (fixes renv lock errors)
	pre-commit clean
	pre-commit gc
	pre-commit install
	@echo "Pre-commit cache cleaned and hooks reinstalled."

# ── Code Quality ─────────────────────────────────────────────
.PHONY: style
style: ## Auto-format R code with styler
	Rscript -e 'styler::style_pkg()'

.PHONY: lint
lint: ## Run lintr on package
	Rscript -e 'lintr::lint_package()'

.PHONY: document
document: ## Regenerate roxygen2 docs (NAMESPACE + man/)
	Rscript -e 'devtools::document()'

.PHONY: pre-commit
pre-commit: ## Run all pre-commit hooks on all files
	pre-commit run --all-files

# ── Testing ──────────────────────────────────────────────────
.PHONY: test
test: ## Run test suite
	Rscript -e 'devtools::test()'

.PHONY: test-file
test-file: ## Run a single test file (usage: make test-file F=steps-basic)
	Rscript -e 'devtools::test(filter = "$(F)")'

.PHONY: check
check: ## Full R CMD check (0 errors / 0 warnings expected)
	Rscript -e 'devtools::check()'

.PHONY: coverage
coverage: ## Generate test coverage report
	Rscript -e 'covr::report(covr::package_coverage(), file = "coverage-report.html"); browseURL("coverage-report.html")'

# ── Build ────────────────────────────────────────────────────
.PHONY: build
build: document ## Build package tarball
	R CMD build .

.PHONY: install
install: ## Install package locally
	Rscript -e 'devtools::install()'

# ── Docker (API + MongoDB) ──────────────────────────────────
.PHONY: docker-up
docker-up: ## Start API, MongoDB, Worker, and Shiny via docker compose
	docker compose up --build -d

.PHONY: docker-down
docker-down: ## Stop all containers
	docker compose down

.PHONY: docker-seed
docker-seed: ## Seed MongoDB with recipes, workflows, and ANDA metadata
	@echo "Running MongoDB setup..."
	docker compose exec mongo mongosh "mongodb://metasurvey:metasurvey-dev@localhost:27017/?authSource=admin" inst/scripts/setup_mongodb.js
	@echo "Seeding recipes and workflows..."
	docker compose exec api Rscript inst/scripts/seed_ech_recipes.R
	@echo "Seeding ANDA metadata..."
	docker compose exec api Rscript inst/scripts/seed_anda_metadata.R
	@echo "Seed complete."

.PHONY: docker-logs
docker-logs: ## Tail logs from all containers
	docker compose logs -f

# ── Cleanup ──────────────────────────────────────────────────
.PHONY: clean
clean: ## Remove build artifacts
	rm -rf metasurvey.Rcheck/ metasurvey_*.tar.gz
	rm -f tests/testthat/testthat-problems.rds
	rm -f Rplots.pdf
	rm -rf Meta/

# ── Help ─────────────────────────────────────────────────────
.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'
