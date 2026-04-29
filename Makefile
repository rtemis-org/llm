.PHONY: format format-r document document-r install install-r test test-r

# ── Format ───────────────────────────────────────────────────────────────────
format: format-r

format-r:
	@echo "==> R: Formatting rtemis.llm"
	cd r && air format .

# ── Document ─────────────────────────────────────────────────────────────────
document: document-r

document-r: format-r
	@echo "=> R: Documenting rtemis.llm"
	cd r && Rscript -e "devtools::document()"

# ── Document & Install ───────────────────────────────────────────────────────
install: install-r

install-r: document-r
	@echo "==> R: Installing rtemis.llm"
	cd r && Rscript -e "devtools::install()"

# ── Test ─────────────────────────────────────────────────────────────────────
test: test-r

test-r:
	@echo "==> R: Testing rtemis.llm"
	cd r && Rscript -e "devtools::test(stop_on_failure = TRUE)"

# ── URL Check ────────────────────────────────────────────────────────────────
url-check-r:
	@echo "==> R: Checking URLs in rtemis.llm"
	cd r && Rscript -e "urlchecker::url_check()"

# ── Check ────────────────────────────────────────────────────────────────────
check: check-r

check-r:
	@echo "==> R: Checking rtemis.llm"
	cd r && R CMD build . && R CMD check rtemis.llm_*.tar.gz --as-cran && rm rtemis.llm_*.tar.gz

# ── Build Site ───────────────────────────────────────────────────────────────
site: site-r

site-r:
	@echo "==> R: Building pkgdown site for rtemis.llm"
	cd r && Rscript -e "pkgdown::build_site()"

# ── Build ────────────────────────────────────────────────────────────────────
build-r:
	@echo "==> R: Building rtemis.llm"
	cd r && R CMD build .
