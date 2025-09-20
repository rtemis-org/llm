# Wikipedia Tool Specification

This document specifies a language-agnostic contract for a Wikipedia query tool. It is derived from the current R implementation (`R/tool_wikipedia.R`) but is intentionally framework- and language-neutral so it can be reimplemented in R, Python, Julia, TypeScript, etc.

## Purpose

Provide a small set of operations to:
- Search Wikipedia by a free-text query
- Retrieve the sections of a specific page
- Retrieve clean plaintext for a specific section
- Produce a combined, row-wise dataset of search results augmented with section metadata, section text, and page URL, optionally serialized to JSON

## External Service

- Base: `https://{language}.wikipedia.org/w/api.php`
- API: MediaWiki Action API

## Common Concepts and Conventions

- Language code: a short code like `en`, `de`, `fr`, optionally with hyphen (e.g., `en-us`).
  - Sanitization: lowercase; must match regex `^[a-z-]+$`; otherwise default to `en`.
- Page ID: numeric MediaWiki `pageid` for a page.
- Section ID (aka section index): numeric index provided by the `parse` API `sections` listing.
- Logging/verbosity: optional informational logging only; does not affect results.
- HTTP behavior:
  - Provide a descriptive User-Agent.
  - Retry transient HTTP errors (timeouts, 429, 5xx) with a short capped backoff.
  - Fail fast and raise a clear error on non-2xx responses after retries.
- Text cleanup (clean_refs): remove bracketed numeric citation markers like `[1]`, `[2, 3]` from text.

## Data Models

Note: Types are expressed in a language-neutral way. Use the closest idioms for your target language.

- SearchResult
  - title: string
  - pageid: integer
  - snippet: string (optional; HTML or plaintext snippet as provided by the API)
  - Other fields returned by the API MAY be present and SHOULD be preserved if convenient, but are not required by this spec.

- Section
  - index: integer (the section ID used to query section text)
  - line: string (the visible header text/title of the section)
  - Additional fields from the API (e.g., level, number, anchor) MAY be present.

- CombinedRow (output of the high-level query)
  - page_id: integer
  - title: string
  - section: string (section title)
  - section_id: integer (the `index` from Section)
  - text: string (clean plaintext for the section)
  - url: string (canonical page URL for the page ID)

## Operations

### 1) wiki_search

Search Wikipedia for pages matching a query.

Inputs
- query: string (required)
- limit: integer (default 3; must be between 1 and 50 inclusive; clamp invalid values to the range, default to 3 if not parseable)
- language: string (default "en"; sanitize as described above)
- verbosity: integer (default 1; affects logging only)

Output
- Array/list of SearchResult elements (possibly empty).

HTTP
- Endpoint: `action=query&list=search&srsearch={query}&format=json&srlimit={limit}`

Notes
- If no results are returned, return an empty list/array.
- The order SHOULD match the order provided by the API.

### 2) wiki_sections

Retrieve the list of sections for a specific page.

Inputs
- pageid: integer (required)
- language: string (default "en")
- verbosity: integer (default 1)

Output
- Array/list of Section elements (possibly empty).

HTTP
- Endpoint: `action=parse&pageid={pageid}&prop=sections&redirects=true&format=json`

Notes
- If the page has no sections or does not exist, return an empty list/array.
- The order SHOULD match the order provided by the API.

### 3) wiki_section_text

Retrieve plaintext for a specific section of a page.

Inputs
- pageid: integer (required)
- section_id: integer (required; the section index from wiki_sections)
- language: string (default "en")
- clean_refs: boolean (default true)
- verbosity: integer (default 1)

Output
- text: string (possibly empty)

HTTP
- Endpoint: `action=parse&pageid={pageid}&prop=text§ion={section_id}&redirects=true&format=json`

Post-processing
- Parse HTML returned by the API and extract readable text.
- Prefer selecting and concatenating the text content of block-level nodes: paragraphs and headings. A reasonable default is CSS selectors `p, h2, h3` in document order.
- Join extracted node texts with blank lines (e.g., `\n\n`).
- If `clean_refs` is true, remove bracketed numeric citation markers. A language-agnostic equivalent of the R regex used is:
  - Replace occurrences matching the pattern for citations like `[1]`, `[1, 2]`, `[3, 4, 5]` with empty string.
  - Example regex (PCRE-style): `\[(?:\d+)(?:\s*,\s*\d+)*\]`
- Collapse incidental multiple spaces created by removals to a single space.

Failure/Edges
- If the section has no text, return an empty string.

### 4) query_wikipedia (high-level aggregator)

Search and assemble per-section rows for the top N pages, including the section text and a canonical page URL.

Inputs
- query: string (required)
- limit: integer (default 3; sanitized as in wiki_search)
- language: string (default "en")
- verbosity: integer (default 1)
- max_sections_per_page: integer or null (default null). If provided, limit the number of sections included per page to this many from the start of the list.
- clean_refs: boolean (default true; forwarded to wiki_section_text)
- output: enum { "JSON", "data.table" } (default "JSON"). This spec generalizes it as { "JSON", "table" } — see Output below.
- json_pretty: boolean (default false). Pretty-print JSON if output is JSON.

Output
- If output == "JSON": a JSON array of CombinedRow objects (possibly empty).
- Else: a tabular structure (rows with the CombinedRow fields). In non-R languages, use a native table/list-of-objects type.

Algorithm
1) Call `wiki_search` for `(query, limit, language)`. If empty, return an empty result in the requested format.
2) For each search result in order:
   a) Extract `pageid` and `title`.
   b) Call `wiki_sections(pageid, language)`; if empty, skip this page.
   c) If `max_sections_per_page` is provided, keep only the first K sections.
   d) Compute `page_url = "https://{language}.wikipedia.org/?curid={pageid}"`.
   e) For each kept section in order:
      - `section_id = section.index`
      - `section = section.line`
      - `text = wiki_section_text(pageid, section_id, language, clean_refs)`
      - Append a `CombinedRow` with the five fields and `url = page_url`.
3) Return the aggregated rows as either JSON or a table-like structure.

Serialization
- JSON output MUST be a JSON array (possibly empty) of objects with the CombinedRow fields.
- Pretty-printing is optional and controlled by `json_pretty`.
- Null handling: if any field is missing, represent it as the target language’s `null`/`None`/`NA` equivalent or omit the field (preferred: use `null`).

## Error Handling

- HTTP errors: after retrying transient failures, raise an exception/error including the status code and request URL.
- Input validation: sanitize language and limit as described; other invalid inputs SHOULD raise helpful errors.
- Empty results: return empty arrays/tables rather than null.

## Performance and Limits

- Respect MediaWiki API limits (e.g., `srlimit` up to 50 for anonymous clients). The tool clamps `limit` to [1, 50].
- Implement modest retries with exponential or linear backoff for transient failures, capped (e.g., 3–4 attempts, <5s max backoff).
- Consider adding a global rate limiter if embedding in a high-throughput system.

## Security and Politeness

- Identify the client with a descriptive User-Agent string and a contact URL/email.
- Do not hardcode secrets; none are required for public Wikipedia reads.

## Pseudocode

Below is illustrative pseudocode for each operation.

wiki_search(query, limit=3, language="en", verbosity=1)
- lang = sanitize_language(language)
- n = clamp(to_int(limit) or 3, 1, 50)
- url = base_url(lang)
- req = GET url with query params: action=query, list=search, srsearch=query, format=json, srlimit=n
- resp = perform_with_retry(req)
- ensure_success(resp)
- body = parse_json(resp)
- results = body.query.search or []
- return map_to_SearchResult(results)

wiki_sections(pageid, language="en", verbosity=1)
- lang = sanitize_language(language)
- url = base_url(lang)
- req = GET url with params: action=parse, pageid=pageid, prop=sections, redirects=true, format=json
- resp = perform_with_retry(req)
- ensure_success(resp)
- body = parse_json(resp)
- sections = body.parse.sections or []
- return map_to_Section(sections)

wiki_section_text(pageid, section_id, language="en", clean_refs=true, verbosity=1)
- lang = sanitize_language(language)
- req = GET base_url(lang) with params: action=parse, pageid=pageid, prop=text, section=section_id, redirects=true, format=json
- resp = perform_with_retry(req)
- ensure_success(resp)
- body = parse_json(resp)
- html = body.parse.text["*"] or ""
- if html is empty: return ""
- doc = parse_html(html)
- nodes = select(doc, "p, h2, h3")
- txt_list = text_content(nodes) if nodes not empty else [text_content(doc)]
- out = join(txt_list, "\n\n")
- if clean_refs: out = remove_numeric_citations(out)
- return out

query_wikipedia(query, limit=3, language="en", verbosity=1, max_sections_per_page=null, clean_refs=true, output="JSON", json_pretty=false)
- results = wiki_search(query, limit, language, verbosity)
- rows = []
- for each r in results:
  - pageid = r.pageid; title = r.title
  - secs = wiki_sections(pageid, language, verbosity)
  - if secs empty: continue
  - if max_sections_per_page not null: secs = take_first(secs, max_sections_per_page)
  - page_url = format("https://{lang}.wikipedia.org/?curid={pageid}")
  - for s in secs:
    - sid = int(s.index)
    - text = wiki_section_text(pageid, sid, language, clean_refs, verbosity)
    - rows.append({ page_id: pageid, title: title, section: s.line, section_id: sid, text: text, url: page_url })
- if output == "JSON": return to_json(rows, pretty=json_pretty)
- else: return rows (as a table/list of objects)

Helper: remove_numeric_citations(text)
- Replace all patterns like `[1]`, `[1, 2]`, `[3, 4, 5]` with ""
- Regex example: `\[(?:\d+)(?:\s*,\s*\d+)*\]`
- Collapse multiple spaces to a single space.

## Edge Cases to Consider

- Invalid or unsupported language codes → fallback to `en`.
- `limit <= 0` or non-integer → clamp to [1, 50].
- Pages with no sections → skip in aggregation.
- Sections with only templates/tables and no paragraphs → fallback to document-level text extraction.
- HTML variations across languages → selection of `p, h2, h3` should remain robust.
- Rate limiting (HTTP 429) → backoff and retry; still fail clearly if exhausted.

## Minimal Compliance Checklist

- Implements all four operations with the specified inputs and outputs.
- Sanitizes `language` and clamps `limit` to [1, 50].
- Uses polite HTTP behavior (User-Agent, retries).
- Aggregator returns CombinedRow objects with the exact field names.
- JSON output is an array; pretty-printing is optional.
- Cleans numeric citation markers when requested.
- Returns empty arrays/tables for no-result scenarios; does not return null for collections.

## Example

Given:
- query = "MAPT"
- limit = 2
- language = "en"

`query_wikipedia(...)` returns rows like:
- { page_id: 12345, title: "MAPT", section: "History", section_id: 1, text: "...", url: "https://en.wikipedia.org/?curid=12345" }
- { page_id: 12345, title: "MAPT", section: "Function", section_id: 2, text: "...", url: "https://en.wikipedia.org/?curid=12345" }
- { page_id: 67890, title: "Tau protein", section: "Structure", section_id: 1, text: "...", url: "https://en.wikipedia.org/?curid=67890" }

(Exact content will vary based on live Wikipedia data.)

## Notes

- This spec does not mandate specific libraries; choose appropriate HTTP, HTML parsing, and JSON libraries for your environment.
- Consider caching, parallelization, or batching if implementing for larger workloads—while respecting API etiquette.
