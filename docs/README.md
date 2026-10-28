# Mule website

This directory is a dependency-free static GitHub Pages site. It uses local
styles, SVG graphics, and transparent PNG branding, without a JavaScript bundle, external fonts, or a build
service. `index.html` and `style.css` define the landing page.

## Branding

The selected artwork lives in `assets/brand/`: `mule-stride-v1.png` is the
header wordmark, `mule-sly-red-eye-v1.png` is the hero mascot and site icon,
and `mule-mark-v1.png` is the standalone mule used in the footer. These are
transparent raster images; the original generation prompts are saved beside
them. The other concepts in that directory are not used by the page.

## Preview locally

From the repository root:

```sh
uv run python -m http.server 8000 --bind 127.0.0.1 --directory docs
```

Open http://127.0.0.1:8000. Stop the server with Ctrl-C.

## Performance snapshot

`assets/results.json` preserves the complete recorded suite run from
`20261028T163732.408543Z`. It includes all 14 cases across CPython 3.15.0, Mule
`-O2` without LTO, and SBCL 2.6.4. The measured checkout was `0a9e3e863`. It is not a
measurement of every later commit. The report retains individual samples,
commands, input sizes, source and binary hashes, and revision metadata. The run
used four concurrent scenario jobs, so timings may include contention.

`assets/kernel.svg`, `assets/process.svg`, `assets/trend.svg`, and `assets/index.html` come from the
same renderer as ordinary performance runs. The README embeds `kernel.svg`.
To regenerate their appearance without running benchmarks or changing the data:

```sh
uv run python - <<'PY'
import json
from pathlib import Path
from perf.suite.perfutils.report import write_report

directory = Path('docs/assets')
report = json.loads((directory / 'results.json').read_text())
write_report(report, directory)
PY
```

To publish a new measurement, first run the complete suite as described in
`perf/README.md`. Review its statuses, provenance, and variability. Export the
selected `results.json` with `write_report`, then update the dated captions in
`AGENTS.md`, `docs/index.html`, and this file. Do not substitute a different
baseline, omit failures, or select only faster workloads.

## Publish on GitHub Pages

The site is prepared locally. These instructions do not mean it is deployed.

1. Review, commit, and push the intended website files when authorized.
2. Open the repository's **Settings → Pages**.
3. Select **Deploy from a branch** as the source.
4. Select **master** and the **/docs** folder, then save.
5. Wait for GitHub's Pages deployment to finish and check its reported URL.

The expected project URL is https://justinvanwinkle.github.io/mule/.
All local site links are relative so the `/mule/` prefix works. `.nojekyll`
keeps the site as plain static files. No custom domain or repository access
settings are changed by these files.
