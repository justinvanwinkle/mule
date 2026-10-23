import xml.etree.ElementTree as ET

import pytest

from perf.suite.perfutils.report import chart, dashboard, trend_chart, update_site

NS = {"svg": "http://www.w3.org/2000/svg"}


def report(seconds=(0.010, 0.020, 0.040)):
    runtimes = ("cpython", "mule-o2", "sbcl")
    return {
        "baseline": "cpython",
        "created_utc": "20261019T011706.939668Z",
        "platform": "Linux & x86-64",
        "options": {"samples": 5, "seed": 42},
        "runtimes": [{"name": name} for name in runtimes],
        "rows": [
            {
                "case": "scenario_one",
                "runtime": name,
                "status": "ok",
                "summary": {"kernel_seconds": {"median": value}},
                "time_ratio": {"kernel_seconds": 999},
            }
            for name, value in zip(runtimes, seconds, strict=False)
        ],
    }


def bars(root):
    return root.findall(".//svg:rect[@data-ms]", NS)


def test_vertical_bars_include_baseline_and_scale_linearly_from_zero():
    root = ET.fromstring(chart(report(), "kernel_seconds"))
    plotted = bars(root)
    assert [bar.attrib["data-runtime"] for bar in plotted] == [
        "cpython",
        "mule-o2",
        "sbcl",
    ]
    heights = [float(bar.attrib["height"]) for bar in plotted]
    assert heights[1] == pytest.approx(2 * heights[0], abs=0.02)
    assert heights[2] == pytest.approx(4 * heights[0], abs=0.02)
    zero = root.find(".//*[@data-zero-y]")
    assert zero is not None
    for bar in plotted:
        assert float(bar.attrib["y"]) + float(bar.attrib["height"]) == pytest.approx(
            float(zero.attrib["data-zero-y"]), abs=0.02
        )
    assert [float(bar.attrib["data-ms"]) for bar in plotted] == [10, 20, 40]
    assert "10 ms" in " ".join(root.itertext())


def test_equal_times_have_equal_heights_and_runtime_labels_below_bars():
    root = ET.fromstring(chart(report((0.1, 0.1, 0.1)), "kernel_seconds"))
    assert len({bar.attrib["height"] for bar in bars(root)}) == 1
    labels = root.findall(".//svg:text[@data-runtime]", NS)
    assert len(labels) == 3
    for label, bar in zip(labels, bars(root), strict=True):
        assert float(label.attrib["y"]) > float(bar.attrib["y"]) + float(
            bar.attrib["height"]
        )


@pytest.mark.parametrize("seconds", [-1, float("nan"), float("inf"), None])
def test_invalid_times_remain_visible_without_misleading_bars(seconds):
    root = ET.fromstring(chart(report((seconds,)), "kernel_seconds"))
    assert bars(root) == []
    assert "not available" in " ".join(root.itertext())


def test_zero_elapsed_time_is_a_zero_height_bar():
    root = ET.fromstring(chart(report((0,)), "kernel_seconds"))
    assert float(bars(root)[0].attrib["height"]) == 0


def test_failed_comparison_never_plots_stale_timing():
    source = report((0.1,))
    source["rows"][0]["status"] = "incorrect_result"
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert bars(root) == []
    assert "incorrect_result" in " ".join(root.itertext())


def test_scenarios_use_independent_scales_with_consistent_runtime_order():
    source = report()
    source["rows"] += [
        dict(
            row,
            case="scenario_two",
            summary={
                "kernel_seconds": {
                    "median": row["summary"]["kernel_seconds"]["median"] * 100
                }
            },
        )
        for row in reversed(source["rows"])
    ]
    root = ET.fromstring(chart(source, "kernel_seconds"))
    panels = root.findall(".//svg:g[@data-case]", NS)
    assert len(panels) == 2
    first, second = [bars(panel) for panel in panels]
    assert [bar.attrib["data-runtime"] for bar in first] == [
        bar.attrib["data-runtime"] for bar in second
    ]
    assert [bar.attrib["height"] for bar in first] == [
        bar.attrib["height"] for bar in second
    ]
    assert [bar.attrib["fill"] for bar in first] == [
        bar.attrib["fill"] for bar in second
    ]


def test_svg_escapes_labels_and_contains_accessible_descriptions():
    source = report((0.1,))
    source["rows"][0]["case"] = 'parse <tag> & "value"'
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert 'parse <tag> & "value"' in " ".join(root.itertext())
    assert root.find("svg:title", NS) is not None
    assert root.find("svg:desc", NS) is not None


def test_empty_report_and_missing_runtime_rows_render_without_bars():
    root = ET.fromstring(chart(report(()), "kernel_seconds"))
    assert bars(root) == []
    source = report((0.1,))
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert len(bars(root)) == 1
    assert "missing" in " ".join(root.itertext())


def test_dashboard_escapes_failure_details_and_provenance():
    source = report((0.1,))
    source["rows"][0].update(status="build_failed", error="<script>bad()</script>")
    source["runtimes"][1]["version"] = "<version & build>"
    page = dashboard(source)
    assert "&lt;script&gt;bad()&lt;/script&gt;" in page
    assert "&lt;version &amp; build&gt;" in page
    assert "<script>" not in page


@pytest.mark.parametrize("ratio", [0, 0.5, 1, 3])
def test_dashboard_displays_elapsed_time_ratio(ratio):
    source = report((0.1,))
    row = source["rows"][0]
    row["summary"] = {
        metric: {"median": 0.1, "stdev": 0}
        for metric in ("kernel_seconds", "process_seconds")
    }
    row["time_ratio"]["kernel_seconds"] = ratio
    page = dashboard(source)
    assert f"<td>{ratio:.3g}</td>" in page


@pytest.mark.parametrize("ratio", [None, -1, float("nan"), float("inf")])
def test_dashboard_displays_unavailable_ratio(ratio):
    source = report((0.1,))
    row = source["rows"][0]
    row["summary"] = {
        metric: {"median": 0.1, "stdev": 0}
        for metric in ("kernel_seconds", "process_seconds")
    }
    row["time_ratio"]["kernel_seconds"] = ratio
    assert "<td>—</td>" in dashboard(source)


def test_update_site_writes_assets_and_updates_captions(tmp_path):
    (tmp_path / "docs/assets").mkdir(parents=True)
    (tmp_path / "docs/index.html").write_text(
        "<figcaption>October 18, 2026 · 14 workloads</figcaption>\n"
        "<p>Measured checkout: <code>2d5e473ee</code> with uncommitted tracked changes.</p>\n",
        encoding="utf-8",
    )
    (tmp_path / "docs/README.md").write_text(
        "preserves the complete recorded suite run from\n"
        "`20261018T000000.000000Z`. The measured checkout was `2d5e473ee` with\n"
        "uncommitted tracked changes, identified by its recorded diff digest.\n",
        encoding="utf-8",
    )
    (tmp_path / "AGENTS.md").write_text(
        "from the recorded October 18,\n"
        "2026 suite run.\n"
        "The measured checkout\n"
        "was `2d5e473ee` with uncommitted tracked changes.\n",
        encoding="utf-8",
    )
    source = report((0.1, 0.2, 0.3))
    source["created_utc"] = "20261019T173501.070793Z"
    source["runtimes"][1]["revision"] = {
        "commit": "ae24f3227abcdef0123456789",
        "tracked_diff_sha256": "dirty",
    }
    for row in source["rows"]:
        row["summary"]["process_seconds"] = {"median": 0.1, "stdev": 0.01}
        row["summary"]["kernel_seconds"]["stdev"] = 0.01
    update_site(source, tmp_path)
    assert (tmp_path / "docs/assets/results.json").is_file()
    assert (tmp_path / "docs/assets/kernel.svg").is_file()
    assert (tmp_path / "docs/assets/process.svg").is_file()
    assert (tmp_path / "docs/assets/trend.svg").is_file()
    assert (tmp_path / "docs/assets/index.html").is_file()
    assert "trend.svg" in (tmp_path / "docs/assets/index.html").read_text()
    assert "October 19, 2026 · " in (tmp_path / "docs/index.html").read_text()
    assert "<code>ae24f3227</code>" in (tmp_path / "docs/index.html").read_text()
    assert "20261019T173501.070793Z" in (tmp_path / "docs/README.md").read_text()
    assert "`ae24f3227`" in (tmp_path / "docs/README.md").read_text()
    assert "October 19,\n2026 suite run." in (tmp_path / "AGENTS.md").read_text()
    assert "was `ae24f3227`" in (tmp_path / "AGENTS.md").read_text()


def test_paired_typed_and_untyped_scenarios_render_together_in_charts_dashboard_and_history():
    from perf.suite.perfutils import history

    source = report((0.020, 0.040, 0.010))
    for row in source["rows"]:
        row["case"] = "float_nbody"
        row["summary"]["kernel_seconds"]["stdev"] = 0.001
        row["summary"]["process_seconds"] = {"median": 0.050, "stdev": 0.001}
    typed_seconds = (0.020, 0.012, 0.004)
    for runtime_row, sec in zip(list(source["rows"]), typed_seconds, strict=True):
        source["rows"].append(
            {
                "case": "float_nbody_typed",
                "runtime": runtime_row["runtime"],
                "status": "ok",
                "summary": {
                    "kernel_seconds": {"median": sec, "stdev": 0.001},
                    "process_seconds": {"median": 0.050, "stdev": 0.001},
                },
                "time_ratio": {"kernel_seconds": sec / 0.020, "process_seconds": 1.0},
            }
        )

    root = ET.fromstring(chart(source, "kernel_seconds"))
    panels = root.findall(".//svg:g[@data-case]", NS)
    assert len(panels) == 1
    assert panels[0].attrib["data-case"] == "float_nbody"
    assert panels[0].attrib["data-typed-case"] == "float_nbody_typed"
    plotted = bars(panels[0])
    assert len(plotted) == 6
    assert [
        (b.attrib["data-runtime"], b.attrib["data-variant"], b.attrib["data-case"])
        for b in plotted
    ] == [
        ("cpython", "untyped", "float_nbody"),
        ("cpython", "typed", "float_nbody_typed"),
        ("mule-o2", "untyped", "float_nbody"),
        ("mule-o2", "typed", "float_nbody_typed"),
        ("sbcl", "untyped", "float_nbody"),
        ("sbcl", "typed", "float_nbody_typed"),
    ]

    page = dashboard(source)
    assert (
        'class="paired-untyped" data-paired-case="float_nbody" data-variant="untyped"'
        in page
    )
    assert (
        'class="paired-typed" data-paired-case="float_nbody" data-variant="typed"'
        in page
    )
    assert "typed / untyped: 0.3" in page

    runs = [
        {
            "created_utc": "20261022T120000.000000Z",
            "baseline": "cpython",
            "options": {"samples": 3},
            "runtimes": [
                {"name": "cpython", "commit": None},
                {"name": "mule-o2", "commit": "0123456789abcdef"},
            ],
            "results": [
                {
                    "case": "float_nbody",
                    "runtime": "mule-o2",
                    "status": "ok",
                    "kernel_ms": 40.0,
                    "kernel_time_ratio": 2.0,
                },
                {
                    "case": "word_frequency",
                    "runtime": "mule-o2",
                    "status": "ok",
                    "kernel_ms": 15.0,
                    "kernel_time_ratio": 1.5,
                },
                {
                    "case": "float_nbody_typed",
                    "runtime": "mule-o2",
                    "status": "ok",
                    "kernel_ms": 12.0,
                    "kernel_time_ratio": 0.6,
                },
            ],
        }
    ]
    lines = history.table(runs)
    assert (
        lines[1].index("float_nbody")
        < lines[1].index("float_nbody_typed")
        < lines[1].index("word_frequency")
    )
    filtered = history.table(runs, cases=["float_nbody"])
    assert "float_nbody" in filtered[1] and "float_nbody_typed" in filtered[1]


def test_trend_chart_plots_historical_series_and_pairs_typed_variants():
    from perf.suite.perfutils import history

    run1 = report((0.020, 0.080, 0.010))
    run1["created_utc"] = "20261020T100000.000000Z"
    run2 = report((0.020, 0.040, 0.010))
    run2["created_utc"] = "20261021T100000.000000Z"
    run3 = report((0.020, 0.025, 0.010))
    run3["created_utc"] = "20261022T100000.000000Z"
    history_runs = [history.entry(run1), history.entry(run2)]

    root = ET.fromstring(trend_chart(run3, history_runs))
    assert "3 recorded runs" in " ".join(root.itertext())
    panel = root.find(".//svg:g[@data-case='scenario_one']", NS)
    assert panel is not None
    lines = panel.findall(".//svg:polyline[@data-runtime='mule-o2']", NS)
    assert len(lines) == 1
    points = panel.findall(".//*[@data-runtime='mule-o2'][@data-ms]")
    assert [float(pt.attrib["data-ms"]) for pt in points] == [80.0, 40.0, 25.0]
    cxs = [float(pt.attrib.get("cx", pt.attrib.get("data-x"))) for pt in points]
    assert cxs[0] < cxs[1] < cxs[2]
    assert [pt.attrib.get("data-clipped") for pt in points] == ["true", "true", None]
    # Only the recent value fits the new scale. Its position is linear from zero.
    zero = float(panel.find(".//*[@data-zero-y]").attrib["data-zero-y"])
    assert zero - float(points[2].attrib["cy"]) == pytest.approx(
        160 * 25 / 30, abs=0.02
    )

    # Paired untyped and typed scenarios share a single trend panel.
    for r, typed_mule in ((run1, 0.030), (run2, 0.015), (run3, 0.012)):
        for row in list(r["rows"]):
            row["case"] = "float_nbody"
        r["rows"].append(
            {
                "case": "float_nbody_typed",
                "runtime": "mule-o2",
                "status": "ok",
                "summary": {"kernel_seconds": {"median": typed_mule}},
                "time_ratio": {"kernel_seconds": typed_mule / 0.020},
            }
        )
    paired_root = ET.fromstring(
        trend_chart(run3, [history.entry(run1), history.entry(run2)])
    )
    paired_panels = paired_root.findall(".//svg:g[@data-case]", NS)
    assert len(paired_panels) == 1
    assert paired_panels[0].attrib["data-case"] == "float_nbody"
    assert paired_panels[0].attrib["data-typed-case"] == "float_nbody_typed"
    untyped_line = paired_panels[0].find(
        ".//svg:polyline[@data-runtime='mule-o2'][@data-variant='untyped']", NS
    )
    typed_line = paired_panels[0].find(
        ".//svg:polyline[@data-runtime='mule-o2'][@data-variant='typed']", NS
    )
    assert untyped_line is not None
    assert typed_line is not None
    assert "stroke-dasharray" in typed_line.attrib


def test_trend_chart_breaks_lines_on_failed_runs():
    from perf.suite.perfutils import history

    run1 = report((0.020, 0.060, 0.010))
    run1["created_utc"] = "20261020T100000.000000Z"
    run2 = report((0.020, 0.050, 0.010))
    run2["created_utc"] = "20261021T100000.000000Z"
    run2["rows"][1]["status"] = "timeout"
    run3 = report((0.020, 0.030, 0.010))
    run3["created_utc"] = "20261022T100000.000000Z"

    root = ET.fromstring(trend_chart(run3, [history.entry(run1), history.entry(run2)]))
    panel = root.find(".//svg:g[@data-case='scenario_one']", NS)
    points = panel.findall(".//*[@data-runtime='mule-o2'][@data-ms]")
    # Failed middle run has no point and breaks the polyline so it does not bridge across failure.
    assert [float(pt.attrib["data-ms"]) for pt in points] == [60.0, 30.0]
    assert panel.findall(".//svg:polyline[@data-runtime='mule-o2']", NS) == []


@pytest.mark.parametrize(
    "run_count,window", [(1, 1), (3, 1), (9, 3), (40, 10), (48, 10)]
)
def test_trend_scale_uses_recent_quarter_capped_at_ten_runs(run_count, window):
    from perf.suite.perfutils import history

    runs = []
    for index in range(run_count):
        source = report((0.010,))
        source["created_utc"] = f"run-{index}"
        if index == run_count - window - 1:
            source["rows"][0]["summary"]["kernel_seconds"]["median"] = 100
        if index == run_count - window:
            source["rows"][0]["summary"]["kernel_seconds"]["median"] = 0.020
        runs.append(source)
    root = ET.fromstring(trend_chart(runs[-1], [history.entry(r) for r in runs]))
    panel = root.find(".//svg:g[@data-case='scenario_one']", NS)
    assert float(panel.attrib["data-upper-ms"]) == pytest.approx(24)
    assert int(panel.attrib["data-scale-runs"]) == window
    points = panel.findall(".//*[@data-ms]")
    assert len(points) == run_count
    clipped = panel.findall(".//svg:path[@data-clipped='true']", NS)
    assert len(clipped) == (1 if run_count > window else 0)
    if clipped:
        assert float(clipped[0].attrib["data-ms"]) == 100000
        assert "100000 ms" in " ".join(panel.itertext())


@pytest.mark.parametrize("run_count", [30, 100])
def test_trend_long_above_scale_history_keeps_panel_compact_and_timings_available(
    run_count,
):
    from perf.suite.perfutils import history

    runs = []
    for index in range(run_count):
        source = report((0.010, 0.080 if index < run_count - 10 else 0.020, 0.010))
        source["created_utc"] = f"run-{index}"
        runs.append(source)
    root = ET.fromstring(trend_chart(runs[-1], [history.entry(r) for r in runs]))
    panel = root.find(".//svg:g[@data-case='scenario_one']", NS)
    assert float(panel.find("svg:rect", NS).attrib["height"]) <= 400
    points = panel.findall(".//*[@data-runtime='mule-o2'][@data-ms]")
    assert [float(point.attrib["data-ms"]) for point in points] == [
        80 if index < run_count - 10 else 20 for index in range(run_count)
    ]
    for point in points:
        assert point.find("svg:title", NS) is not None
    summary = panel.find("svg:text[@data-above-scale-runtime='mule-o2']", NS)
    assert float(summary.attrib["data-min-ms"]) == 80
    assert float(summary.attrib["data-max-ms"]) == 80


def test_trend_clips_lines_at_actual_scale_crossing_and_preserves_failure_gaps():
    from perf.suite.perfutils import history

    runs = []
    for index, value in enumerate((100, 10, None, 100, 10, 10, 10, 10)):
        source = report(((value or 10) / 1000,))
        source["created_utc"] = f"run-{index}"
        if value is None:
            source["rows"][0]["status"] = "timeout"
        runs.append(source)
    root = ET.fromstring(trend_chart(runs[-1], [history.entry(r) for r in runs]))
    panel = root.find(".//svg:g[@data-case='scenario_one']", NS)
    zero = float(panel.find(".//*[@data-zero-y]").attrib["data-zero-y"])
    points = panel.findall(".//svg:circle[@data-ms]", NS)
    left = float(panel.find(".//*[@data-clipped='true']").attrib["data-x"])
    second_x = float(points[0].attrib["cx"])
    lines = panel.findall(".//svg:polyline[@data-runtime='cpython']", NS)
    assert len(lines) == 2
    first_segment = [
        tuple(map(float, pair.split(","))) for pair in lines[0].attrib["points"].split()
    ]
    # The line enters the scale near the second run, not below the outlier marker.
    assert first_segment[0][0] == pytest.approx(
        left + (second_x - left) * 88 / 90, abs=0.02
    )
    assert first_segment[0][1] == pytest.approx(zero - 160, abs=0.02)
    for line in lines:
        for pair in line.attrib["points"].split():
            _, y = map(float, pair.split(","))
            assert zero - 160 <= y <= zero
