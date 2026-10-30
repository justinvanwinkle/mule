"""Document tokenization, stemming, inverted indexing, and config parsing workloads.

Exercises full-document splitlines/split, suffix stemming, character-class
counting, n-gram slicing, dict frequency aggregation, and INI/config unescaping.
"""

import sys
import time

DEFAULT_SIZE = 1000
DEFAULT_LOOPS = 1

_PARAGRAPHS = (
    (
        "The compiler translates high-level Python source code into static single\n"
        "assignment intermediate representations before lowering operations to\n"
        "machine instructions. Type inference propagates concrete constraints\n"
        "across control flow joins, eliminating redundant dynamic dispatches!"
    ),
    (
        "Web frameworks process incoming HTTP requests by routing URL paths,\n"
        "validating session cookies, querying relational databases, and rendering\n"
        "escaped HTML templates. Reducing intermediate string allocations directly\n"
        "improves request throughput and tail latency across services."
    ),
    (
        "Lexical scanners iterate over character streams, classifying identifiers,\n"
        "numeric literals, punctuation tokens, and quoted string constants.\n"
        "Fast character membership checks and zero-copy slice comparisons keep\n"
        "tokenization overhead minimal during large builds."
    ),
    (
        "Inverted indexes map normalized stems and character trigrams to posting\n"
        "lists of document identifiers. Computing vowel ratios, prefix buckets,\n"
        "and term frequencies requires tight interaction between string methods\n"
        "and hash table lookups."
    ),
)

_CONFIG_SECTIONS = (
    (
        "# Database pool configuration\n"
        "[database.primary]\n"
        "host = \"db-primary.internal.example.com\"\n"
        "port = 5432\n"
        "options = \"-c statement_timeout=5000 -c lock_timeout=1000\"\n"
        "dsn = \"postgresql://app_user:secret@db-primary:5432/prod_db\\nsslmode=require\"\n"
    ),
    (
        "[cache.redis]\n"
        "endpoints = \"redis-01.local:6379, redis-02.local:6379, redis-03.local:6379\"\n"
        "prefix = \"dj_cache:v2:\\tprod:\"\n"
        "max_connections = 128 # per worker process\n"
        "compression = true\n"
    ),
    (
        "[logging.formatters]\n"
        "pattern = \"%(asctime)s [%(levelname)s] %(name)s: %(message)s\"\n"
        "datefmt = \"%Y-%m-%dT%H:%M:%S%z\"\n"
        "escape_newlines = \"true\"\n"
    ),
)


def prepare(size: int) -> tuple[list[str], str]:
    docs: list[str] = []
    config_chunks: list[str] = []
    state = 271828183
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        p1 = _PARAGRAPHS[state % len(_PARAGRAPHS)]
        p2 = _PARAGRAPHS[(state >> 4) % len(_PARAGRAPHS)]
        docs.append(f"Document #{i}\n{p1}\n{p2}\n")

        cfg = _CONFIG_SECTIONS[(state >> 8) % len(_CONFIG_SECTIONS)]
        config_chunks.append(cfg + f"instance_id = {i}\n\n")

    return docs, "".join(config_chunks)


def _stem_word(word: str) -> str:
    n = len(word)
    if n > 6 and word.endswith("ing"):
        return word[:-3]
    if n > 6 and word.endswith("ations"):
        return word[:-6] + "e"
    if n > 5 and word.endswith("ed"):
        return word[:-2]
    if n > 5 and word.endswith("ly"):
        return word[:-2]
    if n > 4 and word.endswith("es"):
        return word[:-2]
    if n > 4 and word.endswith("s") and not word.endswith("ss"):
        return word[:-1]
    return word


def scenario_document_indexing(docs: list[str]) -> tuple[int, int, int, int]:
    """Tokenize documents, stem words, count vowels, and build term & trigram frequencies."""
    term_freq: dict[str, int] = {}
    trigram_freq: dict[str, int] = {}
    total_vowels = 0
    total_tokens = 0

    for _ in range(325):
        for doc in docs:
            for line in doc.splitlines():
                for raw_word in line.split():
                    word = raw_word.strip(".,!?;:\"'()#").lower()
                    if not word or not word.isalpha():
                        continue
                    total_tokens += 1
                    for ch in word:
                        if ch in "aeiou":
                            total_vowels += 1

                    stem = _stem_word(word)
                    term_freq[stem] = term_freq.get(stem, 0) + 1

                    if len(stem) >= 5:
                        tri = stem[:3]
                        trigram_freq[tri] = trigram_freq.get(tri, 0) + 1

    top_term_sum = 0
    for stem, count in term_freq.items():
        if count > 10:
            top_term_sum += count * len(stem)

    return total_tokens, total_vowels, top_term_sum, len(trigram_freq)


def scenario_config_parser(config_text: str) -> tuple[int, int, int]:
    """Parse INI-like configuration text with comments, sections, and escape sequences."""
    section_count = 0
    numeric_sum = 0
    string_bytes = 0
    current_section = ""

    for _ in range(6500):
        for raw_line in config_text.splitlines():
            line = raw_line.strip()
            if not line or line.startswith("#"):
                continue
            if line.startswith("[") and line.endswith("]"):
                current_section = line[1:-1].strip()
                section_count += 1
                continue

            key, eq, rhs = line.partition("=")
            if not eq:
                continue
            norm_key = key.strip()
            val = rhs.strip()

            if val.startswith('"'):
                end_quote = val.rfind('"')
                if end_quote > 0:
                    inner = val[1:end_quote]
                    if "\\" in inner:
                        inner = (
                            inner.replace("\\n", "\n")
                            .replace("\\t", "\t")
                            .replace('\\"', '"')
                        )
                    string_bytes += len(current_section) + len(norm_key) + len(inner)
            else:
                # Strip trailing inline comment
                clean_val, _hash, _comment = val.partition("#")
                clean_val = clean_val.strip()
                if clean_val.isdigit():
                    numeric_sum += int(clean_val)
                elif clean_val in ("true", "false"):
                    numeric_sum += 1 if clean_val == "true" else 0

    return section_count, numeric_sum, string_bytes


def run_all(data: tuple[list[str], str]) -> tuple[int, int, int, int, int, int, int]:
    docs, config_text = data
    r1 = scenario_document_indexing(docs)
    r2 = scenario_config_parser(config_text)
    return r1 + r2


def main() -> None:
    size = DEFAULT_SIZE
    loops = DEFAULT_LOOPS
    scenario = "all"
    args = sys.argv[1:]
    i = 0
    while i < len(args):
        if args[i] == "--size" and i + 1 < len(args):
            size = int(args[i + 1])
            i += 2
        elif args[i] == "--loops" and i + 1 < len(args):
            loops = int(args[i + 1])
            i += 2
        elif args[i] == "--scenario" and i + 1 < len(args):
            scenario = args[i + 1]
            i += 2
        else:
            raise ValueError(f"Unknown argument: {args[i]}")

    t0 = time.perf_counter()
    data = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    idx_s = 0.0
    cfg_s = 0.0
    docs, config_text = data
    for _ in range(loops):
        r1 = ()
        r2 = ()
        if scenario in ("all", "doc_indexing"):
            s0 = time.perf_counter()
            r1 = scenario_document_indexing(docs)
            idx_s += time.perf_counter() - s0
        if scenario in ("all", "config_parser"):
            s1 = time.perf_counter()
            r2 = scenario_config_parser(config_text)
            cfg_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (idx_s + cfg_s) / loops
    print(f"topic\tstring/text_analytics")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "doc_indexing"):
        print(f"doc_indexing\t{idx_s / loops:.6f}")
    if scenario in ("all", "config_parser"):
        print(f"config_parser\t{cfg_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
