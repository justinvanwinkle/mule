"""Compile and exercise Mule's object runtime through native C probes."""

import shutil
import subprocess
from pathlib import Path

import pytest

RUNTIME = Path(__file__).resolve().parents[3] / "mule/compiler/runtime.c"

PROBE = r"""
#include <stdio.h>
#include <string.h>
#include "runtime.h"
#include <stdlib.h>

static const char *scenario;

static int show(MuleValue value) {
    if (!value) return -1;
    int status = mule_format(stdout, value, 1, 0);
    putchar('\n');
    mule_release(value);
    return status;
}

static MuleValue add_bias(MuleValue globals, MuleValue args) {
    if (mule_check_args(args, 1) < 0) return NULL;
    MuleValue value = mule_arg(args, 0);
    MuleValue bias = mule_load_global(globals, "bias");
    if (!value || !bias) {
        mule_release(value); mule_release(bias); return NULL;
    }
    MuleValue result = mule_binary(0, value, bias, 0);
    mule_release(value); mule_release(bias);
    return result;
}

static MuleValue double_value(MuleValue globals, MuleValue args) {
    (void)globals;
    if (mule_check_args(args, 1) < 0) return NULL;
    MuleValue value = mule_arg(args, 0);
    if (!value) return NULL;
    MuleValue result = mule_binary(0, value, value, 0);
    mule_release(value);
    return result;
}

static MuleValue recurse(MuleValue globals, MuleValue args) {
    static int depth;
    if (mule_check_args(args, 0) < 0) return NULL;
    if (++depth >= 100000) {
        mule_error("RuntimeError", "recursion guard missing");
        return NULL;
    }
    MuleValue function = mule_load_global(globals, "recurse");
    if (!function) return NULL;
    MuleValue result = mule_call(function, 0, NULL);
    mule_release(function);
    return result;
}

static MuleValue module_entry(MuleValue globals, MuleValue args) {
    if (mule_check_args(args, 0) < 0) return NULL;
    if (!strcmp(scenario, "literals")) {
        if (show(mule_literal_int("-123456789012345678901234567890")) < 0 ||
            show(mule_literal_float(1.25)) < 0 ||
            show(mule_literal_text("snow \xe2\x98\x83\0end", 12)) < 0 ||
            show(mule_literal_bytes("a\0b", 3)) < 0 ||
            show(mule_none()) < 0 || show(mule_bool(0)) < 0 ||
            show(mule_bool(1)) < 0) return NULL;
    } else if (!strcmp(scenario, "huge_int")) {
        char decimal[6002];
        decimal[0] = '1'; memset(decimal + 1, '0', 6000); decimal[6001] = 0;
        MuleValue actual = mule_literal_int(decimal);
        MuleValue ten = mule_literal_int("10"), exponent = mule_literal_int("6000");
        MuleValue expected = mule_binary(7, ten, exponent, 0);
        mule_release(ten); mule_release(exponent);
        if (!actual || !expected) { mule_release(actual); mule_release(expected); return NULL; }
        MuleValue equal = mule_compare(0, actual, expected);
        mule_release(actual); mule_release(expected);
        if (show(equal) < 0) return NULL;
        decimal[0] = '-'; decimal[1] = '1';
        MuleValue negative = mule_literal_int(decimal);
        if (!negative) return NULL;
        MuleValue zero = mule_literal_int("0");
        equal = mule_compare(2, negative, zero);
        mule_release(negative); mule_release(zero);
        if (show(equal) < 0) return NULL;
    } else if (!strcmp(scenario, "recursion")) {
        MuleValue function = mule_function(globals, (void *)recurse, "recurse");
        if (!function) return NULL;
        if (mule_store_global(globals, "recurse", function) < 0) {
            mule_release(function); return NULL;
        }
        MuleValue result = mule_call(function, 0, NULL);
        mule_release(function); return result;
    } else if (!strcmp(scenario, "binary")) {
        MuleValue left = mule_literal_int("7"), right = mule_literal_int("2");
        for (int op = 0; op < 13; op++) {
            if (op == 6) continue;
            if (show(mule_binary(op, left, right, 0)) < 0) {
                mule_release(left); mule_release(right); return NULL;
            }
        }
        mule_release(left); mule_release(right);
    } else if (!strcmp(scenario, "unary")) {
        MuleValue value = mule_literal_int("7");
        for (int op = 0; op < 4; op++) {
            if (show(mule_unary(op, value)) < 0) { mule_release(value); return NULL; }
        }
        mule_release(value);
    } else if (!strcmp(scenario, "compare")) {
        MuleValue left = mule_literal_int("1000000000000000000000"), right = mule_literal_int("1000000000000000000000");
        for (int op = 0; op < 8; op++) {
            if (show(mule_compare(op, left, right)) < 0) goto compare_error;
        }
        MuleObject *items = mule_allocate(MULE_ARGS);
        if (items) {
            items->as.args.values = malloc(sizeof(MuleValue));
            if (!items->as.args.values) {
                mule_error("MemoryError", "sequence allocation failed");
                mule_release(items); items = NULL;
            } else {
                items->as.args.count = 1;
                items->as.args.values[0] = mule_retain(left);
            }
        }
        if (!items) goto compare_error;
        int status = show(mule_compare(8, left, items));
        if (status == 0) status = show(mule_compare(9, left, items));
        mule_release(items);
        if (status < 0) goto compare_error;
        mule_release(left); mule_release(right);
        goto done;
        compare_error:
        mule_release(left); mule_release(right); return NULL;
    } else if (!strcmp(scenario, "inplace")) {
        MuleValue left = mule_list(), right = mule_list();
        MuleValue value = mule_literal_int("5");
        if (!left || !right || !value || mule_list_append(right, value) < 0) return NULL;
        MuleValue result = mule_binary(0, left, right, 1);
        if (!result) return NULL;
        printf("same=%d\n", result == left);
        show(result); mule_release(left); mule_release(right); mule_release(value);
    } else if (!strcmp(scenario, "globals")) {
        MuleValue value = mule_literal_int("42");
        if (!value || mule_store_global(globals, "answer", value) < 0) return NULL;
        mule_release(value);
        if (show(mule_load_global(globals, "answer")) < 0) return NULL;
        MuleValue builtin = mule_load_global(globals, "print");
        if (!builtin) return NULL;
        printf("callable=%d\n", !mule_is_fixnum(builtin) && ((MuleObject *)builtin)->kind == MULE_FUNCTION); mule_release(builtin);
        value = mule_none();
        if (mule_store_global(globals, "print", value) < 0) return NULL;
        mule_release(value);
        if (show(mule_load_global(globals, "print")) < 0) return NULL;
        if (show(mule_load_global(globals, "__name__")) < 0) return NULL;
    } else if (!strcmp(scenario, "function_metadata")) {
        MuleValue bias = mule_literal_int("40");
        if (!bias || mule_store_global(globals, "bias", bias) < 0) return NULL;
        mule_release(bias);
        MuleValue first = mule_function(globals, (void *)add_bias, "first");
        if (!first) return NULL;
        int definitions = 0;
        for (int index = 0; index < 10000; index++) {
            MuleValue replacement = mule_function(globals, (void *)add_bias, "first");
            if (!replacement) { mule_release(first); return NULL; }
            if (replacement == first) {
                mule_error("RuntimeError", "definition reused callable identity");
                mule_release(replacement); mule_release(first); return NULL;
            }
            MuleObject *replacement_object = replacement;
            if (strcmp(replacement_object->as.function.name, "first") ||
                replacement_object->as.function.entry != add_bias) {
                mule_error("RuntimeError", "replacement lost function metadata");
                mule_release(replacement); mule_release(first); return NULL;
            }
            mule_release(replacement);
            definitions++;
        }
        MuleValue second = mule_function(globals, (void *)add_bias, "second");
        if (!second) { mule_release(first); return NULL; }
        MuleValue other_entry = mule_function(globals, (void *)double_value, "first");
        if (!other_entry) { mule_release(first); mule_release(second); return NULL; }
        printf("definitions=%d\n", definitions);
        MuleObject *first_object = first, *second_object = second;
        int status = show(mule_literal_text(first_object->as.function.name, (intptr_t)strlen(first_object->as.function.name)));
        if (status == 0) status = show(mule_literal_text(second_object->as.function.name, (intptr_t)strlen(second_object->as.function.name)));
        MuleValue value = mule_literal_int("2");
        MuleValue values[] = {value};
        if (status == 0) status = show(mule_call(first, 1, values));
        if (status == 0) status = show(mule_call(second, 1, values));
        if (status == 0) status = show(mule_call(other_entry, 1, values));
        mule_release(first); mule_release(second); mule_release(other_entry); mule_release(value);
        if (status < 0) return NULL;
    } else if (!strcmp(scenario, "function") || !strcmp(scenario, "arity")) {
        MuleValue bias = mule_literal_int("40");
        if (!bias || mule_store_global(globals, "bias", bias) < 0) return NULL;
        mule_release(bias);
        MuleValue function = mule_function(globals, (void *)add_bias, "add_bias");
        if (!function) return NULL;
        if (!strcmp(scenario, "arity")) {
            MuleValue result = mule_call(function, 0, NULL);
            mule_release(function); return result;
        }
        MuleValue value = mule_literal_int("2");
        MuleValue values[] = {value};
        MuleValue result = mule_call(function, 1, values);
        mule_release(value); mule_release(function);
        if (show(result) < 0) return NULL;
    } else if (!strcmp(scenario, "builtin_call")) {
        MuleValue function = mule_load_global(globals, "print");
        MuleValue value = mule_literal_text("native callback", 15);
        MuleValue values[] = {value};
        if (!function || !value) return NULL;
        MuleValue result = mule_call(function, 1, values);
        mule_release(function); mule_release(value);
        if (!result) return NULL;
        mule_release(result);
    } else if (!strcmp(scenario, "truth")) {
        MuleValue zero = mule_literal_int("0"), one = mule_literal_int("1");
        printf("%d %d\n", mule_truth(zero), mule_truth(one));
        mule_release(zero); mule_release(one);
    } else if (!strcmp(scenario, "zero_division")) {
        MuleValue left = mule_literal_int("1"), right = mule_literal_int("0");
        MuleValue result = mule_binary(3, left, right, 0);
        mule_release(left); mule_release(right); return result;
    } else if (!strcmp(scenario, "name_error")) {
        return mule_load_global(globals, "absent_name");
    } else if (!strcmp(scenario, "local_error")) {
        return mule_load_local(NULL, "absent_local");
    } else if (!strcmp(scenario, "not_callable")) {
        MuleValue value = mule_literal_int("1");
        MuleValue result = mule_call(value, 0, NULL);
        mule_release(value); return result;
    } else if (!strcmp(scenario, "release_error")) {
        MuleValue value = mule_namespace();
        MuleValue owned = mule_literal_text("owned child", 11);
        if (!value || !owned || mule_bind(value, "child", owned) < 0) {
            mule_release(value); mule_release(owned); return NULL;
        }
        mule_release(owned);
        mule_error("ValueError", "original failure");
        mule_release(value);
        /* An attempted cleanup error must not replace the pending failure. */
        mule_error("RuntimeError", "cleanup failure");
        return NULL;
    } else if (!strcmp(scenario, "null_without_error")) {
        return NULL;
    } else {
        mule_error("RuntimeError", "unknown probe"); return NULL;
    }
    done:
    return mule_none();
}

int main(int argc, char **argv) {
    if (argc != 2) return 99;
    scenario = argv[1];
    return mule_run((void *)module_entry);
}
"""


@pytest.fixture(scope="module")
def runtime_probe(tmp_path_factory):
    clang = shutil.which("clang")
    assert clang is not None, "native runtime tests require clang"
    directory = tmp_path_factory.mktemp("runtime-probe")
    source = directory / "probe.c"
    executable = directory / "probe"
    source.write_text(PROBE)
    command = [
        clang,
        "-std=c11",
        "-Wall",
        "-Wextra",
        "-Werror",
        "-O3",
        f"-I{RUNTIME.parent}",
        str(source),
        str(RUNTIME),
        str(RUNTIME.with_name("numeric.c")),
        "-lgmp",
        "-lm",
        "-o",
        str(executable),
    ]
    result = subprocess.run(command, check=False, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    return executable


@pytest.mark.parametrize(
    ("scenario", "output"),
    [
        (
            "literals",
            "-123456789012345678901234567890\n1.25\n'snow ☃\\x00end'\nb'a\\x00b'\nNone\nFalse\nTrue\n",
        ),
        ("binary", "9\n5\n14\n3.5\n3\n1\n49\n28\n1\n2\n5\n7\n"),
        ("huge_int", "True\nTrue\n"),
        ("unary", "7\n-7\n-8\nFalse\n"),
        (
            "compare",
            "True\nFalse\nFalse\nTrue\nFalse\nTrue\nFalse\nTrue\nTrue\nFalse\n",
        ),
        ("inplace", "same=1\n[5]\n"),
        ("globals", "42\ncallable=1\nNone\n'__main__'\n"),
        ("function", "42\n"),
        ("function_metadata", "definitions=10000\n'first'\n'second'\n42\n42\n4\n"),
        ("builtin_call", "native callback\n"),
        ("truth", "0 1\n"),
    ],
)
def test_runtime_values_and_direct_native_calls(runtime_probe, scenario, output):
    result = subprocess.run(
        [runtime_probe, scenario], check=False, capture_output=True, text=True
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == output
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("scenario", "error"),
    [
        ("zero_division", "ZeroDivisionError"),
        ("name_error", "NameError: name 'absent_name' is not defined"),
        ("local_error", "UnboundLocalError"),
        ("not_callable", "TypeError"),
        ("arity", "TypeError"),
        ("release_error", "ValueError: original failure"),
        ("null_without_error", "SystemError"),
        ("recursion", "RecursionError"),
    ],
)
def test_runtime_errors_exit_nonzero(runtime_probe, scenario, error):
    result = subprocess.run(
        [runtime_probe, scenario], check=False, capture_output=True, text=True
    )
    assert result.returncode != 0
    assert error in result.stderr
    assert "cleanup failure" not in result.stderr
