/* Mule's object services for LLVM-generated code. No interpreter dependency. */
#define _GNU_SOURCE
#include "runtime.h"
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

static _Thread_local char error_kind[64];
static _Thread_local char error_message[1024];
static _Thread_local unsigned call_depth;
static MuleValue builtins;
static MuleObject none_object = {.references = SIZE_MAX, .kind = MULE_NONE};
static MuleObject false_object = {.references = SIZE_MAX, .kind = MULE_BOOL, .as.boolean = 0};
static MuleObject true_object = {.references = SIZE_MAX, .kind = MULE_BOOL, .as.boolean = 1};

void mule_error(const char *kind, const char *format, ...) {
    if (error_kind[0]) return;
    snprintf(error_kind, sizeof(error_kind), "%s", kind);
    va_list args;
    va_start(args, format);
    vsnprintf(error_message, sizeof(error_message), format, args);
    va_end(args);
}
int mule_has_error(void) { return error_kind[0] != 0; }
void mule_clear_error(void) { error_kind[0] = error_message[0] = 0; }
const char *mule_error_kind(void) { return error_kind; }
const char *mule_error_message(void) { return error_message; }

static char *copy_name(const char *name) {
    size_t length = strlen(name);
    char *copy = malloc(length + 1);
    if (!copy) { mule_error("MemoryError", "allocation failed"); return NULL; }
    memcpy(copy, name, length + 1);
    return copy;
}

MuleObject *mule_allocate(MuleKind kind) {
    MuleObject *object = calloc(1, sizeof(*object));
    if (!object) { mule_error("MemoryError", "allocation failed"); return NULL; }
    object->references = 1;
    object->kind = kind;
    return object;
}
MuleValue mule_retain(MuleValue value) {
    if (value && !mule_is_fixnum(value)) {
        MuleObject *object = value;
        if (object->references != SIZE_MAX) object->references++;
    }
    return value;
}
void mule_release(MuleValue value) {
    if (!value || mule_is_fixnum(value)) return;
    MuleObject *object = value;
    if (object->references == SIZE_MAX || --object->references) return;
    switch (object->kind) {
        case MULE_BIGINT: mpz_clear(object->as.integer); break;
        case MULE_TEXT: case MULE_BYTES: free(object->as.string.data); break;
        case MULE_FUNCTION: free(object->as.function.name); break;
        case MULE_ARGS: case MULE_LIST:
            for (size_t index = 0; index < object->as.args.count; index++) mule_release(object->as.args.values[index]);
            free(object->as.args.values); break;
        case MULE_NAMESPACE: {
            MuleBinding *binding = object->as.namespace_.bindings;
            while (binding) {
                MuleBinding *next = binding->next;
                mule_release(binding->value); free(binding->name); free(binding);
                binding = next;
            }
            break;
        }
        case MULE_TYPE: free(object->as.type.name); mule_release(object->as.type.attributes); break;
        case MULE_INSTANCE: mule_release(object->as.instance.type); break;
        default: break;
    }
    free(object);
}
MuleValue mule_none(void) { return &none_object; }
MuleValue mule_bool(int value) { return value ? &true_object : &false_object; }

static MuleValue string_value(MuleKind kind, const char *data, intptr_t length) {
    if (length < 0 || (uintmax_t)length >= SIZE_MAX) {
        mule_error("OverflowError", "invalid string length"); return NULL;
    }
    MuleObject *object = mule_allocate(kind);
    if (!object) return NULL;
    object->as.string.data = malloc((size_t)length + 1);
    if (!object->as.string.data) {
        mule_error("MemoryError", "allocation failed"); mule_release(object); return NULL;
    }
    object->as.string.length = (size_t)length;
    if (length) memcpy(object->as.string.data, data, (size_t)length);
    object->as.string.data[length] = 0;
    return object;
}
MuleValue mule_literal_text(const char *data, intptr_t length) { return string_value(MULE_TEXT, data, length); }
MuleValue mule_literal_bytes(const char *data, intptr_t length) { return string_value(MULE_BYTES, data, length); }
MuleValue mule_namespace(void) { return mule_allocate(MULE_NAMESPACE); }
MuleValue mule_list(void) { return mule_allocate(MULE_LIST); }

int mule_list_append(MuleValue list, MuleValue value) {
    if (!list || mule_is_fixnum(list) || ((MuleObject *)list)->kind != MULE_LIST) {
        mule_error("TypeError", "list append requires a list"); return -1;
    }
    MuleObject *object = list;
    size_t count = object->as.args.count;
    if (count >= SIZE_MAX / sizeof(MuleValue)) { mule_error("MemoryError", "list too large"); return -1; }
    MuleValue *values = realloc(object->as.args.values, (count + 1) * sizeof(*values));
    if (!values) { mule_error("MemoryError", "allocation failed"); return -1; }
    object->as.args.values = values;
    values[count] = mule_retain(value);
    object->as.args.count++;
    return 0;
}

MuleValue mule_lookup(MuleValue namespace_, const char *name) {
    if (!namespace_ || mule_is_fixnum(namespace_) || ((MuleObject *)namespace_)->kind != MULE_NAMESPACE) {
        mule_error("TypeError", "expected a Mule namespace"); return NULL;
    }
    for (MuleBinding *binding = ((MuleObject *)namespace_)->as.namespace_.bindings; binding; binding = binding->next) {
        if (!strcmp(binding->name, name)) return binding->value;
    }
    return NULL;
}
int mule_bind(MuleValue namespace_, const char *name, MuleValue value) {
    if (!namespace_ || mule_is_fixnum(namespace_) || ((MuleObject *)namespace_)->kind != MULE_NAMESPACE || !value) {
        mule_error("TypeError", "invalid namespace binding"); return -1;
    }
    MuleObject *object = namespace_;
    for (MuleBinding *binding = object->as.namespace_.bindings; binding; binding = binding->next) {
        if (!strcmp(binding->name, name)) {
            MuleValue previous = binding->value;
            binding->value = mule_retain(value);
            mule_release(previous);
            return 0;
        }
    }
    MuleBinding *binding = malloc(sizeof(*binding));
    if (!binding) { mule_error("MemoryError", "allocation failed"); return -1; }
    binding->name = copy_name(name);
    if (!binding->name) { free(binding); return -1; }
    binding->value = mule_retain(value);
    binding->next = object->as.namespace_.bindings;
    object->as.namespace_.bindings = binding;
    return 0;
}
static int initialize_builtins(void);
MuleValue mule_load_global(MuleValue globals, const char *name) {
    MuleValue value = mule_lookup(globals, name);
    if (value) return mule_retain(value);
    if (mule_has_error() || initialize_builtins() < 0) return NULL;
    value = mule_lookup(builtins, name);
    if (value) return mule_retain(value);
    mule_error("NameError", "name '%s' is not defined", name);
    return NULL;
}
int mule_store_global(MuleValue globals, const char *name, MuleValue value) { return mule_bind(globals, name, value); }
MuleValue mule_load_local(MuleValue value, const char *name) {
    if (value) return mule_retain(value);
    mule_error("UnboundLocalError", "cannot access local variable '%s' where it is not associated with a value", name);
    return NULL;
}

MuleValue mule_function(MuleValue globals, void *nativecode, const char *name) {
    MuleObject *function = mule_allocate(MULE_FUNCTION);
    if (!function) return NULL;
    function->as.function.name = copy_name(name);
    if (!function->as.function.name) { mule_release(function); return NULL; }
    function->as.function.entry = (MuleNative)nativecode;
    function->as.function.globals = globals; /* The executing module owns this environment. */
    return function;
}
int mule_check_args(MuleValue args, int count) {
    if (!args || mule_is_fixnum(args) || ((MuleObject *)args)->kind != MULE_ARGS) {
        mule_error("SystemError", "native arguments must be an argument view"); return -1;
    }
    size_t actual = ((MuleObject *)args)->as.args.count;
    if (count < 0 || actual != (size_t)count) {
        mule_error("TypeError", "expected %d positional arguments, got %zu", count, actual); return -1;
    }
    return 0;
}
MuleValue mule_arg(MuleValue args, int index) {
    if (!args || mule_is_fixnum(args) || ((MuleObject *)args)->kind != MULE_ARGS || index < 0 || (size_t)index >= ((MuleObject *)args)->as.args.count) {
        mule_error("IndexError", "native argument index out of range"); return NULL;
    }
    return mule_retain(((MuleObject *)args)->as.args.values[index]);
}
MuleValue mule_call(MuleValue callable, intptr_t argc, MuleValue *argv) {
    if (mule_has_error()) return NULL;
    if (argc < 0 || !callable || mule_is_fixnum(callable)) {
        mule_error("TypeError", "object is not callable"); return NULL;
    }
    MuleObject *function = callable;
    if (function->kind == MULE_TYPE) {
        if (argc) { mule_error("TypeError", "instance constructor takes no arguments"); return NULL; }
        MuleObject *instance = mule_allocate(MULE_INSTANCE);
        if (instance) instance->as.instance.type = mule_retain(callable);
        return instance;
    }
    if (function->kind != MULE_FUNCTION) { mule_error("TypeError", "object is not callable"); return NULL; }
    if (call_depth >= 1000) { mule_error("RecursionError", "maximum native recursion depth exceeded"); return NULL; }
    MuleObject args = {.references = SIZE_MAX, .kind = MULE_ARGS, .as.args = {(size_t)argc, argv}};
    call_depth++;
    MuleValue result = function->as.function.entry(function->as.function.globals, &args);
    call_depth--;
    if (!result && !mule_has_error()) mule_error("SystemError", "native function returned NULL without an error");
    return result;
}
MuleValue mule_special(MuleValue value, const char *name, intptr_t argc, MuleValue *argv) {
    if (!value || mule_is_fixnum(value) || ((MuleObject *)value)->kind != MULE_INSTANCE) return NULL;
    MuleObject *type = ((MuleObject *)value)->as.instance.type;
    MuleValue method = mule_lookup(type->as.type.attributes, name);
    if (!method) return NULL;
    mule_retain(method);
    MuleValue result = mule_call(method, argc, argv);
    mule_release(method);
    return result;
}

int mule_truth(MuleValue value) {
    if (mule_has_error()) return -1;
    if (mule_is_fixnum(value)) return mule_fixnum(value) != 0;
    if (!value) { mule_error("SystemError", "NULL truth operand"); return -1; }
    MuleObject *object = value;
    switch (object->kind) {
        case MULE_NONE: return 0;
        case MULE_BOOL: return object->as.boolean;
        case MULE_BIGINT: return mpz_sgn(object->as.integer) != 0;
        case MULE_FLOAT: return object->as.real != 0.0;
        case MULE_TEXT: case MULE_BYTES: return object->as.string.length != 0;
        case MULE_ARGS: case MULE_LIST: return object->as.args.count != 0;
        case MULE_NAMESPACE: return object->as.namespace_.bindings != NULL;
        case MULE_INSTANCE: {
            MuleValue result = mule_special(value, "__bool__", 0, NULL);
            if (!result) return mule_has_error() ? -1 : 1;
            if (mule_is_fixnum(result) || ((MuleObject *)result)->kind != MULE_BOOL) {
                mule_error("TypeError", "__bool__ should return bool"); mule_release(result); return -1;
            }
            int truth = ((MuleObject *)result)->as.boolean;
            mule_release(result); return truth;
        }
        default: return 1;
    }
}

static int is_string(MuleValue value) {
    return value && !mule_is_fixnum(value) && (((MuleObject *)value)->kind == MULE_TEXT || ((MuleObject *)value)->kind == MULE_BYTES);
}
static MuleValue concatenate(MuleObject *left, MuleObject *right) {
    size_t a = left->as.string.length, b = right->as.string.length;
    if (b > SIZE_MAX - a || a + b > INTPTR_MAX) { mule_error("OverflowError", "string too large"); return NULL; }
    MuleValue result = string_value(left->kind, NULL, 0);
    if (!result) return NULL;
    MuleObject *object = result;
    unsigned char *data = realloc(object->as.string.data, a + b + 1);
    if (!data) { mule_error("MemoryError", "allocation failed"); mule_release(result); return NULL; }
    object->as.string.data = data; object->as.string.length = a + b;
    memcpy(data, left->as.string.data, a); memcpy(data + a, right->as.string.data, b); data[a + b] = 0;
    return result;
}
static MuleValue repeat_string(MuleObject *string, MuleValue count) {
    mpz_t number; mpz_init(number);
    if (mule_integer_export(count, number) < 0) { mpz_clear(number); return NULL; }
    if (mpz_sgn(number) <= 0 || !string->as.string.length) {
        mpz_clear(number); return string_value(string->kind, "", 0);
    }
    if (!mpz_fits_ulong_p(number)) { mpz_clear(number); mule_error("OverflowError", "repeated string too large"); return NULL; }
    unsigned long repetitions = mpz_get_ui(number); mpz_clear(number);
    size_t width = string->as.string.length;
    if (repetitions > (size_t)INTPTR_MAX / width) { mule_error("OverflowError", "repeated string too large"); return NULL; }
    size_t length = width * repetitions;
    unsigned char *data = malloc(length ? length : 1);
    if (!data) { mule_error("MemoryError", "allocation failed"); return NULL; }
    for (size_t offset = 0; offset < length; offset += width) memcpy(data + offset, string->as.string.data, width);
    MuleValue result = string_value(string->kind, (char *)data, (intptr_t)length);
    free(data); return result;
}
MuleValue mule_binary(int op, MuleValue left, MuleValue right, int inplace) {
    if (mule_has_error()) return NULL;
    if (is_string(left) && is_string(right) && op == 0 && ((MuleObject *)left)->kind == ((MuleObject *)right)->kind) return concatenate(left, right);
    if (op == 2 && is_string(left) && mule_is_integer(right)) return repeat_string(left, right);
    if (op == 2 && is_string(right) && mule_is_integer(left)) return repeat_string(right, left);
    if (op == 0 && left && right && !mule_is_fixnum(left) && !mule_is_fixnum(right) && ((MuleObject *)left)->kind == MULE_LIST && ((MuleObject *)right)->kind == MULE_LIST) {
        MuleObject *a = left, *b = right;
        MuleValue result = inplace ? mule_retain(left) : mule_list();
        if (!result) return NULL;
        size_t count = b->as.args.count; /* self concatenation must use the original size. */
        if (!inplace) for (size_t index = 0; index < a->as.args.count; index++) {
            if (mule_list_append(result, a->as.args.values[index]) < 0) { mule_release(result); return NULL; }
        }
        for (size_t index = 0; index < count; index++) {
            MuleValue value = b->as.args.values[index];
            if (mule_list_append(result, value) < 0) { mule_release(result); return NULL; }
        }
        return result;
    }
    return mule_numeric_binary(op, left, right);
}
MuleValue mule_unary(int op, MuleValue value) {
    if (op == 3) { int truth = mule_truth(value); return truth < 0 ? NULL : mule_bool(!truth); }
    return mule_numeric_unary(op, value);
}

static int compare_strings(MuleObject *left, MuleObject *right) {
    size_t a = left->as.string.length, b = right->as.string.length;
    int result = memcmp(left->as.string.data, right->as.string.data, a < b ? a : b);
    return result ? (result < 0 ? -1 : 1) : (a > b) - (a < b);
}
static int contains(MuleValue container, MuleValue item) {
    if (container && !mule_is_fixnum(container)) {
        MuleObject *object = container;
        if (is_string(container)) {
            if (object->kind == MULE_BYTES && mule_is_integer(item)) {
                mpz_t integer; mpz_init(integer);
                if (mule_integer_export(item, integer) < 0) { mpz_clear(integer); return -1; }
                if (mpz_sgn(integer) < 0 || mpz_cmp_ui(integer, 255) > 0) {
                    mpz_clear(integer); mule_error("ValueError", "byte must be in range(0, 256)"); return -1;
                }
                int byte = (int)mpz_get_ui(integer); mpz_clear(integer);
                return memchr(object->as.string.data, byte, object->as.string.length) != NULL;
            }
            if (!is_string(item) || ((MuleObject *)item)->kind != object->kind) {
                mule_error("TypeError", "incompatible string containment operand"); return -1;
            }
            MuleObject *needle = item;
            size_t width = needle->as.string.length, length = object->as.string.length;
            if (width > length) return 0;
            for (size_t offset = 0; offset <= length - width; offset++) {
                if (!memcmp(object->as.string.data + offset, needle->as.string.data, width)) return 1;
            }
            return 0;
        }
        if (object->kind == MULE_ARGS || object->kind == MULE_LIST) {
            for (size_t index = 0; index < object->as.args.count; index++) {
                if (object->as.args.values[index] == item) return 1;
                MuleValue equal = mule_compare(0, item, object->as.args.values[index]);
                if (!equal) return -1;
                int truth = mule_truth(equal); mule_release(equal);
                if (truth != 0) return truth;
            }
            return 0;
        }
    }
    mule_error("TypeError", "object does not support containment"); return -1;
}
MuleValue mule_compare(int op, MuleValue left, MuleValue right) {
    if (mule_has_error()) return NULL;
    if (op == 6 || op == 7) return mule_bool(op == 6 ? left == right : left != right);
    if (op == 8 || op == 9) { int found = contains(right, left); return found < 0 ? NULL : mule_bool(op == 8 ? found : !found); }
    if (op < 0 || op > 5) { mule_error("SystemError", "unknown comparison operation"); return NULL; }
    static const char *methods[] = {"__eq__", "__ne__", "__lt__", "__le__", "__gt__", "__ge__"};
    MuleValue args[] = {right};
    MuleValue special = mule_special(left, methods[op], 1, args);
    if (special || mule_has_error()) return special;
    int ordering;
    if (mule_is_integer(left) || (left && !mule_is_fixnum(left) && ((MuleObject *)left)->kind == MULE_FLOAT)) {
        if (mule_is_integer(right) || (right && !mule_is_fixnum(right) && ((MuleObject *)right)->kind == MULE_FLOAT)) {
            if (mule_numeric_compare(left, right, &ordering) < 0) return NULL;
        } else if (op < 2) return mule_bool(op == 1);
        else { mule_error("TypeError", "incompatible comparison operands"); return NULL; }
    } else if (is_string(left) && is_string(right) && ((MuleObject *)left)->kind == ((MuleObject *)right)->kind) {
        ordering = compare_strings(left, right);
    } else if (op < 2) return mule_bool(op == 0 ? left == right : left != right);
    else { mule_error("TypeError", "incompatible comparison operands"); return NULL; }
    if (ordering == 2) return mule_bool(op == 1); /* IEEE unordered NaN. */
    switch (op) {
        case 0: return mule_bool(ordering == 0);
        case 1: return mule_bool(ordering != 0);
        case 2: return mule_bool(ordering < 0);
        case 3: return mule_bool(ordering <= 0);
        case 4: return mule_bool(ordering > 0);
        default: return mule_bool(ordering >= 0);
    }
}

static uint32_t decode_utf8(const unsigned char *data, size_t length, size_t *offset) {
    size_t index = (*offset)++;
    unsigned byte = data[index];
    if (byte < 0x80) return byte;
    unsigned count;
    uint32_t value;
    if (byte >= 0xc2 && byte <= 0xdf) { count = 1; value = byte & 31; }
    else if (byte >= 0xe0 && byte <= 0xef) { count = 2; value = byte & 15; }
    else if (byte >= 0xf0 && byte <= 0xf4) { count = 3; value = byte & 7; }
    else { mule_error("UnicodeDecodeError", "invalid UTF-8 text"); return 0; }
    if (length - *offset < count) { mule_error("UnicodeDecodeError", "incomplete UTF-8 text"); return 0; }
    for (unsigned part = 0; part < count; part++) {
        byte = data[(*offset)++];
        if ((byte & 0xc0) != 0x80) { mule_error("UnicodeDecodeError", "invalid UTF-8 text"); return 0; }
        value = (value << 6) | (byte & 63);
    }
    if ((count == 1 && value < 0x80) || (count == 2 && value < 0x800) || (count == 3 && value < 0x10000) || value > 0x10ffff) {
        mule_error("UnicodeDecodeError", "invalid UTF-8 text"); return 0;
    }
    return value; /* Surrogates are retained for escaped source literals. */
}
static int escaped_string(FILE *stream, MuleObject *object, int ascii_only) {
    const unsigned char *data = object->as.string.data;
    size_t length = object->as.string.length;
    int quote = '\'';
    if (memchr(data, '\'', length) && !memchr(data, '"', length)) quote = '"';
    if (object->kind == MULE_BYTES) fputc('b', stream);
    fputc(quote, stream);
    for (size_t offset = 0; offset < length;) {
        size_t start = offset;
        uint32_t code = object->kind == MULE_BYTES ? data[offset++] : decode_utf8(data, length, &offset);
        if (mule_has_error()) return -1;
        if (code == (unsigned)quote || code == '\\') { fputc('\\', stream); fputc((int)code, stream); }
        else if (code == '\n') fputs("\\n", stream);
        else if (code == '\r') fputs("\\r", stream);
        else if (code == '\t') fputs("\\t", stream);
        else if (code < 32 || code == 127 || (code >= 128 && code <= 160) ||
                 (object->kind == MULE_BYTES && code >= 128) ||
                 (ascii_only && code >= 128) || (code >= 0xd800 && code <= 0xdfff) ||
                 code == 0x2028 || code == 0x2029 || code == 0xfeff || (code & 0xffff) >= 0xfffe) {
            if (code <= 255) fprintf(stream, "\\x%02x", code);
            else if (code <= 65535) fprintf(stream, "\\u%04x", code);
            else fprintf(stream, "\\U%08x", code);
        } else fwrite(data + start, 1, offset - start, stream);
    }
    fputc(quote, stream);
    return ferror(stream) ? -1 : 0;
}
static void format_float(FILE *stream, double value) {
    if (isnan(value)) { fputs("nan", stream); return; }
    if (isinf(value)) { fputs(signbit(value) ? "-inf" : "inf", stream); return; }
    char buffer[128];
    for (int precision = 1; precision <= 17; precision++) {
        snprintf(buffer, sizeof(buffer), "%.*g", precision, value);
        double parsed = strtod(buffer, NULL);
        if (parsed == value && signbit(parsed) == signbit(value)) break;
    }
    char *exponent_text = strchr(buffer, 'e');
    if (!exponent_text) {
        fputs(buffer, stream);
        if (!strchr(buffer, '.')) fputs(".0", stream);
        return;
    }
    int exponent = atoi(exponent_text + 1);
    if (exponent < -4 || exponent >= 16) { fputs(buffer, stream); return; }
    char digits[32]; size_t count = 0;
    const char *position = buffer;
    if (*position == '-') { fputc('-', stream); position++; }
    while (position < exponent_text) {
        if (*position != '.') digits[count++] = *position;
        position++;
    }
    int point = exponent + 1;
    if (point <= 0) {
        fputs("0.", stream);
        for (int index = 0; index < -point; index++) fputc('0', stream);
        fwrite(digits, 1, count, stream);
    } else {
        for (int index = 0; index < point; index++) fputc((size_t)index < count ? digits[index] : '0', stream);
        fputc('.', stream);
        if ((size_t)point < count) fwrite(digits + point, 1, count - (size_t)point, stream);
        else fputc('0', stream);
    }
}
int mule_format(FILE *stream, MuleValue value, int representation, int ascii_only) {
    if (!value) { mule_error("SystemError", "NULL formatting operand"); return -1; }
    if (mule_is_fixnum(value)) fprintf(stream, "%lld", (long long)mule_fixnum(value));
    else {
        MuleObject *object = value;
        switch (object->kind) {
            case MULE_NONE: fputs("None", stream); break;
            case MULE_BOOL: fputs(object->as.boolean ? "True" : "False", stream); break;
            case MULE_BIGINT: {
                char *decimal = mpz_get_str(NULL, 10, object->as.integer);
                if (!decimal) { mule_error("MemoryError", "integer formatting failed"); return -1; }
                fputs(decimal, stream); free(decimal); break;
            }
            case MULE_FLOAT: format_float(stream, object->as.real); break;
            case MULE_TEXT:
                if (!representation) {
                    for (size_t offset = 0; offset < object->as.string.length;) {
                        uint32_t code = decode_utf8(object->as.string.data, object->as.string.length, &offset);
                        if (mule_has_error()) return -1;
                        if (code >= 0xd800 && code <= 0xdfff) { mule_error("UnicodeEncodeError", "cannot encode surrogate to UTF-8"); return -1; }
                    }
                    fwrite(object->as.string.data, 1, object->as.string.length, stream); break;
                }
                /* fall through */
            case MULE_BYTES: if (escaped_string(stream, object, ascii_only) < 0) return -1; break;
            case MULE_ARGS: case MULE_LIST:
                fputc(object->kind == MULE_LIST ? '[' : '(', stream);
                for (size_t index = 0; index < object->as.args.count; index++) {
                    if (index) fputs(", ", stream);
                    if (mule_format(stream, object->as.args.values[index], 1, ascii_only) < 0) return -1;
                }
                if (object->kind == MULE_ARGS && object->as.args.count == 1) fputc(',', stream);
                fputc(object->kind == MULE_LIST ? ']' : ')', stream); break;
            case MULE_NAMESPACE: {
                fputc('{', stream); int first = 1;
                for (MuleBinding *binding = object->as.namespace_.bindings; binding; binding = binding->next) {
                    if (!first) fputs(", ", stream);
                    first = 0;
                    MuleValue name = mule_literal_text(binding->name, (intptr_t)strlen(binding->name));
                    if (!name) return -1;
                    int status = mule_format(stream, name, 1, ascii_only); mule_release(name);
                    if (status < 0) return -1;
                    fputs(": ", stream);
                    if (mule_format(stream, binding->value, 1, ascii_only) < 0) return -1;
                }
                fputc('}', stream); break;
            }
            case MULE_FUNCTION: fprintf(stream, "<native function %s>", object->as.function.name); break;
            case MULE_TYPE: fprintf(stream, "<class '%s'>", object->as.type.name); break;
            case MULE_INSTANCE: fprintf(stream, "<%s object>", ((MuleObject *)object->as.instance.type)->as.type.name); break;
        }
    }
    if (ferror(stream)) { mule_error("OSError", "output failed"); return -1; }
    return 0;
}

static MuleObject *arguments(MuleValue args) { return args; }
static int arity(MuleValue args, size_t minimum, size_t maximum) {
    size_t count = arguments(args)->as.args.count;
    if (count < minimum || count > maximum) { mule_error("TypeError", "invalid argument count"); return -1; }
    return 0;
}
static MuleValue argument(MuleValue args, size_t index) { return arguments(args)->as.args.values[index]; }
static MuleValue formatted_value(MuleValue value, int representation, int ascii_only) {
    char *data = NULL; size_t length = 0;
    FILE *stream = open_memstream(&data, &length);
    if (!stream) { mule_error("MemoryError", "format buffer allocation failed"); return NULL; }
    int status = mule_format(stream, value, representation, ascii_only);
    if (fclose(stream) < 0) { mule_error("OSError", "formatting failed"); status = -1; }
    MuleValue result = status < 0 ? NULL : mule_literal_text(data, (intptr_t)length);
    free(data); return result;
}
static MuleValue builtin_print(MuleValue globals, MuleValue args) {
    (void)globals;
    for (size_t index = 0; index < arguments(args)->as.args.count; index++) {
        if (index) fputc(' ', stdout);
        if (mule_format(stdout, argument(args, index), 0, 0) < 0) return NULL;
    }
    if (fputc('\n', stdout) == EOF) { mule_error("OSError", "output failed"); return NULL; }
    return mule_none();
}
static MuleValue builtin_abs(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 1, 1) < 0) return NULL;
    MuleValue value = argument(args, 0);
    if (mule_is_integer(value)) {
        mpz_t integer; mpz_init(integer);
        if (mule_integer_export(value, integer) < 0) { mpz_clear(integer); return NULL; }
        mpz_abs(integer, integer); MuleValue result = mule_integer_import(integer); mpz_clear(integer); return result;
    }
    if (value && !mule_is_fixnum(value) && ((MuleObject *)value)->kind == MULE_FLOAT) return mule_literal_float(fabs(((MuleObject *)value)->as.real));
    mule_error("TypeError", "bad operand type for abs"); return NULL;
}
static MuleValue builtin_bool(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 0, 1) < 0) return NULL;
    if (!arguments(args)->as.args.count) return mule_bool(0);
    int truth = mule_truth(argument(args, 0)); return truth < 0 ? NULL : mule_bool(truth);
}
static MuleValue builtin_len(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 1, 1) < 0) return NULL;
    MuleValue value = argument(args, 0);
    if (!value || mule_is_fixnum(value)) { mule_error("TypeError", "object has no len"); return NULL; }
    MuleObject *object = value; size_t length = 0;
    switch (object->kind) {
        case MULE_BYTES: length = object->as.string.length; break;
        case MULE_TEXT:
            for (size_t offset = 0; offset < object->as.string.length; length++) {
                decode_utf8(object->as.string.data, object->as.string.length, &offset);
                if (mule_has_error()) return NULL;
            }
            break;
        case MULE_ARGS: case MULE_LIST: length = object->as.args.count; break;
        case MULE_NAMESPACE:
            for (MuleBinding *binding = object->as.namespace_.bindings; binding; binding = binding->next) length++;
            break;
        default: mule_error("TypeError", "object has no len"); return NULL;
    }
    mpz_t integer; mpz_init_set_ui(integer, length);
    MuleValue result = mule_integer_import(integer); mpz_clear(integer); return result;
}
static MuleValue builtin_repr(MuleValue globals, MuleValue args) {
    (void)globals; return arity(args, 1, 1) < 0 ? NULL : formatted_value(argument(args, 0), 1, 0);
}
static MuleValue builtin_ascii(MuleValue globals, MuleValue args) {
    (void)globals; return arity(args, 1, 1) < 0 ? NULL : formatted_value(argument(args, 0), 1, 1);
}
static MuleValue builtin_str(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 0, 1) < 0) return NULL;
    if (!arguments(args)->as.args.count) return mule_literal_text("", 0);
    MuleValue value = argument(args, 0);
    if (is_string(value) && ((MuleObject *)value)->kind == MULE_TEXT) return mule_retain(value);
    return formatted_value(value, 0, 0);
}
static MuleValue builtin_input(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 0, 1) < 0) return NULL;
    if (arguments(args)->as.args.count && mule_format(stdout, argument(args, 0), 0, 0) < 0) return NULL;
    if (fflush(stdout) < 0) { mule_error("OSError", "output failed"); return NULL; }
    char *line = NULL; size_t capacity = 0;
    ssize_t length = getline(&line, &capacity, stdin);
    if (length < 0) {
        free(line); mule_error(feof(stdin) ? "EOFError" : "OSError", "could not read input"); return NULL;
    }
    if (length && line[length - 1] == '\n') { length--; if (length && line[length - 1] == '\r') length--; }
    MuleValue result = mule_literal_text(line, length); free(line); return result;
}

static MuleValue parse_integer(MuleObject *string, int base) {
    const unsigned char *data = string->as.string.data;
    size_t start = 0, end = string->as.string.length;
    while (start < end && isspace(data[start])) start++;
    while (end > start && isspace(data[end - 1])) end--;
    int negative = 0;
    if (start < end && (data[start] == '+' || data[start] == '-')) { negative = data[start] == '-'; start++; }
    int prefixed = 0;
    if (end - start >= 2 && data[start] == '0') {
        unsigned marker = (unsigned)tolower(data[start + 1]);
        int prefix = marker == 'x' ? 16 : marker == 'o' ? 8 : marker == 'b' ? 2 : 0;
        if (prefix && (base == 0 || base == prefix)) { base = prefix; start += 2; prefixed = 1; }
    }
    int autodetect = base == 0;
    if (!base) base = 10;
    if (start == end || base < 2 || base > 36) { mule_error("ValueError", "invalid literal for int"); return NULL; }
    char *digits = malloc(end - start + 1);
    if (!digits) { mule_error("MemoryError", "allocation failed"); return NULL; }
    size_t length = 0;
    int previous_digit = 0, nonzero = 0;
    for (size_t index = start; index < end; index++) {
        unsigned byte = data[index];
        if (byte == '_') {
            if ((!previous_digit && !(prefixed && index == start)) || index + 1 == end || data[index + 1] == '_') goto invalid;
            previous_digit = 0; continue;
        }
        int digit = byte >= '0' && byte <= '9' ? (int)byte - '0' :
                    byte >= 'a' && byte <= 'z' ? (int)byte - 'a' + 10 :
                    byte >= 'A' && byte <= 'Z' ? (int)byte - 'A' + 10 : -1;
        if (digit < 0 || digit >= base) goto invalid;
        digits[length++] = (char)byte;
        previous_digit = 1; nonzero |= digit != 0;
    }
    if (!length || (autodetect && data[start] == '0' && nonzero)) goto invalid;
    digits[length] = 0;
    mpz_t integer; mpz_init(integer);
    if (mpz_set_str(integer, digits, base) < 0) { mpz_clear(integer); goto invalid; }
    if (negative) mpz_neg(integer, integer);
    MuleValue result = mule_integer_import(integer);
    mpz_clear(integer); free(digits); return result;
    invalid:
    free(digits); mule_error("ValueError", "invalid literal for int"); return NULL;
}
static MuleValue builtin_int(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 0, 2) < 0) return NULL;
    size_t count = arguments(args)->as.args.count;
    if (!count) return mule_small(0);
    MuleValue value = argument(args, 0);
    int base = 10;
    if (count == 2) {
        mpz_t integer; mpz_init(integer);
        if (mule_integer_export(argument(args, 1), integer) < 0) { mpz_clear(integer); return NULL; }
        if (!mpz_fits_slong_p(integer)) { mpz_clear(integer); mule_error("ValueError", "int base must be >= 2 and <= 36, or 0"); return NULL; }
        long number = mpz_get_si(integer); mpz_clear(integer);
        if (number != 0 && (number < 2 || number > 36)) { mule_error("ValueError", "int base must be >= 2 and <= 36, or 0"); return NULL; }
        base = (int)number;
    }
    if (is_string(value)) return parse_integer(value, base);
    if (count == 2) { mule_error("TypeError", "int with explicit base requires a string"); return NULL; }
    if (mule_is_integer(value)) {
        mpz_t integer; mpz_init(integer);
        if (mule_integer_export(value, integer) < 0) { mpz_clear(integer); return NULL; }
        MuleValue result = mule_integer_import(integer); mpz_clear(integer); return result;
    }
    if (value && !mule_is_fixnum(value) && ((MuleObject *)value)->kind == MULE_FLOAT) {
        double number = ((MuleObject *)value)->as.real;
        if (!isfinite(number)) { mule_error(isnan(number) ? "ValueError" : "OverflowError", "cannot convert nonfinite float to integer"); return NULL; }
        mpz_t integer; mpz_init_set_d(integer, number);
        MuleValue result = mule_integer_import(integer); mpz_clear(integer); return result;
    }
    mule_error("TypeError", "int requires a number or string"); return NULL;
}
static MuleValue builtin_float(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 0, 1) < 0) return NULL;
    if (!arguments(args)->as.args.count) return mule_literal_float(0.0);
    MuleValue value = argument(args, 0);
    if (is_string(value)) {
        MuleObject *string = value;
        const unsigned char *data = string->as.string.data;
        size_t length = string->as.string.length;
        char *text = malloc(length + 1);
        if (!text) { mule_error("MemoryError", "allocation failed"); return NULL; }
        size_t used = 0;
        for (size_t index = 0; index < length; index++) {
            if (data[index] == '_') {
                if (!index || index + 1 == length || !isdigit(data[index - 1]) || !isdigit(data[index + 1])) {
                    free(text); mule_error("ValueError", "could not convert string to float"); return NULL;
                }
            } else text[used++] = (char)data[index];
        }
        text[used] = 0;
        char *end;
        double number = strtod(text, &end);
        int consumed = end != text;
        while (*end && isspace((unsigned char)*end)) end++;
        int valid = consumed && (size_t)(end - text) == used;
        free(text);
        if (!valid) { mule_error("ValueError", "could not convert string to float"); return NULL; }
        return mule_literal_float(number);
    }
    double number;
    return mule_number_double(value, &number) < 0 ? NULL : mule_literal_float(number);
}
static MuleValue builtin_tuple(MuleValue globals, MuleValue args) {
    (void)globals; return arity(args, 0, 0) < 0 ? NULL : mule_allocate(MULE_ARGS);
}
static MuleValue builtin_dict(MuleValue globals, MuleValue args) {
    (void)globals; return arity(args, 0, 0) < 0 ? NULL : mule_namespace();
}
static MuleValue builtin_list(MuleValue globals, MuleValue args) {
    (void)globals; return arity(args, 0, 0) < 0 ? NULL : mule_list();
}
static MuleValue builtin_staticmethod(MuleValue globals, MuleValue args) {
    (void)globals; return arity(args, 1, 1) < 0 ? NULL : mule_retain(argument(args, 0));
}
static MuleValue builtin_type(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 3, 3) < 0) return NULL;
    MuleValue name = argument(args, 0), bases = argument(args, 1), attributes = argument(args, 2);
    if (!is_string(name) || ((MuleObject *)name)->kind != MULE_TEXT ||
        !bases || mule_is_fixnum(bases) || ((MuleObject *)bases)->kind != MULE_ARGS || ((MuleObject *)bases)->as.args.count ||
        !attributes || mule_is_fixnum(attributes) || ((MuleObject *)attributes)->kind != MULE_NAMESPACE) {
        mule_error("TypeError", "type requires a name, empty bases, and namespace"); return NULL;
    }
    MuleObject *type = mule_allocate(MULE_TYPE);
    if (!type) return NULL;
    MuleObject *text = name;
    if (memchr(text->as.string.data, 0, text->as.string.length)) {
        mule_error("ValueError", "type name contains NUL"); mule_release(type); return NULL;
    }
    type->as.type.name = copy_name((const char *)text->as.string.data);
    if (!type->as.type.name) { mule_release(type); return NULL; }
    type->as.type.attributes = mule_retain(attributes);
    return type;
}
static MuleValue builtin_setattr(MuleValue globals, MuleValue args) {
    (void)globals;
    if (arity(args, 3, 3) < 0) return NULL;
    MuleValue receiver = argument(args, 0), name = argument(args, 1);
    if (!receiver || mule_is_fixnum(receiver) || ((MuleObject *)receiver)->kind != MULE_TYPE ||
        !is_string(name) || ((MuleObject *)name)->kind != MULE_TEXT) {
        mule_error("TypeError", "setattr requires a type and text attribute name"); return NULL;
    }
    MuleObject *text = name;
    if (memchr(text->as.string.data, 0, text->as.string.length)) {
        mule_error("ValueError", "attribute name contains NUL"); return NULL;
    }
    MuleObject *type = receiver;
    if (mule_bind(type->as.type.attributes, (const char *)text->as.string.data, argument(args, 2)) < 0) return NULL;
    return mule_none();
}

static int initialize_builtins(void) {
    if (builtins) return 0;
    builtins = mule_namespace();
    if (!builtins) return -1;
    static const struct { const char *name; MuleNative entry; } entries[] = {
        {"print", builtin_print}, {"abs", builtin_abs}, {"int", builtin_int},
        {"input", builtin_input}, {"len", builtin_len}, {"bool", builtin_bool},
        {"ascii", builtin_ascii}, {"repr", builtin_repr}, {"str", builtin_str},
        {"float", builtin_float}, {"tuple", builtin_tuple}, {"dict", builtin_dict},
        {"list", builtin_list}, {"type", builtin_type}, {"setattr", builtin_setattr},
        {"staticmethod", builtin_staticmethod}
    };
    for (size_t index = 0; index < sizeof(entries) / sizeof(*entries); index++) {
        MuleValue function = mule_function(NULL, (void *)entries[index].entry, entries[index].name);
        if (!function) goto failure;
        int status = mule_bind(builtins, entries[index].name, function);
        mule_release(function);
        if (status < 0) goto failure;
    }
    return 0;
    failure:
    mule_release(builtins); builtins = NULL; return -1;
}
int mule_run(void *native_module_init) {
    mule_clear_error(); call_depth = 0;
    MuleValue globals = mule_namespace();
    MuleValue name = mule_literal_text("__main__", 8);
    MuleValue result = NULL;
    if (globals && name && mule_bind(globals, "__name__", name) == 0 && initialize_builtins() == 0 && mule_bind(globals, "__builtins__", builtins) == 0) {
        MuleObject args = {.references = SIZE_MAX, .kind = MULE_ARGS, .as.args = {0, NULL}};
        result = ((MuleNative)native_module_init)(globals, &args);
        if (!result && !mule_has_error()) mule_error("SystemError", "native module returned NULL without an error");
    }
    mule_release(result); mule_release(name); mule_release(globals);
    mule_release(builtins); builtins = NULL;
    if (fflush(stdout) < 0) mule_error("OSError", "output failed");
    if (mule_has_error()) { fprintf(stderr, "%s: %s\n", error_kind, error_message); return 1; }
    return 0;
}
