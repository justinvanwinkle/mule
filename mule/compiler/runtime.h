#ifndef MULE_RUNTIME_H
#define MULE_RUNTIME_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <gmp.h>

typedef void *MuleValue;
typedef MuleValue (*MuleNative)(MuleValue globals, MuleValue args);
typedef enum {
    MULE_NONE, MULE_BOOL, MULE_BIGINT, MULE_FLOAT, MULE_TEXT, MULE_BYTES,
    MULE_FUNCTION, MULE_ARGS, MULE_LIST, MULE_NAMESPACE, MULE_TYPE, MULE_INSTANCE
} MuleKind;

typedef struct MuleObject MuleObject;
typedef struct MuleBinding MuleBinding;
struct MuleBinding { char *name; MuleValue value; MuleBinding *next; };
struct MuleObject {
    size_t references;
    MuleKind kind;
    union {
        int boolean;
        mpz_t integer;
        double real;
        struct { size_t length; unsigned char *data; } string;
        struct { MuleNative entry; MuleValue globals; char *name; } function;
        struct { size_t count; MuleValue *values; } args;
        struct { MuleBinding *bindings; } namespace_;
        struct { char *name; MuleValue attributes; } type;
        struct { MuleValue type; } instance;
    } as;
};

static inline int mule_is_fixnum(MuleValue value) { return ((uintptr_t)value & 1) != 0; }
static inline int64_t mule_fixnum(MuleValue value) { return (int64_t)(intptr_t)value >> 1; }
static inline MuleValue mule_small(int64_t value) {
    return (MuleValue)(uintptr_t)(((uint64_t)value << 1) | 1);
}
#define MULE_FIXNUM_MIN (-(INT64_C(1) << 62))
#define MULE_FIXNUM_MAX ((INT64_C(1) << 62) - 1)

MuleObject *mule_allocate(MuleKind kind);
MuleValue mule_retain(MuleValue value);
void mule_release(MuleValue value);
void mule_error(const char *kind, const char *format, ...);
int mule_has_error(void);
void mule_clear_error(void);
const char *mule_error_kind(void);
const char *mule_error_message(void);

int mule_is_integer(MuleValue value);
int mule_integer_export(MuleValue value, mpz_t result);
MuleValue mule_integer_import(const mpz_t value);
int mule_number_double(MuleValue value, double *result);
MuleValue mule_literal_int(const char *decimal);
MuleValue mule_literal_float(double value);
MuleValue mule_literal_text(const char *data, intptr_t length);
MuleValue mule_literal_bytes(const char *data, intptr_t length);
MuleValue mule_none(void);
MuleValue mule_bool(int value);
MuleValue mule_load_global(MuleValue globals, const char *name);
int mule_store_global(MuleValue globals, const char *name, MuleValue value);
MuleValue mule_load_local(MuleValue value, const char *name);
MuleValue mule_binary(int op, MuleValue left, MuleValue right, int inplace);
MuleValue mule_unary(int op, MuleValue value);
MuleValue mule_numeric_binary(int op, MuleValue left, MuleValue right);
MuleValue mule_numeric_unary(int op, MuleValue value);
int mule_numeric_compare(MuleValue left, MuleValue right, int *ordering);
MuleValue mule_compare(int op, MuleValue left, MuleValue right);
int mule_truth(MuleValue value);
MuleValue mule_call(MuleValue callable, intptr_t argc, MuleValue *argv);
MuleValue mule_function(MuleValue globals, void *nativecode, const char *name);
int mule_check_args(MuleValue args, int count);
MuleValue mule_arg(MuleValue args, int index);
int mule_run(void *native_module_init);
MuleValue mule_namespace(void);
MuleValue mule_list(void);
int mule_list_append(MuleValue list, MuleValue value);
MuleValue mule_lookup(MuleValue namespace_, const char *name);
int mule_bind(MuleValue namespace_, const char *name, MuleValue value);
MuleValue mule_special(MuleValue value, const char *name, intptr_t argc, MuleValue *argv);
int mule_format(FILE *stream, MuleValue value, int representation, int ascii_only);

#endif
