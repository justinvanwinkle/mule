/* Native numeric services: tagged signed integers, GMP integers, and doubles. */
#include "runtime.h"

#include <limits.h>
#include <math.h>

static int integer_quotient_double(const mpz_t numerator, const mpz_t denominator,
                                    double *result);

static int is_float(MuleValue value) {
    return value && !mule_is_fixnum(value) && ((MuleObject *)value)->kind == MULE_FLOAT;
}

int mule_is_integer(MuleValue value) {
    if (!value) return 0;
    if (mule_is_fixnum(value)) return 1;
    MuleKind kind = ((MuleObject *)value)->kind;
    return kind == MULE_BIGINT || kind == MULE_BOOL;
}

static int small_integer(MuleValue value, int64_t *result) {
    if (mule_is_fixnum(value)) {
        *result = mule_fixnum(value);
        return 1;
    }
    if (value && ((MuleObject *)value)->kind == MULE_BOOL) {
        *result = ((MuleObject *)value)->as.boolean != 0;
        return 1;
    }
    return 0;
}

int mule_integer_export(MuleValue value, mpz_t result) {
    int64_t small;
    if (small_integer(value, &small)) {
        mpz_set_si(result, (long)small);
        return 0;
    }
    if (value && ((MuleObject *)value)->kind == MULE_BIGINT) {
        mpz_set(result, ((MuleObject *)value)->as.integer);
        return 0;
    }
    mule_error("TypeError", "an integer is required");
    return -1;
}

MuleValue mule_integer_import(const mpz_t value) {
    if (mpz_fits_slong_p(value)) {
        long small = mpz_get_si(value);
        if (small >= MULE_FIXNUM_MIN && small <= MULE_FIXNUM_MAX) {
            return mule_small(small);
        }
    }
    MuleObject *object = mule_allocate(MULE_BIGINT);
    if (!object) return NULL;
    mpz_init_set(object->as.integer, value);
    return object;
}

MuleValue mule_literal_int(const char *decimal) {
    const char *digits = decimal;
    int negative = *digits == '-';
    if (*digits == '-' || *digits == '+') digits++;
    if (!*digits) {
        mule_error("ValueError", "invalid integer literal");
        return NULL;
    }
    uint64_t magnitude = 0;
    uint64_t limit = negative ? (UINT64_C(1) << 62) : MULE_FIXNUM_MAX;
    int overflow = 0;
    for (const char *cursor = digits; *cursor; cursor++) {
        if (*cursor < '0' || *cursor > '9') {
            mule_error("ValueError", "invalid integer literal");
            return NULL;
        }
        unsigned digit = (unsigned)(*cursor - '0');
        if (!overflow) {
            if (magnitude > (limit - digit) / 10) overflow = 1;
            else magnitude = magnitude * 10 + digit;
        }
    }
    if (!overflow) return mule_small(negative ? -(int64_t)magnitude : (int64_t)magnitude);
    mpz_t value;
    mpz_init(value);
    if (mpz_set_str(value, digits, 10)) {
        mpz_clear(value);
        mule_error("ValueError", "invalid integer literal");
        return NULL;
    }
    if (negative) mpz_neg(value, value);
    MuleValue result = mule_integer_import(value);
    mpz_clear(value);
    return result;
}

MuleValue mule_literal_float(double value) {
    MuleObject *object = mule_allocate(MULE_FLOAT);
    if (!object) return NULL;
    object->as.real = value;
    return object;
}

int mule_number_double(MuleValue value, double *result) {
    int64_t small;
    if (small_integer(value, &small)) {
        *result = (double)small;
        return 0;
    }
    if (is_float(value)) {
        *result = ((MuleObject *)value)->as.real;
        return 0;
    }
    if (value && ((MuleObject *)value)->kind == MULE_BIGINT) {
        /* mpz_get_d truncates. Reuse correctly-rounded integer division. */
        mpz_t unit;
        mpz_init_set_ui(unit, 1);
        int status = integer_quotient_double(((MuleObject *)value)->as.integer,
                                            unit, result);
        mpz_clear(unit);
        return status;
    }
    mule_error("TypeError", "a real number is required");
    return -1;
}

/* Round an exact integer quotient to binary64, including subnormal ties. */
static int integer_quotient_double(const mpz_t numerator, const mpz_t denominator,
                                    double *result) {
    int negative = (mpz_sgn(numerator) < 0) != (mpz_sgn(denominator) < 0);
    if (!mpz_sgn(denominator)) {
        mule_error("ZeroDivisionError", "division by zero");
        return -1;
    }
    if (!mpz_sgn(numerator)) {
        *result = negative ? -0.0 : 0.0;
        return 0;
    }
    mpz_t a, b, scaled, quotient, remainder;
    mpz_inits(a, b, scaled, quotient, remainder, NULL);
    mpz_abs(a, numerator);
    mpz_abs(b, denominator);
    size_t abits = mpz_sizeinbase(a, 2), bbits = mpz_sizeinbase(b, 2);
    if (abits > bbits && abits - bbits > 1024) goto overflow;
    if (bbits > abits && bbits - abits > 1076) {
        *result = negative ? -0.0 : 0.0;
        goto success;
    }
    long exponent = abits >= bbits ? (long)(abits - bbits) : -(long)(bbits - abits);
    if (exponent >= 0) {
        mpz_mul_2exp(scaled, b, (mp_bitcnt_t)exponent);
        if (mpz_cmp(a, scaled) < 0) exponent--;
    } else {
        mpz_mul_2exp(scaled, a, (mp_bitcnt_t)-exponent);
        if (mpz_cmp(scaled, b) < 0) exponent--;
    }
    if (exponent > 1023) goto overflow;
    long scale = exponent >= -1022 ? 52 - exponent : 1074;
    if (scale >= 0) mpz_mul_2exp(a, a, (mp_bitcnt_t)scale);
    else mpz_mul_2exp(b, b, (mp_bitcnt_t)-scale);
    mpz_fdiv_qr(quotient, remainder, a, b);
    mpz_mul_2exp(remainder, remainder, 1);
    int halfway = mpz_cmp(remainder, b);
    if (halfway > 0 || (halfway == 0 && mpz_odd_p(quotient))) {
        mpz_add_ui(quotient, quotient, 1);
    }
    *result = ldexp((double)mpz_get_ui(quotient), (int)-scale);
    if (!isfinite(*result)) goto overflow;
    if (negative) *result = -*result;
success:
    mpz_clears(a, b, scaled, quotient, remainder, NULL);
    return 0;
overflow:
    mpz_clears(a, b, scaled, quotient, remainder, NULL);
    mule_error("OverflowError", "integer division result too large for a float");
    return -1;
}

static int in_fixnum(__int128 value) {
    return value >= MULE_FIXNUM_MIN && value <= MULE_FIXNUM_MAX;
}

static MuleValue small_binary(int op, int64_t left, int64_t right) {
    __int128 result;
    switch (op) {
        case 0: result = (__int128)left + right; break;
        case 1: result = (__int128)left - right; break;
        case 2: result = (__int128)left * right; break;
        case 3:
            if (!right) {
                mule_error("ZeroDivisionError", "division by zero");
                return NULL;
            }
            if (left >= -(INT64_C(1) << 53) && left <= (INT64_C(1) << 53) &&
                right >= -(INT64_C(1) << 53) && right <= (INT64_C(1) << 53)) {
                return mule_literal_float((double)left / (double)right);
            }
            return NULL;
        case 4:
        case 5: {
            if (!right) {
                mule_error("ZeroDivisionError", "integer division or modulo by zero");
                return NULL;
            }
            int64_t quotient = left / right, remainder = left % right;
            if (remainder && ((remainder < 0) != (right < 0))) {
                quotient--;
                remainder += right;
            }
            result = op == 4 ? quotient : remainder;
            break;
        }
        case 8:
        case 9:
            if (right < 0) {
                mule_error("ValueError", "negative shift count");
                return NULL;
            }
            if (op == 9) return mule_small(right >= 63 ? (left < 0 ? -1 : 0) : left >> right);
            if (!left) return mule_small(0);
            if (right >= 63) return NULL;
            /* Multiplication avoids undefined left shifting of negative C ints. */
            result = (__int128)left * ((__int128)1 << right);
            break;
        case 10: return mule_small(left & right);
        case 11: return mule_small(left ^ right);
        case 12: return mule_small(left | right);
        default: return NULL;
    }
    return in_fixnum(result) ? mule_small((int64_t)result) : NULL;
}

static void float_divmod(double left, double right, double *quotient, double *modulo) {
    double mod = fmod(left, right);
    double div = (left - mod) / right;
    if (mod != 0.0) {
        if ((right < 0.0) != (mod < 0.0)) {
            mod += right;
            div -= 1.0;
        }
    } else mod = copysign(0.0, right);
    if (div != 0.0) {
        *quotient = floor(div);
        if (div - *quotient > 0.5) *quotient += 1.0;
    } else *quotient = copysign(0.0, left / right);
    *modulo = mod;
}

static MuleValue float_binary(int op, MuleValue lhs, MuleValue rhs) {
    if (op >= 8 || op == 6) {
        mule_error("TypeError", "operation requires integer operands");
        return NULL;
    }
    double left, right, result;
    if (mule_number_double(lhs, &left) < 0 || mule_number_double(rhs, &right) < 0) return NULL;
    switch (op) {
        case 0: result = left + right; break;
        case 1: result = left - right; break;
        case 2: result = left * right; break;
        case 3:
        case 4:
        case 5:
            if (right == 0.0) {
                mule_error("ZeroDivisionError", "float division or modulo by zero");
                return NULL;
            }
            if (op == 3) result = left / right;
            else {
                double quotient, modulo;
                float_divmod(left, right, &quotient, &modulo);
                result = op == 4 ? quotient : modulo;
            }
            break;
        case 7:
            if (left == 0.0 && right < 0.0) {
                mule_error("ZeroDivisionError", "zero cannot be raised to a negative power");
                return NULL;
            }
            if (left < 0.0 && isfinite(left) && isfinite(right) && right != floor(right)) {
                mule_error("ValueError", "complex power results are not supported");
                return NULL;
            }
            result = pow(left, right);
            if (isinf(result) && isfinite(left) && isfinite(right)) {
                mule_error("OverflowError", "numerical result out of range");
                return NULL;
            }
            break;
        default:
            mule_error("TypeError", "unsupported numeric operation");
            return NULL;
    }
    return mule_literal_float(result);
}

static MuleValue integer_binary(int op, MuleValue lhs, MuleValue rhs) {
    int64_t left, right;
    if (small_integer(lhs, &left) && small_integer(rhs, &right)) {
        MuleValue result = small_binary(op, left, right);
        if (result || mule_has_error()) return result;
    }
    mpz_t a, b, result;
    mpz_inits(a, b, result, NULL);
    mule_integer_export(lhs, a);
    mule_integer_export(rhs, b);
    MuleValue value = NULL;
    switch (op) {
        case 0: mpz_add(result, a, b); break;
        case 1: mpz_sub(result, a, b); break;
        case 2: mpz_mul(result, a, b); break;
        case 3: {
            double quotient;
            if (!integer_quotient_double(a, b, &quotient)) value = mule_literal_float(quotient);
            goto done;
        }
        case 4:
        case 5:
            if (!mpz_sgn(b)) {
                mule_error("ZeroDivisionError", "integer division or modulo by zero");
                goto done;
            }
            if (op == 4) mpz_fdiv_q(result, a, b);
            else mpz_fdiv_r(result, a, b);
            break;
        case 7:
            if (mpz_sgn(b) < 0) {
                value = float_binary(op, lhs, rhs);
                goto done;
            }
            if (!mpz_sgn(b)) mpz_set_ui(result, 1);
            else if (!mpz_sgn(a)) mpz_set_ui(result, 0);
            else if (mpz_cmpabs_ui(a, 1) == 0) {
                mpz_set_si(result, mpz_sgn(a) < 0 && mpz_odd_p(b) ? -1 : 1);
            } else if (!mpz_fits_ulong_p(b)) {
                mule_error("OverflowError", "exponent too large");
                goto done;
            } else mpz_pow_ui(result, a, mpz_get_ui(b));
            break;
        case 8:
        case 9:
            if (mpz_sgn(b) < 0) {
                mule_error("ValueError", "negative shift count");
                goto done;
            }
            if (!mpz_sgn(a)) mpz_set_ui(result, 0);
            else if (!mpz_fits_ulong_p(b)) {
                if (op == 9) mpz_set_si(result, mpz_sgn(a) < 0 ? -1 : 0);
                else {
                    mule_error("OverflowError", "shift count too large");
                    goto done;
                }
            } else if (op == 8) {
                unsigned long shift = mpz_get_ui(b);
                if (shift > LONG_MAX || mpz_sizeinbase(a, 2) > (size_t)LONG_MAX - shift) {
                    mule_error("OverflowError", "shift count too large");
                    goto done;
                }
                mpz_mul_2exp(result, a, shift);
            } else mpz_fdiv_q_2exp(result, a, mpz_get_ui(b));
            break;
        case 10: mpz_and(result, a, b); break;
        case 11: mpz_xor(result, a, b); break;
        case 12: mpz_ior(result, a, b); break;
        default:
            mule_error("TypeError", "unsupported numeric operation");
            goto done;
    }
    value = mule_integer_import(result);
done:
    mpz_clears(a, b, result, NULL);
    return value;
}

MuleValue mule_numeric_binary(int op, MuleValue left, MuleValue right) {
    if ((!mule_is_integer(left) && !is_float(left)) ||
        (!mule_is_integer(right) && !is_float(right))) {
        mule_error("TypeError", "numeric operands required");
        return NULL;
    }
    if (op >= 10 && op <= 12 && !mule_is_fixnum(left) && !mule_is_fixnum(right) &&
        ((MuleObject *)left)->kind == MULE_BOOL && ((MuleObject *)right)->kind == MULE_BOOL) {
        int a = ((MuleObject *)left)->as.boolean != 0;
        int b = ((MuleObject *)right)->as.boolean != 0;
        return mule_bool(op == 10 ? a & b : op == 11 ? a ^ b : a | b);
    }
    if (is_float(left) || is_float(right)) return float_binary(op, left, right);
    return integer_binary(op, left, right);
}

MuleValue mule_numeric_unary(int op, MuleValue value) {
    if (is_float(value)) {
        double real = ((MuleObject *)value)->as.real;
        if (op == 0) return mule_retain(value);
        if (op == 1) return mule_literal_float(-real);
        mule_error("TypeError", "integer operand required");
        return NULL;
    }
    if (!mule_is_integer(value)) {
        mule_error("TypeError", "numeric operand required");
        return NULL;
    }
    if (op == 0 && ((mule_is_fixnum(value)) || ((MuleObject *)value)->kind == MULE_BIGINT)) {
        return mule_retain(value);
    }
    int64_t small;
    if (small_integer(value, &small)) {
        if (op == 0) return mule_small(small);
        if (op == 2) return mule_small(~small);
        if (op == 1 && small != MULE_FIXNUM_MIN) return mule_small(-small);
    }
    mpz_t integer;
    mpz_init(integer);
    mule_integer_export(value, integer);
    if (op == 1) mpz_neg(integer, integer);
    else if (op == 2) mpz_com(integer, integer);
    else if (op != 0) {
        mpz_clear(integer);
        mule_error("TypeError", "unsupported unary operation");
        return NULL;
    }
    MuleValue result = mule_integer_import(integer);
    mpz_clear(integer);
    return result;
}

int mule_numeric_compare(MuleValue left, MuleValue right, int *ordering) {
    if ((!mule_is_integer(left) && !is_float(left)) ||
        (!mule_is_integer(right) && !is_float(right))) {
        mule_error("TypeError", "numeric operands required");
        return -1;
    }
    if (is_float(left) && is_float(right)) {
        double a = ((MuleObject *)left)->as.real, b = ((MuleObject *)right)->as.real;
        *ordering = isnan(a) || isnan(b) ? 2 : (a > b) - (a < b);
        return 0;
    }
    if (mule_is_integer(left) && mule_is_integer(right)) {
        int64_t a, b;
        if (small_integer(left, &a) && small_integer(right, &b)) {
            *ordering = (a > b) - (a < b);
            return 0;
        }
        mpz_t a_big, b_big;
        mpz_inits(a_big, b_big, NULL);
        mule_integer_export(left, a_big);
        mule_integer_export(right, b_big);
        int comparison = mpz_cmp(a_big, b_big);
        *ordering = (comparison > 0) - (comparison < 0);
        mpz_clears(a_big, b_big, NULL);
        return 0;
    }
    int reverse = is_float(left);
    double real = ((MuleObject *)(reverse ? left : right))->as.real;
    if (isnan(real)) {
        *ordering = 2;
        return 0;
    }
    int comparison;
    if (isinf(real)) comparison = real > 0 ? -1 : 1;
    else {
        mpz_t integer;
        mpz_init(integer);
        mule_integer_export(reverse ? right : left, integer);
        comparison = mpz_cmp_d(integer, real);
        mpz_clear(integer);
        comparison = (comparison > 0) - (comparison < 0);
    }
    *ordering = reverse ? -comparison : comparison;
    return 0;
}
