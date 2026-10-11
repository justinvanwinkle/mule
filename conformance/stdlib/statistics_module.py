"""statistics: mean/fmean/median variants/mode/multimode/stdev/variance/quantiles on exact data, Fraction/Decimal inputs, error cases."""
import statistics as st
from fractions import Fraction
from decimal import Decimal

data = [2, 4, 4, 4, 5, 5, 7, 9]
print("mean", st.mean(data), type(st.mean(data)).__name__)
print("mean float result", st.mean([1, 2]), st.mean([1, 2, 4]))
print("fmean", st.fmean(data), st.fmean([1, 2, 3], weights=[3, 2, 1]))
print("median even", st.median(data), st.median([1, 3, 2, 4]))
print("median odd", st.median([5, 1, 3]))
print("median_low/high", st.median_low([1, 2, 3, 4]), st.median_high([1, 2, 3, 4]))
print("median_grouped", st.median_grouped([1, 2, 2, 3, 4, 4, 4, 4, 4, 5]))
print("mode", st.mode(data), st.mode("aabbbc"), st.mode([1, 1, 2, 2]))
print("multimode", st.multimode([1, 1, 2, 2, 3]), st.multimode("abc"), st.multimode([]))
print("pstdev", st.pstdev(data), st.pvariance(data))
print("stdev", st.stdev([1, 2, 3, 4]), st.variance([1, 2, 3, 4]))
print("variance with mean", st.variance([1, 2, 3, 4], xbar=2.5))
print("fractions", st.mean([Fraction(1, 2), Fraction(1, 3)]), st.variance([Fraction(1), Fraction(2), Fraction(4)]))
print("decimals", st.mean([Decimal("1.1"), Decimal("2.2")]), st.median([Decimal("1.5"), Decimal("2.5")]))
print("geometric", round(st.geometric_mean([1, 4, 16]), 9), st.harmonic_mean([40, 60]))
print("quantiles", st.quantiles([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], n=4))
print("quantiles inclusive", st.quantiles([1, 2, 3, 4, 5], n=4, method="inclusive"))
print("correlation", st.correlation([1, 2, 3, 4], [2, 4, 6, 8]), st.covariance([1, 2, 3], [1, 2, 3]))
lr = st.linear_regression([1, 2, 3, 4], [3, 5, 7, 9])
print("linear_regression", lr.slope, lr.intercept)
nd = st.NormalDist(100, 15)
print("NormalDist", nd.mean, nd.stdev, round(nd.cdf(100), 6), round(nd.inv_cdf(0.5), 6))
print("NormalDist arith", (nd + 10).mean, (nd * 2).stdev)
print("NormalDist from_samples", st.NormalDist.from_samples([1, 2, 3, 4, 5]).mean)
print("generator input", st.mean(x * x for x in range(5)))
print("bool data", st.mean([True, False, True, True]))
for fn, arg in [(st.mean, []), (st.median, []), (st.stdev, [1]), (st.variance, []), (st.mode, []), (st.harmonic_mean, [-1])]:
    try:
        fn(arg)
    except st.StatisticsError as e:
        print("error", fn.__name__, type(e).__name__, isinstance(e, ValueError))
