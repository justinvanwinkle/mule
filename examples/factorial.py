def factorial(n):
    result = 1
    while n > 1:
        result *= n
        n -= 1
    return result


iterations = 0
while True:
    x = factorial(1500)
    iterations += 1

    if iterations > 1000:
        break
