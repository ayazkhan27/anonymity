def goalSeek(func, lowLimit, highLimit, target=0, maxError=0.01):
    while lowLimit <= highLimit:
        midPoint = (lowLimit + highLimit) / 2
        guess = func(midPoint)
        if abs(guess - target) <= maxError:
            return midPoint
        elif guess < target:
            lowLimit = midPoint
        else:
            highLimit = midPoint
    return None

def makePoly(A, B, C, D):
    def poly(x):
        return A * x**3 + B * x**2 + C * x + D
    return poly

path = r"C:\Users\admin\Downloads\poly.txt"

with open(path, "r") as file:
    lines = file.readlines()[1:]

for line in lines:
    if line[0] == '#':
        continue
    values = line.split()
    A, B, C, D, Lo, Hi = map(float, values[:6])
    polyFunc = makePoly(A, B, C, D)
    root = goalSeek(polyFunc, Lo, Hi)
    print(f"{A:10.2f} {B:10.2f} {C:10.2f} {D:10.2f} -> {root:10.2f}")
