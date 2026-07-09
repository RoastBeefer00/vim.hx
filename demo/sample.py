def calculate(value, multiplier):
    result = value * multiplier
    message = "done"
    items = [result, message, multiplier]
    return items


def main():
    output = calculate(10, 3)
    print(output)
