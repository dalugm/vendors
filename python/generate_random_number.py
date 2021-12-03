'''
generate random numbers following normal distribution.
'''
import random
import math


def normal_random():
    """
    :return: a normally distributed random number
    """
    y_1 = y_2 = random.random()

    return math.cos(2 * 3.14 * y_2) * math.sqrt(-2. * math.log(y_1))


while True:
    medium = input("Input medium value: ")
    variance = input("Input variance: ")
    number = input("Input the number of random numbers: ")

    for day in range(int(number)):
        result = normal_random() * float(variance) + float(medium)
        print(result)
        # 保留三位有效数字
        print("{:.3g}".format(result))
