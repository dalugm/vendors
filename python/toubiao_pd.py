"""
区间法计算投标报价
"""

import sys
import math
import pandas as pd

input_file = sys.argv[1]
output_file = sys.argv[2]


def calculate(offer, benchmark):
    '''
    @return 最终得分
    '''
    point = 50.0
    rate = ((offer / benchmark) - 1) * 100
    if rate < -5:
        overpass = math.ceil(abs(rate) - 5)
        point -= overpass
    elif rate > 0:
        overpass = math.ceil(rate)
        point -= overpass * 2
    # print(max(point, 0))
    return max(point, 0)


# while True:
#     calculate(float(input("offer: ")), float(input("benchmark: ")))

data_frame = pd.read_excel(input_file,
                           sheet_name='Sheet1')
data_frame = data_frame.iloc[:, 5:]

for jizhun in data_frame.iloc[1:, 0]:
    for baojia in data_frame.iloc[0, 1:]:
        data = [[calculate(baojia, jizhun) for baojia in data_frame.iloc[0, 1:]] for jizhun in data_frame.iloc[1:, 0]]

data_frame = pd.DataFrame(data)
writer = pd.ExcelWriter(output_file)

data_frame.to_excel(writer,
                    index=False)

writer.save()
