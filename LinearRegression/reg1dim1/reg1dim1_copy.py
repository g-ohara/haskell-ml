import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def reg1dim1(x, y):
    a = np.dot(x, y) / (x**2).sum()
    return a


x = np.array([1, 2, 4, 6, 7])
y = np.array([1, 3, 3, 5, 4])
a = reg1dim1(x, y)

np.savetxt("reg1dim1.csv", x, y, delimiter=",")

# plt.scatter(x, y, color="k")
msft = pd.read_csv("reg1dim1.csv")
xmax = x.max()
msft.plot([0, xmax], [0, a * xmax], color="k")
# plt.plot([0, xmax], [0, a * xmax], color="k")
# plt.show()
