import numpy as np
from scipy import linalg


class LinearRegression:
    def __init__(self):
        self.w_ = None

    def fit(self, X, t):
        Xtil = np.c_[np.ones(X.shape[0]), X]
        A = np.dot(Xtil.T, Xtil)
        b = np.dot(Xtil.T, t)
        print("inv(A)")
        print(A)
        print("b = {0}".format(b))
        self.w_ = np.dot(linalg.inv(A), b) #linalg.solve(A, b)
        print("inv(A) * b = {0}".format(self.w_))

    def predict(self, X):
        if X.ndim == 1:
            X = X.reshape(1, -1)
        Xtil = np.c_[np.ones(X.shape[0]), X]
        return np.dot(Xtil, self.w_)
