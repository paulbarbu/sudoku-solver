#!/usr/bin/env python

if __name__ == '__main__':
    k=0

    for i in range(0, 9):
        for j in range(0, 9):
            endstr = "   " if j in [2,5] else " "
            prefix = "0" if k < 10 else ""
            print(str(i) + str(j) + "(" + prefix + str(k) + ")", end=endstr)
            k+=1
        print()

        if i in [2,5]:
            print()
