import os
import numpy as np
import matplotlib.pyplot as plt
import time


def print_progress(iteration, total, prefix='', suffix='', decimals=1, length=100, fill='█', print_end="\r"):
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + ' ' * (length - filledLength)
    print(f'\r{prefix} |{bar}| {percent}% {suffix}', end=print_end)
    if iteration == total:
        print()


def measure(files, quantity):
    os.system("touch measures.txt")
    os.system("rm measures.txt")
    os.system("touch measures.txt")
    k = 0
    for f in files:
        print_progress(k, len(files), prefix='Progress:', suffix='Complete', length=50)
        os.system("./satsolver-opt " + f[0] + " " + str(f[1]) + " " + quantity + " >> measures.txt")
        print_progress(len(files), len(files), prefix='Progress:', suffix='Complete', length=50)


def coord_from_file(path, taken=0, unit=6):
    x, y = [], []
    file = open(path, "r")
    for m in file.read().split("\n"):
        if m != "":
            x.append(int(m.split('-')[0]))
            y.append(float(m.split('-')[1]))
    return np.array([np.array(x), np.array(y)])


def graph(coord, name, xlabel, ylabel="Temps d'exécution moyen (en $ms$)"):
    x, y = coord[0], coord[1]
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.plot(x, y/1000, label=name)

files = [
    ["test/UF020.91/uf20-0", 100, 91]
]
quantity = "bool"

measure(files, quantity)

if quantity == "time":
    c = coord_from_file("measures.txt")
    graph(c, "Naive solver", "Nombre de litéraux")

    plt.style.use("Solarize_Light2")
    plt.title("Comparaison des satsolvers")
    plt.legend()
    plt.show()

