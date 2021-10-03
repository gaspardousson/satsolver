import os
import numpy as np
import matplotlib.pyplot as plt


def print_progress(iteration, total, prefix='', suffix='', decimals=1, length=100, fill='█', print_end="\r"):
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filled_length = int(length * iteration // total)
    bar = fill * filled_length + ' ' * (length - filled_length)
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
        k += 1
    print_progress(k, len(files), prefix='Progress:', suffix='Complete', length=50)


def coord_from_file(path):
    x, y = [0], [0]
    file = open(path, "r")
    m = file.read().split("\n")
    for i in range(len(m)):
        if m[i] != "":
            x.append(files[i][2])
            y.append(float(m[i]))
    return np.array([np.array(x), np.array(y)])


def bool_from_file(path):
    b = []
    file = open(path, "r")
    for m in file.read().split("\n"):
        if m != "":
            b.append(m == "sat")
    return b


def graph(coord, name, xlabel, ylabel="Temps d'exécution moyen (en $ms$)"):
    x, y = coord[0], coord[1]
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.plot(x, y/1000, label=name)

files = [
    ["test/UF020.91/uf20-0", 100, 20, True]
]
quantity = "bool"
plt.style.use("Solarize_Light2")

measure(files, quantity)

if quantity == "time":
    c = coord_from_file("measures.txt")
    graph(c, "Naive solver", "Nombre de litéraux")

    plt.title("Comparaison des satsolvers")
    plt.legend()
    plt.show()

if quantity == "bool":
    b = bool_from_file("measures.txt")
    test = True
    i = 0
    for f in files:
        for j in range(1, f[1]+1):
            if b[i] != f[3]:
                test = False
                print("\33[101m" + "Solver output doesn't match expected result on " + f[0] + str(j))
            i += 1
    if test:
        print("\33[102m" + "Test successfully passed!")