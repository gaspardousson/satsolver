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
        os.system("./satsolver-opt " + f[0] + " " + str(f[1]) + " " + str(f[2]) + " " + quantity + " >> measures.txt")
        k += 1
    print_progress(k, len(files), prefix='Progress:', suffix='Complete', length=50)


def coord_from_file(path):
    x, y = [0], [0]
    file = open(path, "r")
    for m in file.read().split("\n"):
        if m != "":
            m1, m2 = m.split("-")
            x.append(int(m1))
            y.append(float(m2))
    return np.array([np.array(x), np.array(y)])


def bool_from_file(path):
    b = []
    file = open(path, "r")
    for m in file.read().split("\n"):
        if m == "sat":
            b.append(True)
        if m == "unsat":
            b.append(False)
    return b


files = [
    ["test/UF20.91/uf20-0", 100, 20, True],
    #["test/UF50.218/uf50-0", 1000, 50, True],
    #["test/UUF50.218/uuf50-0", 100, 50, False],
]
measures = [
    ["naive_solver", True],
    ["quine_solver", True]
]
plt.style.use("Solarize_Light2")

task = "graph"

if task == "graph":
    for solver in measures:
        c = coord_from_file("measures/"+solver[0]+".txt")
        x, y = c[0], c[1]
        plt.plot(x, y, label=solver[0], marker='x', linestyle='')

    plt.title("Comparaison des solvers")
    plt.xlabel("Nombre de litéraux")
    plt.ylabel("Temps d'exécution moyen (en $ms$)")
    plt.legend()
    plt.show()


if task == "time":
    measure(files, task)
    c = coord_from_file("measures.txt")
    x, y = c[0], c[1]
    plt.plot(x, y, marker='x')

    plt.title("Comparaison des solvers")
    plt.xlabel("Nombre de litéraux")
    plt.ylabel("Temps d'exécution moyen (en $ms$)")
    plt.legend()
    plt.show()


if task == "bool":
    measure(files, task)
    b = bool_from_file("measures.txt")
    test = True
    i = 0
    for f in files:
        for j in range(1, f[1]+1):
            if b[i] != f[3]:
                test = False
                print("\33[101m" + "Solver output doesn't match expected result on " + f[0] + str(j) + "\33[0m")
            i += 1
    if test:
        print("\33[102m" + "Test successfully passed!" + "\33[0m")