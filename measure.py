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


def measure(f):
    os.system("touch measures.txt")
    os.system("rm measures.txt")
    os.system("touch measures.txt")
    k = 0
    for file in f:
        print_progress(k, len(f), prefix='Progress:', suffix='Complete', length=50)
        os.system("./satsolver-opt " + file[0] + " " + str(file[1]) + " time >> measures.txt")
        k += 1
    print_progress(k, len(f), prefix='Progress:', suffix='Complete', length=50)


def coord_from_file(path):
    x1, x2, y, e = [], [], [], []
    file = open(path, "r")
    for m in file.read().split("\n"):
        if m != "":
            m1, m2, m3, m4 = m.split("-")
            x1.append(int(m1))
            x2.append(int(m2))
            y.append(float(m3))
            e.append(float(m4))
    return np.array([np.array(x1), np.array(x2), np.array(y), np.array(e)])


def test(f):
    os.system("touch measures.txt")
    os.system("rm measures.txt")
    os.system("touch measures.txt")

    k = 0
    for file in f:
        print_progress(k, len(f), prefix='Progress:', suffix='Complete', length=50)
        os.system("./satsolver-opt " + file[0] + " " + str(file[1]) + " >> measures.txt")
        k += 1
    print_progress(k, len(f), prefix='Progress:', suffix='Complete', length=50)

    b = []
    file = open("measures.txt", "r")
    for m in file.read().split("\n"):
        if m == "sat":
            b.append(True)
        if m == "unsat":
            b.append(False)

    expected = True
    i = 0
    for file in f:
        for j in range(1, file[1] + 1):
            if b[i] != file[2]:
                expected = False
                print("\33[101m" + "Solver output doesn't match expected result on " + file[0] + str(j) + "\33[0m")
            i += 1
    if expected:
        print("\33[102m" + "Test successfully passed!" + "\33[0m")


def graph(limit, solver, sigma=3):
    fig = plt.figure()
    ax1 = fig.add_subplot(111)
    ax2 = ax1.secondary_xaxis('top', functions=(lambda x: 4.26*x+5.23, lambda y: (y-5.23)/4.26))

    if limit != 0:
        x = range(0, 150)
        y = [1000 * limit] * len(x)
        ax1.plot(x, y, label="limit", linestyle='-')

    for s in solver:
        c = coord_from_file("measures/" + s + ".txt")
        x1, x2, y, e = c[0], c[1], c[2], c[3]

        fit = np.polyfit(x1, np.log(y), 1)
        f = np.poly1d(fit)
        x0 = [1]
        y0 = [np.exp(f(1))]

        while y0[-1] < 1200:
            x0.append(x0[-1] + 1)
            y0.append(np.exp(f(x0[-1])))

        ax1.plot(x0, y0, label=s + " (model)", marker='', linestyle='--')
        ax1.errorbar(x1, y, yerr=sigma*e, label=s + " (data)", marker='x', linestyle='')

    plt.title("Comparaison des solvers")
    ax1.set_xlabel("Nombre de variables")
    ax2.set_xlabel("Nombre de clauses")
    plt.ylabel("Temps d'exécution moyen (en $ms$)")
    plt.yscale("log")
    plt.legend()
    plt.show()


files = [
    ["test/UF20.91/uf20-0", 1000, True],
    ["test/UF50.218/uf50-0", 1000, True],
    ["test/UF75.325/uf75-0", 100, True],
    #["test/UF100.430/uf100-0", 1000, True],
    #["test/UF125.538/uf125-0", 100, True],
    #["test/UF150.645/uf150-0", 100, True],
    ["test/UUF50.218/uuf50-0", 1000, False],
    #["test/UUF75.325/uuf75-0", 100, False],
    #["test/UUF100.430/uuf100-0", 1000, False],
]
measures = [
    # "naive_solver",
    "quine_solver",
    "dpll_solver"
]
plt.style.use("ggplot")

test(files)
#measure(files)
graph(0, [
    # "naive_solver",
    #"quine_solver",
    "dpll_solver"
], sigma=2)
