import os
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


def print_progress(iteration, total, length=40, print_end="\r"):
    progress = int(length * iteration // total)
    bar = '\uEE04' * progress + '\uEE01' * (length - progress)
    if progress == 0: print(f'\uEE00{bar}\uEE02', end=print_end)
    elif progress == length: print(f'\uEE03{bar}\uEE05', end=print_end)
    else: print(f'\uEE03{bar}\uEE02', end=print_end)
    if iteration == total:
        print()


def measure(f):
    os.system("touch measures.txt")
    os.system("rm measures.txt")
    os.system("touch measures.txt")

    s = 0
    for file in f:
        s += file[1]

    k = 0
    for file in f:
        print_progress(k, s)
        os.system("./satsolver-opt " + file[0] + " " + str(file[1]) + " time >> measures.txt")
        k += file[1]
    print_progress(k, s)


def coord_from_file(path):
    x1, x2, y, e = [], [], [], []
    file = open(path, "r")
    for m in file.read().split("\n"):
        if m != "":
            m1, m2, m3, m4 = m.split("-")
            x1.append(int(m1))
            x2.append(int(m2))
            y.append(float(m3)*10**-3)
            e.append(float(m4)*10**-3)
    return np.array([np.array(x1), np.array(x2), np.array(y), np.array(e)])


def test(f):
    os.system("touch measures.txt")
    os.system("rm measures.txt")
    os.system("touch measures.txt")

    s = 0
    for file in f:
        s += file[1]

    k = 0
    print_progress(k, s)
    for file in f:
        os.system("./satsolver-opt " + file[0] + " " + str(file[1]) + " >> measures.txt")
        k += file[1]
        print_progress(k, s)

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

    for i in range(len(solver)):
        c = coord_from_file("measures/" + solver[i] + ".txt")
        x1, x2, y, e = c[0], c[1], c[2], c[3]

        fit = np.polyfit(x1, np.log(y), 1)
        f = np.poly1d(fit)
        n = 1
        while np.exp(f(n)) < 10**-5:
            n += 1
        x0 = [n]
        y0 = [np.exp(f(n))]

        while x0[-1] < 260 and y0[-1] < 2*10**2:
            x0.append(x0[-1] + 1)
            y0.append(np.exp(f(x0[-1])))

        ax1.plot(x0, y0, marker='', linestyle='dotted', color=colors[i])
        ax1.errorbar(x1, y, yerr=sigma*e, label= solver[i], marker='x', linestyle='', color=colors[i])

    plt.title("Comparaison des solvers")
    ax1.set_xlabel("Nombre de variables")
    ax2.set_xlabel("Nombre de clauses")
    plt.ylabel("Temps d'exécution moyen (en $s$)")
    plt.yscale("log")
    plt.legend(loc=2)
    plt.show()

colors = sns.color_palette("colorblind")
files = [
    #   Trié des plus petits aux plus gros problèmes (SAT puis UNSAT en cas d'égalité)
    ["test/UF20.91/uf20-0", 1000, True],          #SAT
    ["test/UF50.218/uf50-0", 1000, True],         #SAT
    ["test/UUF50.218/uuf50-0", 1000, False],      #UNSAT
    ["test/UF75.325/uf75-0", 100, True],          #SAT
    ["test/UUF75.325/uuf75-0", 100, False],       #UNSAT
    #["test/UF100.430/uf100-0", 1000, True],     #SAT
    #["test/UUF100.430/uuf100-0", 1000, False],  #UNSAT
    #["test/UF125.538/uf125-0", 100, True],      #SAT
    #["test/UUF125.538/uuf125-0", 100, False],   #UNSAT
    #["test/UF150.645/uf150-0", 100, True],      #SAT
    #["test/UUF150.645/uuf150-0", 100, False],   #UNSAT
    #["test/UF175.753/uf175-0", 100, True],      #SAT
    #["test/UUF175.753/uuf175-0", 100, False],   #UNSAT
    #["test/UF200.860/uf200-0", 100, True],      #SAT
    #["test/UUF200.860/uuf200-0", 99, False],    #UNSAT
    #["test/UF225.960/uf225-0", 100, True],      #SAT
    #["test/UUF225.960/uuf225-0", 100, False],   #UNSAT
    #["test/UF250.1065/uf250-0", 100, True],     #SAT
    #["test/UUF250.1065/uuf250-0", 100, False]   #UNSAT
]
measures = [
    #"dpll_naive_sat",
    #"dpll_naive_unsat",
    #"dpll_thermal_sat",
    #"dpll_thermal_unsat",
    #"cdcl_naive_sat",
    #"cdcl_naive_unsat",
    #"cdcl_thermal_sat",
    #"cdcl_thermal_unsat",
    #"cdcl_clean_sat",
    #"cdcl_clean_unsat"
]
plt.style.use("ggplot")

test(files)
#measure(files)
#graph(0, measures, sigma=2)
