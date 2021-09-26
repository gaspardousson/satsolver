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


def test(sat, n_lits, n_clauses, n_files):
    os.system("touch test.txt")
    os.system("rm test.txt")
    os.system("touch test.txt")
    start_time = time.time()
    if sat:
        for i in range(1, n_files+1):
            if n_lits < 100:
                os.system("./satsolver-opt test/UF0" + str(n_lits) + "." + str(n_clauses) + "/uf" + str(n_lits) + "-0" + str(i) + ".cnf >> test.txt")
            else:
                os.system("./satsolver-opt test/UF" + str(n_lits) + "." + str(n_clauses) + "/uf" + str(n_lits) + "-0" + str(i) + ".cnf >> test.txt")
    else:
        for i in range(1, n_files+1):
            if n_lits < 100:
                os.system("./satsolver-opt test/UUF0" + str(n_lits) + "." + str(n_clauses) + "/uf" + str(n_lits) + "-0" + str(i) + ".cnf >> test.txt")
            else:
                os.system("./satsolver-opt test/UUF" + str(n_lits) + "." + str(n_clauses) + "/uf" + str(n_lits) + "-0" + str(i) + ".cnf >> test.txt")
    print(time.time() - start_time)


def measure(files):
    os.system("touch measures.txt")
    os.system("rm measures.txt")
    os.system("touch measures.txt")
    print_progress(0, len(files), prefix='Progress:', suffix='Complete', length=50)
    for f in files:
        os.system(str(f[1]) + "-" + str(test(f[0], f[1], f[2], f[3])) + "/" + str(f[3]) + "_" + " >> measures.txt")


def coord_from_file(path, taken=0, unit=6):
    x, y = [], []
    file = open(path, "r")
    for m in file.read().split("_"):
        x.append(int(m.split('-')[0]))
        r = m.split("-")[1].split("/")
        y.append(float(r[0]) * 10**unit /float(r[1]))
    return np.array([np.array(x), np.array(y)])


def graph(coord, name, xlabel, ylabel="Temps d'exécution moyen (en $ms$)"):
    x, y = coord[0], coord[1]
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.plot(x, y/1000, label=name)


def reg(coord):
    slope, intercept, r, p_value, std_err = stats.linregress(coord[0], coord[1])
    graph(coord, "Mesures", "Longueur du mot de tresse")
    plt.plot(np.linspace(coord[0][0], coord[0][-1]), slope*np.linspace(coord[0][0], coord[0][-1])+intercept, label="Régression")
    print(str(slope)+"x"+str(intercept))
    print(r**2)


#plt.style.use("Solarize_Light2")


measure([True, 20, 91, 10])

c = coord_from_file("measure.txt")
graph(c, "Temps d'exécution moyen", "Nombre de litéraux")

plt.legend()
plt.show()

