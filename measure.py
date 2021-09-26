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


def test(path, n):
    os.system("touch test.txt")
    os.system("rm test.txt")
    os.system("touch test.txt")
    start_time = time.time()
    for i in range(1, n+1):
        os.system("./satsolver-opt " + path + str(i) + ".cnf >> test.txt")
    return time.time() - start_time


def measure(files):
    os.system("touch measure.txt")
    os.system("rm measure.txt")
    os.system("touch measure.txt")
    k = 0
    for f in files:
        print_progress(k, len(files), prefix='Progress:', suffix='Complete', length=50)
        os.system("echo " + str(f[2]) + "-" + str(test(f[0], f[1]) / f[1]) + " >> measure.txt")
        k += 1
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


def reg(coord):
    slope, intercept, r, p_value, std_err = stats.linregress(coord[0], coord[1])
    graph(coord, "", "")
    plt.plot(np.linspace(coord[0][0], coord[0][-1]), slope*np.linspace(coord[0][0], coord[0][-1])+intercept, label="Régression")
    print(str(slope)+"x"+str(intercept))
    print(r**2)


#plt.style.use("Solarize_Light2")


measure([
    ["test/UF020.91/uf20-0", 100, 91]
    ])

c = coord_from_file("measure.txt")
graph(c, "Naive solver", "Nombre de litéraux")

plt.title("Comparaison des satsolvers")
plt.legend()
plt.show()

