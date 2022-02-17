import os
import time
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Affichage d'une barre de progression, trouvée sur Internet et adaptée
def print_progress(iteration, total, length=40, print_end="\r"):
    progress = int(length * iteration // total)
    bar = '\uEE04' * progress + '\uEE01' * (length - progress)
    if progress == 0: print(f'\uEE00{bar}\uEE02', end=print_end)
    elif progress == length: print(f'\uEE03{bar}\uEE05', end=print_end)
    else: print(f'\uEE03{bar}\uEE02', end=print_end)
    if iteration == total:
        print()


def mesurer(problemes):
    """
    Mesure le temps d'exécution du solveur pour les problèmes fournis en entrée.
    :param problemes: Liste de problèmes au format (chemin, nombre, satisfiable).
    """
    os.system("touch mesures.txt")
    os.system("rm mesures.txt")
    os.system("touch mesures.txt")

    n_pb = 0
    for p in problemes:
        n_pb += p[1]

    k = 0
    for p in problemes:
        print_progress(k, n_pb)
        os.system("./satsolver-opt " + p[0] + " " + str(p[1]) + " time >> mesures.txt")
        k += p[1]
    print_progress(k, n_pb)


def lire_mesures(chemin):
    """
    Lis le fichier de mesures passé en entrée, par exemple pour en tracer le graphe.
    :param chemin: Chaîne de caractères indiquant le chemin jusqu'à un fichier contenant des mesures.
    :return: Mesures au format (nombre de variables, nombre de clauses, temps d'exécution, incertitude).
    """
    x1, x2, y, e = [], [], [], []
    fichier = open(chemin, "r")
    for mesure in fichier.read().split("\n"):
        if mesure != "":
            n_var, n_clauses, temps, erreur = mesure.split("-")
            x1.append(int(n_var))
            x2.append(int(n_clauses))
            y.append(float(temps)*10**-3)
            e.append(float(erreur)*10**-3)
    return (np.array(x1), np.array(x2), np.array(y), np.array(e))


def verifier(problemes):
    """
    Vérifie si le solveur répond correctement à tous les problèmes fournis en entrée.
    :param problemes: Liste de problèmes au format (chemin, nombre, satisfiable).
    """
    os.system("touch test.txt")
    os.system("rm test.txt")
    os.system("touch test.txt")

    n_pb = 0
    for p in problemes:
        n_pb += p[1]

    k = 0
    for p in problemes:
        print_progress(k, n_pb)
        os.system("./satsolver-opt " + p[0] + " " + str(p[1]) + " >> test.txt")
        k += p[1]
    print_progress(k, n_pb)

    resultats = []
    test = open("test.txt", 'r')
    for ligne in test.read().split('\n'):
        if ligne == "sat":
            resultats.append(True)
        if ligne == "unsat":
            resultats.append(False)

    est_correct = True
    i = 0
    for p in problemes:
        for j in range(1, p[1] + 1):
            if resultats[i] != p[2]:
                est_correct = False
                print(u"\u001b[38;5;124m" + "La réponse est incorrecte pour " + p[0] + str(j) + ".\u001b[0m")
            i += 1
    if est_correct:
        print(u"\u001b[38;5;70m" + "Toutes les réponses sont correctes. \u001b[0m")

    os.system("rm test.txt")


def grapher(solveurs, sigma=2, echelle_log=True):
    """
    Trace les graphes des mesures, accompagnées de leurs incertitudes, des solveurs fournis en entrée et ajoute une courbe continue approchée des résultats pour chacun d'eux.
    :param solveurs: Liste de solveurs.
    :param sigma: Nombre de sigmas à considérer.
    :param echelle_log: Booléen précisant l'échelle à utiliser.
    """
    fig = plt.figure()
    axe1 = fig.add_subplot(111)
    axe2 = axe1.secondary_xaxis('top', functions=(lambda x: 4.26*x+5.23, lambda y: (y-5.23)/4.26))

    for i in range(len(solveurs)):
        c = lire_mesures("mesures/" + solveurs[i] + ".txt")
        x1, x2, y, e = c[0], c[1], c[2], c[3]

        fit = np.polyfit(x1, np.log(y), 2)
        f = np.poly1d(fit)
        n = 1
        while np.exp(f(n)) < 10**-5:
            n += 1
        x0 = [n]
        y0 = [np.exp(f(n))]

        while x0[-1] < 260 and y0[-1] < 2*10**2:
            x0.append(x0[-1] + 1)
            y0.append(np.exp(f(x0[-1])))

        axe1.plot(x0, y0, marker='', linestyle='dotted', color=couleurs[i])
        axe1.errorbar(x1, y, yerr=sigma*e, label=solveurs[i], marker='x', linestyle='', color=couleurs[i])

    axe1.set_xlabel("Nombre de variables")
    axe2.set_xlabel("Nombre de clauses")
    plt.ylabel("Temps d'exécution moyen (en $s$)")
    if echelle_log: plt.yscale("log")
    plt.legend(loc=2)
    plt.show()


couleurs = sns.color_palette("colorblind")
problemes = [
    # Trié des plus petits aux plus gros problèmes (SAT puis UNSAT en cas d'égalité)
    # Format (chemin, nombre, satisfiable)

    ("problemes/UF20.91/uf20-0", 1000, True),
    ("problemes/UF50.218/uf50-0", 1000, True),
    ("problemes/UUF50.218/uuf50-0", 1000, False),
    ("problemes/UF75.325/uf75-0", 100, True),
    ("problemes/UUF75.325/uuf75-0", 100, False),
    ("problemes/UF100.430/uf100-0", 1000, True),
    #("problemes/UUF100.430/uuf100-0", 1000, False),
    #("problemes/UF125.538/uf125-0", 100, True),
    #("problemes/UUF125.538/uuf125-0", 100, False),
    #("problemes/UF150.645/uf150-0", 100, True),
    #("problemes/UUF150.645/uuf150-0", 100, False),
    #("problemes/UF175.753/uf175-0", 100, True),
    #("problemes/UUF175.753/uuf175-0", 100, False),
    #("problemes/UF200.860/uf200-0", 100, True),
    #("problemes/UUF200.860/uuf200-0", 99, False),
    #("problemes/UF225.960/uf225-0", 100, True),
    #("problemes/UUF225.960/uuf225-0", 100, False),
    #("problemes/UF250.1065/uf250-0", 100, True),
    #("problemes/UUF250.1065/uuf250-0", 100, False),

    # Graph Colouring
    #("problemes/flat30-60/flat30-", 100, True),
    #("problemes/flat50-115/flat50-", 999, True),
    #("problemes/flat75-180/flat75-", 100, True),
    #("problemes/flat125-301/flat125-", 100, True),
    #("problemes/flat150-360/flat150-", 100, True),
    #("problemes/flat175-417/flat175-", 100, True),
    #("problemes/flat200-479/flat200-", 100, True),

    # Blocks world
    #("problemes/blocksworld/bw_", 7, True)

    #("problemes/ais/ais", 4, True)
]
solveurs = [
    #"dpll_naive_sat",
    #"dpll_naive_unsat",
    #"dpll_thermal_sat",
    #"dpll_thermal_unsat",
    #"cdcl_naive_sat",
    #"cdcl_naive_unsat",
    #"cdcl_thermal_sat",
    #"cdcl_thermal_unsat",
    #"cdcl_clean_sat",
    #"cdcl_clean_unsat",
]
plt.style.use("ggplot")

verifier(problemes)
#mesurer(problemes)
#grapher(solveurs)
