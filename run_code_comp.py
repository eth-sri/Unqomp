def print_relative_vals(nbl_q, nbl_u):
    perc_l_saved = int(round((nbl_u - nbl_q) / nbl_q * 100))
    return perc_l_saved

folder_str = "evaluation/code_complexity/"
file_names = ["adder.py", "dj.py", "grover.py", "integercomparator.py", "mcry.py", "mcx.py", "mult.py", "piecewiseLinRot.py", "polypauliRot.py", "weightedadder.py"]
algo_names = ["Adder", "Deutsch-Jozsa", "Grover", "IntegerComparator", "MCRY", "MCX", "Multiplier", "PiecewiseLinearR", "PolynomialPauliR", "WeightedAdder"]

print("Example name; Number of lines in Qiskit; Number of lines with Unqomp ; % lines saved by Unqomp")

for i in range(len(file_names)):
    nbLines = [0, 0]
    j = -1
    file = open(folder_str + file_names[i], 'r')
    for line in file:
        if line[0] == '#':
            j += 1
        else:
            nbLines[j] += 1
    print(algo_names[i] + "; " + str(nbLines[0]) + "; " + str(nbLines[1]) + "; " + str(print_relative_vals(nbLines[0], nbLines[1])))