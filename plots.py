import argparse

parser = argparse.ArgumentParser(description='Computes the values used to plot the graphs in Fig. 9, and saves them in evaluation/plot_values/')
parser.add_argument('--all', dest='all_values', action='store_true',
                    help='computes complete values (default: compute only the first few ones for a faster run)')
parser.set_defaults(all_values=False)
args = parser.parse_args()

all_values = args.all_values

import sys
original_stdout = sys.stdout # Save a reference to the original standard output

def mcry(nb_vars_max = 40):
    from unqomp.ancillaallocation import AncillaCircuit
    from qiskit import QuantumRegister, QuantumCircuit
    from unqomp.examples.mcx import makeQiskitMCRY

    nb_vars_t = []
    nb_qb_qi = []
    nb_qb_mi = []
    nb_qb_bq = []
    nb_gates_qi = []
    nb_gates_mi = []
    nb_gates_bq = []

    for nb_vars in range(3, nb_vars_max, 1):
        n = nb_vars
        ctrls = QuantumRegister(n, 'ctrls')
        target = QuantumRegister(1, 'target')
        circuit1 = AncillaCircuit(ctrls, target)
        circuit1.mcry(0.2, ctrls, target)
        circuit1 = circuit1.circuitWithUncomputation()

        qiskitMCX = makeQiskitMCRY(0.2, n)
        
        nb_vars_t.append(nb_vars)
        #qiskit
        nb_qb_qi.append(qiskitMCX.num_qubits)
        nb_gates_qi.append(qiskitMCX.decompose().decompose().decompose().decompose().decompose().count_ops())

        #mine
        nb_qb_mi.append(circuit1.num_qubits)
        nb_gates_mi.append(circuit1.decompose().decompose().decompose().decompose().decompose().count_ops())
        
        #qiskit buggy
        if n < 15:
            ctrls2 = QuantumRegister(n, 'ctrls')
            target2 = QuantumRegister(1, 'target')
            anc = QuantumRegister(n - 2, 'anc')
            circuit2 = QuantumCircuit(ctrls2, target2, anc)
            circuit2.mcry(0.2, ctrls2,target2[0], anc, mode = 'basic')
            nb_qb_bq.append(circuit2.num_qubits)
            nb_gates_bq.append(circuit2.decompose().decompose().decompose().decompose().decompose().count_ops())
        else:
            nb_qb_bq.append(0)
            nb_gates_bq.append({'u3' : 0, 'cx' : 0})

    with open('evaluation/plot_values/mcry.csv', 'w') as f:
        sys.stdout = f 
        print("input size; n_qb Qiskit without regression bug; n_gates Qiskit without regression bug; n_cx_gates Qiskitwithout regression bug;  n_qb Unqomp; n_gates Unqomp ; n_cx_gates Uncomp; n_qb Qiskit with regression bug; n_gates Qiskit with regression bug ; n_cx_gates Qiskit with regression bug")
        for i in range(len(nb_vars_t)):
            print (nb_vars_t[i], ";" ,nb_qb_qi[i], ";", nb_gates_qi[i]['u3'] + 
                nb_gates_qi[i]['cx'], ";",  nb_gates_qi[i]['cx'], ";", nb_qb_mi[i], ";", 
                nb_gates_mi[i]['u3'] + nb_gates_mi[i]['cx'], ";", nb_gates_mi[i]['cx'], ";", nb_qb_bq[i], ";", 
                nb_gates_bq[i]['u3'] + nb_gates_bq[i]['cx'], ";", nb_gates_bq[i]['cx'])
        sys.stdout = original_stdout 

def weightedAdder(nb_vars_max = 32):
    weights = [1, 2, 3, 2, 5, 6, 5, 3, 4, 5, 8, 2, 3, 6, 7, 5, 4, 3, 4, 9, 4, 5, 7, 1, 2, 3, 4, 5, 6, 7, 8, 6]

    if nb_vars_max > len(weights):
        print("More variables than weights. Weigths should be changed")
        return

    from unqomp.examples.weightedadder import makeWeightedAdderWOExtraCtrlsQb, makesQiskitWA, makeWeightedAdder
    from qiskit.circuit.library.arithmetic import WeightedAdder

    nb_vars_t = []
    nb_qb_qi = []
    nb_qb_mi = []
    nb_qb_bq = []
    nb_qb_mib = []
    nb_gates_qi = []
    nb_gates_mi = []
    nb_gates_bq = []
    nb_gates_mib = []

    for nb_vars in range(3, nb_vars_max, 3):
        circuit1 = makeWeightedAdderWOExtraCtrlsQb(nb_vars, weights[:nb_vars])
        circuit1 = circuit1.circuitWithUncomputation()

        qiskitMCX = makesQiskitWA(nb_vars, weights[:nb_vars])
        
        buggyQ = WeightedAdder(nb_vars, weights[:nb_vars])
        
        circuit2 = makeWeightedAdder(nb_vars, weights[:nb_vars])
        circuit2 = circuit2.circuitWithUncomputation()
        
        nb_vars_t.append(nb_vars)
        #qiskit
        nb_qb_qi.append(qiskitMCX.num_qubits)
        nb_gates_qi.append(qiskitMCX.decompose().decompose().decompose().decompose().decompose().count_ops())

        #mine
        nb_qb_mi.append(circuit1.num_qubits)
        nb_gates_mi.append(circuit1.decompose().decompose().decompose().decompose().decompose().count_ops())
        
        #buggy qiskit
        nb_qb_bq.append(buggyQ.num_qubits)
        nb_gates_bq.append(buggyQ.decompose().decompose().decompose().decompose().decompose().count_ops())
        
        #mine w more qb
        nb_qb_mib.append(circuit2.num_qubits)
        nb_gates_mib.append(circuit2.decompose().decompose().decompose().decompose().decompose().count_ops())

    with open('evaluation/plot_values/weightedadder.csv', 'w') as f:
        sys.stdout = f # Change the standard output to the file we created.
        print("input size; n_qb Qiskit without regression bug; n_gates Qiskit without regression bug; n_cx_gates Qiskit without regression bug;  n_qb Unqomp; n_gates Unqomp ; n_cx_gates Uncomp; n_qb Qiskit with regression bug; n_gates Qiskit with regression bug ; n_cx_gates Qiskit with regression bug; n_qb Unqomp alt. impl.; n_gates Unqomp alt. impl.; n_cx_gates Unqomp alt. impl.")
        for i in range(len(nb_vars_t)):
            print (nb_vars_t[i], ";" ,nb_qb_qi[i], ";", nb_gates_qi[i]['u3'] + 
                nb_gates_qi[i]['cx'], ";",  nb_gates_qi[i]['cx'], ";", nb_qb_mi[i], ";", 
                nb_gates_mi[i]['u3'] + nb_gates_mi[i]['cx'], ";", nb_gates_mi[i]['cx'], ";", nb_qb_bq[i], ";", 
                nb_gates_bq[i]['u3'] + nb_gates_bq[i]['cx'], ";", nb_gates_bq[i]['cx'], ";", nb_qb_mib[i], ";", 
                nb_gates_mib[i]['u3'] + nb_gates_mib[i]['cx'], ";", nb_gates_mib[i]['cx'])
        sys.stdout = original_stdout # Reset the standard output to its original value
        

def piecewiseLinearRot(fixed_nb_breakpoints, nb_vars_max = 14):
    from unqomp.examples.piecewiselinrot import makesPLR # poly linear rot
    from qiskit.circuit.library.arithmetic.piecewise_linear_pauli_rotations import PiecewiseLinearPauliRotations

    breakpoints = [1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 14]
    slopes = [1, 2, 3, 4, 3, 4, 3, 4, 5, 6, 4]
    offsets =  [2, 5, 7, 5, 5, 6, 5, 6, 6, 3, 4]

    if nb_vars_max > 9 or not fixed_nb_breakpoints:
        sys.setrecursionlimit(1800)

    if fixed_nb_breakpoints:
        nb_vars_t = []
        nb_qb_qi = []
        nb_qb_mi = []
        nb_gates_qi = []
        nb_gates_mi = []


        for nb_vars in range(4, nb_vars_max):
            nb_vars_t.append(nb_vars)
            qcirc = PiecewiseLinearPauliRotations(nb_vars, breakpoints, slopes, offsets)
            #qcirc = circuitQiskit.construct_circuit()
            #qiskit
            nb_qb_qi.append(qcirc.num_qubits)
            nb_gates_qi.append(qcirc.decompose().decompose().decompose().decompose().decompose().count_ops())

            #mine
            djcirc = makesPLR(nb_vars, breakpoints, slopes, offsets)
            djcirc = djcirc.circuitWithUncomputation()
            nb_qb_mi.append(djcirc.num_qubits)
            nb_gates_mi.append(djcirc.decompose().decompose().decompose().decompose().decompose().count_ops())
        
        with open('evaluation/plot_values/piecewiselinearrotfixedbreakpoints.csv', 'w') as f:
            sys.stdout = f # Change the standard output to the file we created.
            print("input size; n_qb Qiskit; n_gates Qiskit; n_cx_gates Qiskit; n_qb Unqomp; n_gates Unqomp ; n_cx_gates Uncomp")
            for i in range(len(nb_vars_t)):
                print (nb_vars_t[i], ";" ,nb_qb_qi[i], ";", nb_gates_qi[i]['u3'] + 
                    nb_gates_qi[i]['cx'], ";",  nb_gates_qi[i]['cx'], ";", nb_qb_mi[i], ";", 
                    nb_gates_mi[i]['u3'] + nb_gates_mi[i]['cx'], ";", nb_gates_mi[i]['cx'])
            sys.stdout = original_stdout # Reset the standard output to its original value
 

    else:
        if nb_vars_max > len(breakpoints) + 1:
            print("more breaks than breakpoints, changed nb_breaks to len(breakpoints")
            nb_vars_max = len(breakpoints) + 1

        nbbreaks = []
        nb_qb_qi = []
        nb_qb_mi = []
        nb_gates_qi = []
        nb_gates_mi = []
        nb_vars = 12

        for nb_breaks in range(2, nb_vars_max):
            nbbreaks.append(nb_breaks)
            qcirc = PiecewiseLinearPauliRotations(nb_vars, breakpoints[:nb_breaks], slopes[:nb_breaks], offsets[:nb_breaks])

            #qiskit
            nb_qb_qi.append(qcirc.num_qubits)
            nb_gates_qi.append(qcirc.decompose().decompose().decompose().decompose().decompose().count_ops())

            #mine
            djcirc = makesPLR(nb_vars, breakpoints[:nb_breaks], slopes[:nb_breaks], offsets[:nb_breaks])
            djcirc = djcirc.circuitWithUncomputation()
            nb_qb_mi.append(djcirc.num_qubits)
            nb_gates_mi.append(djcirc.decompose().decompose().decompose().decompose().decompose().count_ops())

        with open('evaluation/plot_values/piecewiselinearrotfixedinputsize.csv', 'w') as f:
            sys.stdout = f # Change the standard output to the file we created.
            print("n_breakpoints; n_qb Qiskit; n_gates Qiskit; n_cx_gates Qiskit;  n_qb Unqomp; n_gates Unqomp ; n_cx_gates Uncomp")
            for i in range(len(nbbreaks)):
                print (nbbreaks[i], ";" ,nb_qb_qi[i], ";", nb_gates_qi[i]['u3'] + 
                    nb_gates_qi[i]['cx'], ";",  nb_gates_qi[i]['cx'], ";", nb_qb_mi[i], ";", 
                    nb_gates_mi[i]['u3'] + nb_gates_mi[i]['cx'], ";", nb_gates_mi[i]['cx'])
            sys.stdout = original_stdout # Reset the standard output to its original value 
        
def polyPauliRotation(nb_vars_max = 15):
    from unqomp.examples.polynomialpaulirot import makesQiskitPolyPauliRot
    from unqomp.examples.polynomialpaulirot import makesPolyPauliRot
    from qiskit.circuit.library import PolynomialPauliRotations
    #from unqomp.examples.mcx import makeQiskitMCRY

    nb_vars_t4 = []
    nb_qb_qi4 = []
    nb_qb_mi4 = []
    nb_gates_qi4 = []
    nb_gates_mi4 = []
    nb_qb_bq4 = []
    nb_gates_bq4 = []

    coeffs = [1 for i in range(nb_vars_max)]

    for nb_vars in range(3, nb_vars_max):
        circuit1 = makesPolyPauliRot(7, coeffs[:nb_vars]).circuitWithUncomputation()

        qiskitMCX = makesQiskitPolyPauliRot(7, coeffs[:nb_vars])
        
        buggyQi = PolynomialPauliRotations(7, coeffs[:nb_vars])
        
        nb_vars_t4.append(nb_vars)
        #qiskit
        nb_qb_qi4.append(qiskitMCX.num_qubits)
        nb_gates_qi4.append(qiskitMCX.decompose().decompose().decompose().decompose().decompose().count_ops())
        
        #bad qiskit
        nb_qb_bq4.append(buggyQi.num_qubits)
        nb_gates_bq4.append(buggyQi.decompose().decompose().decompose().decompose().decompose().count_ops())

        #mine
        nb_qb_mi4.append(circuit1.num_qubits)
        nb_gates_mi4.append(circuit1.decompose().decompose().decompose().decompose().decompose().count_ops())

        with open('evaluation/plot_values/polynomialpaulirotation.csv', 'w') as f:
            sys.stdout = f # Change the standard output to the file we created.
            print("input size; n_qb Qiskit without regression bug; n_gates Qiskit without regression bug; n_cx_gates Qiskitwithout regression bug;  n_qb Unqomp; n_gates Unqomp ; n_cx_gates Uncomp; n_qb Qiskit with regression bug; n_gates Qiskit with regression bug ; n_cx_gates Qiskit with regression bug")
            for i in range(len(nb_vars_t4)):
                print (nb_vars_t4[i], ";" ,nb_qb_qi4[i], ";", nb_gates_qi4[i]['u3'] + 
                    nb_gates_qi4[i]['cx'], ";",  nb_gates_qi4[i]['cx'], ";", nb_qb_mi4[i], ";", 
                    nb_gates_mi4[i]['u3'] + nb_gates_mi4[i]['cx'], ";", nb_gates_mi4[i]['cx'], ";", nb_qb_bq4[i], ";", 
                    nb_gates_bq4[i]['u3'] + nb_gates_bq4[i]['cx'], ";", nb_gates_bq4[i]['cx'])
            sys.stdout = original_stdout # Reset the standard output to its original value 
        

def recSeq(nb_vars_max = 15):
    from unqomp.examples.recursivecircuit import makeRecursiveCircuitUnqomp, makeRecursiveCircuitQiskit

    nb_vars_t4 = []
    nb_qb_qi4 = []
    nb_qb_mi4 = []
    nb_gates_qi4 = []
    nb_gates_mi4 = []

    for nb_vars in range(2, nb_vars_max):
        circuit1 = makeRecursiveCircuitUnqomp(nb_vars, 6).circuitWithUncomputation()

        (qiskitMCX, ancillas) = makeRecursiveCircuitQiskit(nb_vars, 6)
        
        nb_vars_t4.append(nb_vars)
        #qiskit
        nb_qb_qi4.append(qiskitMCX.num_qubits)
        for i in range(nb_vars + 1):
            qiskitMCX = qiskitMCX.decompose()

        nb_gates_qi4.append(qiskitMCX.count_ops())
        
        #mine
        nb_qb_mi4.append(circuit1.num_qubits)
        for i in range(nb_vars + 1):
            circuit1 = circuit1.decompose()
        nb_gates_mi4.append(circuit1.count_ops())
        
    with open('evaluation/plot_values/recursiveexample.csv', 'w') as f:
        sys.stdout = f # Change the standard output to the file we created.
        print("rec_depth; n_qb Qiskit; n_gates Qiskit; n_cx_gates Qiskit;  n_qb Unqomp; n_gates Unqomp ; n_cx_gates Uncomp")
        for i in range(len(nb_vars_t4)):
            print (nb_vars_t4[i], ";" ,nb_qb_qi4[i], ";", nb_gates_qi4[i]['u3'] + 
                nb_gates_qi4[i]['cx'], ";",  nb_gates_qi4[i]['cx'], ";", nb_qb_mi4[i], ";", 
                nb_gates_mi4[i]['u3'] + nb_gates_mi4[i]['cx'], ";", nb_gates_mi4[i]['cx'])
        sys.stdout = original_stdout # Reset the standard output to its original value 
        
# to get all the values for the plots
if all_values:
    mcry()
    weightedAdder()
    piecewiseLinearRot(True)
    piecewiseLinearRot(False, 12)
    polyPauliRotation()
    recSeq()

# for a fast run
if not all_values:
    mcry(5)
    weightedAdder(12)
    piecewiseLinearRot(True, 5)
    piecewiseLinearRot(False, 6)
    polyPauliRotation(5)
    recSeq(5)
