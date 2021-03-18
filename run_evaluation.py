import argparse

parser = argparse.ArgumentParser(description='Compares, for all examples implemented in unqomp/examples/, the circuits encoded without Unqomp and with it, as shown in Table 2. By default, outputs the % saved by Unqomp.')
parser.add_argument('--absolute', dest='relative_numbers', action='store_false',
                    help='outputs absolute values, as shown in Table 4 (in Supplemental Text)')
parser.set_defaults(relative_numbers=True)
args = parser.parse_args()

relative_numbers = args.relative_numbers

def print_relative_vals(qb_q, cx_q, u3_q, qb_u, cx_u, u3_u):
    perc_qb_saved = int(round((qb_u - qb_q) / qb_q * -100))
    perc_cx_saved = int(round((cx_u - cx_q) / cx_q * -100))
    perc_g_saved = int(round((cx_u + u3_u - cx_q - u3_q) / (cx_q + u3_q) * -100))
    print(str(perc_g_saved) + ' ; ' + str(perc_cx_saved) + ' ; ' + str(perc_qb_saved))

def dj(relative_numbers):
    import unqomp.examples.deutschjozsa as dj
    import numpy as np
    from qiskit import QuantumCircuit, QuantumRegister

    n = 10
    circuitQiskit = dj.QiskitDJ(n)
    qcirc = circuitQiskit.construct_circuit()

    print('Deutsch-Jozsa  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    (djcirc, varreg) = dj.makesDJ(n)
    djcirc = djcirc.circuitWithUncomputation()
    nb_qb_mi = djcirc.num_qubits
    nb_gates_mi = djcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not(relative_numbers):
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])
    
def mcry(relative_numbers):
    from unqomp.ancillaallocation import AncillaCircuit
    from qiskit import QuantumRegister, QuantumCircuit
    from unqomp.examples.mcx import makeQiskitMCRY

    n = 12
    ctrls = QuantumRegister(n, 'ctrls')
    target = QuantumRegister(1, 'target')
    circuit1 = AncillaCircuit(ctrls, target)
    circuit1.mcry(2.0, ctrls, target)
    circuit1 = circuit1.circuitWithUncomputation()

    qiskitMCRY = makeQiskitMCRY(2.0, n)
    
    print('MCRY with regression bug  ; ', end = '')
   
    #qiskit buggy
    ctrls2 = QuantumRegister(n, 'ctrls')
    target2 = QuantumRegister(1, 'target')
    anc = QuantumRegister(n - 2, 'anc')
    circuit2 = QuantumCircuit(ctrls2, target2, anc)
    circuit2.mcry(2.0, ctrls2,target2[0], anc, mode = 'basic')
    nb_qb_qi = circuit2.num_qubits
    nb_gates_qi = circuit2.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + str(' ; '), end = '')

    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

    #qiskit with bug fixed
    nb_qb_qi = qiskitMCRY.num_qubits
    nb_gates_qi = qiskitMCRY.decompose().decompose().decompose().decompose().decompose().count_ops()
    print('MCRY *, regression bug fixed  ; ', end = '')
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; '+ str(nb_gates_qi['cx']) + str(' ; '), end = '')
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))
    else:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])
    
    
def mcx(relative_numbers):
    from unqomp.ancillaallocation import AncillaCircuit
    from qiskit import QuantumRegister, QuantumCircuit

    n = 12

    print('MCX  ; ', end = '')
    #for qiskit 
    ctrls2 = QuantumRegister(n, 'ctrls')
    target2 = QuantumRegister(1, 'target')
    anc = QuantumRegister(n - 2, 'anc')
    circuit2 = QuantumCircuit(ctrls2, target2, anc)
    circuit2.mcx(ctrls2, target2, anc, mode = 'v-chain')
    nb_qb_qi = circuit2.num_qubits
    nb_gates_qi = circuit2.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')
    
    
    ctrls = QuantumRegister(n, 'ctrls')
    target = QuantumRegister(1, 'target')
    circuit1 = AncillaCircuit(ctrls, target)
    circuit1.mcx(ctrls, target)
    circuit1 = circuit1.circuitWithUncomputation()
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])
    
def grover(relative_numbers):
    import unqomp.examples.grover as grover

    n = 10

    (circuit1, working_bits) = grover.makesGroverCircuit(n)
    circuit1 = circuit1.circuitWithUncomputation()

    qcirc = grover.QiskitGroverCstOracle((1 << n) - 1, n).construct_circuit()
    
    print('Grover  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))
    
    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

def integercomparator(relative_numbers):
    from qiskit.circuit.library import IntegerComparator
    from unqomp.examples.intergercomparator import makeIntegerComparator

    n = 12
    v = 40

    circuit1 = makeIntegerComparator(n, v).circuitWithUncomputation()

    qcirc = IntegerComparator(n, v)
    print('IntegerComparator  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

def plr(relative_numbers):
    from unqomp.examples.piecewiselinrot import makesPLR 
    from qiskit.circuit.library.arithmetic.piecewise_linear_pauli_rotations import PiecewiseLinearPauliRotations

    import sys

    sys.setrecursionlimit(2000)

    breakpoints = [1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 14]
    slopes = [1, 2, 3, 4, 3, 4, 3, 4, 5, 6, 4]
    offsets =  [1, 2, 3, 4, 3, 4, 3, 4, 5, 6, 4]
    n = 12

    circuit1 = makesPLR(n, breakpoints, slopes, offsets).circuitWithUncomputation()

    qcirc = PiecewiseLinearPauliRotations(n, breakpoints, slopes, offsets)
    print('PiecewiseLinearR  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

def ppr(relative_numbers):
    from unqomp.examples.polynomialpaulirot import makesPolyPauliRot, makesQiskitPolyPauliRot
    from qiskit.circuit.library import PolynomialPauliRotations

    coeffs = [1,2,3,4,5,4,1,2,3,4,5]
    n = 8

    circuit1 = makesPolyPauliRot(8, coeffs).circuitWithUncomputation()

    qcirc = PolynomialPauliRotations(num_state_qubits=8, coeffs=coeffs)
    print('PolynomialPauliR with regression bug  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

    #qiskit with bug fixed
    qiskitMCX = makesQiskitPolyPauliRot(n, coeffs)
    nb_qb_qi = qiskitMCX.num_qubits
    nb_gates_qi = qiskitMCX.decompose().decompose().decompose().decompose().decompose().count_ops()
    print('PolynomialPauliR *, regression bug fixed ; ', end = '')
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + str(' ; '), end = '')
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))
    else:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

def wa(relative_numbers):
    from unqomp.examples.weightedadder import makeWeightedAdder, makesQiskitWA
    from qiskit.circuit.library.arithmetic import WeightedAdder

    vals = [1,2,3,2,5,6,5,3,4,5,8,2]
    n = 12

    circuit1 = makeWeightedAdder(n, vals).circuitWithUncomputation()

    qcirc = WeightedAdder(n, vals)
    print('WeightedAdder with regression bug  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

    #qiskit with bug fixed
    qiskitMCX = makesQiskitWA(n, vals)
    nb_qb_qi = qiskitMCX.num_qubits
    nb_gates_qi = qiskitMCX.decompose().decompose().decompose().decompose().decompose().count_ops()
    print('WeightedAdder *, regression bug fixed ; ', end = '')
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + str(' ; '), end = '')
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))
    else:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

def wasaveqb(relative_numbers):
    from unqomp.examples.weightedadder import makeWeightedAdderWOExtraCtrlsQb, makesQiskitWA
    from qiskit.circuit.library.arithmetic import WeightedAdder

    vals = [1,2,3,2,5,6,5,3,4,5,8,2]
    n = 12

    circuit1 = makeWeightedAdderWOExtraCtrlsQb(n, vals).circuitWithUncomputation()

    qcirc = WeightedAdder(n, vals)
    print('WeightedAdder alt. impl., with regression bug ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

    # qiskit with bug fixed
    qiskitMCX = makesQiskitWA(n, vals)
    nb_qb_qi = qiskitMCX.num_qubits
    nb_gates_qi = qiskitMCX.decompose().decompose().decompose().decompose().decompose().count_ops()
    print('WeightedAdder alt, impl. *, regression bug fixed  ; ', end = '')
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + str(' ; '), end = '')
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))
    else:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

def adder(relative_numbers):
    from unqomp.examples.adder import makesAdder, makesMult, makesCirqAdder, makesCirqMult

    n = 12

    circuit1 = makesAdder(n).circuitWithUncomputation()

    qcirc = makesCirqAdder(n)
    print('Adder  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

def mult(relative_numbers):
    from unqomp.examples.adder import makesAdder, makesMult, makesCirqAdder, makesCirqMult

    n = 12

    circuit1 = makesMult(n).circuitWithUncomputation()

    qcirc = makesCirqMult(n)
    print('Multiplier  ; ', end = '')

    #qiskit
    nb_qb_qi = qcirc.num_qubits
    nb_gates_qi = qcirc.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_qi) + ' ; ' + str(nb_gates_qi['cx'] + nb_gates_qi['u3']) + ' ; ' + str(nb_gates_qi['cx']) + ' ; ', end = '')

    #qiskit++
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    if not relative_numbers:
        print(str(nb_qb_mi) + ' ; ' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ' ; ' + str(nb_gates_mi['cx']))

    if relative_numbers:
        print_relative_vals(nb_qb_qi, nb_gates_qi['cx'], nb_gates_qi['u3'], nb_qb_mi, nb_gates_mi['cx'], nb_gates_mi['u3'])

if relative_numbers:
    print("Example name ; % gates saved by Unqomp ; % CX gates saved by Unqomp ; % qubits saved by Unqomp")
else:
    print('Example name  ;  number of qubits in Qiskit  ;  number of gates in Qiskit  ;  number of CX gates in Qiskit  ;  number of qubits with Unqomp  ;  number of gates with Unqomp  ;  number of CX gates with Unqomp')
adder(relative_numbers)
dj(relative_numbers)
grover(relative_numbers)
integercomparator(relative_numbers)
mcry(relative_numbers)
mcx(relative_numbers)
mult(relative_numbers)
plr(relative_numbers)
ppr(relative_numbers)
wa(relative_numbers)
wasaveqb(relative_numbers)
