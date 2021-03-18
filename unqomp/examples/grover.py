#with kick back, this is the good one

import numpy as np
from qiskit import QuantumCircuit, QuantumRegister
from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit, AncillaGate

from qiskit.aqua.components.oracles import CustomCircuitOracle
from qiskit.aqua.algorithms import Grover

def makesOracle(i, n):
    # Creates the oracle finding exactly i on n qubits (+ 1 for target)(or its lowest n bits if i >= 2^n)
    # Could use some uncomputation...
    ctrls = QuantumRegister(n)
    target = QuantumRegister(1) 
    fcirc = AncillaCircuit(ctrls, target, name="oracle_" + str(i) + "_" + str(n))
    format_str = '{0:0' + str(n) + 'b}'
    binary_i = format_str.format(i)[::-1]
    for j in range(n):
        if binary_i[j] == '0':
            fcirc.x(ctrls[j])
    fcirc.mcx(ctrls[:], target[0])
    for j in range(n):
        if binary_i[j] == '0':
            fcirc.x(ctrls[j])
    return fcirc.to_ancilla_gate()
    
def makesGroverCircuit(n, oracle = None, nb_sols = 1):
    # grover circuit on n qubits, without measurements as uncomp cannot deal with that yet
    nbIter = int(np.floor(np.pi / 4.0 * np.sqrt(pow(2, n))))

    working_qubits = QuantumRegister(n, name = 'r')
    phase_qubit = QuantumRegister(1, name = 'p')
    circ = AncillaCircuit(working_qubits, phase_qubit)
    circ.x(phase_qubit[0])
    circ.h(phase_qubit[0])

    circ.h(working_qubits)
        
    for l in range(nbIter):
        if oracle:
            circ.append(oracle, [*working_qubits[:], phase_qubit[0]])
        else:
            circ.mcx(working_qubits, phase_qubit)

        #Grover diffusion operator
        circ.h(working_qubits)

        circ.x(working_qubits)
        circ.h(working_qubits[-1])
        circ.mcx(working_qubits[:-1], working_qubits[-1])
        circ.h(working_qubits[-1])
        circ.x(working_qubits)
        
        circ.h(working_qubits)

    # bring the phase qubit back to 0, we can't uncompute it, as it went through cz, non qfree -> no need to uncomp it, Qiskit doesn't 
    #circ.h(phase_qubit[0])
    #circ.x(phase_qubit)
    return (circ, working_qubits)

def makesOracleForQiskit(i, n):
    # Creates the oracle finding exactly i on n qubits (+ 1 for target)(or its lowest n bits if i >= 2^n)
    # Could use some uncomputation...
    ctrls = QuantumRegister(n)
    target = QuantumRegister(1) 
    ancillas = QuantumRegister(n - 2)
    fcirc = QuantumCircuit(ctrls, target, ancillas, name="oracle_" + str(i) + "_" + str(n))
    format_str = '{0:0' + str(n) + 'b}'
    binary_i = format_str.format(i)[::-1]
    for j in range(n):
        if binary_i[j] == '0':
            fcirc.x(ctrls[j])
    
    fcirc.mct(ctrls, target, ancillas, mode = 'basic')
    for j in range(n):
        if binary_i[j] == '0':
            fcirc.x(ctrls[j])
    return (fcirc, ctrls, target, ancillas)

def makesClassicalEval(i, n, measurement):
    assignment = [(var + 1) * (int(tf) * 2 - 1) for tf, var in zip(measurement[::-1], range(len(measurement)))]
    format_str = '{0:0' + str(n) + 'b}'
    binary_i = format_str.format(i)[::-1]
    for j in range(n):
        if binary_i[j] != measurement[j]:
            return (False, assignment)
    return (True, assignment)

def QiskitGroverCstOracle(i, n):
    # takes care of creating the oracle and calling qiskit's aqua grover
    (oracle_circuit, ctrls, target, ancillas) = makesOracleForQiskit(i, n)
    classical_eval = lambda measurement : (makesClassicalEval(i, n, measurement))
    oracle = CustomCircuitOracle(variable_register = ctrls, output_register = target, ancillary_register = ancillas, circuit = oracle_circuit, evaluate_classically_callback = classical_eval)
    grover = Grover(oracle, init_state=None, incremental=False, num_iterations=int(np.floor(np.pi / 4.0 * np.sqrt(pow(2, n)))), mct_mode='basic', quantum_instance=None)
    return grover