import numpy as np
from qiskit import QuantumCircuit, QuantumRegister
from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit, AncillaGate

from qiskit.aqua.components.oracles import CustomCircuitOracle
from qiskit.aqua.algorithms.education import DeutschJozsa

def makesDJ(num_qubits, oracle_gate = None):
    #Builds the Deutsch Jozsa circuit for n + 1 qubits, finding the value 111...111
    var_reg = QuantumRegister(num_qubits, name = 'vals')
    out_reg = QuantumRegister(1, name = 'out')

    circ = AncillaCircuit(var_reg, out_reg)

    circ.h(var_reg)
    circ.x(out_reg)
    circ.h(out_reg)

    if oracle_gate:
        circ.append(oracle_gate, [*var_reg, out_reg[0]])
    else:
        circ.mcx(var_reg, out_reg)

    circ.h(var_reg)

    return (circ, var_reg)

def makesClassicalEval(i, n, measurement):
    print("measurement in classical eval " + measurement)
    assignment = [(var + 1) * (int(tf) * 2 - 1) for tf, var in zip(measurement[::-1], range(len(measurement)))]
    format_str = '{0:0' + str(n) + 'b}'
    binary_i = format_str.format(i)[::-1]
    print("in classical eval i " + binary_i)
    for j in range(n):
        if binary_i[j] != measurement[j]:
            return (False, assignment)
    return (True, assignment)

def makesOracleForQiskit(n):
    # Creates the oracle finding exactly 111....111 on n qubits (+ 1 for target)
    ctrls = QuantumRegister(n)
    target = QuantumRegister(1) 
    ancillas = QuantumRegister(n - 2)

    fcirc = QuantumCircuit(ctrls, target, ancillas, name="oracle_" + str(n))
    fcirc.mcx(ctrls, target, ancillas, mode = 'v-chain')

    return (fcirc, ctrls, target, ancillas)

def QiskitDJ(n):
    # takes care of creating the oracle and calling qiskit's aqua Deutsch Jozsa, for the oracle finding the value 111...111
    (oracle_circuit, ctrls, target, ancillas) = makesOracleForQiskit(n)
    classical_eval = lambda measurement : (makesClassicalEval((2 << n) - 1, n, measurement))
    oracle = CustomCircuitOracle(variable_register = ctrls, output_register = target, ancillary_register = ancillas, circuit = oracle_circuit, evaluate_classically_callback = classical_eval)
    dj = DeutschJozsa(oracle, quantum_instance=None)
    return dj