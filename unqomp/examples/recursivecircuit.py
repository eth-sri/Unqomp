from qiskit.circuit import QuantumRegister, QuantumCircuit
import numpy as np

from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

def makesFGate(m, auto_uncomp = True):
    # takes as input m bits, outputs m bits.
    # for now : 2x+1
    qr_input = QuantumRegister(m, "input")
    qr_output = QuantumRegister(m, "output")
    circuit = QuantumCircuit(qr_input, qr_output, name = "f")
    for i in range(m - 1):
        circuit.cx(qr_input[i], qr_output[i + 1])
    circuit.x(qr_output[0])
    return circuit.to_gate()

def makeRecursiveCircuitQiskit(n, m):
    # creates (recursively) a circuit to compute u_n where u_0 is the input on m bits, and u_(n+1) = f(u_n), with manual uncomputation
    f_gate = makesFGate(m, False)
    qr_input = QuantumRegister(m, "input")
    qr_output = QuantumRegister(m, "output")

    if n == 1:
        circuit = QuantumCircuit(qr_input, qr_output)
        circuit.append(f_gate, [*qr_input, *qr_output])
        return (circuit, [])

    qr_ancillas = QuantumRegister(m, "ancilla-" + str(n))
    circuit = QuantumCircuit(qr_input, qr_output, qr_ancillas)
    (rec_circ, ancilla_list) = makeRecursiveCircuitQiskit(n - 1, m)
    rec_gate = rec_circ.to_gate()

    old_ancillas = [anc for qr_anc in ancilla_list for anc in qr_anc]
    for qr_anc in ancilla_list:
        circuit.add_register(qr_anc)
    circuit.append(rec_gate, [*qr_input, *qr_ancillas, *old_ancillas])
    circuit.append(f_gate, [*qr_ancillas, *qr_output])

    circuit.append(rec_gate.inverse(), [*qr_input, *qr_ancillas, *old_ancillas])

    ancilla_list.append(qr_ancillas)
    return (circuit, ancilla_list)

def makeRecursiveCircuitUnqomp(n, m):
    # creates (recursively) a circuit to compute u_n where u_0 is the input on m bits, and u_(n+1) = f(u_n), returns an AncillaCircuit, without uncomputation
    f_gate = makesFGate(m, True)
    qr_input = QuantumRegister(m, "input")
    qr_output = QuantumRegister(m, "output")

    if n == 1:
        circuit = AncillaCircuit(qr_input, qr_output)
        circuit.append(f_gate, [*qr_input, *qr_output])
        return circuit

    qr_ancillas = AncillaRegister(m, "ancilla-" + str(n))
    circuit = AncillaCircuit(qr_input, qr_output, qr_ancillas)
    rec_circ = makeRecursiveCircuitUnqomp(n - 1, m)
    rec_gate = rec_circ.to_ancilla_gate()

    circuit.append(rec_gate, [*qr_input, *qr_ancillas])
    circuit.append(f_gate, [*qr_ancillas, *qr_output])

    return circuit
