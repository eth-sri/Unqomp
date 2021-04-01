from qiskit import QuantumCircuit,  QuantumRegister

# we build the Qiskit circuit equivalent to the Quipper gate, and decompose it to basic gates u3 and cx.
def CCXDecomposition():
    circuit = QuantumCircuit(3)
    circuit.x(0)
    circuit.x(1)
    circuit.ccx(0, 1, 2)
    circuit.x(0)
    circuit.x(1)
    nb_qb_mi = circuit.num_qubits
    nb_gates_mi = circuit.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("not, arity 1, controls 0+2; " + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

    circuit = QuantumCircuit(3)
    circuit.x(1)
    circuit.ccx(0, 1, 2)
    circuit.x(1)
    nb_qb_mi = circuit.num_qubits
    nb_gates_mi = circuit.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("not, arity 1, controls 1+1; " + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))
    
    circuit = QuantumCircuit(3)
    circuit.ccx(0, 1, 2)
    nb_qb_mi = circuit.num_qubits
    nb_gates_mi = circuit.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("not, arity 1, controls 2; " + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

    circuit = QuantumCircuit(2)
    circuit.cx(0, 1)
    nb_qb_mi = circuit.num_qubits
    nb_gates_mi = circuit.decompose().decompose().decompose().decompose().decompose().count_ops()
    if(not nb_gates_mi.get('u3')):
            nb_gates_mi['u3'] = 0
    print("not, arity 1, controls 1; " + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

    circuit = QuantumCircuit(2)
    circuit.x(0)
    circuit.cx(0, 1)
    circuit.x(1)
    nb_qb_mi = circuit.num_qubits
    nb_gates_mi = circuit.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("not, arity 1, controls 0+1; " + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

    print("Init1; 1; 0")
    print("Term1; 1; 0")
    print("H, arity 1; 1; 0")
    print("not, arity 1; 1; 0")
print("Quipper gate; number of gates in Qiskit; number of CX gates in Qiskit")
CCXDecomposition()