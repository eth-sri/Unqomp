#without unqomp from https://github.com/quantumlib/Cirq/blob/4ca1c1da1dadbaf6412ead0796d1e917b3411693/examples/basic_arithmetic.p
def makesCircMult(num_qubits):
    b = QuantumRegister(num_qubits)
    y = QuantumRegister(num_qubits)
    x = QuantumRegister(num_qubits)
    a = QuantumRegister(num_qubits)
    anc = QuantumRegister(num_qubits)
    circuit = QuantumCircuit(b, y, x)
    for i, x_i in enumerate(x):
        for a_qubit, y_qubit in zip(a[i:], y[:n-i]):
            circuit.ccx(x_i, y_qubit, a_qubit)
        circuit.append(makesCirqAdder(num_qubits).to_gate(), [*a[:], *b[:], *anc[:]])
        for a_qubit, y_qubit in zip(a[i:], y[:n-i]):
            circuit.ccx(x_i, y_qubit, a_qubit)
#with unqomp
def makesMultCirc(num_qubits):
    b = QuantumRegister(num_qubits)
    y = QuantumRegister(num_qubits)
    x = QuantumRegister(num_qubits)
    circuit = AncillaCircuit(b, y, x)
    for i, x_i in enumerate(x):
        a = circuit.new_ancilla_register(num_qubits)
        for a_qubit, y_qubit in zip(a[i:], y[:num_qubits-i]):
            circuit.ccx(x_i, y_qubit, a_qubit)
        circuit.append(makesAdderCircuit(num_qubits).to_ancilla_gate(), [*a[:], *b[:]])
    return circuit
