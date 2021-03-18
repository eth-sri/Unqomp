#without unqomp, from https://github.com/quantumlib/Cirq/blob/4ca1c1da1dadbaf6412ead0796d1e917b3411693/examples/basic_arithmetic.p
def makesCirqAdder(num_qubits):
    def carry_gate():
        carry_circ = QuantumCircuit(4)
        c0, a, b, c1 = carry_circ[0], carry_circ[1], carry_circ[2], carry_circ[3]
        carry_circ.ccx(a, b, c1)
        carry_circ.cx(a, b)
        carry_circ.ccx(c0, b, c1)
        return carry_circ.to_gate()
    def uncarry():
        uncarry_circ = QuantumCircuit(4)
        c0, a, b, c1 = uncarry_circ[0], uncarry_circ[1], uncarry_circ[2], uncarry_circ[3]
        uncarry_circ.ccx(c0, b, c1)
        uncarry_circ.cx(a, b)
        uncarry_circ.ccx(a, b, c1)
        return uncarry_circ.to_gate()
    def carry_sum(self, *qubits):
        sum_circ = QuantumCircuit(3)
        c0, a, b = sum_circ.qubits[0], sum_circ.qubits[1], sum_circ.qubits[2]
        sum_circ.cx(a, b)
        sum_circ.cx(c0, b)
    a = QuantumRegister(num_qubits, name = "a")
    b = QuantumRegister(num_qubits, name = "b")
    c = AncillaRegister(num_qubits, name = "c")
    circuit = QuantumCircuit(a, b, c)
    for i in range(n-1):
        circuit.append(carry_gate(), [c[i], a[i], b[i], c[i+1]])
    circuit.append(carry_sum(), [c[n-1], a[n-1], b[n-1]])
    for i in range(n-2, -1, -1):
        circuit.append(uncarry(), [c[i], a[i], b[i], c[i+1]])
        circuit.append(carry_sum(), [c[i], a[i], b[i]])
    return circuit

# with unqomp
def makesAdderCircuit(num_qubits):
   def neg_mct_gate():
        neg_mct = AncillaCircuit(4)
        neg_mct.cx(1, 2)
        neg_mct.ccx(0, 2, 3)
        neg_mct.cx(1, 2)
        return neg_mct.to_ancilla_gate(True)
    def carry_gate():
        carry_circ = AncillaCircuit(4)
        (c0, a, b, c1) = (carry_circ.qubits[0], carry_circ.qubits[1], carry_circ.qubits[2], carry_circ.qubits[3])
        carry_circ.ccx(a, b, c1)
        carry_circ.append(neg_mct_gate(), [c0, a, b, c1])
        return carry_circ.to_ancilla_gate()
    def sum_gate():
        sum_circ = AncillaCircuit(3)
        (c0, a, b) = (sum_circ.qubits[0], sum_circ.qubits[1], sum_circ.qubits[2])
        sum_circ.cx(a, b)
        sum_circ.cx(c0, b)
        return sum_circ.to_ancilla_gate()
    a = QuantumRegister(num_qubits, name = "a")
    b = QuantumRegister(num_qubits, name = "b")
    c = AncillaRegister(num_qubits, name = "c")
    circuit = AncillaCircuit(a, b, c)
    for i in range(num_qubits-1):
        circuit.append(carry_gate(), [c[i], a[i], b[i], c[i+1]])
        circuit.append(sum_gate(), [c[i], a[i], b[i]])
    circuit.append(sum_gate(), [c[num_qubits-1], a[num_qubits-1], b[num_qubits-1]])
    return circuit
