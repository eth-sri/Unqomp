from qiskit.circuit import QuantumRegister, QuantumCircuit

from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

def makesAdder(num_qubits):
    #[a, b, s]: s = a + b, a and b made of num_qubits each
    # returns an AncillaCircuit, without uncomputation
    def neg_mct_gate(lneg):
        neg_mct = AncillaCircuit(3)
        for i in lneg:
            neg_mct.x(i)
        neg_mct.ccx(0, 1, 2)
        for i in lneg:
            neg_mct.x(i)
        return neg_mct.to_ancilla_gate(True)

    a = QuantumRegister(num_qubits, name = "a")
    b = QuantumRegister(num_qubits, name = "b")
    s = QuantumRegister(num_qubits, name = "s")
    c = AncillaRegister(num_qubits, name = "c")

    circuit = AncillaCircuit(a, b, s, c)

    for i in range(num_qubits-1):
        #sum
        t = circuit.new_ancilla_register(1, name = 't' + str(i))
        circuit.cx(a[i], t)
        circuit.cx(b[i], t) #to be quipper like
        circuit.cx(t, s[i])
        circuit.cx(c[i], s[i])

        #carry
        v = circuit.new_ancilla_register(1, name = 'v' + str(i))
        circuit.append(neg_mct_gate([0, 1]), [a[i], b[i], v])
        circuit.append(neg_mct_gate([1]), [v, c[i], c[i+1]])
        circuit.x(c[i+1])

    circuit.cx(a[num_qubits-1], s[num_qubits-1])
    circuit.cx(b[num_qubits-1], s[num_qubits-1])
    circuit.cx(c[num_qubits-1], s[num_qubits-1])

    return circuit
