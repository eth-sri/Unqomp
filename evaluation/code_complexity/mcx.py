#without unqomp taken from https://github.com/Qiskit/qiskit-terra/blob/d7e469e33c212e8acf39308497bd312cfe792ca6/qiskit/circuit/library/standard_gates/x.py
def mcx():
        q = QuantumRegister(self.num_qubits, name='q')
        q_controls = q[:self.num_ctrl_qubits]
        q_target = q[self.num_ctrl_qubits]
        q_ancillas = q[self.num_ctrl_qubits + 1:]
        definition = []
        definition.append((RCCXGate(), [q_controls[0], q_controls[1], q_ancillas[0]], []))
        i = 0
        for j in range(2, self.num_ctrl_qubits - 1):
            definition.append((RCCXGate(), [q_controls[j], q_ancillas[i], q_ancillas[i + 1]], []))
            i += 1
        definition.append((CCXGate(), [q_controls[-1], q_ancillas[i], q_target], []))
        for j in reversed(range(2, self.num_ctrl_qubits - 1)):
            definition.append((RCCXGate(), [q_controls[j], q_ancillas[i - 1], q_ancillas[i]], []))
            i -= 1
        definition.append((RCCXGate(), [q_controls[0], q_controls[1], q_ancillas[i]], []))
#mcx with unqomp 
def mcx():
	q_controls = QuantumRegister(n, name='ctrls')
    q_target = QuantumRegister(1, name='target')
    circuit = AncillaCircuit(q_controls, q_target)
    q_ancillas = circuit.new_ancilla_register(n-2, name = 'anc')
    circuit.addQfreeGate(RCCXGate())
    circuit.append(RCCXGate(), [q_controls[0], q_controls[1], q_ancillas[0]])
    i = 0
    for j in range(2, n - 1):
        circuit.append(RCCXGate(), [q_controls[j], q_ancillas[i], q_ancillas[i + 1]])
        i += 1
    circuit.append(RCCXGate(), [q_controls[-1], q_ancillas[i], q_target])
	return circuit.circuitWithUncomputation()
